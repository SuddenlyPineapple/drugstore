-module(orders_handler).
-import(util, [response/3, map_to_json/1]).
-import(db, [find_by_id/2, insert/3, update/3, delete/2, generate_id/1, db/2]).
-behavior(cowboy_handler).

-export([init/2]).

init(Req=#{method := <<"GET">>}, State) ->
	Params = cowboy_req:parse_qs(Req),
	{Status, Values} = case Params of 
		[{<<"id">>, OrderId}] ->
			find_by_id(orders, binary_to_list(OrderId));
		[] -> 
			F = fun (Order, Acc) -> Acc1 = [map_to_json(Order) | Acc], Acc1 end,
			Items = db(orders, fun() -> dets:foldl(F, [], records_db) end),
			Items1 = lists:sort(Items),
			Body = "
			{
				\"orders\": [~s]
			}",
			{200, io_lib:format(Body, [Items1])}
	end,
    Res = response(Status, Values, Req),
    {ok, Res, State};

init(Req=#{method := <<"POST">>}, State) ->
	OrderId = generate_id(orders),
	Order = order_from_body(OrderId, Req),
	Inserted = insert(orders, OrderId, Order),
	Res = response(201, Inserted, Req),
    {ok, Res, State};

init(Req=#{method := <<"PUT">>}, State) ->
	[{<<"id">>, OrderIdRaw}] = cowboy_req:parse_qs(Req),
	OrderId = binary_to_list(OrderIdRaw),
	Order = order_from_body(OrderId, Req),
	Updated = update(orders, OrderId, Order),
    Res = response(200, Updated, Req),
    {ok, Res, State};

init(Req=#{method := <<"DELETE">>}, State) ->
	[{<<"id">>, OrderId}] = cowboy_req:parse_qs(Req),
	delete(orders, binary_to_list(OrderId)),
    {ok, response(204, <<"">>, Req), State};

init(Req, State) ->
    Res = cowboy_req:reply(405, #{}, Req),
    {ok, Res, State}.

order_from_body(Id, Req) -> 
	{ok, [
		{<<"offerId">>, OfferId},
		{<<"quantity">>, Quantity},
		{<<"amount">>, Amount},
		{<<"buyer">>, Buyer}
	], _ } = cowboy_req:read_urlencoded_body(Req),
	#{
		id => {Id, text},
		offer_id => {OfferId, text},
		quantity => {Quantity, float},
		amount => {Amount, float},
		buyer => {Buyer, text}
	}.