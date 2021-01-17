-module(orders_handler).
-import(util, [response/3, map_to_json/1, generate_id/1, db/2]).
-behavior(cowboy_handler).

-export([init/2]).

init(Req=#{method := <<"GET">>}, State) ->
	Params = cowboy_req:parse_qs(Req),
	{Status, Values} = case Params of 
		[{<<"id">>, OrderId}] ->
			find_order_by_id(binary_to_list(OrderId));
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
	RecordId = generate_id(orders),
    {ok, [
		{<<"offerId">>, OfferId},
		{<<"quantity">>, Quantity},
		{<<"amount">>, Amount},
		{<<"buyer">>, Buyer}
	], _ } = cowboy_req:read_urlencoded_body(Req),
	Order = #{
			id => {RecordId, text},
			offer_id => {OfferId, text},
			quantity => {Quantity, float},
			amount => {Amount, float},
			buyer => {Buyer, text}
		},
	ok = db(orders, fun () -> 
    	ok = dets:insert(records_db, {RecordId, Order}),
    	dets:sync(records_db)
	end),
	{200, Inserted} = find_order_by_id(RecordId),
	Res = response(201, Inserted, Req),
    {ok, Res, State};

init(Req=#{method := <<"PUT">>}, State) ->
    Res = cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/plain">>
    }, <<"Hello world!">>, Req),
    {ok, Res, State};

init(Req=#{method := <<"DELETE">>}, State) ->
	[{<<"id">>, OrderId}] = cowboy_req:parse_qs(Req),
    RecordId1 = binary_to_list(OrderId),
	ok = db(orders, fun () -> dets:delete(records_db, RecordId1) end),
    {ok, response(204, <<"">>, Req), State};

init(Req, State) ->
    Res = cowboy_req:reply(405, #{}, Req),
    {ok, Res, State}.

find_order_by_id(OrderId) -> 
	Orders = db(orders, fun() -> dets:lookup(records_db, OrderId) end),
	case Orders of
		[{OrderId2, Data}] ->
			{200, map_to_json({OrderId2, Data})};
		[] ->
			{404, io_lib:format("{\"not_found\": \"record ~s not found\"}", [OrderId])};
		_ ->
			{500, io_lib:format("{\"extra_records\": \"extra records for ~s\"}", [OrderId])}
	end.
