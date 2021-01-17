-module(offers_handler).
-import(util, [response/3, map_to_json/1]).
-import(db, [find_by_id/2, insert/3, update/3, delete/2, generate_id/1, db/2]).
-behavior(cowboy_handler).

-export([init/2]).

init(Req=#{method := <<"GET">>}, State) ->
	Params = cowboy_req:parse_qs(Req),
	{Status, Values} = case Params of 
		[{<<"id">>, OfferId}] ->
			find_by_id(offers, binary_to_list(OfferId));
		[] -> 
			F = fun (Offer, Acc) -> Acc1 = [map_to_json(Offer) | Acc], Acc1 end,
			Items = db(offers, fun() -> dets:foldl(F, [], records_db) end),
			Items1 = lists:sort(Items),
			Body = "
			{
				\"offers\": [~s]
			}",
			{200, io_lib:format(Body, [Items1])}
	end,
    Res = response(Status, Values, Req),
    {ok, Res, State};

init(Req=#{method := <<"POST">>}, State) ->
	OfferId = generate_id(offers),
	Offer = offer_from_body(OfferId, Req),
	Inserted = insert(offers, OfferId, Offer),
	Res = response(201, Inserted, Req),
    {ok, Res, State};

init(Req=#{method := <<"PUT">>}, State) ->
	[{<<"id">>, OfferIdRaw}] = cowboy_req:parse_qs(Req),
	OfferId = binary_to_list(OfferIdRaw),
	Offer = offer_from_body(OfferId, Req),
	Updated = update(offers, OfferId, Offer),
    Res = response(200, Updated, Req),
    {ok, Res, State};

init(Req=#{method := <<"DELETE">>}, State) ->
	[{<<"id">>, OfferId}] = cowboy_req:parse_qs(Req),
	delete(offers, binary_to_list(OfferId)),
    {ok, response(204, <<"">>, Req), State};

init(Req, State) ->
    Res = cowboy_req:reply(405, #{}, Req),
    {ok, Res, State}.

offer_from_body(Id, Req) -> 
	{ok, [
		{<<"name">>, Name},
		{<<"price">>, Price},
		{<<"description">>, Description}
	], _ } = cowboy_req:read_urlencoded_body(Req),
	#{
		id => {Id, text},
		name => {Name, text},
		price => {list_to_float(binary_to_list(Price)), float},
		description => {Description, text}
	}.
