-module(offers_handler).
-import(util, [response/3, map_to_json/1, generate_id/1, db/2]).
-behavior(cowboy_handler).

-export([init/2]).

init(Req=#{method := <<"GET">>}, State) ->
	Params = cowboy_req:parse_qs(Req),
	{Status, Values} = case Params of 
		[{<<"id">>, OfferId}] ->
			find_offer_by_id(binary_to_list(OfferId));
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
	RecordId = generate_id(offers),
    {ok, [
		{<<"name">>, Name},
		{<<"price">>, Price},
		{<<"description">>, Description}
	], _ } = cowboy_req:read_urlencoded_body(Req),
	Offer = #{
			id => {RecordId, text},
			name => {Name, text},
			price => {Price, float},
			description => {Description, text}
		},
	ok = db(offers, fun () -> 
    	ok = dets:insert(records_db, {RecordId, Offer}),
    	dets:sync(records_db)
	end),
	{200, Inserted} = find_offer_by_id(RecordId),
	Res = response(201, Inserted, Req),
    {ok, Res, State};

init(Req=#{method := <<"PUT">>}, State) ->
    Res = cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/plain">>
    }, <<"Hello world!">>, Req),
    {ok, Res, State};

init(Req=#{method := <<"DELETE">>}, State) ->
	[{<<"id">>, OfferId}] = cowboy_req:parse_qs(Req),
    RecordId1 = binary_to_list(OfferId),
	ok = db(offers, fun () -> dets:delete(records_db, RecordId1) end),
    {ok, response(204, <<"">>, Req), State};

init(Req, State) ->
    Res = cowboy_req:reply(405, #{}, Req),
    {ok, Res, State}.

find_offer_by_id(OfferId) -> 
	Offers = db(offers, fun() -> dets:lookup(records_db, OfferId) end),
	case Offers of
		[{OfferId2, Data}] ->
			{200, map_to_json({OfferId2, Data})};
		[] ->
			{404, io_lib:format("{\"not_found\": \"record ~s not found\"}", [OfferId])};
		_ ->
			{500, io_lib:format("{\"extra_records\": \"extra records for ~s\"}", [OfferId])}
	end.