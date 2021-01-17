-module(offers_handler).
-import(util, [response/3, generate_id/1, db/2]).
-behavior(cowboy_handler).

-export([init/2]).

init(Req=#{method := <<"GET">>}, State) ->
	Params = cowboy_req:parse_qs(Req),
	{Status, Values} = case Params of 
		[{<<"id">>, OfferId}] ->
			RecordId1 = binary_to_list(OfferId),
			Records = db(offers, fun() -> dets:lookup(records_db, RecordId1) end),
			case Records of
				[{RecordId2, Data}] ->
					{200, offer_to_json({RecordId2, Data})};
				[] ->
					{404, io_lib:format("{\"not_found\": \"record ~s not found\"}", [RecordId1])};
				_ ->
					{500, io_lib:format("{\"extra_records\": \"extra records for ~s\"}", [RecordId1])}
			end;
		[] -> 
			F = fun (Offer, Acc) -> Acc1 = [offer_to_json(Offer) | Acc], Acc1 end,
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
    {ok, [{<<"content">>, Content}], _} = cowboy_req:read_urlencoded_body(Req),
    RecordId = generate_id(offers),
	Action  = fun () -> 
    	ok = dets:insert(records_db, {RecordId, Content}),
    	dets:sync(records_db)
		end,
	ok = db(offers, Action),
	Res = response(201, RecordId, Req),
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

offer_to_json({Id, Content}) ->
	lists:flatten(io_lib:format("{\"id\": \"~s\", \"record\": \"~s\"},", [Id, binary_to_list(Content)])).


