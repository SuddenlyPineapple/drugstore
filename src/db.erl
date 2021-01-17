-module(db).
-import(util, [map_to_json/1]).
-export([find_by_id/2, insert/3, update/3, delete/2, generate_id/1, db/2]).

find_by_id(Collection, RecordId) -> 
	Records = db(Collection, fun() -> dets:lookup(records_db, RecordId) end),
	case Records of
		[{RecordId2, Data}] ->
			{200, map_to_json({RecordId2, Data})};
		[] ->
			{404, io_lib:format("{\"not_found\": \"record ~s not found\"}", [RecordId])};
		_ ->
			{500, io_lib:format("{\"extra_records\": \"extra records for ~s\"}", [RecordId])}
	end.

insert(Collection, Id, Record) -> 
	ok = db(Collection, fun () -> 
    	ok = dets:insert(records_db, {Id, Record}),
    	dets:sync(records_db)
	end),
	{200, Inserted} = find_by_id(Collection, Id),
	Inserted.

update(Collection, Id, Record) ->
	{200, _} = find_by_id(Collection, Id),
	delete(Collection, Id),
	insert(Collection, Id, Record).

delete(Collection, RecordId) ->
	ok = db(Collection, fun () -> dets:delete(records_db, RecordId) end).


generate_id(Collection) ->
	Filename = case Collection of
		offers -> offers_state_file_name;
		orders -> orders_state_file_name
	end,
	{ok, Statefilename} = application:get_env(pw, Filename),
    dets:open_file(state_db, [{file, Statefilename}, {type, set}]),
    Records = dets:lookup(state_db, current_id),
    Response = case Records of
        [{current_id, CurrentId}] ->
            NextId = CurrentId + 1,
            dets:insert(state_db, {current_id, NextId}),
            lists:flatten(io_lib:format("id_~4..0B", [CurrentId]));
        [] ->
            error
    end,
    dets:close(state_db),
    Response.

db(Collection, Action) ->
	Filename = case Collection of
		offers -> offers_records_file_name;
		orders -> orders_records_file_name
	end,
    {ok, Recordfilename} = application:get_env(pw, Filename),
    {ok, _} = dets:open_file(records_db, [{file, Recordfilename}, {type, set}]),
    Result = Action(),
    ok = dets:close(records_db),
	Result.
