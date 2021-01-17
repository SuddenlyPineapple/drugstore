-module(util).
-export([response/3, generate_id/1, db/2]).

response(Status, Body, Req) ->
	cowboy_req:reply(Status, #{
        <<"content-type">> => <<"application/json">>
    }, Body, Req).

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