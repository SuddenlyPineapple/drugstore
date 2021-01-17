-module(util).
-export([response/3, map_to_json/1]).

response(Status, Body, Req) ->
	cowboy_req:reply(Status, #{
        <<"content-type">> => <<"application/json">>
    }, Body, Req).

map_to_json({_, Content}) ->
	Mapper = fun (Field, {Value, Type}, Acc) -> 
		Json = case Type of 
			text -> io_lib:format("\"~s\": \"~s\",", [Field, Value]);
			float -> io_lib:format("\"~s\": ~p,", [Field, Value])
		end,
		lists:append([Json], Acc)
	end,
	OfferFields = maps:fold(Mapper, [], Content),
	lists:flatten(io_lib:format("{~s},", [OfferFields])).