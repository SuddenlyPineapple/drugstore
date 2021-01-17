-module(offers_handler).
-behavior(cowboy_handler).

-export([init/2]).

init(Req=#{method := <<"GET">>}, State) ->
    Res = cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/plain">>
    }, <<"Hello world!">>, Req),
    {ok, Res, State};

init(Req=#{method := <<"POST">>}, State) ->
    Res = cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/plain">>
    }, <<"Hello world!">>, Req),
    {ok, Res, State};

init(Req=#{method := <<"PUT">>}, State) ->
    Res = cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/plain">>
    }, <<"Hello world!">>, Req),
    {ok, Res, State};

% init(Req=#{method := <<"DELETE">>}, State) ->
%     Res = cowboy_req:reply(200, #{
%         <<"content-type">> => <<"text/plain">>
%     }, <<"Hello world!">>, Req),
%     {ok, Res, State};

init(Req, State) ->
    Res = cowboy_req:reply(405, #{
        <<"allow">> => <<"GET">>
    }, Req),
    {ok, Res, State}.