
-module(hello_demo).

-export([start/0, stop/0]).

-export([handle/2]).

start() ->
    ok = application:start(crypto),
    ok = application:start(esockd),
    mochiweb:start_http(8080, [{max_clients, 1024}, {acceptors, 2}],
                        {?MODULE, handle, [docroot()]}).

stop() ->
    mochiweb:stop_http(8080).

handle(Req, DocRoot) ->
    io:format("~s ~s~n", [Req:method(), Req:get(path)]),
    try
        case Req:get(method) of
            'GET' ->
                "/" ++ Path = Req:get(path),
                Req:serve_file(Path, DocRoot);
            'POST' ->
                Req:not_found();
            _ ->
                Req:respond({501, [], []})
        end
    catch
        Type:What ->
            Report = ["web request failed",
                      {path, Req:get(path)},
                      {type, Type}, {what, What},
                      {trace, erlang:get_stacktrace()}],
            error_logger:error_report(Report),
            Req:respond({500, [{"Content-Type", "text/plain"}],
                         "request failed, sorry\n"})
    end.

docroot() ->
    {file, Here} = code:is_loaded(?MODULE),
    Dir = filename:dirname(filename:dirname(Here)),
    filename:join([Dir, "priv", "www"]).

