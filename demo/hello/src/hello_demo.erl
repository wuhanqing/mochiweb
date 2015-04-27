
-module(hello_demo).

-export([start/0, stop/0]).

-export([handle/2]).

start() ->
    ok = ensure_started(crypto),
    ok = ensure_started(esockd),
    mochiweb:start_http(8080, [{max_clients, 1024}, {acceptors, 2}],
                        {?MODULE, handle, [docroot()]}).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

stop() ->
    mochiweb:stop_http(8080).

handle(Req, DocRoot) ->
    "/" ++ Path = Req:get(path),
	io:format("PATH: ~p~n", [Path]),
    try
        case Req:get(method) of
            Method when Method =:= 'GET'; Method =:= 'HEAD' ->
                case Path of
                    _ ->
                        Req:serve_file(Path, DocRoot)
                end;
            'POST' ->
                case Path of
                    _ ->
                        Req:not_found()
                end;
            _ ->
                Req:respond({501, [], []})
        end
    catch
        Type:What ->
            Report = ["web request failed",
                      {path, Path},
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
