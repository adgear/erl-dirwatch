-module(filewatch).

-export([start/2, stop/1]).

-type handle() :: pid().

-record(state,
        {pid :: pid(),
         port :: port(),
         paths :: #{integer() => file:name_all()}}).

priv_dir() ->
    case code:priv_dir(?MODULE) of
        {error, _} ->
            filename:join(filename:dirname(filename:dirname(code:which(?MODULE))),
                          "priv");
        Dir -> Dir
    end.

%% If you put this in on_load, it will immediately get unloaded when that child dies.
load() ->
    case erl_ddll:load_driver(priv_dir(), "filewatch") of
        ok -> ok;
        {error, already_loaded} -> ok;
        {error, Message} ->
            error_logger:error_msg("filewatch: error loading driver: ~p",
                                   [erl_ddll:format_error(Message)])
    end.

-spec start(pid(), [file:name_all()]) -> {ok, handle()} | {error, _}.

start(Self, Paths) ->
    Pid = spawn_link(fun () -> new_watcher(Self, Paths) end),
    {ok, Pid}.

-spec stop(handle()) -> ok | {error, _}.

stop(Handle) ->
    Handle ! terminate,
    ok.

new_watcher(Pid, Paths) ->
    ok = load(),
    Port = open_port({spawn_driver, "filewatch"}, [in]),
    Map = lists:foldl(fun (Path, Map) ->
        {ok, Descriptor} = erlang:port_call(Port, 1337, Path),
        Map#{Descriptor => Path}
    end, #{}, Paths),
    watch(#state{pid=Pid, port=Port, paths=Map}).

-spec watch(#state{}) -> ok.

watch(S=#state{pid = Pid, port = Port, paths = Paths}) ->
    receive
        {Port, Descriptors} ->
            Pid ! {filewatch, self(), [maps:get(D, Paths) || D <- Descriptors]},
            watch(S);
        terminate ->
            ok;
        _ ->
            exit(unexpected_message)
    end.
