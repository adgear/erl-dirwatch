-module(filewatch).

-export([start/2, stop/1]).

-type handle() :: pid().

-record(state,
        {pid :: pid(),
         port :: port(),
         map :: #{integer() => file:name_all()}}).

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

start(Self, Pairs) ->
    Pid = spawn_link(fun () -> init(Self, Pairs) end),
    {ok, Pid}.

-spec stop(handle()) -> ok | {error, _}.

stop(Handle) ->
    Handle ! terminate,
    ok.

init(Pid, Pairs) ->
    ok = load(),
    Port = open_port({spawn_driver, "filewatch"}, [in]),
    WatchMap = add_watches(create_watch_map(Pairs), Port),
    watch(#state{pid=Pid, port=Port, map=WatchMap}).

create_watch_map(Pairs) ->
    create_watch_map(Pairs, maps:new()).
create_watch_map([{Path, Term} = Pair | Pairs], Result) ->
    Dir = filename:dirname(Path),
    NewResult = case maps:get(Dir, Result, undefined) of
                    undefined -> maps:put(Dir, [{filename:basename(Path), Term}], Result);
                    Added -> maps:update(Dir, [Pair | Added], Result)
                end,
    create_watch_map(Pairs, NewResult);
create_watch_map([], Result) -> Result.

add_watches(DirMap, Port) ->
    add_watches(maps:to_list(DirMap), Port, maps:new()).
add_watches([{Path, _Pairs} = Dir | Dirs], Port, Result) ->
    {ok, Descriptor} = erlang:port_call(Port, 1337, Path),
    NewResult = maps:put(Descriptor, Dir, Result),
    add_watches(Dirs, Port, NewResult);
add_watches([], _Port, Result) -> Result.

-spec watch(#state{}) -> ok.

watch(S=#state{pid = Pid, port = Port, map = Map}) ->
    receive
        {Port, Msg, Name, Descriptors} ->
            handle_event(Pid, Msg, Name, [maps:get(D, Map) || D <- Descriptors]),
            watch(S);
        terminate ->
            ok;
        _ ->
            exit(unexpected_message)
    end.

handle_event(Pid, Msg, Name, [{_Dir, Pairs}]) ->
    case proplists:lookup(Name, Pairs) of
        none -> ok;
        {Name, Term} -> Pid ! {filewatch, self(), Msg, Term}
    end.

reopen_wd(S=#state{port = Port, map = Map}, Descriptor) ->
    Path = maps:get(Descriptor, Map),
    {ok, NewDescriptor} = erlang:port_call(Port, 1337, Path),
    NewMap = maps:put(NewDescriptor, Path,
                        maps:remove(Descriptor, Map)),
    S#state{map = NewMap}.
