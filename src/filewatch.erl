-module(filewatch).

-export([start/2, stop/1]).
-compile([export_all]).

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

-spec start(pid(), [{file:name_all(), term()}]) -> {ok, handle()} | {error, _}.

start(Self, Pairs) ->
    CooldownMs = case application:get_env(cooldown) of
                     {ok, Cd} -> Cd;
                     undefined -> 3000
                 end,
    BPairs = [{erlang:iolist_to_binary(filename:absname(Path)), Term} || {Path, Term} <- Pairs],
    Pid = spawn_link(fun () -> init(Self, BPairs, CooldownMs) end),
    {ok, Pid}.

-spec stop(handle()) -> ok | {error, _}.

stop(Handle) ->
    Handle ! terminate,
    ok.

init(Pid, Pairs, CooldownMs) ->
    ok = load(),
    Port = open_port({spawn_driver,
                      ["filewatch ", integer_to_list(CooldownMs, 10)]}, [in]),
    WatchMap = add_watches(create_watch_list(Pairs), Port),
    watch(#state{pid=Pid, port=Port, map=WatchMap}).

create_watch_list(Pairs) ->
    create_watch_list(Pairs, []).
create_watch_list([{Path, Term} | Pairs], Result) ->
    case filelib:is_dir(Path) of
        true  -> add_to_watch_list({Path, "*", Term}, Pairs, Result);
        false -> add_to_watch_list({filename:dirname(Path),
                                   filename:basename(Path),
                                   Term}, Pairs, Result)
    end;
create_watch_list([], Result) -> Result.

add_to_watch_list({DirName, FileName, Term}, Pairs, Result) ->
    Value = case lists:keyfind(DirName, 1, Result) of
                false -> {DirName, [{FileName, Term}]};
                {DirName, Terms} -> {DirName, [{FileName, Term} | Terms]}
            end,
    create_watch_list(Pairs, lists:keystore(DirName, 1, Result, Value)).

add_watches(DirList, Port) ->
    add_watches(DirList, Port, []).
add_watches([{Dir, Files} | Pairs], Port, Result) ->
    case erlang:port_call(Port, 1337, Dir) of
        {ok, Descriptor} ->
            add_watches(Pairs, Port, lists:keystore(Dir, 1, Result,
                                                    {Dir, Descriptor, Files}));
        {error, Reason} ->
            error({filewatch, Reason, Dir})
    end;
add_watches([], _Port, Result) -> Result.

-spec watch(#state{}) -> ok.
watch(S=#state{port = Port}) ->
    receive
        {Port, Msgs} ->
            handle_events(S, Msgs),
            watch(S);
        terminate ->
            ok;
        _ ->
            exit(unexpected_message)
    end.

handle_events(_S, []) -> ok;
handle_events(S=#state{}, [{Name, Wd} | Msgs]) ->
    handle_event(S, Name, Wd),
    handle_events(S, Msgs).

handle_event(#state{pid = Pid, map = Map}, Name, Wd) ->
    BinName = list_to_binary(Name),
    {_Dir, Wd, Pairs} = lists:keyfind(Wd, 2, Map),
    case lists:keyfind(BinName, 1, Pairs) of
        false -> case has_wildcard(Pairs) of
                     {true, Term} -> msg(Pid, Term);
                     false -> ok
                 end;
        {BinName, Term} -> msg(Pid, Term)
    end.

has_wildcard(Pairs) ->
    case lists:keyfind("*", 1, Pairs) of
        {"*", Term} -> {true, Term};
        false -> false
    end.


msg(Pid, Term) ->
    Pid ! {filewatch, self(), Term}.
