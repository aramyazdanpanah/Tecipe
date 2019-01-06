-module(tecipe_acceptor_static).
-behaviour(supervisor).

-export([start_link/4, start_acceptor/4]).

-export([init/1]).

-include("tecipe.hrl").

start_link(Ref, Handler, ListeningSock, ListenerRec) ->
    Pool = ListenerRec#tecipe_listener.acceptor_pool,
    Transport = ListenerRec#tecipe_listener.transport,
    Name = ListenerRec#tecipe_listener.acceptor_name,
    {ok, AcceptorSup} = supervisor:start_link({local, Name}, ?MODULE,
					      [Ref, Handler, Transport, ListeningSock, ListenerRec]),
    [{ok, _} = add_acceptor(AcceptorSup) || _ <- lists:seq(1, Pool)],

    error_logger:info_msg("start_link in ~p", [?MODULE]),
    {ok, AcceptorSup}.

add_acceptor(Pid) ->
    supervisor:start_child(Pid, []).

init([Ref, Handler, Transport, ListeningSock, ListenerRec]) ->
    Acceptor = {{tecipe_acceptor_loop, Ref},
                {?MODULE, start_acceptor, [Handler, Transport, ListeningSock, ListenerRec]},
                permanent,
                3000,
                worker,
                [?MODULE]},

    error_logger:info_msg("init in ~p", [?MODULE]),
    {ok, {{simple_one_for_one, 10, 1}, [Acceptor]}}.

start_acceptor(Handler, Transport, ListeningSock, ListenerRec) ->
    Pid = spawn_link(fun() ->
                             error_logger:info_msg("start acceptor in ~p", [?MODULE]),
                             acceptor_loop(Handler, Transport, ListeningSock, ListenerRec)
		     end),
    {ok, Pid}.

acceptor_loop(Handler, Transport, ListeningSock, ListenerRec) ->
    {ok, Sock} = Transport:accept(ListeningSock),
    error_logger:info_msg("acceptor_loop in ~p", [?MODULE]),
    TecipeSock = tecipe_socket:upgrade(Sock, Transport, ListenerRec),
    WorkerPID = case Handler of
                    {Module, Function, Args} ->
                        proc_lib:spawn(Module, Function, [Transport, TecipeSock, Args]);
                    Function ->
                        spawn(fun() -> Function(Transport, TecipeSock) end)
                end,

    case ListenerRec#tecipe_listener.monitor of
        true ->
            tecipe_monitor:monitor_worker(ListenerRec#tecipe_listener.monitor_name,
                                          TecipeSock, WorkerPID);
        _ ->
            ok
    end,

    gen_tcp:controlling_process(Sock, WorkerPID),
    acceptor_loop(Handler, Transport, ListeningSock, ListenerRec).
