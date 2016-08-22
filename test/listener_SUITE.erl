-module(listener_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

suite() ->
    [{timetrap, {seconds, 60}}].

init_per_suite(Config) ->
    ok = application:ensure_started(tecipe),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

groups() ->
    [].

all() ->
    [static_acceptor, dynamic_acceptor].

static_acceptor(_Config) ->
    Name = foo,
    Port = 9999,
    Handler = {tecipe_utils, echo_handler, []},
    Acceptor = static,
    Transport = tecipe_tcp,
    Pool = 10,
    ListenerOpts = [{acceptor, Acceptor}, {pool, Pool}, {transport, Transport}],

    {ok, ListenerPID, ListenerNames} = tecipe:start_listener(Name, Port, Handler, ListenerOpts),
    {ListenerName, AcceptorName, CollectorName} = ListenerNames,

    ListenerPID = whereis(ListenerName),
    AcceptorPID = whereis(AcceptorName),
    CollectorPID = whereis(CollectorName),

    ?assertEqual(true, is_process_alive(ListenerPID)),
    ?assertEqual(true, is_process_alive(AcceptorPID)),
    ?assertEqual(true, is_process_alive(CollectorPID)),

    {links, AcceptorLinks} = process_info(whereis(AcceptorName), links),
    ?assertEqual(Pool+1, length(AcceptorLinks)),

    ok.

dynamic_acceptor(_Config) ->
    Name = bar,
    Port = 8888,
    Acceptor = dynamic,
    Transport = tecipe_tcp,
    Pool = 10,
    Handler = {tecipe_utils, echo_handler, []},
    ListenerOpts = [{acceptor, Acceptor}, {pool, Pool}, {transport, Transport}],

    {ok, ListenerPID, ListenerNames} = tecipe:start_listener(Name, Port, Handler, ListenerOpts),
    {ListenerName, AcceptorName, CollectorName} = ListenerNames,

    ListenerPID = whereis(ListenerName),
    AcceptorPID = whereis(AcceptorName),
    CollectorPID = whereis(CollectorName),

    ?assertEqual(true, is_process_alive(ListenerPID)),
    ?assertEqual(true, is_process_alive(AcceptorPID)),
    ?assertEqual(true, is_process_alive(CollectorPID)),

    {links, AcceptorLinks} = process_info(whereis(AcceptorName), links),
    ?assertEqual(Pool+1, length(AcceptorLinks)),

    ok.