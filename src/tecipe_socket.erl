-module(tecipe_socket).

-export([upgrade/3]).

-include("tecipe.hrl").

-spec upgrade(tecipe_inet_socket(),
              tecipe_listener_transport(),
              tecipe_listener()) -> tecipe_socket().
upgrade(Sock, Transport, ListenerRec) ->
    error_logger:info_msg("upgrade in ~p", [?MODULE]),
    TecipeSock = tecipe_proxy:check(#tecipe_socket{inet_socket = Sock}, Transport, ListenerRec),
    ok = apply_opts(TecipeSock, Transport, ListenerRec),
    TecipeSock.

%% === private functions

-spec apply_opts(tecipe_socket(),
                 tecipe_listener_transport(),
                 tecipe_listener()) -> ok.
apply_opts(Sock, Transport, #tecipe_listener{transport_default_opts = DefaultOpts}) ->
    error_logger:info_msg("apply_opts in ~p", [?MODULE]),
    Transport:setopts(Sock, DefaultOpts).
