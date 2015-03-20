-module(socks_proxy_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

-define(ACCEPTOR_POOL, tcp_pool).

start(_Type, _Args) ->
  configure_ranch(),
  socks_proxy_sup:start_link().

stop(_State) ->
  ok.


%% @private
configure_ranch() ->
  {ok, DebuggerConf} = application:get_env(?ACCEPTOR_POOL),
  Port = proplists:get_value(port, DebuggerConf),
  Acceptors = proplists:get_value(listeners, DebuggerConf),
  ranch:start_listener(?ACCEPTOR_POOL, Acceptors,
    ranch_tcp, [{port, Port}],
    sp_net_acceptor, []).