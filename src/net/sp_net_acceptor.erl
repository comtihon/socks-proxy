%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Март 2015 20:51
%%%-------------------------------------------------------------------
-module(sp_net_acceptor).
-author("tihon").

-behaviour(gen_server).
-behaviour(ranch_protocol).

%% API
-export([start_link/4]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state,
{
  ref :: ranch:ref(),
  client_socket :: port(),
  proxy_socket :: port(),
  destination :: tuple(),
  transport :: module(),
  mode = accept :: accept | redirect | proxy
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(Ref :: ranch:ref(), Socket :: any(), Transport :: module(), ProtocolOptions :: any()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Ref, Socket, Transport, Opts) ->
  gen_server:start_link(?MODULE, [Ref, Socket, Transport, Opts], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: list()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([Ref, Socket, Transport, _]) ->
  {ok, #state{client_socket = Socket, transport = Transport, ref = Ref}, 0}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info({tcp, Socket, Message}, State = #state{mode = accept}) -> %get greeting packet from client
  process_packet(fun() -> sp_net_protocol:parse_auth(Message) end, Socket, State, redirect);
handle_info({tcp, Socket, Message}, State = #state{mode = redirect}) -> %get redirection packet with address and actions from client
  process_packet(fun() -> sp_net_protocol:parse_redirect(Message) end, Socket, State, proxy);
handle_info({tcp, Client, Message}, State = #state{client_socket = Client, proxy_socket = Proxy, mode = proxy, destination = Dest}) ->  %proxy from client to destination
  parse_request(Client, Dest, Message),
  gen_tcp:send(Proxy, Message),
  {noreply, State};
handle_info({tcp, Proxy, Message}, State = #state{client_socket = Client, proxy_socket = Proxy, mode = proxy}) -> %proxy from destination to client
  gen_tcp:send(Client, Message),
  {noreply, State};
handle_info({tcp_closed, Socket}, State) ->
  lager:info("~p tcp closed.", [Socket]),
  {stop, normal, State};
handle_info({tcp_error, Socket}, State) ->
  lager:info("tcp error occurred on ~p", [Socket]),
  {stop, normal, State};
handle_info(timeout, State = #state{transport = Transport, client_socket = Socket, ref = Ref}) ->
  ok = Transport:setopts(Socket, [{active, true}]),
  ok = ranch:accept_ack(Ref),
  {noreply, State};
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% @private
process_packet(Fun, Socket, State = #state{ref = Ref}, NextState) ->
  try Fun() of
    {Sock, Addr, Port, Reply} ->  %preparing proxy
      ok = gen_tcp:send(Socket, Reply),
      ranch:remove_connection(Ref),
      {noreply, State#state{mode = NextState, proxy_socket = Sock, destination = {Addr, Port}}};
    Reply ->  %parse greeting
      ok = gen_tcp:send(Socket, Reply),
      {noreply, State#state{mode = NextState}}
  catch
    throw:Reply ->  %could not connect to host
      gen_tcp:send(Socket, Reply),
      {stop, normal, State};
    _:E ->
      lager:warning("Error: ~p", [E]),
      {stop, normal, State}
  end.

%% @private
parse_request(Client, {Addr, Port}, Binary) ->
  try erlang:decode_packet(http, Binary, []) of
    {ok, {http_request, _, {abs_path, Path}, _}, Rest} ->
      Host = parse_headers(Rest),
      lager:info("Client ~p -> ~p  ~p (~p:~p)", [Client, Host, Path, Addr, Port])
  catch
    _:_ -> lager:info("Client ~p -> (~p:~p)", [Client, Addr, Port])
  end.

%% @private
parse_headers(Rest) ->
  Headers = binary:split(Rest, [<<"\n">>], [global, trim]),
  catch lists:foldl(
    fun(<<"Host: ", Host/binary>>, _) ->
      [H | _] = binary:split(Host, [<<"\r">>], [trim]),
      throw(H);
      (_, Acc) -> Acc
    end, <<"">>, Headers).