%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Март 2015 18:42
%%%-------------------------------------------------------------------
-module(sp_net_protocol).
-author("tihon").

-define(NOAUTH, 0).
-define(ACCEPT_VERSION, 5).
-define(CONNECT(X, Y), gen_tcp:connect(X, Y, [binary, {reuseaddr, true}, {active, true}, {nodelay, true}])).

%% API
-export([parse_auth/1, parse_redirect/1, get_destination/2]).

%% Parse client's greeting - return reply
-spec parse_auth(binary()) -> binary().
parse_auth(<<Version:8, _/binary>>) when Version /= ?ACCEPT_VERSION ->  %Wrong version
  error(<<<<"Version mismatch: ">>/binary, (integer_to_binary(Version))/binary>>);
parse_auth(<<Version:8, _:8, Auth/binary>>) ->  %Greeting
  case binary:match(Auth, <<?NOAUTH>>) of
    nomatch -> error(<<<<"Not supported: ">>/binary, (integer_to_binary(Auth))/binary>>);
    _ -> <<Version, ?NOAUTH>>
  end.

%% Parse client's requesting address
parse_redirect(<<Version:8, _/binary>>) when Version /= ?ACCEPT_VERSION ->  %Wrong version
  error(<<<<"Version mismatch: ">>/binary, (integer_to_binary(Version))/binary>>);
parse_redirect(<<Version:8, Command:8, 0, AdrType:8, Destination/binary>>) -> %Destination packet
  {IpAddr, Port} = get_destination(AdrType, Destination),
  lager:info("call ~p:~p", [IpAddr, Port]),
  make_connection(Version, Command, IpAddr, Port, AdrType, Destination).


%% @private
make_connection(Version, 1, Adress, Port, AdrType, Destination) -> %connect tcp/ip
  case ?CONNECT(Adress, Port) of
    {ok, Socket} -> {Socket, Adress, Port, <<Version, 0, 0, AdrType, Destination/binary>>};
    {error, Reason} ->
      lager:warning("Can't connect to ~p:~p with reason ~p", [Adress, Port, Reason]),
      throw(<<Version, 4, 0, AdrType, Destination/binary>>)
  end;
make_connection(Version, 2, Ip, Port, AdrType, Destination) -> %binding tcp/ip
  throw(not_implemented);
make_connection(Version, 3, Ip, Port, AdrType, Destination) -> %associate udp
  throw(not_implemented).

%% @private
get_destination(1, Destination) -> %ipv4
  <<A:8, B:8, C:8, D:8, Port:16/big>> = Destination,
  {{A, B, C, D}, Port};
get_destination(3, Destination) -> %domain name
  <<Len:8, Rest/binary>> = Destination,
  Name = binary:part(Rest, 0, Len),
  <<Port:16/big>> = binary:part(Rest, Len, 2),
  {binary_to_list(Name), Port};
get_destination(4, Destination) -> %ipv6
  <<A:16, B:16, C:16, D:16, E:16, F:16, G:16, H:16, Port:16/big>> = Destination,
  {{A, B, C, D, E, F, G, H}, Port}.