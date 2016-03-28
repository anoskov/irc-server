%%%-------------------------------------------------------------------
%%% @author anoskov
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Март 2016 11:31
%%%-------------------------------------------------------------------
-module(broker).

-behavior(gen_server).

-author("anoskov").

%% API
-export([start_link/1]).

-record(state, {nick, username, host, socket, channels}).

start_link(Ref) ->
  gen_server:start_link({global, Ref}, ?MODULE, [], []).

init([]) ->
  {ok, #state{}}.