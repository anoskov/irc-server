%%%-------------------------------------------------------------------
%%% @author Andrew Noskov
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. Март 2016 20:53
%%%-------------------------------------------------------------------
-module(server).

-behaviour(gen_server).

-author("Andrew Noskov").

%% API
-export([start/0]).

-record(state, {users, channels}).

start() ->
  start_link().

start_link() ->
  gen_server:start_link({global, server}, ?MODULE, [], []).

init([]) ->
  io:format("Initializing server...~n"),
  {ok, #state{users=[], channels=[]}}.

handle_call({nick, Pid, Nick}, _From, State) ->
  {reply, ok, #state{users=[], channels=[]}}.

handle_cast({channel, join, Channel, Userpid}, State) ->
  {noreply, #state{users=[], channels=[]}}.

handle_info(Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  io:format("Shutdown server!~n"),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.