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

-export([init/1, handle_call/3]).

-record(state, {users, channels}).

start() ->
  start_link().

start_link() ->
  gen_server:start_link({global, server}, ?MODULE, [], []).

init([]) ->
  io:format("Initializing server...~n"),
  case gen_tcp:listen(6669, [{packet, line}, {reuseaddr, true}]) of
    {ok, Lsocket} -> spawn(server, accept_connection, [Lsocket]);
    {error, Reason} -> io:format("Server listen error: ~p~n", [Reason])
  end,
  {ok, #state{users=[], channels=[]}}.

handle_call({nick, Pid, Nick}, _From, State) ->
  case lists:keysearch(Nick, 2, State#state.users) of
    {value, {_Otherpid, Nick}} -> {reply, fail, State};
    false -> case lists:keysearch(Pid, 1, State#state.users) of
               {value, {Pid, Oldnick}} -> {reply, ok,
                 #state{users=[{Pid, Nick}|lists:delete({Pid, Oldnick}, State#state.users)],
                 channels=State#state.channels}};
               false -> {reply, ok, #state{users=[{Pid, Nick}|State#state.users], channels=State#state.channels}}
             end
  end;

handle_call({channel_pid, Channel}, _From, State) ->
  case lists:keysearch(Channel, 2, State#state.channels) of
    {value, {Pid, Channel}} -> {reply, {ok, Pid}, State};
    false -> {reply, fail, State}
  end.

handle_cast({channel, join, Channel, Userpid}, State) ->
  {noreply, #state{users=[], channels=[]}}.

handle_info(Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  io:format("Shutdown server!~n"),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.