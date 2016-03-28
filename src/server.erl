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

-export([start_link/0, init/1, handle_call/3, terminate/2]).

-export([accept_connection/1]).

-record(state, {users, channels}).

start_link() ->
  gen_server:start_link({global, server}, ?MODULE, [], []).

init([]) ->
  io:format("Initializing server...~n"),
  case gen_tcp:listen(6667, [{packet, line}, {reuseaddr, true}]) of
    {ok, Lsocket} -> spawn(server, accept_connection, [Lsocket]);
    {error, Reason} -> io:format("Server listen error: ~p~n", [Reason])
  end,
  {ok, #state{users=[], channels=[]}}.

handle_call({nick, Pid, Nick}, _From, State) ->
  case lists:keyfind(Nick, 2, State#state.users) of
    {_Otherpid, _Nick} -> {reply, fail, State};
    false -> case lists:keyfind(Pid, 1, State#state.users) of
               {Pid, Oldnick} -> {reply, ok,
                 #state{users=[{Pid, Nick}|lists:delete({Pid, Oldnick}, State#state.users)],
                 channels=State#state.channels}};
               false -> {reply, ok, #state{users=[{Pid, Nick}|State#state.users], channels=State#state.channels}}
             end
  end;

handle_call({get_user, Nick}, _From, State) ->
  case lists:keyfind(Nick, 2, State#state.users) of
    {Pid, _Nick} -> {reply, {ok, Pid}, State};
    false -> {reply, fail, State}
  end;

handle_call({get_channel, Channel}, _From, State) ->
  case lists:keyfind(Channel, 2, State#state.channels) of
    {Pid, _Channel} -> {reply, {ok, Pid}, State};
    false -> {reply, fail, State}
  end;

handle_call({create_channel, Channel}, _From, State) ->
  {reply, ok, State};

handle_call(Request, _From, State) ->
  io:format("Bad request: ~p~n", [Request]),
  {reply, fail, State}.

handle_cast({channel, join, Channel, Userpid}, State) ->
  {noreply, #state{users=[], channels=[]}}.

handle_info(Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  io:format("Shutdown server!~n"),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

accept_connection(LSocket) ->
  case gen_tcp:accept(LSocket) of
    {ok, Socket} ->
      Ref = erlang:make_ref(),
      {ok, Pid} = broker:start_link(Ref),
      gen_tcp:controlling_process(Socket, Pid),
      gen_server:cast(Pid, {create, Socket}),
      server:accept_connection(LSocket);
    {error, Error} ->
      io:format("Accept connection error: ~p~n", [Error]),
      server:accept_connection(LSocket)
  end.