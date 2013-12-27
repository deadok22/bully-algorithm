%%%-------------------------------------------------------------------
%%% @author deadok22
%%% @copyright (C) 2013, <COMPANY>
%%% @doc
%%% A bully algorithm implementation (see http://www.cs.iastate.edu/~cs554/NOTES/Ch6-Election.pdf).
%%% Erlang node names are used as process IDs in algorithm's description
%%% @end
%%% Created : 26. Dec 2013 10:33 PM
%%%-------------------------------------------------------------------
-module(coordinator_server).
-author("deadok22").

%% API
-export([start/1]).

-define(COORDINATOR_SERVER, ?MODULE).

%%% bully algorithm messages
-define(ELECTION_MESSAGE, election).
-define(ELECTION_RESPONSE, i_am_alive).
-define(COORDINATOR_MESSAGE, coordinator).

-define(WAIT_FOR_ELECTION_RESPONSE_TIMEOUT, 50).

-record(state, {nodes = [], coordinatorNode = node(), messageLoopTimeout = infinity}).

start(Nodes) ->
  register(?COORDINATOR_SERVER, self()),
  io:format("Node ~s is running on a vm with PID ~s.~n", [atom_to_list(node()), os:getpid()]),
  server_loop(start_election(#state{nodes = Nodes})).

server_loop(State) ->
  Coordinator = State#state.coordinatorNode,
  Timeout = State#state.messageLoopTimeout,
  NewState = receive
               {?ELECTION_RESPONSE, _Node} -> State#state{messageLoopTimeout = infinity};
               {?ELECTION_MESSAGE, Node} when Node < node() -> send_election_response(Node), start_election(State);
               {?COORDINATOR_MESSAGE, Node} -> set_coordinator(State, Node);
               {nodedown, Coordinator} -> start_election(State);
               {nodedown, _} ->
                 State %% silently consume nodedown messages received due to monitor_node call on node which was dead already
             after
               Timeout -> win_election(State)
             end,
  server_loop(NewState).

start_election(#state{nodes = Nodes} = State) ->
  lists:foreach(fun send_election_message/1, nodes_with_higher_ids(Nodes)),
  State#state{messageLoopTimeout = ?WAIT_FOR_ELECTION_RESPONSE_TIMEOUT}.

win_election(#state{nodes = Nodes} = State) ->
  io:format("Node ~s has declared itself a leader.~n", [atom_to_list(node())]),
  lists:foreach(fun send_coordinator_message/1, nodes_with_lower_ids(Nodes)),
  set_coordinator(State, node()).

set_coordinator(State, Coordinator) ->
  io:format("Node ~p has changed leader from ~p to ~p~n", [node(), State#state.coordinatorNode, Coordinator]),
  monitor_node(State#state.coordinatorNode, false),
  monitor_node(Coordinator, true),
  State#state{coordinatorNode = Coordinator, messageLoopTimeout = infinity}.

send_election_message(Node) ->
  send_bully_message(Node, ?ELECTION_MESSAGE).

send_election_response(Node) ->
  send_bully_message(Node, ?ELECTION_RESPONSE).

send_coordinator_message(Node) ->
  send_bully_message(Node, ?COORDINATOR_MESSAGE).

send_bully_message(Node, BullyAlgorithmMessage) ->
  send_to_node(Node, {BullyAlgorithmMessage, node()}).

send_to_node(Node, Message) ->
  {?COORDINATOR_SERVER, Node} ! Message.

nodes_with_higher_ids(Nodes) ->
  [Node || Node <- Nodes, Node > node()].

nodes_with_lower_ids(Nodes) ->
  [Node || Node <- Nodes, Node < node()].