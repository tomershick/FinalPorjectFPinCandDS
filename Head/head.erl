%%%-------------------------------------------------------------------
%%% @author yonrak
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Aug 2022 14:47
%%%-------------------------------------------------------------------
-module(head).
-author("yonrak").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).
-compile(export_all).
-define(SERVER, ?MODULE).


-define(Hand0_node, 'hand0@132.72.48.230').
-define(Hand1_node, 'hand1@132.72.48.231').
-define(Foot0_node, 'foot0@132.72.54.72').
-define(Foot1_node, 'foot1@132.72.50.250').
-define(Hand0_module, hand0).
-define(Hand1_module, hand1).
-define(Foot0_module, foot0).
-define(Foot1_module, foot1).
-define(Timer,5000).
-define(Name_table, head).


%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%=================================================================================================================
%%% gen_server callbacks
%%%=================================================================================================================

init([]) ->
  process_flag(trap_exit, true),

  Table = ets:new(?Name_table,[public,named_table]),
  ets:insert(?Name_table,{hand0, ?Hand0_node, ?Hand0_module, normal, 0, 0, 0}),
  ets:insert(?Name_table,{hand1, ?Hand1_node, ?Hand1_module, normal, 0, 0, 0}),
  ets:insert(?Name_table,{foot0, ?Foot0_node, ?Foot0_module, normal, 0, 0, 0}),
  ets:insert(?Name_table,{foot1, ?Foot1_node, ?Foot1_module, normal, 0, 0, 0}),
  PidHead = spawn_link(gui,start,[]),
{ok, {State = {normal, 0, 0, 0},Nodes = {?Hand0_node,?Hand1_node,?Foot0_node,?Foot1_node}}}.

%% @private
%% @doc Handling call messages

handle_call({state_change,{_,Type,0,0,0}}, _From, {State,{Hand0Node,Hand1Node,Foot0Node,Foot1Node}}) -> %coming from the GUI. This is for normal/stress states
  %send_all(normal,?Name_table,ets:first(?Name_table)),
  send_all(Type,?Name_table,ets:first(?Name_table)),
  ets:insert(?Name_table,{hand0, Hand0Node, ?Hand0_module, Type, 0, 0, 0}),
  ets:insert(?Name_table,{hand1, Hand1Node, ?Hand1_module, Type, 0, 0, 0}),
  ets:insert(?Name_table,{foot0, Foot0Node, ?Foot0_module, Type, 0, 0, 0}),
  ets:insert(?Name_table,{foot1, Foot1Node, ?Foot1_module, Type, 0, 0, 0}),
  {reply, ok, {State,{Hand0Node,Hand1Node,Foot0Node,Foot1Node}}};

handle_call({state_change,{Organ,Type,Posx,Posy,Counter}}, _From, {State,Nodes}) -> %coming from the GUI. this is for virus/cut states
  case ets:lookup(?Name_table,Organ) of
    []->void;
    [{Organ, Node, Module, _, _, _, _}]->
      gen_server:call({Module,Node},{state,{normal,0,0,0}}),
      gen_server:call({Module,Node},{state,{Type,Posx,Posy,Counter}}),
      ets:insert(?Name_table,{Organ, Node, Module, Type,Posx,Posy,Counter})
  end,
  {reply, ok, {State,Nodes}};


handle_call(new_state_received, _From, {State,Nodes}) -> %coming from the organs
  {reply, ok, {State,Nodes}};

handle_call({new_hand1node_received,NewHand1Node}, _From, {State,{Hand0Node,Hand1Node,Foot0Node,Foot1Node}}) -> %coming from the organs
  [{_,_,_,Type,Posx,Posy,Counter}] = ets:lookup(?Name_table,hand1),
  ets:insert(?Name_table,{hand1, NewHand1Node, ?Hand1_module, Type,Posx,Posy,Counter}),
  {reply, ok, {State,{Hand0Node,NewHand1Node,Foot0Node,Foot1Node}}};

handle_call({new_hand0node_received,NewHand0Node}, _From, {State,{Hand0Node,Hand1Node,Foot0Node,Foot1Node}}) -> %coming from the organs
  [{_,_,_,Type,Posx,Posy,Counter}] = ets:lookup(?Name_table,hand0),
  ets:insert(?Name_table,{hand0, NewHand0Node, ?Hand0_module, Type,Posx,Posy,Counter}),
  {reply, ok, {State,{NewHand0Node,Hand1Node,Foot0Node,Foot1Node}}};

handle_call({new_foot0node_received,NewFoot0Node}, _From, {State,{Hand0Node,Hand1Node,Foot0Node,Foot1Node}}) -> %coming from the organs
  [{_,_,_,Type,Posx,Posy,Counter}] = ets:lookup(?Name_table,foot0),
  ets:insert(?Name_table,{foot0, NewFoot0Node, ?Foot0_module, Type,Posx,Posy,Counter}),
  {reply, ok, {State,{Hand0Node,Hand1Node,NewFoot0Node,Foot1Node}}};

handle_call({new_foot1node_received,NewFoot1Node}, _From, {State,{Hand0Node,Hand1Node,Foot0Node,Foot1Node}}) -> %coming from the organs
  [{_,_,_,Type,Posx,Posy,Counter}] = ets:lookup(?Name_table,foot1),
  ets:insert(?Name_table,{foot1, NewFoot1Node, ?Foot1_module, Type,Posx,Posy,Counter}),
  {reply, ok, {State,{Hand0Node,Hand1Node,Foot0Node,NewFoot1Node}}};

handle_call({organ_state_change,Organ,Type,Posx,Posy,Counter}, _From, {State,Nodes}) -> %coming from the organs
  case ets:lookup(?Name_table,Organ) of
    []->void;
    [{Organ, Node, Module, _, _, _, _}]->ets:insert(?Name_table,{Organ, Node, Module,Type, Posx, Posy, Counter})
    end,
  {reply, ok, {State,Nodes}};

handle_call(start, _From, {State,Nodes = {Hand0Node,Hand1Node,Foot0Node,Foot1Node}}) -> %coming from the organs
  ets:insert(?Name_table,{hand0, Hand0Node, ?Hand0_module, normal, 0, 0, 0}),
  ets:insert(?Name_table,{hand1, Hand1Node, ?Hand1_module, normal, 0, 0, 0}),
  ets:insert(?Name_table,{foot0, Foot0Node, ?Foot0_module, normal, 0, 0, 0}),
  ets:insert(?Name_table,{foot1, Foot1Node, ?Foot1_module, normal, 0, 0, 0}),
  send_all(start,?Name_table,ets:first(?Name_table),0),
  {reply, ok, {State,Nodes}}.


handle_cast(_Request, {State,Nodes}) ->
  {noreply, {State,Nodes}}.



handle_info(_Info, {State,Nodes}) ->
  {noreply, {State,Nodes}}.


terminate(_Reason, {_State,_Nodes}) ->
  ok.


code_change(_OldVsn, {State,Nodes} , _Extra) ->
  {ok, {State,Nodes}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


update_state(New_state) -> gen_server:call(?MODULE,{state_change,New_state}). % New_state = {Type, PosX, PosY, Counter}
                                                                              % New_state = {stress/normal, 0, 0, 0}
start_everything() -> gen_server:call(?MODULE,start).

random_events() -> Pid_screen = spawn_link(?MODULE,random_loop,[20]).

send_all(start,?Name_table,Organ,I) ->
  case ets:lookup(?Name_table,Organ) of
    [] -> finished;
    [{Organ, Node, Module, _, _, _, _}] -> gen_server:call({Module,Node},{start,I}),
      send_all(start,?Name_table,ets:next(?Name_table,Organ),I+1000)
  end.

send_all(Type,?Name_table,Organ) ->
  case ets:lookup(?Name_table,Organ) of
    [] -> finished;
    [{Organ, Node, Module, _, _, _, _}] -> gen_server:call({Module,Node},{state,{Type,0,0,0}}),
      send_all(Type,?Name_table,ets:next(?Name_table,Organ))
  end.


random_loop(0)->finished;
random_loop(Time)->
  Type_index = rand:uniform(10),
  Organ_index = rand:uniform(4),

  Organ = case Organ_index of
            1 -> hand0;
            2 -> hand1;
            3 -> foot0;
            4 -> foot1
          end,
  {Type,Posx,Posy,Counter} = case Type_index of
            1 -> {stress,0,0,0};
            2 -> {virus,rand:uniform(800)+100,rand:uniform(600)+100,rand:uniform(40)+20};
            3 -> {virus,rand:uniform(800)+100,rand:uniform(600)+100,rand:uniform(40)+40};
            4 -> {virus,rand:uniform(800)+100,rand:uniform(600)+100,rand:uniform(40)+60};
            5 -> {virus,rand:uniform(800)+100,rand:uniform(600)+100,rand:uniform(40)+100};
            6 -> {virus,rand:uniform(800)+100,rand:uniform(600)+100,rand:uniform(40)+150};
            7 -> {cut,rand:uniform(800)+100,rand:uniform(600)+100,rand:uniform(40)+40};
            8 -> {cut,rand:uniform(800)+100,rand:uniform(600)+100,rand:uniform(40)+80};
            9 -> {cut,rand:uniform(800)+100,rand:uniform(600)+100,rand:uniform(40)+120};
            10 -> {cut,rand:uniform(800)+100,rand:uniform(600)+100,rand:uniform(40)+160}            
          end,
  if 
    Type == stress ->
			update_state({Organ,Type,Posx,Posy,Counter}),
			receive
			    x -> void
			after 10000 -> update_state({Organ,normal,0,0,0})
			end;
    true-> %type is virus/cut
  	case ets:lookup(?Name_table,Organ) of
		[{Organ, _, _, virus, _, _, _}] -> do_nothing;
		[{Organ, _, _, cut, _, _, _}] -> do_nothin;
		[{Organ, _, _, normal, _, _, _}] -> update_state({Organ,Type,Posx,Posy,Counter});
		[] -> do_nothing
	end	
  end,

  receive
    x -> void
  after ?Timer ->random_loop(Time-1)
  end.












