%%%-------------------------------------------------------------------
%%% @author yonrak
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Aug 2022 15:34
%%%-------------------------------------------------------------------
-module(recoverfoot0).
-author("yonrak").

-compile(export_all).

-export([start/0]).


-define(SERVER, ?MODULE).
-define(Next_node, 'foot1@127.0.0.1').
-define(My_node, 'foot0@127.0.0.1').
-define(Next_module, foot1).
-define(My_module, foot0).
%-define(Recover_table,recover_foot0).
%-define(Cell_Module, cellfoot0).
%-define(Recover_module,recoverfoot0).
-define(Timer,1000).
-define(Head_Module,head).
-define(Head_Node,'head@127.0.0.1').
-define(Init_cells,300).
-define(Send_Msg,new_foot1node_received).





%% API
-export([]).



start()->
  process_flag(trap_exit, true),
  receive %wait 30 seconds before begining
    void -> void
  after 15000 -> void
  end,
  register(recover,self()),
  State = {normal,0,0,0},
  loop(State,0,1,0).

loop(State,Table,Flag,Node)->
  receive
  void-> void
  after ?Timer ->
    gen_server:cast({?Next_module,?Next_node},{recover,?My_node}),
    receive
    {New_State,New_Table} -> if Flag == 1 ->loop(New_State,New_Table,1,Node);
			     true -> restart(Node),io:format("check"),loop(State,Table,1,0)
			     end
    after ?Timer-> 
	if Flag == 1 -> New_node = recover(State,Table),
			loop(State,Table,0,New_node);
	true -> loop(State,Table,0,Node)
	end
    end
  end.



recover(State,Table)->
	%TODO: send current organ where is next one
	%TODO: send head where is the new organ.
  {_,NewNode} = ?Next_module:start_link(),
  receive
  void-> void
  after 1000 -> void
  end,
  gen_server:call(?Next_module,{state,State}),
  gen_server:call(?My_module,{updatenode,NewNode}),
  gen_server:call({?Head_Module,?Head_Node},{?Send_Msg,?My_node}),
  send_all_cells(Table),
NewNode.

send_all_cells([])->finished;
send_all_cells([{CellID,_,Type, PosX, PosY, SpeedX, SpeedY}|T]) ->
   gen_server:cast({?Next_module,?My_node},{newCell,CellID, Type, PosX, PosY, SpeedX, SpeedY}),
   send_all_cells(T).


restart(Node)->gen_server:call({?Next_module,?My_node},{restart}),
	   gen_server:call(?My_module,{updatenode,?Next_node}),
	   gen_server:call({?Head_Module,?Head_Node},{?Send_Msg,?Next_node}),
	   gen_server:stop({?Next_module,?My_node}).
	



