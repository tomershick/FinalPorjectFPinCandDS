%%%-------------------------------------------------------------------
%%% @author yonrak
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Aug 2022 15:34
%%%-------------------------------------------------------------------
-module(recoverhand0).
-author("yonrak").

-compile(export_all).

-export([start/0]).


-define(SERVER, ?MODULE).
-define(Next_node, 'hand1@127.0.0.1').
-define(My_node, 'hand0@127.0.0.1').
-define(Next_module, hand1).
-define(My_module, hand0).
-define(Timer,1000).
-define(Head_Module,head).
-define(Head_Node,'head@127.0.0.1').
-define(Init_cells,300).
-define(Send_Msg,new_hand1node_received).



%% API
-export([]).



start()->
  process_flag(trap_exit, true), %makes sure it stays alive after recovery
  receive %wait 15 seconds before begining (it will crash otherwise
    void -> void
  after 15000 -> void
  end,
  register(recover,self()), %this allows us to respond via a server on another computer
  State = {normal,0,0,0},
  loop(State,0,1,0).

loop(State,Table,Flag,Node)-> % in this function we send the server a messege and wait for a response. if we have no response we initiate a recovery
  receive
  void-> void
  after ?Timer ->
    gen_server:cast({?Next_module,?Next_node},{recover,?My_node}),
    receive %messege recevied from the server
    {New_State,New_Table} -> if Flag == 1 ->loop(New_State,New_Table,1,Node); %all good, continue with the updated table and state
			     true -> restart(Node),io:format("check"),loop(State,Table,1,0) %if a server is back alive, restart it
			     end
    after ?Timer->  %did not receiv a response, initiate a recovery 
	if Flag == 1 -> New_node = recover(State,Table), %recover it and continue
			loop(State,Table,0,New_node);
	true -> loop(State,Table,0,Node) %if its already recovered then continue
	end
    end
  end.



recover(State,Table)-> % recover by starting a new server
  {_,NewNode} = ?Next_module:start_link(),
  receive %timer to let the module start
  void-> void
  after 1000 -> void
  end,
  gen_server:call(?Next_module,{state,State}), %update the new servers state
  gen_server:call(?My_module,{updatenode,NewNode}), %update the previous server where the next (recovered) server is.
  gen_server:call({?Head_Module,?Head_Node},{?Send_Msg,?My_node}), %update thehead where the recovered server is.
  send_all_cells(Table), %update the cells on the recovered server
  NewNode. %return the new server's node.

send_all_cells([])->finished; %send all the cells to the new server
send_all_cells([{CellID,_,Type, PosX, PosY, SpeedX, SpeedY}|T]) ->
   gen_server:cast({?Next_module,?My_node},{newCell,CellID, Type, PosX, PosY, SpeedX, SpeedY}),
   send_all_cells(T).

%when the server is back alive return everything to normal and stop the recovery server
restart(Node)->gen_server:call({?Next_module,?My_node},{restart}), 
	   gen_server:call(?My_module,{updatenode,?Next_node}),
	   gen_server:call({?Head_Module,?Head_Node},{?Send_Msg,?Next_node}),
	   gen_server:stop({?Next_module,?My_node}).
	


