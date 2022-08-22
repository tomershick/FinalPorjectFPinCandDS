%%%-------------------------------------------------------------------
%%% @author yonrak
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Aug 2022 11:08
%%%-------------------------------------------------------------------
-module(hand0).
-author("yonrak").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).
-compile(export_all).


-export([run_screen/2]).
-define(SERVER, ?MODULE).
-define(Next_node, 'hand1@127.0.0.1').
-define(My_node,'hand0@127.0.0.1').
-define(Next_module, hand1).
-define(Name_table, hand0).
-define(Cell_Module, cellhand0).
-define(Recover_module,recoverhand0).
-define(Timer,100).
-define(Xsize,1000). %defines the size of the screen
-define(Ysize,800).
-define(Head_Module,head).
-define(Head_Node,'head@127.0.0.1').
-define(Init_cells,300). %defines the initial number of cells on this organ
-define(Pid_screen_def,pid_screen_hand0).
-define(Pid_print_speed_def,pid_print_speed_hand0).


%%%=================================================================================================================
%%% API
%%%=================================================================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local,?SERVER}, ?MODULE, [], []).

%%%=================================================================================================================
%%% gen_server callbacks
%%%=================================================================================================================

%% @private
%% Initializes the server
init([]) ->
  process_flag(trap_exit, true), %Make the server a system process
  Pid_screen = spawn_link(?MODULE,run_screen,[?Xsize,?Ysize]), %spawn the GUI process
  register(?Pid_screen_def,Pid_screen),
  Pid_print_speed = spawn_link(?MODULE,print_avg_speed,[]), %spawn the statistics for speed of cells
  register(?Pid_print_speed_def,Pid_print_speed),
  Table = ets:new(?Name_table,[public,named_table]), % create an ETS- where we keep track of all cells and viruses/cuts 
  Pid_recover = spawn_link(?Recover_module,start,[]), %spwan the recovery module
  {ok, {State = {normal, 0, 0, 0},NextNode = ?Next_node}}. %in the ETS cellID (first arg) is the Key
      

% Main Kinds of calls (THIS IS A SERVER FOR A SIMPLE ORGAN- hand/feet):
% 1. any cell updating its position. {update, CellID, PosX, PosY}
% 2. any cell entering area. {newCell,CellID,Type = red/white, PosX, PosY, SpeedX, SpeedY}
% 3. any cell leaving area. {leaving, CellID}cc(test_server).
% 4. update state from main server / infection / cut. {state, Type= idle/normal/infection/cut/stress, PosX, PosY}.


%%%=================================================================================================================
%%                                  Communication calls
%%%=================================================================================================================
% Handles a cast telling the Organ that a new cell arrived to its area
handle_cast({newCell, CellID, Type, PosX, PosY, SpeedX, SpeedY}, {State,NextNode}) ->
if (Type == virus) or (Type ==cut) -> ets:insert(?Name_table,{999, erlang:system_time(second), Type, PosX, PosY, 0, 0}); % if its a virus\cut type We put it in the Ets so we can print it later
true->  %red/white cell
Reply = case ets:lookup(?Name_table,CellID) of %the "Reply" is redundent
            [] -> PID = spawn_link(?Cell_Module,cell_loop,[CellID, Type, PosX, PosY, SpeedX, SpeedY,0,0]), % Spawn the cell with the given arguments
              ets:insert(?Name_table,{CellID, PID, Type, PosX, PosY, SpeedX, SpeedY}), %put cell in the ETS. cellID (first arg) is the Key
              PID ! State, %send the cell the state of the organ (normal/virus etc.)
              {received,CellID}; % will be sent to the previous organ.
            [_] -> {duplicate_cell, CellID} % will be sent to the previous organ.
          end
end,
  {noreply,{State,NextNode}};

%used for the recovery. here we answer the recover node from the previous organ and send our state and ETS (as a table)
handle_cast({recover,Node},{State,NextNode})-> 
	{recover,Node} ! {State,ets:tab2list(?Name_table)},
  {noreply,{State,NextNode}}.

% Start message from the "head". spwan "Init_cells" amount of white and red cells
handle_call({start,L}, _From, {State,NextNode}) ->
  PID1 =spawn_link(?MODULE,spawn_N_red_cells,[?Init_cells,L]),
  PID2 =spawn_link(?MODULE,spawn_N_white_cells,[?Init_cells,L]),
{reply, ok1, {State,NextNode}};

%% call for a New cell entering Area. same as the cast just returns a reply to the sender
handle_call({newCell, CellID, Type, PosX, PosY, SpeedX, SpeedY}, _From, {State,NextNode}) ->
  Reply = case ets:lookup(?Name_table,CellID) of
           %[] -> PID = spawn_link(?MODULE,cell_loop,[CellID, Type, PosX, PosY, SpeedX, SpeedY]), %in the ETS cellID (first arg) is the Key
           [] -> PID = spawn_link(?Cell_Module,cell_loop,[CellID, Type, PosX, PosY, SpeedX, SpeedY,0,0]), %in the ETS cellID (first arg) is the Key
              ets:insert(?Name_table,{CellID, PID, Type, PosX, PosY, SpeedX, SpeedY}),
              PID ! State,
              {received,CellID}; % will be sent to the previous organ.
            [_] -> {duplicate_cell, CellID} % will be sent to the previous organ.
          end,
  {reply, Reply, {State,NextNode}};

%% Cell updating its position
handle_call({update, CellID, PosX, PosY, SpeedX, SpeedY, Hit}, _From, {{SType, SPosX, SPosY, Counter},NextNode}) ->
  Reply = case ets:lookup(?Name_table,CellID) of
            [] -> noCell; %if it does not exist
            [{CellID, PID, Type, _, _, _, _}] ->
              ets:insert(?Name_table,{CellID, PID, Type, PosX, PosY, SpeedX, SpeedY}), %update the ets with the new arguments
              updated
          end,
  if %here we check if the cell is close enough to the cut/virus.
    (Hit == 1) and (Counter == 1) -> % enough cells are near the virus -> return everything to normal
      New_state = {normal, 0, 0, 0},
      [{_,StartTime,TypeP,_,_,_,_}] = ets:lookup(?Name_table,999), %used for statistics	
      EndTime = erlang:system_time(second),
      io:format("~p ended in ~p seconds ~n",[TypeP,EndTime-StartTime]),
      ets:delete(?Name_table,999), %delete the virus/cut from the ets
      gen_server:call({?Head_Module,?Head_Node},{organ_state_change,?Name_table,normal, 0, 0, 0}), %report to the head that the state has changed
      send_all(New_state,?Name_table,ets:first(?Name_table)); %tell all cells that the statehas changed
    Hit == 1 -> New_counter = Counter -1, %cell hit the virus/cut 
      New_state = {SType, SPosX, SPosY, New_counter};
    true -> New_state = {SType, SPosX, SPosY, Counter} %cell did not hit the virus/cut
  end,
  {reply, Reply, {New_state,NextNode}};

%% Cell Leaving area
handle_call({leaving, CellID}, _From, {State,NextNode}) ->
  [{CellID, PID, Type, PosX, PosY, SpeedX, SpeedY}]= ets:lookup(?Name_table,CellID),
  ets:delete(?Name_table,CellID),
  gen_server:cast({?Next_module,NextNode},{newCell,CellID, Type, 0, PosY, SpeedX, SpeedY}), %cast to the next organ that a cell with (Args) is entering its area. the x is zero so it will enter from the left 
  {reply, deleted, {State,NextNode}};

%state change from the "head"
handle_call({state, {Type, PosX, PosY, Counter}}, _From, {State,NextNode}) ->
  New_state = {Type, PosX, PosY, Counter},
  if Type == normal ->
    ets:delete(?Name_table,999); %delete the virus/cut
    Type == stress -> 
      ets:delete(?Name_table,999);%delete the virus/cut
    (Type == cut) or (Type == virus) -> 
    Time = erlang:system_time(second), %used for statistics
    ets:insert(?Name_table,{999, Time, Type, PosX, PosY, 0, 0}) %add the virus/cut
  end,
  send_all(New_state,?Name_table,ets:first(?Name_table)), %send all cells the new state
  {reply, new_state_received, {New_state,NextNode}};

%used for when the next node has fallen-> changes the next node to be my node (where the new server will be recovered)
%when the server is back on its original node we will receive this message again and switch it back.
handle_call({updatenode, New_node}, _From, {State,NextNode}) -> 
  	if NextNode == ?Next_node ->
		New_node1 = ?My_node;
	true -> New_node1 =?Next_node
end,
  {reply, new_node_received, {State,New_node1}};


handle_call(stop, _From, {State,NextNode}) ->
  {stop, normal, stopped, {State,NextNode}};

%used to restart the node when it is recovered
handle_call({restart}, _From, {State,Node}) -> send_all_cells(ets:first(?Name_table)),
					  gen_server:call({?Name_table,?My_node},{state,State}),
					{reply, finish, {State,Node}};
%%%=================================================================================================================
%%                                   HELPING CALLS
%%%=================================================================================================================
handle_call({printets}, _From, {State,NextNode}) ->
  ets:match_object(?Name_table, {'$0', '$1','$2', '$3','$4', '$5','$6'}),
  {reply, ets:match_object(?Name_table, {'$0', '$1','$2', '$3','$4', '$5','$6'}), {State,NextNode}};

handle_call({getstate}, _From, {State,NextNode}) ->
  {reply, {State}, {State,NextNode}}.
%%%=================================================================================================================
%%%                                   Internal functions
%%%=================================================================================================================
new_cell_cast(CellID, Type, PosX, PosY, SpeedX, SpeedY) -> gen_server:cast(?MODULE,{newCell,CellID, Type, PosX, PosY, SpeedX, SpeedY}).
new_cell(CellID, Type, PosX, PosY, SpeedX, SpeedY) -> gen_server:call(?MODULE,{newCell,CellID, Type, PosX, PosY, SpeedX, SpeedY}).
update(CellID, PosX, PosY, SpeedX, SpeedY,Hit) -> gen_server:call(?MODULE,{update,CellID, PosX, PosY, SpeedX, SpeedY,Hit}).
leaving_cell(CellID) -> gen_server:call(?MODULE,{leaving,CellID}).
update_state(New_state) -> gen_server:call(?MODULE,{state,New_state}). % New_state = {Type, PosX, PosY, Counter}
update_node(New_node) -> gen_server:call(?MODULE,{updatenode,New_node}).
get_state() -> gen_server:call(?MODULE,{getstate}).
printets() -> gen_server:call(?MODULE,{printets}).

%send all the cells the State of the system (if its a virus/cut, it will not send the state and continue)
send_all(State,?Name_table,Current_key) ->
  case ets:lookup(?Name_table,Current_key) of
    [] -> finished;
    [{_,_,virus,_,_,_,_}] -> send_all(State,?Name_table,ets:next(?Name_table,Current_key));
    [{_,_,cut,_,_,_,_}] -> send_all(State,?Name_table,ets:next(?Name_table,Current_key));
    [{_,PID,_,_,_,_,_}] -> PID ! State,
                          send_all(State,?Name_table,ets:next(?Name_table,Current_key))
  end.


%spawns cell with a random uniform distribution of place and speed. (L is the base initial unique cell ID given to each cell- in the head process, L is different for each organ) 
spawn_N_red_cells(0,_)-> ok;
spawn_N_red_cells(N,L)->
  new_cell(N+L, red, rand:uniform(800)+100, rand:uniform(600)+100, rand:uniform(5), rand:uniform(10)-5),
  spawn_N_red_cells(N-1,L).
spawn_N_white_cells(0,_)-> ok;
spawn_N_white_cells(N,L)->
  new_cell(N+500+L, white, rand:uniform(800)+100, rand:uniform(600)+100, rand:uniform(5), rand:uniform(10)-5),
  spawn_N_white_cells(N-1,L).






%%%=================================================================================================================
%%                                  NECESSARY
%%%=================================================================================================================


%% @private
handle_info(_Info, {State,NextNode}) ->
  {noreply, {State,NextNode}}.

%% @private
%kills all processes under me
terminate(_Reason, {State,NextNode}) ->
  exit(whereis(?Pid_print_speed_def),kill), 
  kill_all_cells(ets:first(?Name_table)), 
  ok.

%% @private
code_change(_OldVsn, {State,NextNode}, _Extra) ->
  {ok, {State,NextNode}}.

%%%=================================================================================================================
%%                                  GUI
%%%=================================================================================================================
%start a new screen
run_screen(Xsize,Ysize)->
  Wx = wx:new(),
  Frame = wxFrame:new(Wx, -1, "Hand0", [{size, {Xsize, Ysize}}]),
  Panel = wxPanel:new(Frame),
  OnPaint = fun(_Evt, _Obj) ->
    Brush = wxBrush:new(),
    Paint = wxPaintDC:new(Panel),
    wxDC:setBrush(Paint, Brush),
    wxDC:drawLabel(Paint,"Waiting for Limb to begin.",{(Xsize) div 3,(Ysize) div 3,200,200}),
    wxBrush:destroy(Brush),
    wxPaintDC:destroy(Paint)
            end,
  wxFrame:connect(Panel, paint, [{callback, OnPaint}]),
  wxFrame:show(Frame),
  screen_loop(Panel).

%here we wait (?Timer will control the frame rate), then draw all cells and viruses from the ETS
screen_loop(Panel) ->
  receive
    x -> {State} = {0}
  after ?Timer -> void
  end,
  OnPaint = fun(_Evt, _Obj) ->
    Brush = wxBrush:new(),
    Paint = wxPaintDC:new(Panel),
    draw_all(Paint,?Name_table,ets:first(?Name_table),Brush),
    wxBrush:destroy(Brush),
    wxPaintDC:destroy(Paint)
            end,
  wxFrame:connect(Panel, paint, [{callback, OnPaint}]),
  wxWindow:refresh(Panel),
  screen_loop(Panel).


%draws all the cells and viruses
draw_all(Paint,?Name_table,Current_key,Brush) ->
  case ets:lookup(?Name_table,Current_key) of
    [] -> finished;
    [{_,_,red,X,Y,_,_}] -> %red cell
      wxBrush:setColour(Brush,{255,0,0}),
      wxDC:setBrush(Paint, Brush),
      wxDC:drawCircle(Paint,{X,Y},3),
      draw_all(Paint,?Name_table,ets:next(?Name_table,Current_key),Brush);
    [{_,_,white,X,Y,_,_}] -> %white cell
      wxBrush:setColour(Brush,{255,255,255}),
      wxDC:setBrush(Paint, Brush),
      wxDC:drawCircle(Paint,{X,Y},3),
      draw_all(Paint,?Name_table,ets:next(?Name_table,Current_key),Brush);
    [{_,_,virus,X,Y,_,_}] -> %virus
      wxBrush:setColour(Brush,{0,255,0}),
      wxDC:setBrush(Paint, Brush),
      wxDC:drawCircle(Paint,{X,Y},10),
      draw_all(Paint,?Name_table,ets:next(?Name_table,Current_key),Brush);
    [{_,_,cut,X,Y,_,_}] -> %cut
      wxBrush:setColour(Brush,{255,0,255}),
      wxDC:setBrush(Paint, Brush),
      wxDC:drawCircle(Paint,{X,Y},10),
      draw_all(Paint,?Name_table,ets:next(?Name_table,Current_key),Brush)

  end.

%%%=================================================================================================================
%%                                  statistics
%%%=================================================================================================================
print_avg_speed() ->
   receive x-> void
   after 1000 -> [AvgWhite,AvgRed] = cal_avg_speed(?Name_table,ets:first(?Name_table),[0,0,0,0]),
		 io:format("White cell averge speed is: ~p and Red cell averge speed is: ~p ~n",[AvgWhite,AvgRed]),
		 print_avg_speed()
   end.

%calculates the average speed by going over all cells.
cal_avg_speed(?Name_table,Current_key,[WhiteSpeed,WhiteCounter,RedSpeed,RedCounter]) -> 
  case ets:lookup(?Name_table,Current_key) of
    [] -> if (WhiteCounter == 0) and (RedCounter == 0) -> [0,0];
 	     WhiteCounter == 0 -> [0,RedSpeed/RedCounter];
	     RedCounter == 0 -> [WhiteSpeed/WhiteCounter,0];
	  true -> [WhiteSpeed/WhiteCounter,RedSpeed/RedCounter]
	  end;
    [{_,_,red,_,_,Xspeed,Yspeed}] -> cal_avg_speed(?Name_table,ets:next(?Name_table,Current_key),[WhiteSpeed,WhiteCounter,RedSpeed+(math:sqrt(Xspeed*Xspeed +Yspeed*Yspeed)),RedCounter+1]);
    [{_,_,white,_,_,Xspeed,Yspeed}] -> cal_avg_speed(?Name_table,ets:next(?Name_table,Current_key),[WhiteSpeed+(math:sqrt(Xspeed*Xspeed +Yspeed*Yspeed)),WhiteCounter+1,RedSpeed,RedCounter]);
    [_] -> cal_avg_speed(?Name_table,Current_key,[WhiteSpeed,WhiteCounter,RedSpeed,RedCounter])
  end.
 

%%%=================================================================================================================
%%                                  recovery
%%%=================================================================================================================
% used when a server is recovered, will send all cells from the temporary server to the recovered server in order to continue with the correct state
send_all_cells(Current_key) ->
   case ets:lookup(?Name_table,Current_key) of
      [] -> finish;
      [{CellID,_,Type, PosX, PosY, SpeedX, SpeedY}] -> gen_server:cast({?Name_table,?My_node},{newCell,CellID, Type, PosX, PosY, SpeedX, SpeedY}),
      send_all_cells(ets:next(?Name_table,Current_key))
   end.
% used when a server is recovered, kills all cell in the temporery server
kill_all_cells(Current_key) ->
   case ets:lookup(?Name_table,Current_key) of
      [] -> exit(whereis(?Pid_screen_def), kill);
      [{_,Pid,red,_,_,_,_}] -> exit(Pid, kill),
			     kill_all_cells(ets:next(?Name_table,Current_key));
      [{_,Pid,white,_,_,_,_}] -> exit(Pid, kill),
			     kill_all_cells(ets:next(?Name_table,Current_key));
      [_] ->kill_all_cells(ets:next(?Name_table,Current_key))
   end.

