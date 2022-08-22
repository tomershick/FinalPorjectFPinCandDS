%%%-------------------------------------------------------------------
%%% @author yonrak
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Aug 2022 19:20
%%%-------------------------------------------------------------------
-module(cellhand0).
-author("yonrak").

-define(Timer,100).
-define(Xsize,1000).
-define(Ysize,800).
-define(Radius,500).
-define(My_Module,hand0).
%% API
-export([cell_loop/8]).
cell_loop(CellID, Type, PosX, PosY, SpeedX, SpeedY,Hit,Sent_hit) ->
  TempPosX = round(PosX + SpeedX),   %update position
  TempPosY = round(PosY + SpeedY),

  if %if x is negetive make sure they stay inside the screen
    TempPosX < 2 -> NewPosX = 1; 
    true -> NewPosX = TempPosX
  end,
  if %if Y is over the size, loop it around
    TempPosY > ?Ysize -> NewPosY = TempPosY-?Ysize;
    TempPosY < 0 -> NewPosY = TempPosY+?Ysize;
    true -> NewPosY = TempPosY
  end,
  if 
    NewPosX > ?Xsize  -> Response = gen_server:call(?My_Module,{leaving,CellID}), New_Sent_hit = 0; %if x is over the size, tell the server that this cell is leaving.
    true -> if
              (Sent_hit == 0) and (Hit == 1) -> Response = gen_server:call(?My_Module,{update,CellID, NewPosX, NewPosY, SpeedX, SpeedY,1}), 
                                                    New_Sent_hit = 1; %This tells the server that the cell "hit" a virus/cut.
              true -> Response = gen_server:call(?My_Module,{update,CellID, NewPosX, NewPosY, SpeedX, SpeedY,0}), 
                      New_Sent_hit = Sent_hit %update the position of the cell
            end

  end,
  if
    Response == deleted -> exit(normal);  %if the response from the server is "deleted" then exit this process, otherwise continue
    true -> void
  end,
  receive

    {normal,  _, _, _} ->  %normal state- change direction uniformly randomly
      flush(),
      NewSpeedX = rand:uniform(5),
      NewSpeedY = rand:uniform(10)-5,
      New_Hit = 0,
      cell_loop(CellID, Type, NewPosX, NewPosY,  NewSpeedX, NewSpeedY,0, 0); %repeat the loop with new speed and position
    {stress,  _, _, _} -> %stress state
      flush(),
      receive
        x ->void
      after ?Timer  ->  self() ! {stress, 0, 0, 0} %send myself the state so i can react again
      end,
      New_Hit = 0,
      if
        Type == k-> %just for testing - will never enter
          NewSpeedX = SpeedX,
          NewSpeedY = SpeedY;
        true -> %all cells
          NewSpeedX = rand:uniform(5) + 10,
          NewSpeedY = rand:uniform(10)-5
      end,
      cell_loop(CellID, Type, NewPosX, NewPosY,  NewSpeedX, NewSpeedY,0, 0);

    {virus, V_PosX, V_PosY, Counter} ->  %Speed are calculated using (Vx^2 + Vy^2 = 100)
      receive
        x ->void
      after ?Timer  ->  self() ! {virus, V_PosX, V_PosY, Counter}
      end,
      Dx = NewPosX - V_PosX, %distance from virus
      Dy = NewPosY - V_PosY,
      if %calculate the ration between Dx and Dy
        abs(Dx) < 1 ->
          if
            Dx < 0 -> Ratio = -Dy;
            true -> Ratio = Dy
          end;
        true ->  Ratio = Dy/Dx
      end,
      if %calculate the new speed for x
        Dx >0 -> TempSpeedX = 10/(math:sqrt(1+Ratio*Ratio));
        true-> TempSpeedX = -10/(math:sqrt(1+Ratio*Ratio))
      end,
      if %calculate the new speed for y
        Dy > 0 -> TempSpeedY = abs(TempSpeedX*Ratio);
        true->  TempSpeedY = -1*abs(TempSpeedX*Ratio)
      end,
      Far_enough = (math:sqrt(Dx*Dx +Dy*Dy)), %distance from the virus
      if
        Type == red -> %red cell
          New_Hit = 0,
          if %calculate movement based on distance from the virus
            Far_enough > ?Radius -> NewSpeedX = rand:uniform(5),
                                NewSpeedY = rand:uniform(20)-10;
            true-> NewSpeedX = TempSpeedX,
                   NewSpeedY = TempSpeedY
          end;
        true -> %white cell
          NewSpeedX = -TempSpeedX, %run towards the virus 
          NewSpeedY = -TempSpeedY,
          if
            Far_enough < 20 -> New_Hit = 1; %if white cell is close enough to the virus, mark a hit which he will send the server
            true -> New_Hit = 0
          end
      end,
      cell_loop(CellID, Type, NewPosX, NewPosY,  NewSpeedX, NewSpeedY,New_Hit, New_Sent_hit);

    
    {cut,  C_PosX, C_PosY, Counter} -> %same as virus just opposite for white and red cells
      receive
        x ->void
      after ?Timer ->  self() ! {cut,  C_PosX, C_PosY, Counter}
      end,
      Dx = NewPosX - C_PosX,
      Dy = NewPosY - C_PosY,
      if
        abs(Dx) < 1 ->
          if
            Dx < 0 -> Ratio = -Dy;
            true -> Ratio = Dy
          end;
        true ->  Ratio = Dy/Dx
      end,
      if
        Dx >0 -> TempSpeedX = 10/(math:sqrt(1+Ratio*Ratio));
        true-> TempSpeedX = -10/(math:sqrt(1+Ratio*Ratio))
      end,
      if
        Dy > 0 -> TempSpeedY = abs(TempSpeedX*Ratio);
        true->  TempSpeedY = -1*abs(TempSpeedX*Ratio)
      end,
      Far_enough = (math:sqrt(Dx*Dx +Dy*Dy)),
      if
        Type == white ->
          New_Hit = 0,
          if
            Far_enough > (?Radius/2) -> NewSpeedX = rand:uniform(5),
                                        NewSpeedY = rand:uniform(20)-10;
            true-> NewSpeedX = TempSpeedX,
                   NewSpeedY = TempSpeedY
          end;
        true -> %red cell
          NewSpeedX = -TempSpeedX,
          NewSpeedY = -TempSpeedY,
          if
          Far_enough < 20 -> New_Hit = 1;
          true -> New_Hit = 0
          end
      end,
      cell_loop(CellID, Type, NewPosX, NewPosY,  NewSpeedX, NewSpeedY,New_Hit, New_Sent_hit);

    _-> io:format("received something wrong"),
      NewSpeedX = SpeedX,
      NewSpeedY = SpeedY,
      New_Hit = 0,
      cell_loop(CellID, Type, NewPosX, NewPosY,  NewSpeedX, NewSpeedY,New_Hit, New_Sent_hit)

  after ?Timer  -> %no message was received (works in normal state), change the direction uniformly randomly to make everything look better
                   Num = rand:uniform(10),
    if
      Num >8 ->  NewSpeedX = rand:uniform(5),
                 NewSpeedY = rand:uniform(10)-5,
                 New_Hit = 0;
      true ->  NewSpeedX = SpeedX,
               NewSpeedY = SpeedY,
               New_Hit = 0
    end,
  cell_loop(CellID, Type, NewPosX, NewPosY,  NewSpeedX, NewSpeedY,New_Hit, New_Sent_hit)
  end.
  %{RandX,RandY} = aLittleRandomnessDoesntHurt(NewSpeedX, NewSpeedY),
  %cell_loop(CellID, Type, NewPosX, NewPosY,  RandX*NewSpeedX, RandY*NewSpeedY).
%cell_loop(CellID, Type, NewPosX, NewPosY,  NewSpeedX, NewSpeedY,New_Hit, New_Sent_hit).

%flushes all messeges
flush() ->
  receive
    _ ->flush()
  after 0 ->
    ok
  end.


