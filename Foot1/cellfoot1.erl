%%%-------------------------------------------------------------------
%%% @author yonrak
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Aug 2022 19:20
%%%-------------------------------------------------------------------
-module(cellfoot1).
-author("yonrak").

-define(Timer,100).
-define(Xsize,1000).
-define(Ysize,800).
-define(Radius,500).
-define(My_Module,foot1).
%% API
-export([cell_loop/8]).
cell_loop(CellID, Type, PosX, PosY, SpeedX, SpeedY,Hit,Sent_hit) ->
  TempPosX = round(PosX + SpeedX),
  TempPosY = round(PosY + SpeedY),

  if
    TempPosX < 2 -> NewPosX = 1;
    true -> NewPosX = TempPosX
  end,
  if
    TempPosY > ?Ysize -> NewPosY = TempPosY-?Ysize;
    TempPosY < 0 -> NewPosY = TempPosY+?Ysize;
    true -> NewPosY = TempPosY
  end,
  if
    NewPosX > ?Xsize  -> Response = gen_server:call(?My_Module,{leaving,CellID}), New_Sent_hit = 0;
    true -> if
              (Sent_hit == 0) and (Hit == 1) -> Response = gen_server:call(?My_Module,{update,CellID, NewPosX, NewPosY, SpeedX, SpeedY,1}),
                                                    New_Sent_hit = 1;
              true -> Response = gen_server:call(?My_Module,{update,CellID, NewPosX, NewPosY, SpeedX, SpeedY,0}),
                      New_Sent_hit = Sent_hit
            end

  end,
  if
    Response == deleted -> exit(normal);
    true -> void
  end,
  receive

    {normal,  _, _, _} ->
      flush(),
      NewSpeedX = rand:uniform(5),
      NewSpeedY = rand:uniform(10)-5,
      New_Hit = 0,
      cell_loop(CellID, Type, NewPosX, NewPosY,  NewSpeedX, NewSpeedY,0, 0);
    {stress,  _, _, _} ->
      flush(),
      receive
        x ->void
      after ?Timer  ->  self() ! {stress, 0, 0, 0}
      end,
      New_Hit = 0,
      if
        Type == k->
        %Type == white ->
          NewSpeedX = SpeedX,
          NewSpeedY = SpeedY;
        true -> %red cell
          NewSpeedX = rand:uniform(5) + 10,
          NewSpeedY = rand:uniform(10)-5
      end,
      cell_loop(CellID, Type, NewPosX, NewPosY,  NewSpeedX, NewSpeedY,0, 0);

    {virus, V_PosX, V_PosY, Counter} ->  %Speed are calculated using (Vx^2 + Vy^2 = 100)
      receive
        x ->void
      after ?Timer  ->  self() ! {virus, V_PosX, V_PosY, Counter}
      end,
      Dx = NewPosX - V_PosX,
      Dy = NewPosY - V_PosY,
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
        Type == red ->
          New_Hit = 0,
          if
            Far_enough > ?Radius -> NewSpeedX = rand:uniform(5),
                                NewSpeedY = rand:uniform(20)-10;
            true-> NewSpeedX = TempSpeedX,
                   NewSpeedY = TempSpeedY
          end;
        true -> %white cell
          NewSpeedX = -TempSpeedX,
          NewSpeedY = -TempSpeedY,
          if
            Far_enough < 20 -> New_Hit = 1;
            true -> New_Hit = 0
          end
      end,
      cell_loop(CellID, Type, NewPosX, NewPosY,  NewSpeedX, NewSpeedY,New_Hit, New_Sent_hit);

    
    {cut,  C_PosX, C_PosY, Counter} ->
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

  after ?Timer  ->
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

flush() ->
  receive
    _ ->flush()
  after 0 ->
    ok
  end.

aLittleRandomnessDoesntHurt(NewSpeedX, NewSpeedY)->
  Num = rand:uniform(),
  Num2 = rand:uniform(),
  if %calc x randomness
    Num > 0.90 ->
      if
        NewSpeedX > 20 -> RandX = -1;
        NewSpeedX < 0 -> RandX = 1;
        true ->
          if
            Num2 > 0.5 -> RandX = 1;
            true -> RandX = -1
          end
      end;
    true -> RandX = 0
  end,
  if %calc x randomness
    Num2 > 0.90 ->
      if
        NewSpeedY > 20 -> RandY = -1;
        NewSpeedY < -20 -> RandY = 1;
        true ->
          if
            Num > 0.5 -> RandY = 1;
            true -> RandY = -1
          end
      end;
    true -> RandY = 0
  end,
  {RandY,RandX}.


