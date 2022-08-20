%% File for wxWidget Lab
-module(gui).
-export([start/0]).
-include_lib("wx/include/wx.hrl").
  -define(max_x,(1024)).
  -define(max_y,(768)).
  -define(Name_table, head).
  

  %% Creeates the window and menus etc.
start() ->
    Wx = wx:new(),
    Frame = wxFrame:new(Wx, -1, "HeadMenu", [{size, {?max_x, ?max_y}}]),
    Panel = wxPanel:new(Frame),
    OnPaint = fun(_Evt, _Obj) ->
	    Brush = wxBrush:new(),
	Paint = wxPaintDC:new(Panel),
	wxDC:setBrush(Paint, Brush),
	wxDC:drawLabel(Paint,"Press Action  ->  select condition \n Press run  ->  Start simulation",{(?max_x) div 3,(?max_y) div 3,200,200})
   end,
   wxFrame:connect(Panel, paint, [{callback, OnPaint}]),

	MenuBar = wxMenuBar:new(),
	wxFrame:setMenuBar (Frame, MenuBar),
	wxFrame:getMenuBar (Frame),
	FileMn = wxMenu:new(),wxMenuBar:append (MenuBar, FileMn, "&Action"),
	Normal=wxMenuItem:new ([{id,100},{text, "&Normal"}]),wxMenu:append (FileMn, Normal),
	Stress=wxMenuItem:new ([{id,200},{text, "&Stress"}]),wxMenu:append (FileMn, Stress),
	CutMn = wxMenu:new(),
	InfectionMn = wxMenu:new(),
	Cut=wxMenuItem:new ([{id,300},{text, "&Cut"},{subMenu,CutMn}]),wxMenu:append (FileMn, Cut),
	CutLeftHand = wxMenuItem:new ([{id,310},{text, "&Left Hand"}]),wxMenu:append (CutMn, CutLeftHand),
	CutRightHand = wxMenuItem:new ([{id,320},{text, "&Right Hand"}]),wxMenu:append (CutMn, CutRightHand),
	CutRightLeg = wxMenuItem:new ([{id,330},{text, "&Left Leg"}]),wxMenu:append (CutMn, CutRightLeg),
	CutLeftLeg = wxMenuItem:new ([{id,340},{text, "&Right Leg"}]),wxMenu:append (CutMn, CutLeftLeg),

	Infection=wxMenuItem:new ([{id,400},{text, "&Infection"},{subMenu,InfectionMn}]),wxMenu:append (FileMn, Infection),
	InfectionLeftHand = wxMenuItem:new ([{id,410},{text, "&Left Hand"}]),wxMenu:append (InfectionMn, InfectionLeftHand),
	InfectionRightHand = wxMenuItem:new ([{id,420},{text, "&Right Hand"}]),wxMenu:append (InfectionMn, InfectionRightHand),
	InfectionRightLeg = wxMenuItem:new ([{id,430},{text, "&Left Leg"}]),wxMenu:append (InfectionMn, InfectionRightLeg),
	InfectionLeftLeg = wxMenuItem:new ([{id,440},{text, "&Right Leg"}]),wxMenu:append (InfectionMn, InfectionLeftLeg),

	RunMn = wxMenu:new(),
	wxMenuBar:append (MenuBar, RunMn, "&Run"),
	Start = wxMenuItem:new ([{id,500},{text,"Start everything"}]),wxMenu:append (RunMn, Start),
	RunRandom = wxMenuItem:new ([{id,510},{text,"Run randomly"}]),wxMenu:append (RunMn, RunRandom),
	Quit = wxMenuItem:new ([{id,600},{text, "&Quit"}]),wxMenu:append (RunMn, Quit),

	wxFrame:connect (Frame, command_menu_selected),
	wxFrame:show(Frame),
  	Params = [],
       loop(Frame,Panel). % pass the needed parameters here
       

%% Handles all the menu bar commands
    loop(Frame,Panel) ->
    receive 
    {_,X,_,_,_}-> 
			io:fwrite("~p ~n", [X]),
			case X of
				100 -> 	head:update_state({hand0,normal,0,0,0}),
					head:update_state({hand1,normal,0,0,0}),
					head:update_state({foot0,normal,0,0,0}),
					head:update_state({foot1,normal,0,0,0}),
					loop(Frame,Panel);

				200 ->	head:update_state({hand0,stress,0,0,0}),
					head:update_state({hand1,stress,0,0,0}),
					head:update_state({foot0,stress,0,0,0}),
					head:update_state({foot1,stress,0,0,0}),
					loop(Frame,Panel);

				310 -> head:update_state({hand0,cut,rand:uniform(800)+100,rand:uniform(600)+100,rand:uniform(100)+50}),loop(Frame,Panel);
				320 -> head:update_state({hand1,cut,rand:uniform(800)+100,rand:uniform(600)+100,rand:uniform(100)+50}),loop(Frame,Panel);
				330 -> head:update_state({foot0,cut,rand:uniform(800)+100,rand:uniform(600)+100,rand:uniform(100)+50}),loop(Frame,Panel);
				340 -> head:update_state({foot1,cut,rand:uniform(800)+100,rand:uniform(600)+100,rand:uniform(100)+50}),loop(Frame,Panel);

				410 -> head:update_state({hand0,virus,rand:uniform(800)+100,rand:uniform(600)+100,rand:uniform(100)+50}),loop(Frame,Panel);
				420 -> head:update_state({hand1,virus,rand:uniform(800)+100,rand:uniform(600)+100,rand:uniform(100)+50}),loop(Frame,Panel);
				430 -> head:update_state({foot0,virus,rand:uniform(800)+100,rand:uniform(600)+100,rand:uniform(100)+50}),loop(Frame,Panel);
				440 -> head:update_state({foot1,virus,rand:uniform(800)+100,rand:uniform(600)+100,rand:uniform(100)+50}),loop(Frame,Panel);

				500 -> head:start_everything(),loop(Frame,Panel);

				510 -> head:random_events(),loop(Frame,Panel);

				600 -> wxFrame:destroy(Frame,Panel);

				_ -> loop(Frame,Panel)
				end
    				after 100 ->
				OnPaint = fun(_Evt, _Obj) ->
						    Brush = wxBrush:new(),
						    Paint = wxPaintDC:new(Panel),
						    wxDC:drawLabel(Paint,"Press Action  ->  select condition \n Press run  ->  Start simulation",{(?max_x) div 3,(?max_y) div 3,200,200}),
					            wxDC:drawLabel(Paint,"Left Hand State:",{((?max_x) div 3)-230,((?max_y) div 3)+80,200,200}),
 						    
						    wxDC:drawLabel(Paint,"Right Hand State:",{((?max_x) div 3)-30,((?max_y) div 3)+80,200,200}),
 						 	
						    wxDC:drawLabel(Paint,"Left Foot State:",{((?max_x) div 3)+180,((?max_y) div 3)+80,200,200}),
 						    
						    wxDC:drawLabel(Paint,"Right Foot State:",{((?max_x) div 3)+380,((?max_y) div 3)+80,200,200}),

						    [{_,_,_,State0,_,_,_}] = ets:match_object(?Name_table, {'hand0', '$1','$2', '$3','$4', '$5','$6'}),	
						    wxDC:drawLabel(Paint,atom_to_list(State0),{((?max_x) div 3)-205,((?max_y) div 3)+100,200,200}),
if 
State0 == virus ->       wxBrush:setColour(Brush,{0,255,0}),
      			 wxDC:setBrush(Paint, Brush),
      			 wxDC:drawCircle(Paint,{((?max_x) div 3)-190,((?max_y) div 3)+150},10);
State0 == cut -> 	 wxBrush:setColour(Brush,{255,0,255}),
      			 wxDC:setBrush(Paint, Brush),
      			 wxDC:drawCircle(Paint,{((?max_x) div 3)-190,((?max_y) div 3)+150},10);
true -> void
end,
 						    [{_,_,_,State1,_,_,_}] = ets:match_object(?Name_table, {'hand1', '$1','$2', '$3','$4', '$5','$6'}),	
						    wxDC:drawLabel(Paint,atom_to_list(State1),{((?max_x) div 3)+000,((?max_y) div 3)+100,200,200}),
if 
State1 == virus ->       wxBrush:setColour(Brush,{0,255,0}),
      			 wxDC:setBrush(Paint, Brush),
      			 wxDC:drawCircle(Paint,{((?max_x) div 3)+15,((?max_y) div 3)+150},10);
State1 == cut -> 	 wxBrush:setColour(Brush,{255,0,255}),
      			 wxDC:setBrush(Paint, Brush),
      			 wxDC:drawCircle(Paint,{((?max_x) div 3)+15,((?max_y) div 3)+150},10);
true -> void
end,
 						    [{_,_,_,State2,_,_,_}] = ets:match_object(?Name_table, {'foot0', '$1','$2', '$3','$4', '$5','$6'}),	
						    wxDC:drawLabel(Paint,atom_to_list(State2),{((?max_x) div 3)+205,((?max_y) div 3)+100,200,200}),
if 
State2 == virus ->       wxBrush:setColour(Brush,{0,255,0}),
      			 wxDC:setBrush(Paint, Brush),
      			 wxDC:drawCircle(Paint,{((?max_x) div 3)+220,((?max_y) div 3)+150},10);
State2 == cut -> 	 wxBrush:setColour(Brush,{255,0,255}),
      			 wxDC:setBrush(Paint, Brush),
      			 wxDC:drawCircle(Paint,{((?max_x) div 3)+220,((?max_y) div 3)+150},10);
true -> void
end,
 						    [{_,_,_,State3,_,_,_}] = ets:match_object(?Name_table, {'foot1', '$1','$2', '$3','$4', '$5','$6'}),	
						    wxDC:drawLabel(Paint,atom_to_list(State3),{((?max_x) div 3)+410,((?max_y) div 3)+100,200,200}),
if 
State3 == virus ->       wxBrush:setColour(Brush,{0,255,0}),
      			 wxDC:setBrush(Paint, Brush),
      			 wxDC:drawCircle(Paint,{((?max_x) div 3)+425,((?max_y) div 3)+150},10);
State3 == cut -> 	 wxBrush:setColour(Brush,{255,0,255}),
      			 wxDC:setBrush(Paint, Brush),
      			 wxDC:drawCircle(Paint,{((?max_x) div 3)+425,((?max_y) div 3)+150},10);
true -> void
end,
						    wxBrush:destroy(Brush),
						    wxPaintDC:destroy(Paint) 
							end,  
						  wxFrame:connect(Panel, paint, [{callback, OnPaint}]),
						  wxWindow:refresh(Panel),
						  loop(Frame,Panel)
				end.



























