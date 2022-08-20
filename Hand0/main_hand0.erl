-module(main_hand0).
-author("yonrak").


%% API
-export([start/0]).

start() ->compile:file(hand0),
	  compile:file(cellhand0),
	  compile:file(recoverhand0),
	  compile:file(hand1),
	  compile:file(cellhand1),
          hand0:start_link().
