-module(main_hand1).
-author("yonrak").


%% API
-export([start/0]).

start() ->compile:file(hand1),
	  compile:file(cellhand1),
	  compile:file(recoverhand1),
	  compile:file(foot0),
	  compile:file(cellfoot0),
          hand1:start_link().
