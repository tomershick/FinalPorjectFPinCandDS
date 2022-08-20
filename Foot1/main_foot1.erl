-module(main_foot1).
-author("yonrak").


%% API
-export([start/0]).

start() ->compile:file(hand0),
	  compile:file(cellhand0),
	  compile:file(foot1),
	  compile:file(cellfoot1),
	  compile:file(recoverfoot1),
          foot1:start_link().
