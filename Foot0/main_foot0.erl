-module(main_foot0).
-author("yonrak").


%% API
-export([start/0]).

start() -> compile:file(foot0),
	  compile:file(cellfoot0),
	  compile:file(recoverfoot0),
	  compile:file(foot1),
	  compile:file(cellfoot1),
          foot0:start_link().
