-module(worker).

-export([start/1]).
-define(TIMEOUT, 2000).

start(MasterPid) ->
    timer:sleep(1000),
    MasterPid ! {ready, self()},
    loop(MasterPid).

loop(MasterPid) -> 
    receive 
        {work, [Ip, Port]} ->
            Options = [list, inet],
            case gen_tcp:connect(Ip, Port, Options, ?TIMEOUT) of
                {ok, Sock} -> 
                    gen_tcp:close(Sock),
                    io:format(Ip ++ " " ++ integer_to_list(Port) ++ " open\n"),
		    MasterPid ! {done, [Ip, Port], self()},
		    loop(MasterPid);
                {error, econnrefused} -> 
                    io:format(Ip ++ "   " ++ integer_to_list(Port) ++ "   closed\n"),
	            MasterPid ! {done, [Ip, Port], self()},
		    loop(MasterPid);
                {error, ehostunreach} -> 
                    io:format(Ip ++ "   " ++ integer_to_list(Port) ++ "   unreachable\n"),
	            MasterPid ! {done, [Ip, Port], self()},
		    loop(MasterPid);
                {error, enetunreach} -> 
                    io:format(Ip ++ "   " ++ integer_to_list(Port) ++ "   unreachable\n"),
	            MasterPid ! {done, [Ip, Port], self()},
		    loop(MasterPid);
                {error, timeout} -> 
                    io:format(Ip ++ "   " ++ integer_to_list(Port) ++ "   timeout\n"),
	            MasterPid ! {done, [Ip, Port], self()},
		    loop(MasterPid)
            end
    end.
