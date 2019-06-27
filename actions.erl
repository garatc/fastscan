-module(actions).

-export([checkServices/3, checkServicesSubnet/4, scanIp/3, checkServicesSubnetTopPorts/4, checkServicesTopPorts/3, scanTopPorts/3, scanSubnetTopPorts/4, scanSubnet/4, scanTopPortsFile/2, scanFile/2, checkServicesFile/2]).
-define(TIMEOUT, 2000).
-define(TOP100FILE, [80,23,443,21,22,25,3389,110,445,139,143,53,135,3306,8080,1723,111,995,993,5900,1025,587,8888,199,1720,465,548,113,81,6001,10000,514,5060,179,1026,2000,8443,8000,32768,554,26,1433,49152,2001,515,8008,49154,1027,5666,646,5000,5631,631,49153,8081,2049,88,79,5800,106,2121,1110,49155,6000,513,990,5357,427,49156,543,544,5101,144,7,389,8009,3128,444,9999,5009,7070,5190,3000,5432,1900,3986,13,1029,9,5051,6646,49157,1028,873,1755,2717,4899,9100,119,37]).
-define(REGIP, "((?:\\d{1,3}\\.){3}\\d{1,3})").

scanIp(Host, Port, CounterPid) -> 
    Options = [list, inet],
    case gen_tcp:connect(Host, Port, Options, ?TIMEOUT) of
        {ok, Sock} -> 
            gen_tcp:close(Sock),
            io:format(Host ++ " " ++ integer_to_list(Port) ++ " open\n"),
	    CounterPid ! counter;
        {error, econnrefused} -> 
            %io:format(Host ++ "   " ++ integer_to_list(Port) ++ "   closed\n"),
	    CounterPid ! counter;
        {error, ehostunreach} -> 
            %io:format(Host ++ "   " ++ integer_to_list(Port) ++ "   unreachable\n"),
	    CounterPid ! counter;
        {error, enetunreach} -> 
            %io:format(Host ++ "   " ++ integer_to_list(Port) ++ "   unreachable\n"),
	    CounterPid ! counter;
        {error, timeout} -> 
	    %io:format("timeout\n"),
	    CounterPid ! counter
    end.

checkServices(Host, Port, CounterPid) ->
    Output = os:cmd("nmap --version-all -Pn -T3 --open -p " ++ integer_to_list(Port) ++ " " ++ Host ++ " | grep '/tcp' | sed -e '/tcp/!d' -e 's./tcp..'"),
    if 
	Output == "" ->
	    io:format(Output);
	true ->
	    io:format(Host ++ " " ++ Output)
    end,
    CounterPid ! counter.

checkServicesSubnet(Ip, Subnet, Port, CounterPid) ->
    IpList = utils:parseSubnet(Ip, Subnet),
    lists:foreach(fun(IntIp) -> spawn( fun() -> checkServices(IntIp, Port, CounterPid) end) end, IpList).

checkServicesFile(HostFile, Port) ->
    File = utils:fileToList(HostFile),
    lists:foreach(fun(Host) -> spawn(scanner, checkServices, [Host, Port]) end, File).

scanFile(HostFile, Port) -> 
    File = utils:fileToList(HostFile),
    lists:foreach(fun(Host) -> spawn(scanner, scanIp, [Host, Port]) end, File).

scanTopPortsFile(HostFile, Top) -> 
    File = utils:fileToList(HostFile),
    lists:foreach(fun(Host) -> spawn(scanner, scanTopPorts, [Host, Top]) end, File).

scanSubnet(Ip, Subnet, Port, CounterPid) -> 
    IpList = utils:parseSubnet(Ip, Subnet),
    lists:foreach(fun(IntIp) -> spawn( fun() -> scanIp(IntIp, Port, CounterPid) end) end, IpList).

scanSubnetTopPorts(Ip, Subnet, Top, CounterPid) -> 
    IpList = utils:parseSubnet(Ip, Subnet),                                           
    lists:foreach(fun(IntIp) -> spawn( fun() -> scanTopPorts(IntIp, Top, CounterPid) end) end, IpList).

scanTopPorts(Host, Top, CounterPid) ->
    Truncated = lists:sublist(?TOP100FILE, Top),
    lists:foreach(fun(Port) -> spawn( fun() -> scanIp(Host, Port, CounterPid) end) end, Truncated).

checkServicesTopPorts(Host, Top, CounterPid) ->
    Truncated = lists:sublist(?TOP100FILE, Top),
    lists:foreach(fun(Port) -> spawn( fun() -> checkServices(Host, Port, CounterPid) end) end, Truncated).

checkServicesSubnetTopPorts(Ip, Subnet, Top, CounterPid) -> 
    IpList = utils:parseSubnet(Ip, Subnet),                   
    lists:foreach(fun(IntIp) -> spawn( fun() -> checkServicesTopPorts(IntIp, Top, CounterPid) end) end, IpList).
    
