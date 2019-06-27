#!/usr/bin/escript
%% -*- erlang -*-
%%! -smp enable -sname fastscan -mnesia debug verbose -pa . -Wall
-module(fastscan).
-define(TOP100FILE, [80,23,443,21,22,25,3389,110,445,139,143,53,135,3306,8080,1723,111,995,993,5900,1025,587,8888,199,1720,465,548,113,81,6001,10000,514,5060,179,1026,2000,8443,8000,32768,554,26,1433,49152,2001,515,8008,49154,1027,5666,646,5000,5631,631,49153,8081,2049,88,79,5800,106,2121,1110,49155,6000,513,990,5357,427,49156,543,544,5101,144,7,389,8009,3128,444,9999,5009,7070,5190,3000,5432,1900,3986,13,1029,9,5051,6646,49157,1028,873,1755,2717,4899,9100,119,37]).

main(Args) ->
    MainPid = self(),
    %try
	OptionServices = utils:index_of("-sV", Args),
	OptionTopPorts = utils:index_of("--top-ports", Args),
	OptionPorts = utils:index_of("-p", Args),
	OptionHostFile = utils:index_of("-H", Args),
	OptionHost = utils:index_of("-t", Args),
        OptionWorker = utils:index_of("-w", Args),
	if
	    OptionTopPorts /= not_found ->
		TopPorts = lists:nth(OptionTopPorts + 1, Args),
		PortList = lists:sublist(?TOP100FILE, list_to_integer(TopPorts)),
                FlagPort = 1;
	    OptionPorts /= not_found ->
		TopPorts = "",
		PortNumbers = lists:nth(OptionPorts + 1, Args),
		PortList = lists:map(fun(Port) -> list_to_integer(Port) end, string:tokens(PortNumbers, ",")),
		FlagPort = 2;
	    true ->
		PortList = [],
		TopPorts = "",
		FlagPort = 0,
		usage()
	end,
	if
	    OptionHostFile /= not_found ->
		HostFile = lists:nth(OptionHostFile + 1, Args),	
		Host = "",
		io:format("HostFile: " ++ HostFile ++ "\n"),
		FlagHost = 1,
                IpList = [];
	    OptionHost /= not_found ->
		HostFile = "",
		Host = lists:nth(OptionHost + 1, Args),
                RawIpList = string:tokens(Host, ","),
                IpList = utils:loadIp(RawIpList),
		FlagHost = 2;
	    true ->
		HostFile = "",
		Host = "",
		FlagHost = 0,
                IpList = [],
		usage()
	end,
	if 
	    OptionServices /= not_found ->
		FlagServices = 1;
            true ->
		FlagServices = 0
	end,
        if 
            OptionWorker /= not_found ->
                WorkerNb = list_to_integer(lists:nth(OptionWorker + 1, Args));
            true ->
                WorkerNb = 50
        end,
	if 
	    (FlagHost == 1) ->
		actions:scanTopPortsFile(HostFile, TopPorts);
	    true ->
                Targets = utils:loadTargets(IpList, PortList),
                MasterPid = spawn(fun() -> master:start(WorkerNb, Targets, MainPid) end)
	end,
    %catch
     %  _:_ ->
      %      usage()
    %end,
    receive
	done -> 
	    ok
    end;
main(_) ->
    usage().

usage() ->
    io:format("Usage\n"),
    halt(1).
