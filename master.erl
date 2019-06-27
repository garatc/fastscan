-module(master).

-export([start/3]).

start(WorkerNb, JobList, MainPid) ->
    startWorkers(WorkerNb, self(), length(JobList)),
    loop(JobList, [], MainPid).

loop(JobList, JobQueue, MainPid) ->
    receive 
	{ready, Pid} ->
	    timer:sleep(200),
            [NextTarget | UpdatedJobList] = JobList,
            UpdatedJobQueue = lists:append([NextTarget], JobQueue),
	    Pid ! {work, NextTarget},
            loop(UpdatedJobList, UpdatedJobQueue, MainPid);
        {done, Target, Pid} ->
	    Remaining = length(JobList),
            if
                (Remaining > 1) ->
		    %io:format("JobList: ~p~nTarget:~p~n", [JobList, Target]),
                    %UpdatedJobList = lists:delete(Target, JobList),
                    NextTarget = utils:firstElement(JobList),
                    UpdatedJobQueue = lists:append([NextTarget], lists:delete(Target, JobQueue)),
                    Pid ! {work, NextTarget},
                    loop(lists:delete(Target, JobList), UpdatedJobQueue, MainPid);
                (Remaining == 0) and (length(JobQueue) > 1) ->
		    loop(lists:delete(Target, JobList), lists:delete(Target, JobQueue), MainPid);
		true ->
	            io:format("Let's end this shit\n"),
                    MainPid ! done
            end
    end.

startWorkers(0, _, _) -> ok;
startWorkers(WorkersNb, MasterPid, N) when WorkersNb > N -> startWorkers(N, MasterPid, WorkersNb);
startWorkers(WorkersNb, MasterPid, N) -> 
    spawn(fun() -> worker:start(MasterPid) end),
    startWorkers(WorkersNb - 1, MasterPid, N).





