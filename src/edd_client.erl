-module(edd_client).

-export([client/1, start/0]).

% -define(CLIENT_DELAY,1500).
-define(DEF_PORT, 		6667).

start()->
	client(?DEF_PORT).


client(Port) ->
	{ok, Sock} = gen_tcp:connect("localhost", Port, 
                                 [binary, {packet, 0}, {active, false}]),
	client_control(Sock).
	% spawn(fun() -> client_receive(Sock) end),
	% spawn(fun() -> client_loop(Sock,0) end).

client_control(Sock) ->
    case gen_tcp:send(Sock,"/Users/tama/Dropbox/faena/current work/edd/examples/queue/") of 
    	ok -> 
			ok;
		{error,Reason} ->
			io:format("Error: ~p\n",[Reason]),
			erlang:error(error)
    end,
	client_receive(Sock),
    case gen_tcp:send(Sock,"queue_old:out_r({[1],lists:seq(1,10)})") of 
    	ok -> 
			ok;
		{error,Reason2} ->
			io:format("Error: ~p\n",[Reason2]),
			erlang:error(error)
    end,
    client_receive(Sock).

client_receive(Sock) ->
	case gen_tcp:recv(Sock, 0) of
        {ok, Data} ->
            io:format("~s", [binary_to_list(Data)]);
        {error, Reason} ->
            io:format("Error: ~p\n",[Reason]),
            erlang:error(error)
    end.


% %% @doc Format a list of string to be sent to a client
% -spec pack([string()]) -> string().
% pack(List) ->
% 	string:join(List, ":") ++ "\n".
