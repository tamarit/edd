-module(edd_client).

-export([client/3, start/2]).

-define(DEF_PORT, 		6667).

start(Dir, Call) ->
	client(?DEF_PORT, Dir, Call).


client(Port, Dir, Call) ->
	{ok, Sock} = gen_tcp:connect("localhost", Port, 
                                 [binary, {packet, 0}, {active, false}]),
	client_control(Sock, Dir, Call).

client_control(Sock, Dir, Call) ->
    case gen_tcp:send(Sock,Dir) of 
    	ok -> 
			ok;
		{error,Reason} ->
			io:format("Error: ~p\n",[Reason]),
			erlang:error(error)
    end,
	client_receive(Sock),
    case gen_tcp:send(Sock,Call) of 
    	ok -> 
			ok;
		{error,Reason2} ->
			io:format("Error: ~p\n",[Reason2]),
			erlang:error(error)
    end,
    client_receive(Sock).
    % client_receive(Sock).

client_receive(Sock) ->
	case gen_tcp:recv(Sock, 0) of
        {ok, Data} ->
            io:format("Answer: ~s\n", [binary_to_list(Data)]);
        {error, Reason} ->
            io:format("Error: ~p\n",[Reason]),
            erlang:error(error)
    end.
