%%%=============================================================================
%%% @doc
%%% @author Salvador Tamarit <tamarit27@gmail.com> 
%%% @end
%%%=============================================================================

-module(edd_server).

%% Exports
-export([start/0]).
-export([server_entry_point/1]).

-define(DEF_PORT, 		6667).


%%%_* application callbacks ----------------------------------------------------


-spec start() -> ok.
start() ->
	server(?DEF_PORT).

%%%_* External functions -------------------------------------------------------


-spec server_entry_point(port()) -> ok.
server_entry_point(Socket) ->
    server_listener(Socket).

%%%_* Internal functions -------------------------------------------------------

-spec server(integer()) -> ok.
server(Port) ->
	edd_control:start(),
    edd_tcp:start(?MODULE, Port, fun server_entry_point/1),
    ok.


server_listener(Socket) ->
	Dir = 
	    case gen_tcp:recv(Socket, 0) of
	        {ok, Data} ->
	            Dir0 = binary_to_list(Data),
	            io:format("Data: ~s~n", [Dir0]),
	            Dir0;
	        % Diconnect procedure
	        {error, closed} ->
	            disconnect(Socket)
	    end,
    case gen_tcp:send(Socket, "ok") of
        ok -> 
            ok;
        {error,closed} ->
            disconnect(Socket)
    end,
    BuggyCall = 
	    case gen_tcp:recv(Socket, 0) of
	        {ok, Data2} ->
	            BuggyCall0 = binary_to_list(Data2),
	            io:format("Data: ~s~n", [BuggyCall0]),
	 			BuggyCall0;
	        % Diconnect procedure
	        {error, closed} ->
	            disconnect(Socket)
	    end,
	G = edd:dd_server(BuggyCall,Dir),
	JSON_G = edd_lib:json_graph(G),
    case gen_tcp:send(Socket, JSON_G) of
        ok -> 
            ok;
        {error,closed} ->
            disconnect(Socket)
    end.

disconnect(_Socket) ->
    ok.

