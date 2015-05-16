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
	% Send debugging tree
	G = edd:dd_server(BuggyCall,Dir),
	JSON_G = edd_lib:json_graph(G),
    case gen_tcp:send(Socket, JSON_G) of
        ok -> 
            ok;
        {error,closed} ->
            disconnect(Socket)
    end,
    %% This could be sent when requested
    Strategy = divide_query,
    {Vertices0,Correct0,NotCorrect0} = 
    	edd_lib:initial_state(G, []),
    FunGetNewStrategy = 
    	fun(_) -> divide_query end,
    FunGetAnswer = 
    	fun(Selected, Vertices, Correct, NotCorrect, Unknown)-> 
    		ask_question(Socket, Selected, Vertices, Correct, NotCorrect, Unknown)
    	end,
    {Correct,NotCorrect,Unknown,_,_} =
	    edd_lib:asking_loop(G, FunGetNewStrategy, FunGetAnswer, 
		   	Strategy,Vertices0,Correct0,NotCorrect0,[],[]),
	case NotCorrect of
	     [-1] ->
	     	ok;
	     _ -> 
	        NotCorrectVertexs = [NCV || NCV <- NotCorrect, 
	                                   (digraph:out_neighbours(G, NCV) -- Correct) == [] ],
	        case NotCorrectVertexs of
	             [] ->
	             	ok;
	             [NotCorrectVertex|_] ->
						JSON_Buggy = 
							lists:flatten(mochijson:encode({struct, [{"buggy_node", integer_to_list(NotCorrectVertex)}]})),
					    case gen_tcp:send(Socket, JSON_Buggy) of
					        ok -> 
					            ok;
					        {error,closed} ->
					            disconnect(Socket)
					    end
	        end
	end.

disconnect(_Socket) ->
    ok.

ask_question(Socket, Question, Vertices, Correct, NotCorrect, Unknown) ->
	io:format("LLEGAASK0: ~p\n", [{Question, Vertices, Correct, NotCorrect, Unknown}]),
	QuestionInfo = 
		{struct, [
			{"node", integer_to_list(Question)},
			{"pending", {array, (lists:map(fun json_node/1, Vertices))}},
			{"correct", {array, (lists:map(fun json_node/1, Correct))}},
			{"no_correct", {array, (lists:map(fun json_node/1, NotCorrect))}},
			{"unknown", {array, (lists:map(fun json_node/1, Unknown))}}
			]},
	io:format("LLEGAASK1: ~p\n", [QuestionInfo]),
	JSON_Question = 
		lists:flatten(mochijson:encode(QuestionInfo)),
    case gen_tcp:send(Socket, JSON_Question) of
        ok -> 
            ok;
        {error,closed} ->
            disconnect(Socket)
    end,
   	io:format("LLEGAASK2\n"),
    case gen_tcp:recv(Socket, 0) of
        {ok, Data2} ->
            Answer0 = binary_to_list(Data2),
            io:format("Data: ~s~n", [Answer0]),
			list_to_atom(Answer0);
        % Diconnect procedure
        {error, closed} ->
            disconnect(Socket)
    end.

json_node(Node) ->
	{struct,[{"node", integer_to_list(Node)}]}.

