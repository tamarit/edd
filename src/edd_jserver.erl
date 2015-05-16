-module(edd_jserver).

-export([start/0]).

-define(JAVA_NODE_NAME,{edd, 'eddjava@localhost'}).

start() ->
	register(edd, self()), 
	?JAVA_NODE_NAME ! {ready, self()},
	io:format("PID sent\n"),
	{Call, Dir} = 
		receive 
			{buggy_call, Call0, Dir0} ->
				{Call0, Dir0}
		end,
	io:format("~p\n", [{Call, Dir}]),
	G = edd:dd_server(Call,Dir),
	JSON_G = edd_lib:json_graph(G),
	?JAVA_NODE_NAME ! {dbg_tree, JSON_G},
	%% TODO: This could be sent when requested
    Strategy = divide_query,
    {Vertices0,Correct0,NotCorrect0} = 
    	edd_lib:initial_state(G, []),
    %% TODO: This should change the strategy
    FunGetNewStrategy = 
    	fun(_) -> divide_query end,
    FunGetAnswer = 
    	fun ask_question/5,
    {Correct,NotCorrect,Unknown,_,_} =
	    edd_lib:asking_loop(G, FunGetNewStrategy, FunGetAnswer, 
		   	Strategy,Vertices0,Correct0,NotCorrect0,[],[]),
	case NotCorrect of
	     [-1] ->
	     	?JAVA_NODE_NAME ! aborted;
	     _ -> 
	        NotCorrectVertexs = [NCV || NCV <- NotCorrect, 
	                                   (digraph:out_neighbours(G, NCV) -- Correct) == [] ],
	        case NotCorrectVertexs of
	             [] ->
	             	%% TODO: Send the list of candidates nodes 
	             	?JAVA_NODE_NAME ! unknown_nodes;
	             [NotCorrectVertex|_] ->
					?JAVA_NODE_NAME ! {buggy_node,NotCorrectVertex}
	        end
	end.

ask_question(Question, Vertices, Correct, NotCorrect, Unknown) ->
	?JAVA_NODE_NAME ! {question, Question, {Vertices, Correct, NotCorrect, Unknown}},
    receive 
		{answer, Answer} ->
			Answer
	end.