-module(edd_jserver).

-export([start/0]).

-define(JAVA_NODE_NAME,{edd, 'eddjava@localhost'}).

start() ->
	register(edd_server, self()), 
	?JAVA_NODE_NAME ! {ready, self()},
	io:format("Server is ready\n"),
	edd_loop().

edd_loop() ->
	receive 
		{buggy_call, Call, Dir, State} ->
			try 
				io:format("Received a debugging request\n"),
				G = edd:dd_server(Call,Dir),
				Send_G = edd_lib:tupled_graph(G),
				?JAVA_NODE_NAME ! {dbg_tree, Send_G},
				{Vertices, Correct, NotCorrect, Unknown} =  
					case State of 
						none -> 
		    				{Vertices0, Correct0, NotCorrect0} = 
		    					edd_lib:initial_state(G, []),
		    				{Vertices0, Correct0, NotCorrect0, []};
		    			_ ->
		    				State
		    		end,
		    	%% TODO: This could be sent when requested
		    	Strategy = divide_query,
			    %% TODO: This should change the strategy
			    FunGetNewStrategy = 
			    	fun(_) -> divide_query end,
			    FunGetAnswer = 
			    	fun ask_question/5,
			    io:format("Start question - answer loop\n"),
			    {CorrectFinal,NotCorrectFinal,_UnknownFinal,_,_} =
		    		edd_lib:asking_loop(G, FunGetNewStrategy, FunGetAnswer, 
			   			Strategy,Vertices,Correct,NotCorrect,Unknown,[]),
		    	io:format("Finished question - answer loop\n"),
				case NotCorrect of
				     [-1] ->
				     	?JAVA_NODE_NAME ! aborted;
				     _ -> 
				        NotCorrectVertexs = [NCV || NCV <- NotCorrectFinal, 
				                                   (digraph:out_neighbours(G, NCV) -- CorrectFinal) == [] ],
				        case NotCorrectVertexs of
				             [] ->
				             	%% TODO: Send the list of candidates nodes 
				             	?JAVA_NODE_NAME ! unknown_nodes;
				             [NotCorrectVertex|_] ->
								?JAVA_NODE_NAME ! {buggy_node,NotCorrectVertex}
				        end
				end
			catch 
				_:_ = Error -> 
					io:format("An error ocurred: ~p\n", [Error]),
					?JAVA_NODE_NAME ! {error, Error},
					ok
			end;
		Msg -> 
			io:format("Message not expected: ~p\n", [Msg]),
			ok
	end,
	edd_loop().


ask_question(Question, Vertices, Correct, NotCorrect, Unknown) ->
	?JAVA_NODE_NAME ! {question, Question, {Vertices, Correct, NotCorrect, Unknown}},
    receive 
		{answer, Answer} ->
			Answer;
		_ ->
			ask_question(Question, Vertices, Correct, NotCorrect, Unknown)
	end.