-module(edd_jserver).

-export([start/0]).

-define(JAVA_NODE_NAME,{edd, 'eddjava@localhost'}).

start() ->
	register(edd_server, self()), 
	?JAVA_NODE_NAME ! {ready, self()},
	io:format("Server is ready\n"),
	% self()!{buggy_call, "stock:test()", "/Users/tama/Documents/git/edd/examples/stock", none},
	%ZOOM TEST
	% self()!{zoom_dbg, "stock:check_item({item, rice, 7},\n\t\t [{item, rice, 5}, {item, bajoqueta, 8}])", "/Users/tama/Documents/git/edd/examples/stock", none},
	% self()!{answer,'1'},
	% self()!{answer,'1'},
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
			   			Strategy,Vertices,Correct,NotCorrect,Unknown, [],-1),
		    	io:format("Finished question - answer loop\n"),
				case NotCorrect of
				     [-1] ->
				     	?JAVA_NODE_NAME ! aborted;
				     _ -> 
				        NotCorrectVertexs = 
				        	[NCV || 
				        		NCV <- NotCorrectFinal, 
				                (digraph:out_neighbours(G, NCV) -- CorrectFinal) == [] ],
				        case NotCorrectVertexs of
				             [] ->
				             	%% TODO: Send the list of candidates nodes 
				             	?JAVA_NODE_NAME ! unknown_nodes;
				             [NotCorrectVertex|_] ->
								?JAVA_NODE_NAME ! 
									{buggy_node,
									 NotCorrectVertex, 
									 edd_lib:get_call_string(G,NotCorrectVertex)}
				        end
				end
			catch 
				_:_ = Error -> 
					io:format("An error ocurred: ~p\n", [Error]),
					?JAVA_NODE_NAME ! {error, Error},
					ok
			end;
		{zoom_dbg, Call, Dir, State} ->
			try 
				io:format("Received a zoom-debugging request\n"),
				G = edd_zoom:zoom_graph_server(Call,Dir),
				Send_G = edd_zoom_lib:tupled_graph(G),
				?JAVA_NODE_NAME ! {dbg_tree, Send_G},
				{Vertices, Correct, NotCorrect, Unknown} =  
						case State of 
							none -> 
			    				{Vertices0, Correct0, NotCorrect0} = 
			    					edd_zoom_lib:initial_state(G),
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
			    	fun ask_question_zoom/6,
			    io:format("Start question - answer loop\n"),
			    {CorrectFinal,NotCorrectFinal,_UnknownFinal,_,_} =
		    		edd_zoom_lib:asking_loop(G, FunGetNewStrategy, FunGetAnswer, 
			   			Strategy,Vertices,Correct,NotCorrect,Unknown,[], -1),
		    	io:format("Finished question - answer loop\n"),
		    	% ok
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
								?JAVA_NODE_NAME ! {buggy_node,NotCorrectVertex, edd_zoom_lib:string_buggy_info(G, NotCorrectVertex)}
				        end
				end
			catch 
				_:_ = Error -> 
					io:format("An error ocurred: ~p\n", [Error]),
					?JAVA_NODE_NAME ! {error, Error},
					ok
			end 
			;
		{buggy_con_call, Call, Dir, Timeout, State} ->
			try 
				io:format("Received a concurrent debugging request\n"),
				{PidsInfo, Communications} = 
					edd_con:ddc_server(Call, Dir, Timeout),
				?JAVA_NODE_NAME ! {pids_info, PidsInfo},
				?JAVA_NODE_NAME ! {communcations, lists:reverse(Communications)}
			catch 
				_:_ = Error -> 
					io:format("An error ocurred: ~p\n", [Error]),
					?JAVA_NODE_NAME ! {error, Error},
					ok
			end 
			;
		Msg -> 
			io:format("Message not expected: ~p\n", [Msg]),
			ok
	end,
	edd_loop().


ask_question(Question, Vertices, Correct, NotCorrect, Unknown) ->
	% Added an atom 'a' to lists in order to avoid Java receiving them as String.
	?JAVA_NODE_NAME ! {question, Question, {[a|Vertices], [a|Correct], [a|NotCorrect], [a|Unknown]}},
    receive 
		{answer, Answer} ->
			Answer;
		_ ->
			ask_question(Question, Vertices, Correct, NotCorrect, Unknown)
	end.

ask_question_zoom(Question, Answers, Vertices, Correct, NotCorrect, Unknown) ->
	% Added an atom 'a' to lists in order to avoid Java receiving them as String.
	?JAVA_NODE_NAME ! {question, Question, Answers, {[a|Vertices], [a|Correct], [a|NotCorrect], [a|Unknown]}},
    receive 
		{answer, Answer} ->
			Answer;
		_ ->
			ask_question_zoom(Question, Answers, Vertices, Correct, NotCorrect, Unknown)
	end.
