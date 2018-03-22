%%%    Copyright (C) 2013 Salvador Tamarit <stamarit@dsic.upv.es>
%%%
%%%    This file is part of Erlang Declarative Debugger.
%%%
%%%    Erlang Declarative Debugger is free software: you can redistribute it and/or modify
%%%    it under the terms of the GNU General Public License as published by
%%%    the Free Software Foundation, either version 3 of the License, or
%%%    (at your option) any later version.
%%%
%%%    Erlang Declarative Debugger is distributed in the hope that it will be useful,
%%%    but WITHOUT ANY WARRANTY; without even the implied warranty of
%%%    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%%    GNU General Public License for more details.
%%%
%%%    You should have received a copy of the GNU General Public License
%%%    along with Erlang Declarative Debugger.  If not, see <http://www.gnu.org/licenses/>.

%%%-----------------------------------------------------------------------------
%%% @author Salvador Tamarit <stamarit@dsic.upv.es>
%%% @copyright 2013 Salvador Tamarit
%%% @version 0.1
%%% @doc Erlang Declarative Debugger auxiliary library for concurrency.
%%% @end
%%%-----------------------------------------------------------------------------

-module(edd_con_lib).

-export([
	dot_graph_file/2, ask/3, any2str/1, 
	tab_lines/1, build_call_string/1,
	question_list/2, format/2,
	initial_state/3, asking_loop/1,
	buggy_node_str/3]).

-include_lib("edd_con.hrl").
	

%%------------------------------------------------------------------------------
%% @doc Created a DOT file and a PDF file containing the tree in the graph 'G'. 
%%      Creates the file 'Name'.dot containing the description of the digraph 
%%      'G' using the plain text graph description language DOT 
%%      ([http://en.wikipedia.org/wiki/DOT_language]). It also generates a visual 
%%      PDF version of the graph 'G' using the generated DOT file and using the
%%      'dot' command. If this program is not installed in the system the PDF 
%%      will not be created but the function will not throw any exception.
%% @end
%%------------------------------------------------------------------------------

-spec dot_graph_file( G :: digraph:graph(), Name :: string() ) -> string().	   
dot_graph_file(G,Name)->
	file:write_file(Name++".dot", list_to_binary("digraph PDG {\n"++dot_graph(G)++"}")),
	os:cmd("dot -Tpdf "++ Name ++".dot > "++ Name ++".pdf").	
	
dot_graph(G)->
	ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Utility functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

space() ->
	"\n**********************************\n".

print_summary_pid({Pid, Call, Sent, Spawned, Result}) ->
	Str =
		space() ++ "PROCESS " ++ any2str(Pid)
		++ "\nFirst call " ++ build_call_string_or_afun(Call)
		++ "\nResult " ++ any2str(Result)
		++ "\n" ++ question_list("sent messages", Sent)
		++ "\n" ++ question_list("spawned processes", Spawned)
		++ space(),
	io:format("~s", [Str]).

complexity_pid({_, _, Sent, Spawned, Result}) ->
	% The call is not counted (Should we add it?)
		1 
	+ 	edd_con:complexity_term(Result)
	+ 	(edd_con:complexity_term(Sent) - 1)
	+ 	(edd_con:complexity_term(Spawned) - 1).

any2str(stuck_receive) ->
    "Blocked because it is waiting for a message";
any2str(Any) ->
    format("~p", [Any]).

tab_lines(String) ->
    Lines = string:tokens(String, "\n"),
    NLines = [[$\t|L] || L <- Lines],
    string:join(NLines, "\n").


format(Format, Data) ->
    lists:flatten(io_lib:format(Format, Data)).

question_list(Text, []) ->
    "No " ++ Text;
question_list(Text, List) ->
    PPList = 
        lists:foldl(fun(E, Acc) -> Acc ++ [$\t | edd_con:pp_item(E)] ++ "\n" end, "", List), 
    capfirst(Text) ++ ":\n" ++ lists:droplast(PPList).

capfirst([Head | Tail]) when Head >= $a, Head =< $z ->
    [Head + ($A - $a) | Tail];
capfirst(Other) ->
    Other.

build_call_string({ModFun,IdFun,ArgsFun}) ->
    case IdFun of 
        {pos_info,{_,File,Line,StrFun}} ->
            StrFun ++ 
            format(
                "(~s)\nfun location: (~s, line ~p)",
                [build_args_list_string(ArgsFun),File, Line]);
        _ ->
            format(
            	"~p:~p(~s)", 
                [ModFun,IdFun,build_args_list_string(ArgsFun)])
    end;
% TODO: This clause is temporal. Should be removed
build_call_string(Format) ->
	% io:format("Format: ~p\n", [Format]),
    "".

build_args_list_string([]) ->
    "";
build_args_list_string([E]) ->
    format("~p", [E]);
build_args_list_string([E | Rest]) ->
    format("~p, ~s", [E, build_args_list_string(Rest)]).

build_call_string_or_afun(Call) -> 
	FunCall = element(2, Call),
	case FunCall of 
		{M,F,As} ->
			build_call_string(FunCall);
		{M,F,As,_} ->
			build_call_string({M,F,As});
		{AnoFun} -> 
			case get(dict_funs) of 
				undefined -> 
					"";
				DictFuns -> 
					PosInfo = {pos_info,{Mod,_,_,_}} = hd(dict:fetch(AnoFun, DictFuns)),
					build_call_string({Mod, PosInfo, []})
			end
	end.

ask_initial_process(Pids) ->
	PidsString = 
		[
			{Pid, format(
				"~p\n\tFirst call: ~s\n\tResult: ~s", 
				[Pid, build_call_string_or_afun(Call), any2str(Result)]
			) }
		|| {Pid, Call, _, _, Result} <- Pids],
	{StrPidsDict,LastId} = 
		lists:mapfoldl(
			fun({Pid, StrPid}, Id) -> 
				{{io_lib:format("~p.- ~s\n", [Id, StrPid]), {Id, Pid}}, Id + 1} 
			end, 
		1, 
		PidsString),
	{StrPids, Dict} = 
		lists:unzip(StrPidsDict),
	Options = 
		lists:concat(StrPids) 
			++ io_lib:format("~p.- Choose an event\n",[LastId])
			++ io_lib:format("~p.- None\n",[LastId + 1]),
	NDict = 
		[{LastId,choose_event}, {LastId + 1,none} | Dict],
	Question = 
		io_lib:format(
				space() 
				++ "Pid selection" 
				++ space() 
				++ Options 
				++ "\nPlease, insert a PID where you have observed a wrong behavior (or ~p to select an event): [1..~p]: ",
			[LastId, LastId + 1]),
	Answer = 
		% get_answer(pids_wo_quotes(Question),lists:seq(0,LastId)),
		get_answer(Question,lists:seq(1,LastId + 1)),
	Result = 
		[Data || {Option,Data} <- NDict, Answer =:= Option],
	{FirstPid, SelectEvent} = 
		case Result of 
			[none] ->
				{[Pid || {_,Pid} <- Dict], false};
			[choose_event] ->
				{[Pid || {_,Pid} <- Dict], true};
			_ -> 
				{Result, false}
		end,
	case FirstPid of 
		[PidSelected] ->
			io:format(
				"\nSelected initial PID: ~s\n",
				[element(2, lists:keyfind(PidSelected, 1, PidsString))]);
		_ ->
			io:format("\nInitial PID not defined.\n")
	end,
	io:format(space()),
	{FirstPid, SelectEvent}.

get_pid_vertex(G, V) ->
	{V,{Pid,_}} = 
		digraph:vertex(G,V),
	Pid.

get_MFA_vertex(G, V) ->
	{V, {Pid, CallRec}} = 
		digraph:vertex(G, V),
	case CallRec of 
		#callrec_stack_item{} ->
			case CallRec#callrec_stack_item.origin_callrec of 
				#call_info{call = {M,F,A}} ->
					{M, F, length(A)};
				#receive_info{} ->
					V
			end;
		_ ->
			V
	end.

buggy_node_str(G, NotCorrectVertex, Message) ->
	{NotCorrectVertex, {Pid, CallRec}} = digraph:vertex(G,NotCorrectVertex),
		case CallRec of 
			#callrec_stack_item{} ->		
				StrProblem = 
					case CallRec#callrec_stack_item.origin_callrec of 
						#call_info{call = {M,F,A}} ->
							format(
			                	"call ~p:~p(~s)", 
			                	[M,F,string:join(lists:map(fun any2str/1, A), ", ")]);
						#receive_info{pos_pp = {{pos_info,{_, F, L, ReceiveStr0}}}} ->
							format("receive\n~s\nin ~s:~p", [ReceiveStr0, F, L])
					end,
				format("~sThe problem is in pid ~p\nwhile running ~s\n",[Message, Pid, StrProblem]);
			_ ->
				format("~sThe problem is in the parameters of the initial call.\n", [Message])
		end.

print_buggy_node(G, NotCorrectVertex, Message) ->
	case get(print_session_info) of 
		true ->
			io:format("\nBuggy node: ~p\n", [NotCorrectVertex]);
		false ->
			ok
	end,
	io:format("~s", [buggy_node_str(G, NotCorrectVertex, Message)]).

get_answer(Message,Answers) ->
   [_|Answer] = 
     lists:reverse(io:get_line(Message)),

   AtomAnswer = 
   		try 
   			list_to_integer(lists:reverse(Answer))
   		catch 
			_:_ ->
		   		try 
		   			list_to_atom(lists:reverse(Answer))
		   		catch 
		   			_:_ -> get_answer(Message,Answers)
		   		end
	   	end,
   case lists:member(AtomAnswer,Answers) of
        true -> AtomAnswer;
        false -> get_answer(Message,Answers)
   end.
 
find_unknown_children(G,Unknown,[V|Vs]) ->
	OutNeighbours = digraph:out_neighbours(G, V),
	OutNeighboursUnknown = OutNeighbours -- (OutNeighbours -- Unknown),
	[V | find_unknown_children(G,Unknown,Vs ++ OutNeighboursUnknown)];
find_unknown_children(_,_,[]) ->
	[].


replicate_receives([{E, Ns} | Tail], Comm, Acc) ->
	NAcc = 
		case lists:any(fun(V) -> V end, lists:map(fun(T) -> is_receive(T, E) end, Comm)) of 
			true -> 
				[{E, Ns}, {E, Ns} | Acc];
			false -> 
				[{E, Ns} | Acc]
		end,
	replicate_receives(Tail, Comm, NAcc);
replicate_receives([], Comm, Acc) ->
	lists:reverse(Acc).

is_receive({received,{message_info,_,_,_,N}}, N) ->
	true;
is_receive(E, N) ->
	% io:format("~p ~p\n", [E, N]),
	false.

is_the_event({_,{message_info,_,_,_,N}}, N) ->
	true;
is_the_event({spawned,{spawn_info,_,_,N}}, N) ->
	true;
is_the_event(_, _) ->
	false.


print_root_info(SummaryPidsInfo) ->
	lists:foreach(
		fun print_summary_pid/1, 
		SummaryPidsInfo).

get_initial_complexity(SummaryPidsInfo) ->
	lists:sum(
		lists:map(
			fun complexity_pid/1, 
			SummaryPidsInfo)).

print_help() ->
	Msg = 
		string:join(
			[
				  space()
				, "#. - Indicates that the corresponding option is wrong"
				, "t. - Means \"trust\". Select this when the process or function is reliable"
				, "d. - Means \"don't know\". Select this when you are not sure what is the answer"
				, "i. - Means \"inadmissible\". Select this when the computation should not take place because it does not satisfy the preconditions (for example because it contains invalid arguments)"
				, "c. - Chooses the question related with a selected event from the sequence diagram"
				, "s. - Changes the search strategy"
				, "p. - Changes the search priority"
				, "u. - Undoes last answer"
				, "h. - Prints this help"
				, "a. - Finishes the debugging session."
			],
			 ".\n"),
	io:format("~s\n", [Msg]),
	io:get_line("Press intro to continue...").

initial_state({PidsInfo, Comm, {G, DictQuestions}, DictTrace}, Strategy, Priority) ->
	SummaryPidsInfo = 
		edd_con:summarizes_pidinfo(PidsInfo),
	IniCorrect = 
		[],
	Vertices = 
		digraph:vertices(G) -- [0|IniCorrect],
	Pids =
		[Pid || {Pid, _, _, _, _} <- SummaryPidsInfo],
	NDictTrace = 
			replicate_receives(lists:sort(dict:to_list(DictTrace)), Comm, []) 
		++ 	[ {{last_node, Pid}, []} || Pid <- lists:sort(Pids)],
	% io:format("NDictTrace: ~p\n", [NDictTrace]),
	% io:format("NDictTrace (length): ~p\n", [length(NDictTrace)]),
	#edd_con_state{
		graph = G,
		dict_questions = DictQuestions,
		strategy = Strategy,
		priority = Priority,
		vertices = Vertices,
		not_correct = [0],
		correct = IniCorrect,
		summary_pids = SummaryPidsInfo,
		pids = Pids,
		dicts_trace = NDictTrace,
		comms = Comm
	}.

obtain_node_seq_diagram(Code, DictsTrace, Comm, G) -> 
		{E, Ns} = lists:nth(Code, DictsTrace),
		% io:format("~p\n~p\n~p\n", [E, DictsTrace, Comm]),
		EventBoolList = 
			lists:map(fun(C) -> is_the_event(C, E) end, Comm),
		% io:format("~p\n", [EventBoolList]),
		{_, PosList} = 
			lists:foldl(
					fun
						(true, {Curr, Acc}) ->
							{Curr + 1, [Curr]};
						(false, {Curr, Acc}) ->
							{Curr + 1, Acc}
					end,
					{1,[]},
					EventBoolList),
		% io:format("PosList: ~p\n", [PosList]),
		io:format("Selected event:\n"),
		Node = 
			case PosList of 
				[Pos] -> 
					case lists:nth(Pos, Comm) of
						{spawned,{spawn_info,Spawner,Spawned,_}} -> 
							io:format("~p spawned ~p", [Spawner, Spawned]);
						{sent,MessageInfo} ->
							io:format("Sent message: ~s", [edd_con:pp_item(MessageInfo)]);
						{received,MessageInfo} ->
							io:format("Consumed message: ~s", [edd_con:pp_item(MessageInfo)])
					end,
					% TODO. Maybe it should be choosen whether it should go to the inner- or outer-most.
					hd(Ns);
				[] ->
					case E of 
						{last_node, PidLN} ->
							io:format("The last event occured in ~p", [PidLN]),
							lists:last(lists:sort([V || V <- digraph:vertices(G), get_pid_vertex(G, V) == PidLN]));
						_ ->
							io:format("Unfortunately, the selected event has not a linked question.\n"),
							io:get_line("Press intro to continue..."),
							none
					end 
			end,
		Node.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Question building
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%------------------------------------------------------------------------------
%% @doc      
%% @end
%%------------------------------------------------------------------------------
ask(Info, Strategy, Priority) ->
	FirstState = #edd_con_state{summary_pids = SummaryPids} = 
		initial_state(Info, Strategy, Priority),
	print_root_info(SummaryPids),
	put(initial_complexity, get_initial_complexity(SummaryPids)),
	put(question_answered, 0),
	put(question_complexity, 0),
	{Pids, ChooseEvent} = ask_initial_process(SummaryPids),
	State0 = 
		FirstState#edd_con_state{pids = Pids, fun_ask_question = fun ask_question/4},
	State1 =
		case ChooseEvent of 
			true ->
				DictTraces = State0#edd_con_state.dicts_trace,
				Comm =  State0#edd_con_state.comms,
				G =  State0#edd_con_state.graph,
				Code = get_answer(
					"Select an event from the sequence diagram: ",
					lists:seq(1,length(DictTraces))),
				case obtain_node_seq_diagram(Code, DictTraces, Comm, G) of 
					none ->
						State0;
					Node ->
						State0#edd_con_state{preselected = Node}
				end;
			false -> 
				State0
		end,
	ask_about(State1).

ask_about(State) -> 
	NState = #edd_con_state{
			correct = Correct,
			not_correct = NotCorrect,
			unknown = Unknown,
			graph = G} = 
	   	asking_loop(State),
	case NotCorrect of
	     [-1] ->
	     	io:format("Debugging process finished\n"),
	     	session_report();
	     _ -> 
	        NotCorrectVertexs = [NCV || NCV <- NotCorrect, 
	                                   (digraph:out_neighbours(G, NCV)--Correct)==[] ],
	        case NotCorrectVertexs of
	             [] ->
	             	io:format("Not enough information.\n"),
	             	session_report(),
	             	NotCorrectWithUnwnownVertexs = 
			  			[NCV || NCV <- NotCorrect, 
	                          (digraph:out_neighbours(G, NCV)--(Correct++Unknown))=:=[]],
	                Maybe0 = 
	                         [U || U <- Unknown, 
	                               NCV <- NotCorrectWithUnwnownVertexs,
	                               lists:member(U,digraph:out_neighbours(G, NCV))],
	                Maybe = find_unknown_children(G,Unknown,Maybe0),
					case get_answer("Do you want to try to answer"
					     ++ " the needed information? [y/n]: ",[y,n]) of
					     y -> 
					     	ask_about(NState#edd_con_state{vertices = Maybe});
					     n -> 
					     	io:format(space()),
			                [print_buggy_node(G, V ,
			                        "Potentially this error can exist:\n") 
			                        || V <- NotCorrectWithUnwnownVertexs],
			                io:format(space()),
			                [print_buggy_node(G, V ,
			                		 "There is not enough information to say whether this is an error or not:\n") 
			                        || V <- Maybe],
			                io:format(space())
					end;
	             [NotCorrectVertex|_] ->
	               	print_buggy_node(G, NotCorrectVertex,
	               		"\nThe error has been detected:\n"),
	               	session_report()
	        end
	end,
	ok.

asking_loop(#edd_con_state{vertices = []} = State) -> 
	State;
asking_loop(#edd_con_state{vertices = [-1]} = State) ->
	State#edd_con_state{
		correct = [-1],
		not_correct = [-1],
		unknown = [-1]
	};
asking_loop(State0 = #edd_con_state{
		preselected = Preselected,
		graph = G,
		dict_questions = DictQuestions,
		strategy = Strategy,
		priority = Priority,
		pids = Pids,
		vertices = Vertices,
		correct = Correct,
		not_correct = NotCorrect,
		unknown = Unknown,
		previous_state = PreviousState,
		summary_pids = SummaryPids,
		dicts_trace = DictsTrace,
		fun_ask_question = FunAsk,
		comms = Comm
	}) ->
	State = 
		State0#edd_con_state{
			preselected = none, 
			previous_state = State0
		},
	GetNodesPids = 
		fun(CVertices) ->
			[V || 
				V <- CVertices, 
			   	lists:member(get_pid_vertex(G,V),Pids)] 
		end,
	NewStateFromNode = 
		fun(CState, Node) ->
			{NPreselected, NPids} = 
				case lists:member(Node, Correct ++ NotCorrect) of 
					true ->
						io:format("\n\nThe question about the selected event is already answered.\n"),
						{none, Pids};
			    	false -> 
			    		{Node, lists:usort([get_pid_vertex(G, Node)|Pids])}		
				end,
			CState#edd_con_state{
		     	preselected = NPreselected,
		     	pids = NPids
		    }
		end,
	NState = 
		case GetNodesPids(Vertices) of 
			[] ->
				State#edd_con_state{
	             	vertices = []
	            };
			VerticesPid ->
				{Selected,NSortedVertices} =
					case Preselected of 
						none ->
							% io:format("State: ~p\n",[State]),
							VerticesNoPid = 
								[V || V <- Vertices, not lists:member(get_pid_vertex(G, V),Pids)],
							% io:format("{VerticesPid, VerticesNoPid}: ~p\n",[{VerticesPid, VerticesNoPid}]),
							VerticesWithValues = 
							  case Strategy of 
							       top_down ->
								        Children = digraph:out_neighbours(G, hd(NotCorrect)),
								        SelectableChildren = Children -- (Children -- VerticesPid), 
								          [{V, -length(digraph_utils:reachable([V], G))} 
								           || V <- SelectableChildren];
							       divide_query ->
										 [{V,begin
										         Reach = digraph_utils:reachable([V], G),
										         TotalReach = length(Reach) - (1 + length(Reach -- VerticesPid)),
										         Rest = (length(VerticesPid) - 1) - TotalReach,
										         abs(TotalReach - Rest)
										     end} || V <- VerticesPid]
							  end,
							% io:format("VerticesWithValues: ~p\n",[VerticesWithValues]),
							SortedVertices = lists:keysort(2,VerticesWithValues),
							%En caso de empate
							FirstValue = element(2,hd(SortedVertices)),
							SameThanFirst = lists:usort([V || {V,VValue} <- SortedVertices, VValue == FirstValue]),
							Selected_ = 
								case Priority of 
									old -> hd(SameThanFirst);
									new -> lists:last(SameThanFirst);
									indet -> element(1,hd(SortedVertices))
								end,
							{Selected_, [V || {V,_} <- SortedVertices, V /= Selected_] ++ VerticesNoPid};
						_ ->
							{Preselected,Vertices -- [Preselected]}
					end,
				FunGetNewPids = 
		       		fun(CVertices) -> 
		    			case GetNodesPids(CVertices) of 
		    				[] -> 
		    					case NotCorrect of 
		    						[0] -> 
		    							[Pid || {Pid, _, _, _, _} <- SummaryPids];
		    						_ ->
		    							Pids
		    					end;
		    				_ -> 
		    					Pids
		    			end
	    			end,
				IsCorrect = 
					begin
						EqualToSelected = 
							[V || V <- Vertices, begin {V,L1} = digraph:vertex(G,V),
							                           {Selected,L2} = digraph:vertex(G,Selected),
							                           (L1 =:= L2) 
							                     end],
						NVerticesCorr = 
							NSortedVertices -- digraph_utils:reachable(EqualToSelected,G),
						State#edd_con_state{
							vertices = NVerticesCorr,
							correct = EqualToSelected ++ Correct,
							pids = FunGetNewPids(NVerticesCorr)
						}
			        end, 
			    IsNotCorrect = 
			    	begin 
			    		NVerticesNotCorr = 
			    			digraph_utils:reachable([Selected],G) -- ([Selected|NotCorrect]++Correct++Unknown),
				    	State#edd_con_state{
				             	vertices = NVerticesNotCorr,
				             	not_correct = [Selected|NotCorrect],
				             	pids = FunGetNewPids(NVerticesNotCorr)
				            }
			        end,
			   	Answer = FunAsk(Selected, hd(dict:fetch(Selected, DictQuestions)), length(DictsTrace), FunAsk),
			   	case Answer of
			        correct -> 
			        	IsCorrect;
			        %i -> YesAnswer;
			        incorrect -> 
			        	IsNotCorrect;
			        trust -> 
			        	{Selected, {Pid, CallRec}} = 
			        		digraph:vertex(G, Selected),
						TrustedPidFun = 
							case CallRec#callrec_stack_item.origin_callrec of 
								#call_info{call = {M,F,A}} ->
									case get_answer("Do you trust the process, the function, both or none?\n[p/f/b/n]: ", [p, f, b, n]) of 
										p -> 
											[process];
										f -> 
											[function];
										b -> 
											[process, function];
										n -> 
											[]
									end;
								#receive_info{} ->
									[process]
							end,
						Trusted1 = 
							case lists:member(function, TrustedPidFun) of 
								true -> 
									[V || 
										V <- digraph:vertices(G), 
										get_MFA_vertex(G,V) =:= get_MFA_vertex(G,Selected)];
								false ->
									[]
							end,
						Trusted2 = 
							case lists:member(process, TrustedPidFun) of 
								true -> 
									[V || 
										V <- digraph:vertices(G), 
										get_pid_vertex(G, V) =:= get_pid_vertex(G, Selected)];
								false ->
									[]
							end,
						NVerticesAfterTrust = 
							NSortedVertices -- digraph_utils:reachable(Trusted1 ++ Trusted2, G),
						NPreviousState = 
							case TrustedPidFun of 
								[] ->
									PreviousState;
								_ ->
									State0
							end, 
						State#edd_con_state{
							vertices = NVerticesAfterTrust,
							correct = EqualToSelected ++ Trusted1 ++ Trusted2,
							pids = FunGetNewPids(NVerticesAfterTrust),
							previous_state = NPreviousState
						};
			        dont_know ->
			        	State#edd_con_state{
			             	vertices = NSortedVertices -- [Selected],
			             	unknown = [Selected|Unknown]
			             };
			        undo -> 
			        	case PreviousState of
			                  none ->
			                     io:format("Nothing to undo\n"),
			                     State#edd_con_state{
		             				previous_state = PreviousState
		            			};
			                  _ ->
			                  	PreviousState
			             end;
			        change_strategy -> 
			        	Nstategy = 
				        	case get_answer("Select a strategy (Didide & Query or "
				                  ++"Top Down) [d/t]: ",[d, t]) of
								t -> 
				                  	top_down;
				                d -> 	
				                  	divide_query
				             end,
				        State#edd_con_state{
		             		strategy = Nstategy
		            	};
			        change_priority -> 
			        	NPriority = 
				        	case get_answer("Select priority (Old first, New first or Indeterminate) [o/n/i]: ",[o,n,i]) of
				                o -> 
				                  	old;
						        n -> 
				                  	new ;
				                i -> 
				                  	indet
				            end,
				        State#edd_con_state{
		             		priority = NPriority
		            	};
			        abort -> 
			        	State#edd_con_state{
					        vertices = [-1]
					    };
			        print_root -> 
			        	print_root_info(SummaryPids),
			        	State#edd_con_state{
		             		previous_state = PreviousState
		            	};
			        {CorrIncorr, {goto,Node}} ->
			        	StateCI = 
			        		case CorrIncorr of 
			        			correct -> 
			        				IsCorrect;
			        			incorrect ->
			        				IsNotCorrect
			        		end,
			        	NewStateFromNode(StateCI, Node);
			        {goto, Node} ->
						NewStateFromNode(State, Node);
					{from_seq_diag, Code} ->
						case obtain_node_seq_diagram(Code, DictsTrace, Comm, G) of 
							none ->
								State;
							Node ->
								NewStateFromNode(State, Node)
						end;
					help -> 
						print_help(),
						State#edd_con_state{
		             		previous_state = PreviousState
		            	};
					other -> 
						State#edd_con_state{
		             		previous_state = PreviousState
		            	}
			   end
				%io:format("Vertices de NState: ~p\n",[NState#debugger_state.vertices]),
		end,
	asking_loop(NState).

ask_question(Selected, #question{text = QuestionStr, answers = Answers}, OptsDiagramSeq, FunAsk) ->
	{DictAnswers, LastOpt} = 
		lists:mapfoldl(
			fun(E, Id) ->
				{{Id, E}, Id + 1}
			end,
			1,
			Answers
		),
	{AnswersList, QuestionComp} = 
		lists:mapfoldl(
			fun({Id, #answer{text = AnswerStr, complexity = Comp}}, Acc) ->
				{
					case get(print_session_info) of 
						true ->
							format("~p. - ~s \n(Complexity: ~p)", [Id, AnswerStr, Comp]);
						false ->
							format("~p. - ~s", [Id, AnswerStr])
					end,
					Acc + Comp
				}
			end,
			0,
			DictAnswers
		),
	AnswersStr = 
		string:join(AnswersList, "\n"),
	Options = 
		[any2str(Opt) || Opt <- lists:seq(1, LastOpt - 1)],
	OptionsStr = 
		string:join(Options, "/"),
	QuestionCompStr = 
		case get(print_session_info) of
			true ->
				format(
					"\n\n<Question complexity: ~p>\n<Node selected: ~p>\n", 
					[QuestionComp, Selected]);
			false ->
				""
		end,
	Prompt = 
		space()
		++ QuestionStr 
		++ "\n"
		++ AnswersStr
		++ QuestionCompStr
		++ "\n[" 
		++ OptionsStr
		++ "/t/d/i/c/s/p/r/u/h/a]: ",
	[_|Answer0] = lists:reverse(io:get_line(Prompt)),
	Answer = lists:reverse(Answer0),
	get_behavior(Answer, DictAnswers, OptsDiagramSeq, FunAsk).


get_behavior("t", _, _, _) ->
	trust;
get_behavior("d", _, _, _) ->
	dont_know;
get_behavior("i", _, _, _) ->
	correct;
get_behavior("s", _, _, _) ->
	change_strategy;
get_behavior("p", _, _, _) ->
	change_priority;
get_behavior("r", _, _, _) ->
	print_root;
get_behavior("u", _, _, _) ->
	undo;
get_behavior("h", _, _, _) ->
	help;
get_behavior("a", _, _, _) ->
	abort;
get_behavior("c", _, OptsDiagramSeq, _) ->
	{
		from_seq_diag, 
		get_answer(
			"Select an event from the sequence diagram: ",
			lists:seq(1,OptsDiagramSeq))
	};
get_behavior(NumberStr, DictAnswers, OptsDiagramSeq, FunAsk) ->
	put(question_answered, get(question_answered) + 1),
	try 
		Number = element(1,string:to_integer(NumberStr)),
		#answer{when_chosen = Behaviour} = element(2, lists:keyfind(Number, 1, DictAnswers)),
		AccComplexity = 
			[(element(2, lists:keyfind(N, 1, DictAnswers)))#answer.complexity
			|| N <- lists:seq(1, Number)],
		put(question_complexity, get(question_complexity) + lists:sum(AccComplexity)),
		case Behaviour of 
			#question{answers = [Answer = #answer{text = TextAns, when_chosen = BehAns}]} ->
				io:format("Automatic selection (only one option):\n" ++ TextAns),
				BehAns;
			#question{} ->
				FunAsk(-1, Behaviour, OptsDiagramSeq, FunAsk);
			_ ->
				Behaviour
		end
	catch
		_:_ ->
			other
	end.
	
marca() ->
  "@@--@@\n".

session_report() ->
	case get(print_session_info) of 
		true -> 
			io:format(marca()),
			io:format("{\n"),
		  	io:format("\t\"Answered questions\" : ~p,\n", [get(question_answered)]),
			io:format("\t\"Questions' complexity\" : ~p,\n", [get(question_complexity)]),
			
			io:format("\t\"Initial PID selection Complexity\" : ~p,\n", [get(initial_complexity)]),
			
			io:format("\t\"Evaluation tree time (microseconds)\" : ~p,\n", [get(eval_tree_time)]),
			io:format("\t\"Evaluation tree memory (bytes)\" : ~p,\n", [get(eval_tree_memory)]),
			io:format("\t\"Evaluation tree nodes\" : ~p,\n", [get(eval_tree_nodes)]),

			io:format("\t\"Sequence diagram time (microseconds)\" : ~p,\n", [get(seq_diag_time)]),
			io:format("\t\"Sequence diagram memory (bytes)\" : ~p,\n", [get(seq_diag_memory)]),
			io:format("\t\"Sequence diagram events\" : ~p,\n", [get(seq_diag_events)]),
			io:format("\t\"Sequence diagram events + Lasts\" : ~p\n", [get(seq_diag_events_lasts)]),
			io:format("}\n"),
			io:format(marca());
		false -> 
			ok 
	end.

%session_report() ->
%	io:format(space()),
%	io:format("SESSION DATA\n"),
%	io:format("Answered questions:\t~p\n", [get(question_answered)]),
%	io:format("Questions' complexity:\t~p\n", [get(question_complexity)]),
%	io:format(space()),
%	io:format(space()),
%	io:format("INITIAL PID SELECTION COMPLEXITY\n"),
%	io:format("Complexity:\t~p\n", [get(initial_complexity)]),
%	io:format(space()),
%	io:format(space()),
%	io:format("EVALUATION TREE BUILDING DATA\n"),
%	io:format("Time:\t~p microseconds\n", [get(eval_tree_time)]),
%	io:format("Memory:\t~p bytes\n", [get(eval_tree_memory)]),
%	io:format("Nodes:\t~p\n", [get(eval_tree_nodes)]),
%	io:format(space()),
%	io:format(space()),
%	io:format("SEQUENCE DIAGRAM BUILDING DATA\n"),
%	io:format("Time:\t\t~p microseconds\n", [get(seq_diag_time)]),
%	io:format("Memory:\t\t~p bytes\n", [get(seq_diag_memory)]),
%	io:format("Events:\t\t~p\n", [get(seq_diag_events)]),
%	io:format("Events + Lasts:\t~p\n", [get(seq_diag_events_lasts)]),
%	io:format(space()).

