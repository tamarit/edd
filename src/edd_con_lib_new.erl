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

-module(edd_con_lib_new).

-export([dot_graph_file/2, ask/3,
	ask_new/3,
	any2str/1, tab_lines/1, build_call_string/1,
	question_list/2, format/2]).

-record(debugger_state, 
	{graph, strategy, priority,
	 vertices = [], correct = [],
	 not_correct = [], unknown = [], 
	 previous_state = [],
	 preselected = none,
	 pids = []}).

-include_lib("edd_con.hrl").


ask(G,Strategy,Priority)->
	ok.
	

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
		space() ++ "PROCESS " ++ edd_con_lib_new:any2str(Pid)
		++ "\nFirst call " ++ build_call_string(element(2, Call))
		++ "\nResult " ++ any2str(Result)
		++ "\n" ++ question_list("sent messages", Sent)
		++ "\n" ++ question_list("spawned processes", Spawned)
		++ space(),
	io:format("~s", [Str]).

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
        lists:foldl(fun(E, Acc) -> Acc ++ [$\t|edd_con:pp_item(E)] ++ "\n" end, "", List), 
    Text ++ ":\n" ++ lists:droplast(PPList).

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
    end.
% TODO: This clause is temporal. Should be removed
% build_call_string(_) ->
%     "".

build_args_list_string([]) ->
    "";
build_args_list_string([E]) ->
    format("~p", [E]);
build_args_list_string([E | Rest]) ->
    format("~p, ~s", [E, build_args_list_string(Rest)]).

ask_initial_process(Pids) ->
	PidsString = 
		[
			{Pid, format(
				"~p\n\tFirst call:~s\n\tResult: ~s", 
				[Pid, build_call_string(element(2, Call)), any2str(Result)]
			) }
		|| {Pid, Call, _, _, Result} <- Pids],
	{StrPidsDict,LastId} = 
		lists:mapfoldl(
			fun({Pid, StrPid}, Id) -> 
				{{io_lib:format("~p.- ~s\n",[Id,StrPid]),{Id,Pid}}, Id+1} 
			end, 
		1, 
		PidsString),
	{StrPids, Dict} = 
		lists:unzip(StrPidsDict),
	Options = 
		lists:concat(StrPids) 
			++ io_lib:format("~p.- None\n",[LastId]),
	NDict = 
		[{LastId,none} | Dict],
	Question = 
		io_lib:format(
				space() 
				++ "Pid selection" 
				++ space() 
				++ Options 
				++ "\nPlease, insert a PID where you have observed a wrong behaviour: [1..~p]: ",
			[LastId]),
	Answer = 
		% get_answer(pids_wo_quotes(Question),lists:seq(0,LastId)),
		get_answer(Question,lists:seq(1,LastId)),
	Result = 
		[Data || {Option,Data} <- NDict, Answer =:= Option],
	FirstPid = 
		case Result of 
			[none] ->
				[Pid || {_,Pid} <- Dict];
			_ -> 
				Result 
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
	FirstPid.

get_pid_vertex(V,G) ->
	{V,{Pid,_}} = digraph:vertex(G,V),
	Pid.

print_buggy_node_new(G, NotCorrectVertex, Message) ->
	{NotCorrectVertex, {Pid, CallRec}} = digraph:vertex(G,NotCorrectVertex),
	StrProblem = 
		case CallRec#callrec_stack_item.origin_callrec of 
			#call_info{call = {M,F,A}} ->
				format(
                	"call ~p:~p(~s)", 
                	[M,F,string:join(lists:map(fun any2str/1, A), ", ")]);
			#receive_info{pos_pp = {{pos_info,{_, F, L, ReceiveStr0}}}} ->
				format("receive\n~s\nin ~s:~p", [ReceiveStr0, F, L])
		end,	
	io:format("~sThe problem is in pid ~p\nwhile running ~s\n",[Message, Pid, StrProblem]).

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



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% New question building
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(edd_con_state, 
	{graph, dict_questions, strategy, priority,
	 vertices = [], correct = [],
	 not_correct = [], unknown = [], 
	 previous_state = none,
	 pids = [],
	 preselected = none,
	 fun_print_root_info,
	 summary_pids = [],
	 dicts_trace = [],
	 comms = []}).

%%------------------------------------------------------------------------------
%% @doc Traverses the tree 'G' asking the programmer until it finds the buggy 
%%      node. The tree 'G' must be a digraph representing the abbreviated proof 
%%      tree of the evaluation of an expression that yields an incorrect value.
%%      When it finds the buggy node, shows the function rule responsible for
%%      the incorrect value. The strategy followed is indicated by its second
%%      argument.      
%% @end
%%------------------------------------------------------------------------------
ask_new({PidsInfo, Comm, {G, DictQuestions}, DictTrace}, Strategy, Priority) ->
	SummaryPidsInfo = edd_con:summarizes_pidinfo(PidsInfo),
	FunPrintRootInfo = 
		fun() ->
			lists:foreach(
				fun print_summary_pid/1, 
				SummaryPidsInfo)
		end,
	FunPrintRootInfo(),
	FirstPid = ask_initial_process(SummaryPidsInfo),
	IniCorrect = [],
	Vertices = digraph:vertices(G) -- [0|IniCorrect],
	FirstState = 
		#edd_con_state{
			graph = G,
			dict_questions = DictQuestions,
			strategy = Strategy,
			priority = Priority,
			vertices = Vertices,
			not_correct = [0],
			correct = IniCorrect,
			pids = FirstPid,
			fun_print_root_info = FunPrintRootInfo,
			summary_pids = SummaryPidsInfo,
			dicts_trace = replicate_receives(lists:sort(dict:to_list(DictTrace)), Comm, []),
			comms = Comm
		},
	ask_about(FirstState).

ask_about(State) -> 
	NState = #edd_con_state{
			correct = Correct,
			not_correct = NotCorrect,
			unknown = Unknown,
			graph = G} = 
	   	asking_loop(State),
	case NotCorrect of
	     [-1] ->
	     	io:format("Debugging process finished\n");
	     _ -> 
	        NotCorrectVertexs = [NCV || NCV <- NotCorrect, 
	                                   (digraph:out_neighbours(G, NCV)--Correct)==[] ],
	        case NotCorrectVertexs of
	             [] ->
	             	io:format("Not enough information.\n"),
	             	NotCorrectWithUnwnownVertexs = 
			  			[NCV || NCV <- NotCorrect, 
	                          (digraph:out_neighbours(G, NCV)--(Correct++Unknown))=:=[]],
	                Maybe0 = 
	                         [U || U <- Unknown, 
	                               NCV <- NotCorrectWithUnwnownVertexs,
	                               lists:member(U,digraph:out_neighbours(G, NCV))],
	                Maybe = find_unknown_children(G,Unknown,Maybe0),
					case get_answer("Do you want to try to answer"
					     ++" the needed information? [y/n]: ",[y,n]) of
					     y -> ask_about(NState#debugger_state{vertices = Maybe});
					     n -> 
			                [print_buggy_node_new(G,V,
			                        "Call to a function that could contain an error") 
			                        || V <- NotCorrectWithUnwnownVertexs],
			                [print_buggy_node_new(G,V,
			                         "This call has not been answered and could contain an error") 
			                        || V <- Maybe]
					end;
	             [NotCorrectVertex|_] ->
	               	print_buggy_node_new(G,NotCorrectVertex,
	               		"\nThe error has been detected:\n")
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
		comms = Comm
	}) ->
	State = State0#edd_con_state{preselected = none},
	GetNodesPids = 
		fun(CVertices) ->
			[V || 
				V <- CVertices, 
			   	lists:member(get_pid_vertex(V,G),Pids)] 
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
								[V || V <- Vertices, not lists:member(get_pid_vertex(V,G),Pids)],
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
									new -> hd(lists:reverse(SameThanFirst));
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
						EqualToSeleceted = 
							[V || V <- Vertices, begin {V,L1} = digraph:vertex(G,V),
							                           {Selected,L2} = digraph:vertex(G,Selected),
							                           (L1 =:= L2) 
							                     end],
						NVerticesCorr = 
							NSortedVertices -- digraph_utils:reachable(EqualToSeleceted,G),
						State#edd_con_state{
							vertices = NVerticesCorr,
							correct = EqualToSeleceted ++ Correct,
							previous_state = State,
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
				             	previous_state = State,
				             	pids = FunGetNewPids(NVerticesNotCorr)
				            }
			        end,
			   	Answer = ask_question(hd(dict:fetch(Selected, DictQuestions)), length(DictsTrace)),
			   	case Answer of
			        correct -> 
			        	IsCorrect;
			        %i -> YesAnswer;
			        incorrect -> 
			        	IsNotCorrect;
			        dont_know ->
			        	State#edd_con_state{
			             	vertices = NSortedVertices -- [Selected],
			             	unknown = [Selected|Unknown],
			             	previous_state = State
			             };
			        undo -> 
			        	case PreviousState of
			                  none ->
			                     io:format("Nothing to undo\n"),
			                     State;
			                  _ ->
			                  	PreviousState
			             end;
			        change_strategy -> 
			        	case get_answer("Select a strategy (Didide & Query or "
			                  ++"Top Down) [d/t]: ",[t,d]) of
			                  t -> 
			                  	State#edd_con_state{
					             	strategy = top_down
					             };
			                  d -> 
			                  	State#edd_con_state{
					             	strategy = divide_query
					             }
			             end;
			        change_priority -> 
			        	case get_answer("Select priority (Old, New or Indeterminate) [o/n/i]: ",[o,n,i]) of
			                  o -> 
			                  	State#edd_con_state{
					             	priority = old
					             };
					          n -> 
			                  	State#edd_con_state{
					             	priority = new
					             };
			                  i -> 
			                  	State#edd_con_state{
					             	priority = indet
					             }
			             end;
			        abort -> 
			        	State#edd_con_state{
					        vertices = [-1]
					    };
			        print_root -> 
			        	FR = State#edd_con_state.fun_print_root_info,
			        	FR(),
			        	State;
			        {CorrIncorr, {goto,Node}} ->
			        	StateCI = 
			        		case CorrIncorr of 
			        			correct -> 
			        				IsCorrect;
			        			incorrect ->
			        				IsNotCorrect
			        		end,
			        	StateCI#edd_con_state{
					     	preselected = Node,
					     	pids = lists:usort([get_pid_vertex(Node,G)|Pids])
					    };
					{from_seq_diag, Code} ->
						{E, Ns} = lists:nth(Code, DictsTrace),
						% io:format("~p\n~p\n~p\n", [E, DictsTrace, Comm]),
						EventBoolList = 
							lists:map(fun(C) -> is_the_event(C, E) end, Comm),
						% io:format("~p\n", [EventBoolList]),
						{_,[Pos]} = 
							lists:foldl(
								fun
									(true, {Curr, Acc}) ->
										{Curr + 1, [Curr]};
									(false, {Curr, Acc}) ->
										{Curr + 1, Acc}
								end,
								{1,[]},
								EventBoolList),
						io:format("Selected event:\n"),
						case lists:nth(Pos, Comm) of
							{spawned,{spawn_info,Spawner,Spawned,_}} -> 
								io:format("~p spawned ~p", [Spawner, Spawned]);
							{sent,MessageInfo} ->
								io:format("Sent message: ~s", [edd_con:pp_item(MessageInfo)]);
							{received,MessageInfo} ->
								io:format("Consumed message: ~s", [edd_con:pp_item(MessageInfo)])
						end,
						Node = hd(Ns),
						State#edd_con_state{
					     	preselected = Node,
					     	pids = lists:usort([get_pid_vertex(Node,G)|Pids])
					    };
					other -> 
						State
			   end
				%io:format("Vertices de NState: ~p\n",[NState#debugger_state.vertices]),
		end,
	asking_loop(NState).

ask_question(#question{text = QuestionStr, answers = Answers}, OptsDiagramSeq) ->
	{DictAnswers, LastOpt} = 
		lists:mapfoldl(
			fun(E, Id) ->
				{{Id, E}, Id + 1}
			end,
			1,
			Answers
			),
	AnswersList = 
		lists:map(
			fun({Id, #answer{text = AnswerStr}}) ->
				format("~p. - ~s", [Id, AnswerStr])
			end,
			DictAnswers
			),
	AnswersStr = 
		string:join(AnswersList, "\n"),
	Options = 
		[any2str(Opt) || Opt <- lists:seq(1, LastOpt - 1)],
	OptionsStr = 
		string:join(Options, "/"),
	Prompt = 
		space()
		++ QuestionStr 
		++ "\n"
		++ AnswersStr
		++ "\n[" 
		++ OptionsStr
		++ "/c/d/s/p/r/u/a]: ",
	[_|Answer0] = lists:reverse(io:get_line(Prompt)),
	Asnwer = lists:reverse(Answer0),
	get_behaviour(Asnwer, DictAnswers, OptsDiagramSeq).

get_behaviour("d", _, _) ->
	dont_know;
get_behaviour("s", _, _) ->
	change_strategy;
get_behaviour("p", _, _) ->
	change_priority;
get_behaviour("r", _, _) ->
	print_root;
get_behaviour("u", _, _) ->
	undo;
get_behaviour("a", _, _) ->
	abort;
get_behaviour("c", _, OptsDiagramSeq) ->
	{
		from_seq_diag, 
		get_answer(
			"Select an event from the sequence diagram: ",
			lists:seq(1,OptsDiagramSeq))
	};
get_behaviour(NumberStr, DictAnswers, OptsDiagramSeq) ->
	try 
		Number = element(1,string:to_integer(NumberStr)),
		#answer{when_chosen = Behaviour} = element(2, lists:keyfind(Number, 1, DictAnswers)),
		case Behaviour of 
			#question{} ->
				ask_question(Behaviour, OptsDiagramSeq);
			_ ->
				Behaviour
		end
	catch
		_:_ ->
			other
	end.


