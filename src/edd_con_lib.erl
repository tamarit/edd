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

-export([dot_graph_file/2, ask/3]).

-record(debugger_state, 
	{graph, strategy, priority,
	 vertices = [], correct = [],
	 not_correct = [], unknown = [], 
	 previous_state = [],
	 preselected = none,
	 pids = []}).



%%------------------------------------------------------------------------------
%% @doc Traverses the tree 'G' asking the programmer until it finds the buggy 
%%      node. The tree 'G' must be a digraph representing the abbreviated proof 
%%      tree of the evaluation of an expression that yields an incorrect value.
%%      When it finds the buggy node, shows the function rule responsible for
%%      the incorrect value. The strategy followed is indicated by its second
%%      argument.      
%% @end
%%------------------------------------------------------------------------------
%-spec ask( G :: digraph:graph(), Strategy,Priority) -> ok.
ask(G,Strategy,Priority)->
	print_root_info(G),
	% STrustedFunctions = 
	%   io:get_line("Please, insert a PID where you have observed a wrong beahviour: "),
	%FirstPid = translate_string_to_pid(STrustedFunctions),
	% IniCorrect = [V || V <- digraph:vertices(G),
	%                    lists:member(get_MFA_Label(G,V),TrustedFunctions)],
	FirstPid = ask_initial_process(G),
	case FirstPid of 
		[PidSelected] ->
			io:format("\nSelected initial PID: ~s\n",[PidSelected]);
		_ ->
			io:format("\nInitial PID not defined.\n")
	end,
	IniCorrect = [],
	Root = look_for_root(G),
	Vertices = digraph:vertices(G) -- [Root|IniCorrect],
	FirstState = 
		#debugger_state{
			graph = G,
			strategy = Strategy,
			priority = Priority,
			vertices = Vertices,
			not_correct = [Root],
			pids = FirstPid
		},
	%_ = io:get_line(""),
	ask_about(FirstState).
	

ask_about(State) -> 
	%{Correct,NotCorrect,Unknown,_,NStrategy} = 
	NState = 
	   asking_loop(State),
	Correct = NState#debugger_state.correct,
	NotCorrect = NState#debugger_state.not_correct,
	Unknown = NState#debugger_state.unknown,
	G = NState#debugger_state.graph,
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
			                [print_buggy_node(G,V,
			                        "Call to a function that could contain an error") 
			                        || V <- NotCorrectWithUnwnownVertexs],
			                [print_buggy_node(G,V,
			                         "This call has not been answered and could contain an error") 
			                        || V <- Maybe]
					end;
	             [NotCorrectVertex|_] ->
	               	print_buggy_node(G,NotCorrectVertex,
	               		"\nThe error has been detected:\n")
	    %            	case get_answer("Do you want to continue the debugging session"
					%      ++" inside this function? [y/n]: ",[y,n]) of
					%      y -> 
					%      	edd_zoom:zoom_graph(get_call_string(G,NotCorrectVertex),Graph);
					%      n -> 
			  %               ok
					% end
	        end
	end,
	ok.
	
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
	 
 
print_buggy_node(G,NotCorrectVertex,Message) ->
	{NotCorrectVertex,Label} = digraph:vertex(G,NotCorrectVertex),
	%io:format("~s:\nVertex:~p\n~s\n",[Message,NotCorrectVertex,build_question(Label)]).
	{Pid,ACall} = 
		case Label of 
			{{to_receive,_,ACall_,_,_,_,_,_},Pid_} ->
				{Pid_,ACall_};
			{{to_value,Expr, Clause, _, _},Pid_} ->
				case Expr of 
					{'receive',{AExpr,Value,Line,File}, MsgSender,Context,_,_} ->
						{Pid_,{'receive',{AExpr,Value,Line,File},Clause,MsgSender,Context,none,none,none,[]}};
					{call,{ACall_,_},File,Line,_} ->
						{Pid_,{'call',ACall_,File,Line}}
				end
		end,
	io:format("~sThe problem is in pid ~s ",[Message,Pid]),
	case ACall of
		{'receive',{AReceive,_,LineR,FileR},_,_,_,_,_,_,_} ->
			io:format("with the receive expression:\n~s.\n",[build_receive(AReceive,FileR,LineR,0,none)]);
		{'call',ACall__,FileCall,LineCall} ->
			io:format("with the call ~s.\n",[erl_prettypr:format(ACall__)]),
			case FileCall of 
				none ->
					ok;
				_ -> 
					io:format("\nfun location: (~s, line ~p)\n",[FileCall, LineCall])
			end	
	end.
			
	% {NotCorrectVertex,{Label,Clause,File,Line}} = digraph:vertex(G,NotCorrectVertex),
	% io:format("~s:\n~s\n",[Message,transform_label(Label,[])]),
	% case File of 
	% 	none ->
	% 		ok;
	% 	_ ->
	% 		io:format("fun location: (~s, line ~p)\n",[File,Line])
	% end,
	% print_clause(G,NotCorrectVertex,Clause).
   
% print_clause(G,NotCorrectVertex,Clause) ->
% 	{Clauses,FunName,Arity} = 
% 		case get_MFA_Label(G,NotCorrectVertex) of 
% 			{{'fun',_,_} = AnoFun ,_,_}  ->
% 				{erl_syntax:fun_expr_clauses(AnoFun),none,none};
% 			{ModName,FunName0,Arity0} ->
% 				{ok,M} = smerl:for_file(atom_to_list(ModName) ++ ".erl"),
% 				Clauses_ = 
% 				  hd([Clauses_ || 
% 		             {function,_,FunName_,Arity_,Clauses_} <- smerl:get_forms(M),
% 		             FunName_ =:= FunName0, Arity_ =:= Arity0]),
% 				{Clauses_,FunName0,Arity0} 
% 		end,
% 	% {ModName,FunName,Arity} = get_MFA_Label(G,NotCorrectVertex),
% 	% {ok,M} = smerl:for_file(atom_to_list(ModName) ++ ".erl"),
% 	% Clauses = hd([Clauses_ || 
% 	%               	{function,_,FunName_,Arity_,Clauses_} <- smerl:get_forms(M),
% 	% 				FunName_ =:= FunName, Arity_ =:= Arity]),
% 	case Clause > length(Clauses)  of
% 	     true -> 		     	
% 	     	io:format("There is no clause matching.\n");
% 	     false -> 
% 	     	io:format("Please, revise the ~s clause:\n",[get_ordinal(Clause)]),
% 			SelectedClause = lists:nth(Clause, Clauses),
% 			ClauseStr = 
% 				case FunName of
% 					none -> 
% 						erl_prettypr:format(erl_syntax:fun_expr([SelectedClause]));
% 					_ -> 
% 						erl_prettypr:format({function,1,FunName,Arity,[SelectedClause]})
% 				end,
% 			io:format("~s\n",[ClauseStr])
% 	end.
	
	
% get_MFA_Label(G,Vertex) ->
% 	{Vertex,{Label,_,File,Line}} = digraph:vertex(G,Vertex),
% 	{ok,Toks,_} = erl_scan:string(lists:flatten(Label)++"."),
% 	{ok,[Aexpr|_]} = erl_parse:parse_exprs(Toks),
% 	{match,_,{call,_,Called,APars},_} = Aexpr,
% 	case Called of 
% 		{remote,_,{atom,_,ModName},{atom,_,FunName}} ->
% 			Arity = length(APars),
% 			{ModName,FunName,Arity};
% 		_ ->
% 			%io:format("Called: ~p\n",[Called]),
% 			{Called,File,Line}
% 	end.

% get_call_string(G,Vertex) ->
% 	{Vertex,{Label,_,File,Line}} = digraph:vertex(G,Vertex),
% 	{ok,Toks,_} = erl_scan:string(lists:flatten(Label)++"."),
% 	{ok,[Aexpr|_]} = erl_parse:parse_exprs(Toks),
% 	{match,_,Call = {call,_,Called,_},_} = Aexpr,
% 	case Called of 
% 		{remote,_,_,_} ->
% 			erl_prettypr:format(Call);
% 		_ ->
% 			{erl_prettypr:format(Call),File,Line}
% 	end.
	
get_ordinal(1) -> "first";
get_ordinal(2) -> "second";
get_ordinal(3) -> "third";
get_ordinal(4) -> "fourth";
get_ordinal(5) -> "fifth";
get_ordinal(6) -> "sixth";
get_ordinal(7) -> "seventh";
get_ordinal(N) ->
	integer_to_list(N)++"th".
	
% translate_string_to_pid("\n") ->
% 	[];
% translate_string_to_pid("") ->
% 	[];
% translate_string_to_pid(List0) ->
% 	List = string:strip(List0),
% 	[$\n|ListPid0] = lists:reverse(List),
% 	lists:reverse(ListPid0).
	
look_for_root(G)->
	case digraph:no_vertices(G) of
	     0 -> no;
	     1 -> hd(digraph:vertices(G));
	     _ -> hd([V||V <- digraph:vertices(G), digraph:in_degree(G, V)==0])
	end.

print_root_info(G) ->
	Root = look_for_root(G),
    {Root,LabelRoot} = digraph:vertex(G,Root),
	{Question,_} = build_question(LabelRoot),
	io:format("\n~s\n",[pids_wo_quotes(Question)]).

ask_initial_process(G) ->
	Root = look_for_root(G),
    {Root,{{root,_,_,Summary},_}} = digraph:vertex(G,Root),
	Pids = [PidSummary || {PidSummary,_,_,_} <- lists:usort(Summary)],
	{StrPidsDict,LastId} = lists:mapfoldl(fun(Pid, Id) -> {{io_lib:format("~p.- ~s\n",[Id,Pid]),{Id,Pid}}, Id+1} end, 0, Pids),
	{StrPids, Dict} = lists:unzip(StrPidsDict),
	Options = lists:concat(StrPids) ++ io_lib:format("~p.- None\n",[LastId]),
	NDict = [{LastId,none} | Dict],
	Question = io_lib:format("\nList of pids:\n" ++ Options ++ "\nPlease, insert a PID where you have observed a wrong beahviour: [0...~p]: ",[LastId]),
	Answer = get_answer(pids_wo_quotes(Question),lists:seq(0,LastId)),
	Result = [Data || {Option,Data} <- NDict, Answer =:= Option],
	case Result of 
		none ->
			[Pid || {_,Pid} <- Dict];
		_ -> 
			Result 
	end.

get_pid_vertex(V,G) ->
	{V,{_,Pid}} = digraph:vertex(G,V),
	Pid.

asking_loop(#debugger_state{vertices = []} = State) -> 
	State;
asking_loop(#debugger_state{vertices = [-1]} = State) ->
	State#debugger_state{
		correct = [-1],
		not_correct = [-1],
		unknown = [-1]
	};
asking_loop(State0) ->
	Preselected = State0#debugger_state.preselected,
	State = State0#debugger_state{preselected = none},
	G  = State#debugger_state.graph,
	Strategy = State#debugger_state.strategy,
	Priority = State#debugger_state.priority,
	Pids = State#debugger_state.pids,
	%io:format("Pids: ~p\n",[Pids]),
	%[io:format("~p\n",[get_pid_vertex(V,G)]) || V <- State#debugger_state.vertices],
	Vertices =  State#debugger_state.vertices,
	Correct = State#debugger_state.correct,
	NotCorrect = State#debugger_state.not_correct,
	Unknown = State#debugger_state.unknown,
	PreviousState = State#debugger_state.previous_state,
	{Selected,NSortedVertices} =
		case Preselected of 
			none ->
				VerticesPid = [V || V <- State#debugger_state.vertices, lists:member(get_pid_vertex(V,G),Pids)],
				VerticesNoPid = [V || V <- State#debugger_state.vertices, not lists:member(get_pid_vertex(V,G),Pids)],
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
				%io:format("VerticesWithValues: ~p\n",[VerticesWithValues]),
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
				%io:format("Vertices: ~p\n",[Vertices]),
				{Preselected,Vertices -- [Preselected]}
		end,
	YesAnswer = begin
	             EqualToSeleceted = 
	                [V || V <- Vertices, begin {V,L1} = digraph:vertex(G,V),
	                                           {Selected,L2} = digraph:vertex(G,Selected),
	                                           (L1 =:= L2) 
	                                     end],
	             State#debugger_state{
	             	vertices = NSortedVertices -- digraph_utils:reachable(EqualToSeleceted,G),
	             	correct = EqualToSeleceted ++ Correct,
	             	previous_state = State
	             }
	            end, 
	Answer = ask_question(G,Selected),
	%io:format("Answer: ~p\n",[Answer]),
	NState = 
	   case Answer of
	        y -> YesAnswer;
	        %i -> YesAnswer;
	        n -> 
	        	State#debugger_state{
	             	vertices = digraph_utils:reachable([Selected],G) -- ([Selected|NotCorrect]++Correct++Unknown),
	             	not_correct = [Selected|NotCorrect],
	             	previous_state = State
	             };
	        d ->
	        	State#debugger_state{
	             	vertices = NSortedVertices -- [Selected],
	             	unknown = [Selected|Unknown],
	             	previous_state = State
	             };
	        u -> case PreviousState of
	                  [] ->
	                     io:format("Nothing to undo\n"),
	                     State;
	                  _ ->
	                  	PreviousState
	             end;
	        s -> case get_answer("Select a strategy (Didide & Query or "
	                  ++"Top Down) [d/t]: ",[t,d]) of
	                  t -> 
	                  	State#debugger_state{
			             	strategy = top_down
			             };
	                  d -> 
	                  	State#debugger_state{
			             	strategy = divide_query
			             }
	             end;
	        p -> case get_answer("Select priority (Old, New or Indeterminate) [o/n/i]: ",[o,n,i]) of
	                  o -> 
	                  	State#debugger_state{
			             	priority = old
			             };
			          n -> 
	                  	State#debugger_state{
			             	priority = new
			             };
	                  i -> 
	                  	State#debugger_state{
			             	priority = indet
			             }
	             end;
	        a -> 
	        	State#debugger_state{
			        vertices = [-1]
			    };
	        r -> 
	        	print_root_info(G),
	        	State;
	        {c,Node} ->
	        	%io:format("NSortedVertices: ~p\n",[NSortedVertices]),
	        	State#debugger_state{
	        		vertices = NSortedVertices -- digraph_utils:reachable([Selected],G),
	        		correct = [Selected| Correct],
			     	preselected = Node,
			     	%pids = [lists:flatten(io_lib:format("~p",[get_pid_vertex(Node,G)]))]
			     	pids = [lists:flatten(get_pid_vertex(Node,G))]
			    };
	        _ -> State
	   end,
	%io:format("Vertices de NState: ~p\n",[NState#debugger_state.vertices]),
	asking_loop(NState).
	
ask_question(G,V)->
	{V,Label} = digraph:vertex(G,V),
	%build_question(Label),
	io:format("\n"),
	{Question,Dict} = build_question(Label),
	%io:format("\nVertex: ~p\n~s",[V, Question]),
	io:format("~s",[pids_wo_quotes(Question)]),
	%NLabel = transform_label(lists:flatten(Label),[]),
	% case File of 
	% 	none ->
	% 		io:format("~s",[NLabel]);
	% 	_ ->
	% 		io:format("~s\nfun location: (~s, line ~p)",[NLabel,File,Line])
	% end,
	Answers = build_answers(Dict),
	[_|Answer]=lists:reverse(io:get_line("[" ++ Answers ++ "d/s/p/r/u/a]: ")),
	AAsnwer = list_to_atom(lists:reverse(Answer)),
	transform_answer(AAsnwer,Dict).
	
build_answers([]) ->
	"y/n/";
build_answers(Dict) ->
	lists:flatten([integer_to_list(Option)++ "/" || {Option,_} <- lists:reverse(Dict)]).
	
transform_label([],Acc) -> Acc;
transform_label([$[|Tail],Acc) ->
	{Numbers,[_|NTail]} = lists:splitwith(fun($]) -> false; (_) -> true end, Tail),
	Tokens = string:tokens(Numbers, ", \n\t"),
	case analyze_tokens(Tokens) of
	     {ok,List} -> transform_label(NTail,Acc++io_lib:format("~p",[List]));
	     error -> transform_label(Tail,Acc++[$[])
	end;
transform_label([Char|Tail],Acc) ->
	transform_label(Tail,Acc++[Char]).

transform_answer(Answer,[]) ->
	Answer;
transform_answer(Answer,Dict) ->
	case lists:member(Answer,[list_to_atom(integer_to_list(Option)) || {Option,_} <- Dict]) of 
		true ->
			Num = list_to_integer(atom_to_list(Answer)),
			Action = hd([Action_ || {Id,Action_} <- Dict,Num == Id]),
			%io:format("Action: ~p\n",[Action]),
			case Action of 
				correct ->
					y;
				incorrect ->
					n;
				{incorrect,_} ->
					n;
				{correct,{go_to,Node}} ->
					{c,Node};
				_ ->
					o
			end;
		false ->
			case lists:member(Answer,[d,s,p,r,u,a]) of 
				true ->
					Answer;
				false ->
					o 
			end
	end.

	
analyze_tokens([]) -> {ok,[]};
analyze_tokens([H|T]) -> 
	case string:to_integer(H) of
	     {Int,[]} when Int >= 32, Int =< 126 ->  
	     	case analyze_tokens(T) of
	     	     {ok,List} -> {ok,[Int|List]};
	     	     error -> error
	     	end;
	     _ -> error
	end.


question_to_receive_value(ACallReceive,ToValueReceive,Pid,Sent,Spawned,Transition_,Bindings) ->
	%io:format("ACallReceive: ~p\n",[ACallReceive]),
	{Transition,Context} = 
		case ACallReceive of
			{'receive',_,_,_,Context_,_,_,_,_} ->
				{{},Context_};
			_ -> 
				{Transition_,[]}
		end,
	%io:format("Context: ~p\n",[Context]),
	StartQuestion = 
		io_lib:format("Pid ~s",[Pid]) ++
		case ACallReceive of 
			{'receive',{AReceive,_,LineR,FileR},Clause,Consumed,_,_,_,_,_} ->
				" evaluates the receive expression:\n" ++ build_receive(AReceive,FileR,LineR,Clause,Consumed);
			{'call',ACall,FileCall,LineCall} ->
				" calls " ++ erl_prettypr:format(ACall) ++
				case FileCall of 
					none ->
						"";
					_ -> 
						io_lib:format("\nfun location: (~s, line ~p)",[FileCall, LineCall])
				end
		end ++
		"\nIs there anything incorrect?",
	{StrOption1,CurrenOption1, CurrenDict1} = 
		case Transition of
			{} -> 
				{"",1,[]};
			_ ->
				{_,_,_,NodeReceive,_} = Transition,
				{"\n1. - Previous evaluated receive:\n" ++ build_transition(Transition),
				2,
				[{1,{correct,{go_to,NodeReceive}}}]}
		end,
	{StrOption2,CurrenOption2, CurrenDict2} = 
		{io_lib:format("\n~p. - The spawned processes or sent messages:",[CurrenOption1]) ++ print_sent_spawned(Sent, Spawned),
		CurrenOption1 + 1,
		[{CurrenOption1,incorrect}|CurrenDict1]},
	{StrOption3,CurrenOption3,CurrenDict3} =
		case Context of 
			[] ->
				{"",CurrenOption2,CurrenDict2};
			_ -> 
				{io_lib:format("\n~p. - The context:\n",[CurrenOption2]) ++ get_context(Context),
				CurrenOption2 + 1,
				[{CurrenOption2,correct}|CurrenDict2]}
		end,
	{StrOption4,CurrenOption4,CurrenDict4} =
		case ACallReceive of
			{'receive',_,ClauseR,ConsumedR,_,_,_,_,DiscardedMessages} ->
				case ClauseR of 
					none ->
						{io_lib:format("\n~p. - To not consume any message",[CurrenOption3]) ++ 
						 case DiscardedMessages of 
						 	[] -> " because there is not any message receipt.";
						 	_ -> ", having discarded the following messages:" ++ get_messages(DiscardedMessages)
						 end,
						CurrenOption3 + 1,[{CurrenOption3,correct}|CurrenDict3]};
					_ ->
						questions_receive(CurrenOption3, ClauseR,ConsumedR,Bindings,CurrenDict3,DiscardedMessages)
				end;
			_ -> 
				{"",CurrenOption3,CurrenDict3}
		end,
	StrValue = 
		case ToValueReceive of 
			{'receive',AExpr,File,Line} -> 
				io_lib:format("\n~p. - To reach the receive expression:\n",[CurrenOption4]) ++ 
				 erl_prettypr:format(AExpr,[{paper, 300},{ribbon, 300}])++
				 io_lib:format("\nlocation (~s, line ~p)",[File,Line]);
			stuck_receive ->
				io_lib:format("\n~p. - To get blocked.",[CurrenOption4]);
			_ ->
				io_lib:format("\n~p. - To evaluate to ",[CurrenOption4]) ++ erl_prettypr:format(ToValueReceive)					
		end,
	{StrOption5,CurrenOption5,CurrenDict5} =
		{StrValue,CurrenOption4 + 1,[{CurrenOption4,incorrect}|CurrenDict4]},
	{StrOption6,_,CurrentDict6} =
		{io_lib:format("\n~p. - Nothing.\n",[CurrenOption5]) ,
		 CurrenOption5 + 1,
		 [{CurrenOption5,correct}|CurrenDict5]},
    %io:format("Dict: ~p\n",[CurrentDict6]),
	{StartQuestion ++ StrOption1 ++ StrOption2 ++ StrOption3 ++
	StrOption4 ++ StrOption5 ++ StrOption6,CurrentDict6}.
 
questions_receive(CurrenOption,Clause,{Sender,Msg,NodeSend},Bindings,CurrentDict,DiscardedMessages) ->
	{Str1,CurrenOption1,CurrentDict1} = 
		case DiscardedMessages of 
			[] ->
				{"",CurrenOption,CurrentDict};
			_ ->
				{io_lib:format("\n~p. - The discarded messages:\n~s",[CurrenOption,get_messages(DiscardedMessages)]),
				CurrenOption + 1,
				[{CurrenOption,incorrect}]}
		end,
	StartingQuestion =
		io_lib:format("~s\n~p. - The consumed message:\n\t",[Str1,CurrenOption1]) ++ Msg ++
		io_lib:format(", sent by ~p, ",[Sender]) ++
		io_lib:format("\n~p. - To enter in the ~s clause.",[CurrenOption1 + 1,get_ordinal(Clause)]),
	%io:format("NodeSend: ~p\n",[NodeSend]),
	StartingDict = 
		%[{CurrenOption1 + 1,{incorrect,{go_to,clauses}}}, {CurrenOption1,correct} |CurrentDict1],
		[{CurrenOption1 + 1,{incorrect,{go_to,clauses}}}, {CurrenOption1,{correct,{go_to,NodeSend}}} |CurrentDict1],
	case Bindings of 
		[] ->
			{StartingQuestion,CurrenOption1 + 2,StartingDict};
		_ ->
			{io_lib:format("~s\n~p. - The bindings:\n",[StartingQuestion,CurrenOption1 + 2]) ++ get_context(Bindings),
			CurrenOption1 + 3,
			[{CurrenOption1 + 2, incorrect,{go_to,clauses}}|StartingDict]}
	end.




build_question({{root,AExpr,AResult,Summary},Pid}) ->
	{io_lib:format("Main pid ~s\n",[Pid]) ++
	AExpr ++ 
	case AResult of 
		stuck_receive ->
			" is blocked";
		_ ->
			" = " ++ AResult
	end
	++ "\n" ++
	"Summary:\n" ++
	lists:flatten([io_lib:format("Pid: ~p; Call: ~s; Spawned: ~p; Sent: [",[PidSummary,Call,Spawned]) ++ print_sent(Sent) ++"]\n"
	|| {PidSummary,Call,Spawned,Sent} <- lists:usort(Summary)]),[]};
build_question({{to_receive,AExpr,ACall,File,Line,Sent,Spawned,Transition_},Pid}) ->
	question_to_receive_value(ACall,{'receive',AExpr,File,Line},Pid,Sent,Spawned,Transition_,[]);
	% io_lib:format("Pid ~p",[Pid]) ++
	% case ACall of 
	% 	{'receive',{AReceive,LineR,FileR},Clause,Consumed} ->
	% 		"evaluates " ++ build_receive(AReceive,FileR,LineR,Clause,Consumed);
	% 	_ ->
	% 		" calls " ++ erl_prettypr:format(ACall)
	% end ++
	% " and then reaches the receive expression:\n" ++
	% erl_prettypr:format(AExpr,[{paper, 300},{ribbon, 300}]) ++
	% io_lib:format("\nlocation (~s, line ~p)\n",[File,Line]) ++ 
	% build_transition(Transition) ++ 
	% print_sent_spawned(Sent, Spawned);
build_question({{to_value,Expr, Clause, Sent, Spawned},Pid}) ->
	case Expr of 
			{'receive',{AExpr,Value,Line,File}, MsgSender,Context,Bindings,DiscardedMessages} ->
				question_to_receive_value({'receive',{AExpr,Value,Line,File},Clause,MsgSender,Context,none,none,none,DiscardedMessages},Value,Pid,Sent,Spawned,{},Bindings);
			{call,{ACall,Value},File,Line,Transition} ->
				question_to_receive_value({'call',ACall,File,Line},Value,Pid,Sent,Spawned,Transition,[])
	end.
	% {PExpr,Value,Transition,Context} = 
	% 	case Expr of 
	% 		{'receive',{AExpr,Value_,Line,File}, MsgSender,Context_,Bindings_} ->
	% 			{" evaluates the receive expression:\n" ++ build_receive(AExpr,File,Line,Clause,MsgSender),
	% 			 Value_,{},Context_};
	% 		{call,{ACall,Value_},File,Line,Transition_} ->
	% 			{" calls " ++ erl_prettypr:format(ACall) ++
	% 				case File of 
	% 					none ->
	% 						"";
	% 					_ -> 
	% 						io_lib:format("\nfun location: (~s, line ~p)",[File, Line])
	% 				end,
	% 			Value_,Transition_,[]}
	% 	end,
	% io_lib:format("Pid ~p",[Pid]) ++ PExpr ++
	% case Value of 
	% 	stuck_receive ->
	% 		" and gets blocked.";
	% 	_ ->
	% 		" that evaluates to " ++ erl_prettypr:format(Value) ++ "."
	% end ++
	% build_transition(Transition) ++ 
	% case Context of 
	% 	[] -> 
	% 		"";
	% 	_ ->
	% 		"\nContext:\n" ++ get_context(Context) 
	% end ++ 
	% print_sent_spawned(Sent, Spawned);

% Given the context:

% The receive expression:

% matching with the ith clause succeed
% with the message sent by
% Bindings:
% Is this correct?


% Guard of the ith clause succeed
% Is this correct?

% build_question({receive_clause,PatGuard,FailSucced,{AExpr,_,Line,File},Pid, {Sender,Msg}, Clause, Bindings,Context} ) ->
% 	{io_lib:format("For Pid ~p,",[Pid]) ++
% 	case Context of 
% 		[] -> 
% 			"";
% 		_ ->
% 			"\ngiven the context:\n" ++
% 			get_context(Context) 
% 	end ++
% 	"\nthe receive expression:\n" ++
% 	erl_prettypr:format(AExpr,[{paper, 300},{ribbon, 300}]) ++
% 	io_lib:format("\nlocation (~s, line ~p)",[File,Line]) ++
% 	case PatGuard of 
% 		pattern ->
% 			io_lib:format("\nmatching with the ~s clause ~ps",[get_ordinal(Clause),FailSucced]);
% 		guard ->
% 			io_lib:format("\nguard of the ~s clause ~ps",[get_ordinal(Clause),FailSucced])
% 	end ++
% 	"\nwith the message " ++ Msg ++ io_lib:format(" sent by ~p",[Sender]) ++
% 	case Bindings of 
% 		[] ->
% 			".";
% 		_ ->
% 			",\nand with the following bindings:\n" ++ get_context(Bindings)
% 	end ++
% 	"\nIs this correct?",[]}.
	% io_lib:format("\nlocation (~s, line ~p)\n",[File,Line]) ++ 
	% io_lib:format("Pid ~p",[Pid]) ++
	% " evaluates the receive expression:\n" ++ 
	% erl_prettypr:format(AExpr,[{paper, 300},{ribbon, 300}]) ++
	% io_lib:format("\nlocation (~s, line ~p)\n",[File,Line]) ++ 
	% " with the message " ++ Msg ++ io_lib:format(" sent by ~p, ",[Sender]) ++
	% "and then, the " ++ 
	% case PatGuard of 
	% 	pattern ->
	% 		"pattern";
	% 	guard ->
	% 		"matching with bounded variables or the guard"
	% end ++
	% " of the " ++ get_ordinal(Clause) ++ " clause" ++
	% io_lib:format(" ~ps.",[FailSucced]) ++ 
	% case Bindings of 
	% 	[] -> 
	% 		"";
	% 	_ ->
	% 		"\nBindings:\n" ++ get_context(Bindings)
	% end.


build_transition({}) ->
	"";
build_transition({{AReceive,Line,File},Clause,{Sender,Msg,_},_NodeReceive,Value}) ->
	build_receive(AReceive,File,Line,Clause,{Sender,Msg}) ++ 
	case Value of 
		stuck_receive ->
			"\nthat is blocked.";
		none ->
			"";
		_ ->
			"\nthat evalautes to " ++ erl_prettypr:format(Value) ++ "."
	end.

build_receive(AExpr,File,Line,_,_) ->
	%"Receive expression:\n" ++
	erl_prettypr:format(AExpr,[{paper, 300},{ribbon, 300}]) ++
	io_lib:format("\nlocation (~s, line ~p)",[File,Line]).
	% ++ 
	% case Clause of
	% 	none -> 
	% 		"\nwithout suitable messages receipt, ";
	% 	_ ->
	% 		io_lib:format("that enters in the ~s clause with the message ",[get_ordinal(Clause)]) ++
	% 		Msg ++
	% 		io_lib:format(" sent by ~p, ",[Sender])
	% end.
		 
print_sent_spawned(Sent, Spawned) ->
	"\n   " ++ io_lib:format("spawned: ~p",[Spawned]) ++ 
	"\n   " ++ "sent: [" ++ print_sent(Sent) ++ "]".

print_sent([])->
	"";
print_sent([{Pid,Msg}])->
	"{" ++ io_lib:format("~p, ",[Pid]) ++ pids_wo_quotes(Msg) ++"}";
print_sent([{Pid,Msg}|T])->
	"{" ++ io_lib:format("~p, ",[Pid]) ++ pids_wo_quotes(Msg) ++"},"++
	print_sent(T).

transform_value(Value) ->
	try erl_syntax:type(Value) of 
		_ ->
			erl_prettypr:format(Value)
	catch
		_:_ ->
			try cerl:concrete(Value)  of 
				Concrete -> 
					lists:flatten(io_lib:format("~p",[Concrete]))
			catch _:_ ->
				lists:flatten(io_lib:format("~p",[Value]))
			end
	end.

get_messages([]) -> "";
get_messages(Msgs) -> 
	get_messages(Msgs,[]).

get_messages([{Sender,Msg}|Msgs],Acc) ->
	StrMsg =
		"\t" ++ io_lib:format("Message ~s sent by ~p",[Msg,Sender]),
	case Msgs of 
		[] -> 
			Acc ++ StrMsg;
		_ -> 
			get_context(Msgs, Acc ++ StrMsg ++ "\n" )
	end.

get_context([]) -> "";
get_context(Deps) ->
	get_context(Deps,[]).

get_context([{VarName,Value}|Deps],Acc) ->
	% {VarName,Value} = 
	% 	case Entry of 
	% 		{VarName_,{Value_,_}} -> 
	% 			{VarName_,Value_};
	% 		{VarName_,Value_} ->
	% 			{VarName_,Value_}
	% 	end,
	VarValue =
		"\t"++atom_to_list(VarName) ++ " = " ++ transform_value(Value),
	case Deps of 
		[] -> 
			Acc ++ VarValue;
		_ -> 
			get_context(Deps, Acc ++ VarValue ++"\n" )
	end.

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
	Vertices = [digraph:vertex(G,V)||V <- digraph:vertices(G)],
	Edges = [{V1,V2}||V1 <- digraph:vertices(G),V2 <- digraph:out_neighbours(G, V1)],
	lists:flatten(lists:map(fun dot_vertex/1,Vertices))++
	lists:flatten(lists:map(fun dot_edge/1,Edges)).
	
dot_vertex({V,L}) ->
	{Question,_} = build_question(L),
	%io:format("\nVertex: ~p\nDict: ~p\n",[V,Dict]),
	%io:format("Vertex: ~p\n",[Esto]),
	integer_to_list(V)++" "++"[shape=ellipse, label=\""
	++ integer_to_list(V)++" .- " 
	% ++ change_new_lines(lists:flatten(
	% transform_label(lists:flatten(Question),[])))  ++ 
	% "\"];\n". 
	++ pids_wo_quotes(change_new_lines(lists:flatten(
		transform_label(lists:flatten(pids_wo_quotes(Question)),[]))))  ++ 
	"\"];\n".     
	    
dot_edge({V1,V2}) -> 
	integer_to_list(V1)++" -> "++integer_to_list(V2)
	++" [color=black, penwidth=3];\n".	
	
change_new_lines([10|Chars]) ->
	[$\\,$l|change_new_lines(Chars)];
change_new_lines([$"|Chars]) ->
	[$\\,$"|change_new_lines(Chars)];
change_new_lines([Other|Chars]) ->
	[Other|change_new_lines(Chars)];
change_new_lines([]) ->
	[].

pids_wo_quotes([$",$<|Chars]) ->
	[$<|pids_wo_quotes(Chars)];
pids_wo_quotes([$>,$"|Chars]) ->
	[$>|pids_wo_quotes(Chars)];
pids_wo_quotes([Other|Chars]) ->
	[Other|pids_wo_quotes(Chars)];
pids_wo_quotes([]) ->
	[].

 
