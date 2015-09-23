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
%%% @doc Erlang Declarative Debugger auxiliary library. This module contains 
%%%      auxiliary functions needed by the Erlang Declarative Debugger 'edd'.
%%% @end
%%%-----------------------------------------------------------------------------

-module(edd_zoom_lib).

-export([parse_expr/1, dot_graph_file/2, ask/2, core_module/1, 
	look_for_root/1, tupled_graph/1, initial_state/1, asking_loop/10,
	string_buggy_info/2]).

%%------------------------------------------------------------------------------
%% @doc Parses a string as if it were an expression. Returns a unitary list 
%%      containing the abstract representation of the expression.
%% @end
%%------------------------------------------------------------------------------
-spec parse_expr(Func::string()) -> {ok, [erl_parse:abstract_expr()]} | {error, parse_error}.
parse_expr(Func) ->
    case erl_scan:string(Func) of
	{ok, Toks, _} ->
	    case erl_parse:parse_exprs(Toks) of
		{ok, _Term} = Res ->
		    Res;
		_Err ->
		    {error, parse_error}
	    end;
	_Err ->
	    {error, parse_error}
    end.

%%------------------------------------------------------------------------------
%% @doc Compiles the Erlang program 'File' into Core Erlang and returns the 
%%      resulting Core program as a binary.
%%      
%% @end
%%------------------------------------------------------------------------------
-spec core_module( File :: string()) -> binary().    
core_module(File) ->
	{ok,_,Core} = compile:file(File,[to_core,binary,no_copt]),
	Core.


look_for_root(G)->
	case digraph:no_vertices(G) of
	     0 -> no;
	     1 -> hd(digraph:vertices(G));
	     _ -> hd([V||V <- digraph:vertices(G), digraph:in_degree(G, V)==0])
	end.

initial_state(G) ->
	Root = look_for_root(G),
	Vertices = digraph:vertices(G) -- [Root],
	{Vertices,[],[Root]}.
	

%%------------------------------------------------------------------------------
%% @doc Traverses the tree 'G' asking the programmer until it finds the buggy 
%%      node. The tree 'G' must be a digraph representing the abbreviated proof 
%%      tree of the evaluation of an expression that yields an incorrect value.
%%      When it finds the buggy node, shows the function rule responsible for
%%      the incorrect value. The strategy followed is indicated by its second
%%      argument.      
%% @end
%%------------------------------------------------------------------------------
-spec ask( G :: digraph:graph(), Strategy :: top_down | divide_query) -> ok.
ask(G,Strategy)->
	Root = look_for_root(G),
	Vertices = digraph:vertices(G) -- [Root],
	{Vertices,Correct,NotCorrect} = 
		initial_state(G),
	ask_about(G,Strategy,Vertices,[],[Root]).

ask_about(G,Strategy,Vertices,Correct0,NotCorrect0) -> 
	FunGetAnswer =
		fun(Question0, Answers, _, _, _, _) -> 
			Question1 =
				case is_integer(Question0) of 
					true -> 
						{Question0,Info} = digraph:vertex(G,Question0),
						{QuestionNode,_} = build_question(Info),
						QuestionNode;
					false ->  
						Question0
				end,
			get_answer(Question1, Answers)	
		end,
	FunGetNewStrategy = 
		fun edd_lib:select_strategy/1,
	{Correct,NotCorrect,Unknown,_,NStrategy} = 
	   asking_loop(G,FunGetNewStrategy, FunGetAnswer, Strategy,
	   		Vertices,Correct0,NotCorrect0,[],[],-1),
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
					     ++ " the needed information? [y/n]: ",[y,n]) of
					     y -> 
					     	ask_about(G,NStrategy,Maybe,Correct,NotCorrect);
					     n -> 
			               [print_buggy_node(G,V,
			                "This could be a reasons for an error") 
			                || V <- NotCorrectWithUnwnownVertexs],
			               [print_buggy_node(G,V,
			                 "This could be a reason for an error if an unanswered question were answered with no") 
					                || V <- Maybe]
					end;
	             [NotCorrectVertex|_] ->
	               	print_buggy_node(G,NotCorrectVertex,
	               	 "This is the reason for the error")
	        end
	end,
	ok.
	
get_answer(Message,Answers) ->
   [_|Answer] = 
     lists:reverse(io:get_line(Message)),
   AtomAnswer = list_to_atom(lists:reverse(Answer)),
   case lists:member(AtomAnswer,Answers) of
        true -> AtomAnswer;
        false -> get_answer(Message,Answers)
   end.

ask_question_dependeces(FunGetAnswer, Deps, CurrentState) ->
	{Vertices,Correct,NotCorrect,Unknown,_State,_Strategy,_PreSelected} = CurrentState,
	{StringDeps,DictDeps} = string_dependences(Deps,1),
	Message = 
		"Related variables:\n" ++ StringDeps ++ "What value is wrong? ",
	Answer = FunGetAnswer(Message,[list_to_atom(integer_to_list(Opt)) || {Opt,_} <-  DictDeps], Vertices,Correct,NotCorrect,Unknown),
	IntAnswer = list_to_integer(atom_to_list(Answer)),
	hd([V || {Opt,V} <-  DictDeps, Opt =:= IntAnswer ]).

string_dependences([{Var,{Value,V}}|Tail],Opt) -> 
	{STail,DictTail} = string_dependences(Tail,Opt + 1),
	SVar = 
		"\t" ++ integer_to_list(Opt) ++ ".- " 
	  	++ atom_to_list(Var) ++ " = " ++ transform_value(Value) ++ "\n",
	{SVar ++ STail,[{Opt,V}|DictTail]};
string_dependences([],_) ->
	{"",[]}.

 
find_unknown_children(G,Unknown,[V|Vs]) ->
	OutNeighbours = digraph:out_neighbours(G, V),
	OutNeighboursUnknown = OutNeighbours -- (OutNeighbours -- Unknown),
	[V | find_unknown_children(G,Unknown,Vs ++ OutNeighboursUnknown)];
find_unknown_children(_,_,[]) ->
	[].
	 
string_buggy_info(G,NotCorrectVertex) ->
	{NotCorrectVertex,InfoError} = 
		digraph:vertex(G,NotCorrectVertex),
	NInfoError = 
		case InfoError of 
			{case_if,CaseInfo,CaseDeps} ->
				[Parent] = digraph:in_neighbours(G, NotCorrectVertex),
				{Parent,ParentInfo} = digraph:vertex(G,Parent),
				case ParentInfo of 
					InfoError -> {case_if_2,CaseInfo,CaseDeps};
					_ -> InfoError
				end;
			_ ->
				InfoError
		end,
	print_buggy_info(NInfoError).

print_buggy_node(G,NotCorrectVertex,Message) ->
	io:format("\n\n~s:\n~s\n",[Message,string_buggy_info(G,NotCorrectVertex)]).

print_buggy_info({'let',{VarName,Value,ALetArg},_}) -> 
	"Variable " ++ atom_to_list(VarName) ++ " is badly assigned " ++ transform_value(Value) ++
	case ALetArg of 
		[] ->
			".";
		[ALetArg_|_] ->
			" in the expression:\n" ++ transform_abstract(ALetArg_) ++ " (line " 
     		++ integer_to_list(element(2,ALetArg_)) ++ ")."
	end;
print_buggy_info({'let_multiple',{InfoVars,ALetArg},_}) -> 
	"The following variables are badly assigned:\n"++get_context(InfoVars)++
	case ALetArg of 
		[] ->
			".";
		[ALetArg_|_] ->
			"\nin the expression:\n" ++ transform_abstract(ALetArg_) ++ " (line " 
     		++ integer_to_list(element(2,ALetArg_)) ++ ")."
	end;
print_buggy_info({case_if,{{ACase,Type},ArgValue,_,_,_,_,_},_}) ->
    "Argument value "++ transform_value(ArgValue) ++" of the " ++ Type ++ " expression:\n" ++
    transform_abstract(ACase) ++ 
    "\nis not correct.";
print_buggy_info({case_if_2,{_,_,_,FinalValue,AFinalExpr,_,_},_}) ->
     final_expression_message(FinalValue,AFinalExpr);
print_buggy_info({case_if_failed,{{ACase,Type},ArgValue,_,_},_}) ->
    "Argument value "++ transform_value(ArgValue) ++" of the " ++ Type ++ " expression:\n" ++
    transform_abstract(ACase) ++ 
    "\nis not correct.";
print_buggy_info({case_if_clause,{{ACase,Type}, _, ClauseNumber,PatGuard,_,_,_}, _}) ->
	"The " ++ atom_to_list(PatGuard) ++ " of the " ++
	get_ordinal(ClauseNumber) ++ " clause of " ++
	Type ++ " expression:\n" ++ transform_abstract(only_one_case_clause(ACase,ClauseNumber,Type));
print_buggy_info({fun_clause,{FunDef,ClauseNumber,PatGuard,_},[]}) ->
	"The " ++ atom_to_list(PatGuard) ++ " of the " ++
	get_ordinal(ClauseNumber) ++ " clause of " ++
	" function definition:\n" ++ transform_abstract(only_one_case_clause(FunDef,ClauseNumber,"fun"));
print_buggy_info({'root',{_,FinalValue,AFinalExpr},_}) -> 
	final_expression_message(FinalValue,AFinalExpr).


final_expression_message(FinalValue,AFinalExpr) ->
	"Value " ++ transform_value(FinalValue) ++ " for the final expression " ++
    case AFinalExpr of 
     	[] -> "";
     	[AFinalExpr_|_] -> 
     		transform_abstract(AFinalExpr_) ++ " (line " 
     			++ integer_to_list(element(2,AFinalExpr_)) ++ ") "
    end ++ "is not correct.".
   

	
get_ordinal(1) -> "first";
get_ordinal(2) -> "second";
get_ordinal(3) -> "third";
get_ordinal(4) -> "fourth";
get_ordinal(5) -> "fifth";
get_ordinal(6) -> "sixth";
get_ordinal(7) -> "seventh";
get_ordinal(N) ->
	integer_to_list(N)++"th".


get_children_with_same_info(G,Selected,Info) ->
	Childrens = digraph:out_neighbours(G, Selected),
	lists:flatten(
		[begin 
			{Children,ChildrenInfo} = digraph:vertex(G,Children),
			case ChildrenInfo of 
				Info -> 
					[Children];
				_ ->
					[]
			end
		 end || Children <- Childrens]).

get_both_case_nodes(G,Selected,Info) ->
	[Parent] = digraph:in_neighbours(G, Selected),
	{Parent,ParentInfo} = digraph:vertex(G,Parent), 
	case ParentInfo of 
		Info -> 
			[Selected,Parent];
		_ -> 
			case get_children_with_same_info(G,Selected,Info) of 
				[H|_] ->
					[H,Selected];
				_ ->
					[Selected]
			end
	end.
	


asking_loop(_,_,_,Strategy,[],Correct,NotCorrect,Unknown,State,_) -> 
	{Correct,NotCorrect,Unknown,State,Strategy};
asking_loop(_,_,_,Strategy,[-1],_,_,_,_,_) -> {[-1],[-1],[-1],[],Strategy};
asking_loop(G,FunGetNewStrategy, FunGetAnswer,Strategy,Vertices,Correct,NotCorrect,Unknown,State,PreSelected) ->
	% io:format("Selectable: ~w\n",[lists:sort(Vertices)]),
	% io:format("Correct: ~w\n",[lists:sort(Correct)]),
	% io:format("NotCorrect: ~w\n",[lists:sort(NotCorrect)]),
	% io:format("Unknown: ~w\n",[lists:sort(Unknown)]),
	{Selected,NSortedVertices} = 
		case PreSelected of
			-1 ->
				VerticesWithValues = 
				  case Strategy of 
				       top_down ->
				       		%io:format("Inicio: ~p\n",[hd(NotCorrect)]),
							Children = digraph:out_neighbours(G, hd(NotCorrect)),
							%io:format("Children: ~p\n",[Children]),	
							SelectableChildren = Children -- (Children -- Vertices), 
							[{V, - length(digraph_utils:reachable([V], G))} 
							  || V <- SelectableChildren];
				       divide_query ->
							 [{V,begin
							         Reach = digraph_utils:reachable([V], G),
							         TotalReach = length(Reach) - (1 + length(Reach -- Vertices)),
							         Rest = (length(Vertices) - 1) - TotalReach,
							         abs(TotalReach - Rest)
							     end} || V <- Vertices]
				  end,			
				SortedVertices = lists:keysort(2,VerticesWithValues), 
				case SortedVertices of 
					[] -> {-1,[]};
					[H|T] -> 
						%io:format("SortedVertices: ~p\n", [SortedVertices]),
						{element(1,H),
						 lists:flatten(
						 	[digraph_utils:reachable([V], G) || {V,_} <- T]) 
						 -- (Correct ++ NotCorrect ++ Unknown)}

				end;
			_ ->
				{PreSelected, Vertices -- [PreSelected]}
		end,
	% io:format("Selected: ~p\nSortedVertices: ~p\n",[Selected,NSortedVertices]),
	YesAnswer = %begin
	             % EqualToSeleceted = 
	             %    [V || V <- Vertices, begin {V,{L1,_}} = digraph:vertex(G,V),
	             %                               {Selected,{L2,_}} = digraph:vertex(G,Selected),
	             %                               L1 =:= L2
	             %                         end],
	             {NSortedVertices -- digraph_utils:reachable([Selected],G),
	             [Selected|Correct],NotCorrect,Unknown,
	             [{Vertices,Correct,NotCorrect,Unknown,PreSelected}|State],Strategy,-1},
	            %end, 
	%io:format("Selected: ~p\n",[Selected]),
	CurrentState = 
		{Vertices,Correct,NotCorrect,Unknown,State,Strategy,PreSelected},
	{Answer,StateQuestion} = 
		ask_question(G,Selected,CurrentState,NSortedVertices, FunGetAnswer),
	%io:format("State: ~p\n",[StateQuestion]),
	{NVertices,NCorrect,NNotCorrect,NUnknown,NState,NStrategy,NPreSelected} = 
	   case Answer of
	        y -> YesAnswer;
	        i ->
	        	{Selected,InfoSelected} = digraph:vertex(G,Selected),
	        	{_,_,DepsSelected} = InfoSelected,
	        	%io:format("DepsSelected: ~p\n", [DepsSelected]),
	        	case DepsSelected of 
	        		[] -> YesAnswer;
	        		[{_,{_,NextQuestion}}] ->
	        			{NSortedVertices -- digraph_utils:reachable([Selected],G),
			             [Selected|Correct],NotCorrect,Unknown,
			             [{Vertices,Correct,NotCorrect,Unknown,PreSelected}|State],Strategy,NextQuestion};
	        		_ -> 
	        			NextQuestion = ask_question_dependeces(FunGetAnswer, DepsSelected, CurrentState),
	        			%Podria passar que estaguera entre els correct, notcorret o unknown?
	        			{NSortedVertices -- digraph_utils:reachable([Selected],G),
			             [Selected|Correct],NotCorrect,Unknown,
			             [{Vertices,Correct,NotCorrect,Unknown,PreSelected}|State],Strategy,NextQuestion}
	        	end;
	        n -> {digraph_utils:reachable([Selected],G)
	              -- ([Selected|NotCorrect] ++ Correct ++ Unknown),
	              Correct,[Selected|NotCorrect],Unknown,
	              [{Vertices,Correct,NotCorrect,Unknown,PreSelected}|State],Strategy,-1};
	        d -> %Hacer memoization?
	             {NSortedVertices -- [Selected],
	              Correct,NotCorrect,[Selected|Unknown],
	              [{Vertices,Correct,NotCorrect,Unknown,PreSelected}|State],Strategy,-1};
	        u -> case State of
	                  [] ->
	                     io:format("Nothing to undo\n"),
	                     CurrentState;
	                  [{PVertices,PCorrect,PNotCorrect,PUnknown,PPreSelected}|PState] ->
	                     {PVertices,PCorrect,PNotCorrect,PUnknown,PState,Strategy,PPreSelected}
	             end;
	        s -> 
	        	SelectedStrategy = FunGetNewStrategy(Strategy),
	            {Vertices,Correct,NotCorrect,Unknown,State,SelectedStrategy,PreSelected};
	        a -> {[-1],Correct,NotCorrect,Unknown,State,Strategy,-1};
	        c -> StateQuestion;
	        _ -> CurrentState
	   end, 
	%asking_loop(G,NStrategy,lists:usort(NVertices),lists:usort(NCorrect),lists:usort(NNotCorrect),lists:usort(NUnknown),NState,NPreSelected).
	asking_loop(G,FunGetNewStrategy, FunGetAnswer, NStrategy,NVertices,
		NCorrect,NNotCorrect,NUnknown,NState,NPreSelected).

	
%TODO: En preguntes dobles tindre en conter el undo	
ask_question(_,-1,CurrentState,[],_)->
	{_,Correct,NotCorrect,Unknown,State,Strategy,PreSelected} = CurrentState,
	NCurrentState = {[],Correct,NotCorrect,Unknown,State,Strategy,PreSelected},
	{c,NCurrentState};
ask_question(G,Selected,CurrentState,NSortedVertices,FunGetAnswer)->
	{Selected,Info} = digraph:vertex(G,Selected),
	% io:format("Info: ~p\n", [Info]),
	{Vertices,Correct,NotCorrect,Unknown,State,Strategy,PreSelected} = CurrentState,
	case Info of 
		{case_if_clause,{_, _, _,pattern,_,_,_}, _} ->
			[Parent] = digraph:in_neighbours(G, Selected),
			NState = 
				{NSortedVertices,Correct,NotCorrect,Unknown,
	             [{Vertices,Correct,NotCorrect,Unknown,PreSelected}|State],Strategy,Parent},
			{c,NState};
		{case_if_clause,{_, _, _,guard,_,_,_}, _} ->
			[Parent] = digraph:in_neighbours(G, Selected),
			case lists:member(Parent,Correct++NotCorrect++Unknown) of 
				true ->
					% {Question,_} = build_question(Info),
					% io:format("~s",[Question]),
					% Options = "? [y/n/d/s/u/a]: ",
					% [_|Answer] = lists:reverse(io:get_line(Options)),
					% {list_to_atom(lists:reverse(Answer)),[]};
					Answer = 
						FunGetAnswer(Selected, [y,n,d,s,u,a], Vertices,Correct,NotCorrect,Unknown),
					{Answer, []};
				false ->
					{c,{NSortedVertices,Correct,NotCorrect,Unknown,
		             [{Vertices,Correct,NotCorrect,Unknown,PreSelected}|State],Strategy,Parent}}
	        end;
		_ ->
			{_Question,AnswerProblemEts} = build_question(Info),
			% io:format("~s\n~p\n",[_Question, AnswerProblemEts]),
			case AnswerProblemEts of 
				[] -> 
					% Options = "? [y/n/d/i/s/u/a]: ",
					% [_|Answer] = lists:reverse(io:get_line(Options)),
					% {list_to_atom(lists:reverse(Answer)),[]};
					Answer = 
						FunGetAnswer(Selected, [y,n,d,i,s,u,a], Vertices,Correct,NotCorrect,Unknown),
					{Answer, []};
				_ -> 
					AnswerProblem = ets:tab2list(AnswerProblemEts),
					% QuestionCase = "[" ++ [integer_to_list(AnswerNum)++"/"
					% 	 || {AnswerNum,_} <-  lists:sort(AnswerProblem)]++"d/s/u/a]? ",
					% Answer =
					%  get_answer(QuestionCase,
					% 	[list_to_atom(integer_to_list(AnswerNum))
					% 	 || {AnswerNum,_} <-  AnswerProblem]++[d,s,u,a]),
					Answers = 
						[list_to_atom(integer_to_list(AnswerNum))
						 || {AnswerNum,_} <-  AnswerProblem] ++ [d,s,u,a],
					% io:format("Info: ~p\n", [Info]),
					Answer = 
						FunGetAnswer(Selected, Answers, Vertices,Correct,NotCorrect,Unknown),
					case Answer of 
						s -> {s,[]};
						u -> {u,[]};
						a -> {a,[]};
						d -> {d,[]}; %Could/Should be improved
						_ ->
							IntAnswer = 
								case is_integer(Answer) of 
									true -> 
										Answer;
									false ->
										list_to_integer(atom_to_list(Answer))
								end,  
							{_, Problem} = 
								hd(ets:lookup(AnswerProblemEts,IntAnswer)),
							ets:delete(AnswerProblemEts),
							NState = 
								case Problem of 
									context -> 
										DepsSelected = element(3,Info),
										NextQuestion = 
								        	case DepsSelected of 
								        		[{_,{_,NextQuestion0}}] ->
								        			NextQuestion0;
								        		_ -> 
								        			ask_question_dependeces(FunGetAnswer, DepsSelected, CurrentState)			
								        	end,
							        	%Podria passar que estaguera entre els correct, notcorret o unknown?
							        	{NSortedVertices -- digraph_utils:reachable([Selected],G),
								             [Selected|Correct],NotCorrect,Unknown,
								             [{Vertices,Correct,NotCorrect,Unknown,PreSelected}|State],Strategy,NextQuestion};
							        arg_value -> 
										[Parent] = digraph:in_neighbours(G, Selected),
										{Parent,ParentInfo} = digraph:vertex(G,Parent),
										WrongVertex = 
											case ParentInfo of 
												Info -> Parent;
												_ -> Selected
											end,
										WrongVertexs = 
											case get_children_with_same_info(G,Selected,Info) of 
												[] -> [Selected];
												[Children] -> [Children,Selected]
											end,
					        			{digraph_utils:reachable([WrongVertex],G) -- digraph_utils:reachable([hd(WrongVertexs)],G), 
					        			 digraph_utils:reachable([hd(WrongVertexs)],G) ++ Correct,[WrongVertex|NotCorrect],Unknown,
							             [{Vertices,Correct,NotCorrect,Unknown,PreSelected}|State],Strategy,-1};
							        clause -> 
							        	%Quant sols te una clausula no te sentit preguntar-li res.
							        	{ACase,Type,CurrentClause,InfoClauses} =
								        	case Info of 
												{case_if,{{ACase_,Type_},_,CurrentClause_,_,_,_,InfoClauses_},_} ->
													{ACase_,Type_,CurrentClause_,InfoClauses_};
												{case_if_failed,{{ACase_,Type_},_,_,InfoClauses_},_} ->
													{ACase_,Type_,length(InfoClauses_)+1,InfoClauses_}
								        	end,
							        	TotalClauses = 
								        	length(
								        		case element(1,ACase) of 
									        		'case' ->
									        			element(4,ACase);
									        		'if' ->
									        			element(3,ACase);
									        		'try' ->
									        			case Type of 
									        				'try' ->
									        					element(4,ACase);
									        				_ ->
									        					element(5,ACase)
									        			end;
									        		_ ->
									        			[]
									        	end),
								  %      	QuestionClauses = "[" ++ [integer_to_list(AnswerNum)++"/"
										% 	 || AnswerNum <-  lists:seq(1, TotalClauses)]++"u/a]? ",
										% AnswerClauses =
										%  get_answer("Which clauses did you expect to be selected " ++ QuestionClauses,
										% 	[list_to_atom(integer_to_list(AnswerNum))
										% 	 || AnswerNum <-  lists:seq(1, TotalClauses)]++[u,a]),
										AnswersClauses = 
											[list_to_atom(integer_to_list(AnswerNum))
												|| AnswerNum <-  lists:seq(1, TotalClauses)] ++ [u,a],
										AnswerClauses = 
											FunGetAnswer("Which clauses did you expect to be selected ", 
												AnswersClauses, Vertices,Correct,NotCorrect,Unknown),
										case AnswerClauses of 
											u ->
												CurrentState;
											a -> 
												{a,[]};
											_ ->
												WrongVertexs = get_both_case_nodes(G, Selected,Info),
												ExpectedClause = list_to_integer(atom_to_list(AnswerClauses)),
												%io:format("ExpectedClause: ~p\nCurrentClause: ~p\n",[ExpectedClause,CurrentClause]),
												FunGetStateClauses = 
													fun(ConsideredClause) -> 
														CorrectClauses =
															lists:flatten( 
																[case NumNodes of 
																 	1 -> 
																 		[IdClause];
																 	_ -> 
																 		[IdClause, IdClause + 1]
																 end
																 || {IdClause,NumNodes,NumClause,_} <- InfoClauses, 
																 	NumClause =/= ConsideredClause]),
														{NotCorretClause,NextQuestion} = 
															case [{IdClause,NumNodes} || 
																	{IdClause,NumNodes,NumClause,_} <- InfoClauses, 
																	NumClause =:= ConsideredClause] of
																[{IdClause_,1}] ->
																	{[IdClause_],-1};
																[{IdClause_,2}] ->
																	{[IdClause_],IdClause_ + 1};
																_ -> 
																	{[],-1}
															end,
														{case NextQuestion of 
															-1 -> 
																[];
															_ -> 
																[NextQuestion]
														 end,
									             		Correct ++ CorrectClauses, NotCorretClause ++ WrongVertexs ++ NotCorrect,Unknown,
									             		[{Vertices,Correct,NotCorrect,Unknown,PreSelected}|State],Strategy,NextQuestion}
													end,
												if 	ExpectedClause < CurrentClause ->
														FunGetStateClauses(ExpectedClause);
													ExpectedClause > CurrentClause ->
														FunGetStateClauses(CurrentClause);
													ExpectedClause =:= CurrentClause ->
														CurrentState
												end
										end;
									bindings ->
										{case_if,{_,_,CurrentClause,_,_,_,InfoClauses},_} = Info,
										[IdClauseSucceed] = 
											[IdClause || {IdClause,_,NumClause,_} <- InfoClauses,NumClause =:= CurrentClause],
										WrongVertexs = get_both_case_nodes(G, Selected,Info),
										{[],Correct ++ digraph:out_neighbours(G,[IdClauseSucceed]), [IdClauseSucceed | WrongVertexs ++ NotCorrect],Unknown,
					             		[{Vertices,Correct,NotCorrect,Unknown,PreSelected}|State],Strategy,-1};
					             	value ->
										{case_if,{_,_,_,_,_,_,InfoClauses},_} = Info,
										CorrectClauses = 
											[IdClause || {IdClause,_,_,_} <- InfoClauses] 
											++ [IdClause + 1 || {IdClause,2,_,_} <- InfoClauses],
										WrongVertexs = 
											case get_children_with_same_info(G,Selected,Info) of 
												[Children] -> [Children, Selected];
												[] -> [Selected]
											end,
										%io:format("CorrectClauses: ~p\nWrongVertexs: ~p\n",[CorrectClauses,WrongVertexs]),
										%io:format("Queden: ~p\n",[digraph_utils:reachable(WrongVertexs, G) -- (CorrectClauses ++ WrongVertexs)]),
					        			{digraph_utils:reachable(WrongVertexs, G) -- (CorrectClauses ++ WrongVertexs),
					        			 CorrectClauses ++ Correct,WrongVertexs ++ NotCorrect,Unknown,
							             [{Vertices,Correct,NotCorrect,Unknown,PreSelected}|State],Strategy,-1};
							        nothing ->
							        	WrongVertexs = 
											case get_children_with_same_info(G,Selected,Info) of 
												[Children] -> [Children, Selected];
												[] -> [Selected]
											end,
							        	ReachableFromSelected = 
							        		digraph_utils:reachable(WrongVertexs, G),
										{NSortedVertices -- ReachableFromSelected,ReachableFromSelected ++ Correct,NotCorrect,Unknown,
							             [{Vertices,Correct,NotCorrect,Unknown,PreSelected}|State],Strategy,-1}
								end,
							case NState of 
								{_,_} -> NState;						
								_ -> {c,NState}
							end
					end
			end
	end.


	% case Info of 
	% 	{case_if,{ACaseIf,_,_,Value,_,Bindings},_} ->
	% 		case AtomAnswer of 
	% 			y ->
	% 				Question2 = build_question({case_if_2,{ACaseIf,Value,Bindings}}),
	% 				io:format("~s",[Question2]),
	% 				[_|Answer2]=lists:reverse(io:get_line(Options)),
	% 				AtomAnswer2 = list_to_atom(lists:reverse(Answer2)),
	% 				case AtomAnswer2 of
	% 					u  ->
	% 						ask_question(G,V);
	% 					_ ->
	% 						AtomAnswer2
	% 				end;
	% 			_ ->
	% 				AtomAnswer
	% 		end;
	% 	_ ->
	% 		AtomAnswer
	% end.

build_question({case_if,{{ACase,Type},ArgValue,ClauseNumber,FinalValue,_,Bindings,_},Deps}) ->
	Label1 = "For the " ++ Type ++ " expression:\n" 
		++ transform_abstract(ACase)++
		"\nIs there anything incorrect?\n",
	CurrentNumber = 1,
	AnswerProblem = ets:new(env_answerproblem,[set]),
	{CurrentNumber2,Label2} = 
		case Deps of 
			[] -> 
				{CurrentNumber,""};
			_ ->
				ets:insert(AnswerProblem, {CurrentNumber,context}),
				{CurrentNumber + 1,
				 integer_to_list(CurrentNumber) ++ ".- The context:\n" 
				 ++ get_context(Deps) ++ "\n"}
		end,
	{CurrentNumber3,Label3} = 
		case ArgValue of 
			{} -> 
				{CurrentNumber2,""};
			_ ->
				ets:insert(AnswerProblem, {CurrentNumber2,arg_value}),
				{CurrentNumber2 + 1,
				 integer_to_list(CurrentNumber2) ++ ".- The argument value: " 
				 ++ transform_value(ArgValue) ++ ".\n"}
		end,
	ets:insert(AnswerProblem, {CurrentNumber3,clause}),
	Label4 = 
		integer_to_list(CurrentNumber3) ++ ".- Enter in the " 
		++ get_ordinal(ClauseNumber) ++ " clause.\n",
	{CurrentNumber5,Label5} = 
		case Bindings of 
			[] -> 
				{CurrentNumber3 + 1,""};
			_ ->
				ets:insert(AnswerProblem, {CurrentNumber3 + 1,bindings}),
				{CurrentNumber3 + 2,
				 integer_to_list(CurrentNumber3 + 1) ++ ".- The bindings:\n" 
				 ++ get_context(Bindings) ++ "\n"}
		end,
	ets:insert(AnswerProblem, {CurrentNumber5,value}),
	Label6 = 
		integer_to_list(CurrentNumber5) ++ ".- The final value: " 
		++ transform_value(FinalValue) ++ ".\n",
	ets:insert(AnswerProblem, {CurrentNumber5+1,nothing}),
	Label7 = 
		integer_to_list(CurrentNumber5+1) ++ ".- Nothing.\n",
	{Label1 ++ Label2 ++ Label3 ++ Label4 ++ Label5 ++ Label6 ++ Label7,AnswerProblem};
build_question({case_if_failed,{{ACase,Type},ArgValue,_,_},Deps}) ->
	Label1 = "For the " ++ Type ++ " expression:\n" 
		++ transform_abstract(ACase)++
		"\nIs there anything incorrect?\n",
	CurrentNumber = 1,
	AnswerProblem = ets:new(env_answerproblem,[set]),
	{CurrentNumber2,Label2} = 
		case Deps of 
			[] -> 
				{CurrentNumber,""};
			_ ->
				ets:insert(AnswerProblem, {CurrentNumber,context}),
				{CurrentNumber + 1,
				 integer_to_list(CurrentNumber) ++ ".- The context:\n" 
				 ++ get_context(Deps) ++ "\n"}
		end,
	{CurrentNumber3,Label3} = 
		case ArgValue of 
			{} -> 
				{CurrentNumber2,""};
			_ ->
				ets:insert(AnswerProblem, {CurrentNumber2,arg_value}),
				{CurrentNumber2 + 1,
				 integer_to_list(CurrentNumber2) ++ ".- The argument value: " 
				 ++ transform_value(ArgValue) ++ ".\n"}
		end,
	ets:insert(AnswerProblem, {CurrentNumber3,clause}),
	Label4 = 
		integer_to_list(CurrentNumber3) ++ ".- Does not enter in any clause.\n",
	ets:insert(AnswerProblem, {CurrentNumber3+1,nothing}),
	Label5 = 
		integer_to_list(CurrentNumber3+1) ++ ".- Nothing.\n",
	{Label1 ++ Label2 ++ Label3 ++ Label4 ++ Label5,AnswerProblem};
build_question(Info) ->
	{_,_,Deps} = Info,
	{case Deps of 
		[] ->
			transform_label(Info);
		_ ->
			case Info of 
				{case_if_clause,{_, _,_,guard,_,_,_}, _} ->
					transform_label(Info);
				_ ->
					"Given the context:\n" ++ get_context(Deps) ++ ",\n" ++ transform_label(Info)
			end
	end, []}.


%falta tratar el case_if_failed
transform_label({'let',{VarName,Value,_},_}) -> 
	"the following variable is assigned:\n"
	++atom_to_list(VarName) ++ " = " ++ transform_value(Value)++ ".\nIs this correct";
transform_label({'let_multiple',{InfoVars,_},_}) -> 
	"the following variables are assigned:\n"++get_context(InfoVars)++ ".\nIs this correct";
transform_label({case_if_clause,{_, _,ClauseNumber,guard,SuccFail,_,_}, _}) ->
	"Guard of the " ++ get_ordinal(ClauseNumber)  ++ " clause " ++ atom_to_list(SuccFail) ++ ".\nIs this correct";
transform_label({case_if_clause,{{ACase,Type}, ArgValue, ClauseNumber,PatGuard,SuccFail,Bindings,GuardsDeps}, _}) ->
	"The " ++ Type ++ " expression:\n" ++ transform_abstract(only_one_case_clause(ACase,ClauseNumber,Type))
	++ "\n" ++ 
	case PatGuard of
		'pattern' ->
			"matching with " ;
		_ ->
			atom_to_list(PatGuard) ++ " of " 
	end
	++ get_ordinal(ClauseNumber) 
	++ " clause " ++ atom_to_list(SuccFail) ++ 
	case ArgValue of
		[] ->
			"";
		_ ->
			"\n" ++ Type ++ " argument value :" ++ transform_value(ArgValue)
	end ++
	case Bindings of 
		[] -> 
			"";
		_ ->
			"\nBindings:\n" ++ get_context(Bindings)
	end ++
	case GuardsDeps of 
		[] -> 
			"";
		_ ->
			"\nGuard dependences:\n" ++ get_context(GuardsDeps)
	end;
transform_label({fun_clause,{FunDef,ClauseNumber,PatGuard,SuccFail},[]}) ->
	"In the function:\n" ++ transform_abstract(only_one_fun_clause(FunDef,ClauseNumber))
    ++ "\n" ++ 
    case PatGuard of
    	'pattern' ->
    			"matching with ";
		_ ->
			atom_to_list(PatGuard) ++ " of "  
    end
    ++ get_ordinal(ClauseNumber) 
	++ " clause " ++ atom_to_list(SuccFail)++".\nIs this correct";
% %THE FOLLOWING CASE IS ONLY FOR DOT. NEVER ASKED.
transform_label({'root',{_,FinalValue,AFinalExpr},_}) -> 
	final_expression_message(FinalValue,AFinalExpr).




% % transform_label({case_if,{{ACase,Type},ArgValue,ClauseNumber,FinalValue,AFinalExpr},_}) ->
% % 	Type ++ " expression:\n" ++ transform_value(ACase) ++
% %      "\nenters in the " ++ get_ordinal(ClauseNumber) 
% %      ++ " clause.\nCase argument value: " ++ transform_value(ArgValue)
% %      ++ "\nFinal case value: " ++ transform_value(FinalValue);
% % transform_label({case_if,{{_,_},_,_,FinalValue,AFinalExpr,_},_}) ->
% % 	%"The " ++ Type ++ " expression:\n" ++ transform_value(ACase) ++ "\n" ++
% %      final_expression_message(FinalValue,AFinalExpr);
% transform_label({case_if_failed,{{ACase,Type},ArgValue,FinalValue},_}) ->
% 	case ArgValue of 
% 		[] -> "";
% 		_ -> 
% 	"and the " ++ Type ++ 
% 	"the " ++ Type ++ " expression:\n" ++ transform_abstract(ACase)++
%      "\ndoes not enter in any clause."
% %      ++ "\nCase argument value: " ++ transform_value(ArgValue);
% transform_label({case_if_clause,{{ACase,Type}, ArgValue, ClauseNumber,PatGuard,SuccFail,Bindings,GuardsDeps}, _}) ->
% 	"The " ++ Type ++ " expression:\n" ++ transform_abstract(only_one_case_clause(ACase,ClauseNumber))
% 	++ "\n" ++ 
% 	case PatGuard of
% 		'pattern' ->
% 			"matching with " ;
% 		_ ->
% 			atom_to_list(PatGuard) ++ " of " 
% 	end
% 	++ get_ordinal(ClauseNumber) 
% 	++ " clause " ++ atom_to_list(SuccFail) ++ 
% 	case ArgValue of
% 		[] ->
% 			"";
% 		_ ->
% 			"\n" ++ Type ++ " argument value :" ++ transform_value(ArgValue)
% 	end ++
% 	case Bindings of 
% 		[] -> 
% 			"";
% 		_ ->
% 			"\nBindings:\n" ++ get_context(Bindings)
% 	end ++
% 	case GuardsDeps of 
% 		[] -> 
% 			"";
% 		_ ->
% 			"\nGuard dependences:\n" ++ get_context(GuardsDeps)
% 	end;
% transform_label({fun_clause,{FunDef,ClauseNumber,PatGuard,SuccFail},[]}) ->
% 	"The function:\n" ++ transform_abstract(only_one_fun_clause(FunDef,ClauseNumber))
%     ++ "\n" ++ atom_to_list(PatGuard) ++ " of " ++ get_ordinal(ClauseNumber) 
% 	++ " clause " ++ atom_to_list(SuccFail);
% %THE FOLLOWING CASE IS ONLY FOR DOT. NEVER ASKED.
% transform_label({'root',{_,FinalValue,AFinalExpr},_}) -> 
% 	final_expression_message(FinalValue,AFinalExpr).




% transform_value(AFun = {'fun',_,_}) ->
% 	erl_prettypr:format(AFun);
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

transform_abstract(Abstract) ->
	erl_prettypr:format(Abstract).


get_context([]) -> "";
get_context(Deps) ->
	get_context(Deps,[]).

get_context([Entry|Deps],Acc) ->
	{VarName,Value} = 
		case Entry of 
			{VarName_,{Value_,_}} -> 
				{VarName_,Value_};
			{VarName_,Value_} ->
				{VarName_,Value_}
		end,
	VarValue =
		"\t"++atom_to_list(VarName) ++ " = " ++ transform_value(Value),
	case Deps of 
		[] -> 
			Acc ++ VarValue;
		_ -> 
			get_context(Deps, Acc ++ VarValue ++"\n" )
	end.

only_one_case_clause(ACase,ClauseNumber,Type) ->
	case Type of 
		"case" ->
			Clauses = erl_syntax:case_expr_clauses(ACase),
			Clause = lists:nth(ClauseNumber,Clauses),
			erl_syntax:revert(erl_syntax:case_expr(erl_syntax:case_expr_argument(ACase),[Clause]));
		"if" ->
			Clauses = erl_syntax:if_expr_clauses(ACase),
			Clause = lists:nth(ClauseNumber,Clauses),
			erl_syntax:revert(erl_syntax:if_expr([Clause]));
		"try" ->
			Clauses = erl_syntax:try_expr_clauses(ACase),
			Clause = lists:nth(ClauseNumber,Clauses),
			erl_syntax:revert(erl_syntax:try_expr(
				erl_syntax:try_expr_body(ACase),
				[Clause],
				erl_syntax:try_expr_handlers(ACase),
				erl_syntax:try_expr_after(ACase)));
		"catch of try" ->
			Clauses = erl_syntax:try_expr_handlers(ACase),
			Clause = lists:nth(ClauseNumber,Clauses),
			erl_syntax:revert(erl_syntax:try_expr(
				erl_syntax:try_expr_body(ACase),
				erl_syntax:try_expr_clauses(ACase),
				[Clause],
				erl_syntax:try_expr_after(ACase)));
		"fun" ->
			Clauses = erl_syntax:function_clauses(ACase),
			Clause = lists:nth(ClauseNumber,Clauses),
			erl_syntax:revert(erl_syntax:function(
				erl_syntax:function_name(ACase),
				[Clause]))
	end.

only_one_fun_clause(AFun,ClauseNumber) ->
	case erl_syntax:type(AFun) of
		'function' ->
			Clauses = erl_syntax:function_clauses(AFun),
			Clause = lists:nth(ClauseNumber,Clauses),
			erl_syntax:revert(erl_syntax:function(erl_syntax:function_name(AFun),[Clause]));
		'fun_expr' ->
			Clauses = erl_syntax:fun_expr_clauses(AFun),
			Clause = lists:nth(ClauseNumber,Clauses),
			erl_syntax:revert(erl_syntax:fun_expr([Clause]))
	end.
	
% analyze_tokens([]) -> {ok,[]};
% analyze_tokens([H|T]) -> 
% 	case string:to_integer(H) of
% 	     {Int,[]} when Int >= 32, Int =< 126 ->  
% 	     	case analyze_tokens(T) of
% 	     	     {ok,List} -> {ok,[Int|List]};
% 	     	     error -> error
% 	     	end;
% 	     _ -> error
% 	end.
	
		  
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
	io:format("\nL: ~p\n",[L]),
	{Label,_} = build_question(L),
	integer_to_list(V)++" "++"[shape=ellipse, label=\""
	++integer_to_list(V)++" .- " 
	++ change_new_lines(lists:flatten(Label)) ++ "\"];\n".

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

%%------------------------------------------------------------------------------
%% @doc Created a tupled representation of a given debugging tree
%% @end
%%------------------------------------------------------------------------------
-spec tupled_graph( G :: digraph:graph()) -> tuple().	   
tupled_graph(G)->
	Vertices = [digraph:vertex(G,V) || V <- digraph:vertices(G)],
	Edges = [{V1,V2} || V1 <- digraph:vertices(G),V2 <- digraph:out_neighbours(G, V1)],
	Tupled_Erlang = 
		{
			{vertices, lists:map(fun tupled_vertex/1,Vertices)},
		 	{edges,lists:map(fun tupled_edge/1,Edges)}
		},
	% io:format("Tupled_Erlang: ~p\n", [Tupled_Erlang]),
	Tupled_Erlang.
	
tupled_vertex({V,L}) ->
	{Question,_} = build_question(L),
	{
		{id, V},
		{question, change_new_lines(lists:flatten(Question))}
	}.    
	    
tupled_edge({V1,V2}) -> 
	{V1, V2}.

