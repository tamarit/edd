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

-module(edd_lib).

-export([parse_expr/1, dot_graph_file/2, json_graph/1, tupled_graph/1,
		ask/4, core_module/1, core_module/2, get_MFA_Label/2,
		asking_loop/10, initial_state/2, get_call_string/2, select_strategy/1,
		get_call_value_string/2]).

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

%%------------------------------------------------------------------------------
%% @doc Compiles the Erlang program 'File' into Core Erlang and returns the 
%%      resulting Core program as a binary.
%%      
%% @end
%%------------------------------------------------------------------------------
-spec core_module( File :: string(), Dir :: string()) -> binary().    
core_module(File, Dir) ->
	{ok,_,Core} = compile:file(Dir ++ "/" ++ File,[to_core,binary,no_copt,{outdir,Dir},{i,Dir}]),
	Core.


%%------------------------------------------------------------------------------
%% @doc Prints current strategy then select the new one.
%% @end
%%------------------------------------------------------------------------------
-spec select_strategy( Strategy0 :: atom()) -> atom().
select_strategy(Strategy0) -> 
	PrintStrategy = 
    	fun
    		(top_down) -> "Top Down";
    		(divide_query) -> "Didide & Query"
    	end,
    io:format("\nCurrent strategy is "++ PrintStrategy(Strategy0) ++ ".\n"),
	SelectedStrategy = 
    	case get_answer("Select the new strategy (Didide & Query or "
              ++ "Top Down): [d/t] ",[d,t]) of
              t -> 
                 top_down;
              d -> 
                 divide_query
         end,
    io:format("Strategy is set to " ++ PrintStrategy(SelectedStrategy) ++ ".\n"),
    SelectedStrategy.

%%------------------------------------------------------------------------------
%% @doc Initial state for asking loop 
%% @end
%%------------------------------------------------------------------------------
-spec initial_state
	( 
		  G :: digraph:graph()
		, TrustedFunctions :: list()
	) 
    -> 
    {
	      Askable :: list()
	    , IniValid :: list()
	    , IniNotValid :: list()
    }.
initial_state(G, TrustedFunctions) ->
	initial_state(G, TrustedFunctions, false, []).

%%------------------------------------------------------------------------------
%% @doc Initial state for asking loop 
%% @end
%%------------------------------------------------------------------------------
-spec initial_state
	( 
		  G :: digraph:graph()
		, TrustedFunctions :: list()
		, LoadTest :: boolean()
		, TestFiles :: list()
	) 
    -> 
    {
	      Askable :: list()
	    , IniValid :: list()
	    , IniNotValid :: list()
    }.
initial_state(G, TrustedFunctions, LoadTest, TestFiles) ->
	ValidTrusted = 
		[V || 	V <- digraph:vertices(G),
	        	lists:member(get_MFA_Label(G,V),TrustedFunctions)],
	Root = look_for_root(G),
	{IniValid, IniNotValid} = 
		case LoadTest of 
			true -> 
				edd_proper_reader:get_initial_set_of_nodes(G, ValidTrusted, Root, TestFiles),
				edd_test_reader:get_initial_set_of_nodes(G, ValidTrusted, Root, TestFiles);
			false -> 
				{ValidTrusted, [Root]}
		end,
	AlreadyKnown = 
		   IniValid 
		++ IniNotValid 
		++ digraph_utils:reachable(IniValid,G),
	% Vertices = 
	% 	digraph:vertices(G) -- 	AlreadyKnown,
	% io:format("Askable vertices: ~p\n", [Vertices]),
	SelectableTrees =
		[{V, digraph_utils:reachable([V],G) -- AlreadyKnown} 
		 || V <- IniNotValid],
	OrderingFunction = 
		fun({_, R1}, {_, R2}) -> 
			length(R1) =< length(R2) 
		end,
	% io:format("Ordered Selectable Trees: ~p\n", [[V || V <- lists:sort(OrderingFunction, SelectableTrees)]]),
	[{InvalidSelected, Vertices} | _] = 
		lists:sort(OrderingFunction, SelectableTrees),
	FinalIniNotValid = 
		[InvalidSelected | (IniNotValid -- [InvalidSelected])],
	{Vertices, IniValid, FinalIniNotValid}.

%%------------------------------------------------------------------------------
%% @doc Traverses the tree 'G' asking the programmer until it finds the buggy 
%%      node. The tree 'G' must be a digraph representing the abbreviated proof 
%%      tree of the evaluation of an expression that yields an incorrect value.
%%      When it finds the buggy node, shows the function rule responsible for
%%      the incorrect value. The strategy followed is indicated by its second
%%      argument.      
%% @end
%%------------------------------------------------------------------------------
-spec ask( 
	  G :: digraph:graph() 
	, Strategy :: top_down | divide_query
	, Graph :: binary()
	, TestInfo :: {Load :: boolean(), Save :: boolean(), Files :: list()}
	) -> ok.
ask(G, Strategy, Graph, {Load, Save, Files}) ->
	% io:get_line(""),
	STrustedFunctions = 
	  io:get_line("Please, insert a list of trusted functions [m1:f1/a1, m2:f2/a2 ...]: "),
	TrustedFunctions = translate_string_to_functions(STrustedFunctions),
	{Vertices0, Valid0, NoValid0} = 
		initial_state(G, TrustedFunctions, Load, Files),
	put(expected_values, []),	
	ask_about(G,Strategy,Vertices0, Valid0, NoValid0, Graph, Save).

ask_about(G, Strategy, Vertices, Valid0, NotValid0, Graph, SaveTests) -> 
	FunGetAnswer =
		fun(Selected, _, _, _, _) -> 
			ask_question(G,Selected) 
		end,
	FunGetNewStrategy = 
		fun select_strategy/1,
	{Valid,NotValid,Unknown,_,NStrategy} = 
	   asking_loop(G, FunGetNewStrategy, FunGetAnswer, 
	   	Strategy,Vertices,Valid0,NotValid0,[],[],-1),
	case NotValid of
	     [-1] ->
	     	io:format("Debugging process finished\n");
	     _ -> 
	        NotValidVertexs = [NCV || NCV <- NotValid, 
	                                   (digraph:out_neighbours(G, NCV) -- Valid) == [] ],
	        case NotValidVertexs of
	             [] ->
	             	io:format("Not enough information.\n"),
	             	NotValidWithUnwnownVertexs = 
			  			[NCV || NCV <- NotValid, 
	                          (digraph:out_neighbours(G, NCV)--(Valid ++ Unknown)) =:= []],
	                Maybe0 = 
	                         [U || U <- Unknown, 
	                               NCV <- NotValidWithUnwnownVertexs,
	                               lists:member(U,digraph:out_neighbours(G, NCV))],
	                Maybe = find_unknown_children(G,Unknown,Maybe0),
					case get_answer("Do you want to try to answer"
					     ++" the needed information? [y/n]: ",[y,n]) of
					     y -> ask_about(G,NStrategy,Maybe,Valid,NotValid,Graph, SaveTests);
					     n -> 
			                [print_buggy_node(G,V,
			                        "Call to a function that could contain an error") 
			                        || V <- NotValidWithUnwnownVertexs],
			                [print_buggy_node(G,V,
			                         "This call has not been answered and could contain an error") 
			                        || V <- Maybe]
					end;
	             [NotValidVertex|_] ->
	               	print_buggy_node(G,NotValidVertex,
	               		"Call to a function that contains an error"),
	               	case SaveTests of 
	               		true -> 
	               			{RemoveableValidTest, RemovableNotValidTest} = get(test_to_NOT_store),
	               			ExpectedValues = get(expected_values),
	               			ValidNodesToStore = Valid -- RemoveableValidTest,
	               			NotValidNodesToStore0 = NotValid -- RemovableNotValidTest,
	               			{ExpectedValuesNodes, ExpectedValuesTest} = 
	               				lists:unzip(ExpectedValues),
	               			NotValidNodesToStore = 
	               				NotValidNodesToStore0 -- ExpectedValuesNodes,
	               			% io:format("test_to_NOT_store: ~w\n", [{RemoveableValidTest, RemovableNotValidTest}]),
	               			edd_test_writer:write(G, ValidNodesToStore, NotValidNodesToStore, ExpectedValuesTest);
	               		false -> 
	               			ok 
	               	end,
	               	case get_answer("Do you want to continue the debugging session"
					     ++" inside this function? [y/n]: ",[y,n]) of
					     y -> 
					     	edd_zoom:zoom_graph(get_call_string(G,NotValidVertex),Graph);
					     n -> 
			                ok
					end
	        end
	end,
	ok.

asking_loop(_,_,_,Strategy,[],Valid,NotValid,Unknown,State,_) -> 
	{Valid,NotValid,Unknown,State,Strategy};
asking_loop(_,_,_,Strategy,[-1],_,_,_,_,_) -> 
	{[-1],[-1],[-1],[],Strategy};
asking_loop(G, FunGetNewStrategy, FunGetAnswer, 
	Strategy,Vertices,Valid,NotValid,Unknown,State, Preselected) ->
		{Selected, NSortedVertices} = 
			case Preselected of 
				-1 ->
					VerticesWithValues = 
					  case Strategy of 
					       top_down ->
					        Children = digraph:out_neighbours(G, hd(NotValid)),
					        SelectableChildren = Children -- (Children -- Vertices), 
					          [{V, -length(digraph_utils:reachable([V], G))} 
					           || V <- SelectableChildren];
					       divide_query ->
							 [{V,begin
							         Reach = digraph_utils:reachable([V], G) -- [V],
							         TotalReach = Reach -- (Reach -- Vertices),
							         Rest = Vertices -- (TotalReach ++ [V]),
							         % TotalReach = length(Reach) - (1 + length(Reach -- Vertices)),
							         % Rest = length(Vertices) - (1 + TotalReach),
							         abs(length(TotalReach) - length(Rest))
							     end} || V <- Vertices]
					  end,
					OrderingFunction = 
						fun
							({_, O1}, {_, O2}) when O1 < O2 -> 
								true;
							({V1, O1}, {V2, O2}) when O1 == O2 , V1 > V2 -> 
								true;
							(_, _) ->
								false
						end,
					SortedVertices = 
						lists:sort(OrderingFunction, VerticesWithValues),
					% io:format("SortedVertices: ~p\n", [SortedVertices]),
					Selected0 = element(1,hd(SortedVertices)),
					NSortedVertices0 = [V || {V,_} <- tl(SortedVertices)],
					{Selected0, NSortedVertices0};
				_ ->
					{Preselected, Vertices -- [Preselected]}
			end,
	YesAnswer = begin
	             EqualToSelected = 
	                [V || V <- Vertices, begin {V,{L1,_,F1,Line1}} = digraph:vertex(G,V),
	                                           {Selected,{L2,_,F2,Line2}} = digraph:vertex(G,Selected),
	                                           (L1 =:= L2) and (F1 =:= F2) and (Line1 =:= Line2)
	                                     end],
	             {NSortedVertices -- digraph_utils:reachable(EqualToSelected,G),
	             EqualToSelected ++ Valid,NotValid,Unknown,
	             [{Vertices,Valid,NotValid,Unknown}|State],Strategy,-1}
	            end, 
	NoAnswer =
		% Shouldn't look for nodes with the same value and mark them as not valid? 
		{digraph_utils:reachable([Selected],G)
          -- ([Selected|NotValid] ++ digraph_utils:reachable(Valid,G) ++ Unknown),
          Valid,[Selected|NotValid],Unknown,
          [{Vertices,Valid,NotValid,Unknown}|State],Strategy,-1},
	Answer = FunGetAnswer(Selected, Vertices, Valid, NotValid, Unknown),
	{NVertices,NValid,NNotValid,NUnknown,NState,NStrategy, NPreselected} = 
	   case Answer of
	        y -> YesAnswer;
	        i -> YesAnswer;
	        v -> 
				ExpectedValue = 
					io:get_line("What is the value you expected? "),
				case ExpectedValue of 
					[$\n] ->
						[];
					_ ->
						put(expected_values, [{Selected, {get_call_string(G,Selected), lists:droplast(ExpectedValue), equal}} | get(expected_values)])
				end,
	        	NoAnswer;
	        n ->
	        	NoAnswer;
	        d -> %Hacer memoization?
	             {NSortedVertices -- [Selected],
	              Valid,NotValid,[Selected|Unknown],
	              [{Vertices,Valid,NotValid,Unknown}|State],Strategy,-1};
	        u -> case State of
	                  [] ->
	                     io:format("Nothing to undo\n"),
	                     {Vertices,Valid,NotValid,Unknown,State};
	                  [{PVertices,PValid,PNotValid,PUnknown}|PState] ->
	                     {PVertices,PValid,PNotValid,PUnknown,PState,Strategy,-1}
	             end;
	        t -> NewValid = 
	                % lists:flatten([digraph_utils:reachable([V], G) 
	               % [V || V <- Vertices,
	               %       get_MFA_Label(G,V) =:= get_MFA_Label(G,Selected)],
				   [V || V <- digraph:vertices(G),
	                     get_MFA_Label(G,V) =:= get_MFA_Label(G,Selected)],
	             {Vertices -- digraph_utils:reachable(NewValid,G),
	              NewValid ++ Valid,NotValid,Unknown,
	              [{Vertices,Valid,NotValid,Unknown}|State],Strategy,-1};
	        s -> 
	        	{Vertices,Valid,NotValid,Unknown,State,FunGetNewStrategy(Strategy),-1};
	        a -> {[-1],Valid,NotValid,Unknown,State,Strategy,-1};
	        N when is_integer(N) -> 
	        	{Vertices,Valid,NotValid,Unknown,State,Strategy,N};
	        _ ->
	        	io:format("Unsupported answer: ~p\n", [Answer]), 
	        	{Vertices,Valid,NotValid,Unknown,State,Strategy,Preselected}
	   end, 
	asking_loop(G, FunGetNewStrategy, FunGetAnswer, 
		NStrategy,NVertices,NValid,NNotValid,NUnknown,NState,NPreselected).
	
ask_question(G,V)->
	{V,{Label,_,File,Line}} = digraph:vertex(G,V),
	% io:format("Label: ~p\n", [Label]),
	NLabel = transform_label(lists:flatten(Label),[]),
	case File of 
		none ->
			io:format("~s",[NLabel]);
		_ ->
			io:format("~s\nfun location: (~s, line ~p)",[NLabel,File,Line])
	end,
	[_|Answer] = lists:reverse(io:get_line("? [y/n/t/v/d/i/s/u/a]: ")),
	list_to_atom(lists:reverse(Answer)).
	
get_answer(Message,Answers) ->
   [_|Answer] = 
     lists:reverse(io:get_line(Message)),
   AtomAnswer = list_to_atom(lists:reverse(Answer)),
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
	 
 
print_buggy_node(G,NotValidVertex,Message) ->
	{NotValidVertex,{Label,Clause,File,Line}} = digraph:vertex(G,NotValidVertex),
	io:format("~s:\n~s\n",[Message,transform_label(Label,[])]),
	case File of 
		none ->
			ok;
		_ ->
			io:format("fun location: (~s, line ~p)\n",[File,Line])
	end,
	print_clause(G,NotValidVertex,Clause).
   
print_clause(G,NotValidVertex,Clause) ->
	{Clauses,FunName,Arity} = 
		case get_MFA_Label(G,NotValidVertex) of 
			{{'fun',_,_} = AnoFun ,_,_}  ->
				{erl_syntax:fun_expr_clauses(AnoFun),none,none};
			{ModName,FunName0,Arity0} ->
				{ok,M} = smerl:for_file(atom_to_list(ModName) ++ ".erl"),
				Clauses_ = 
				  hd([Clauses_ || 
		             {function,_,FunName_,Arity_,Clauses_} <- smerl:get_forms(M),
		             FunName_ =:= FunName0, Arity_ =:= Arity0]),
				{Clauses_,FunName0,Arity0} 
		end,
	% {ModName,FunName,Arity} = get_MFA_Label(G,NotValidVertex),
	% {ok,M} = smerl:for_file(atom_to_list(ModName) ++ ".erl"),
	% Clauses = hd([Clauses_ || 
	%               	{function,_,FunName_,Arity_,Clauses_} <- smerl:get_forms(M),
	% 				FunName_ =:= FunName, Arity_ =:= Arity]),
	case Clause > length(Clauses)  of
	     true -> 		     	
	     	io:format("There is no clause matching.\n");
	     false -> 
	     	io:format("Please, revise the ~s clause:\n",[get_ordinal(Clause)]),
			SelectedClause = lists:nth(Clause, Clauses),
			ClauseStr = 
				case FunName of
					none -> 
						erl_prettypr:format(erl_syntax:fun_expr([SelectedClause]));
					_ -> 
						erl_prettypr:format({function,1,FunName,Arity,[SelectedClause]})
				end,
			io:format("~s\n",[ClauseStr])
	end.
	
%%------------------------------------------------------------------------------
%% @doc Returns a the MFA corresponding to a node in the tree
%% @end
%%------------------------------------------------------------------------------	
get_MFA_Label(G,Vertex) ->
	{Vertex,{Label,_,File,Line}} = 
		digraph:vertex(G,Vertex),
	{ok,Toks,_} = 
		erl_scan:string(lists:flatten(Label)++"."),
	{ok,[Aexpr|_]} = 
		erl_parse:parse_exprs(Toks),
	{match,_,{call,_,Called,APars},_} = 
		Aexpr,
	case Called of 
		{remote,_,{atom,_,ModName},{atom,_,FunName}} ->
			Arity = length(APars),
			{ModName,FunName,Arity};
		_ ->
			%io:format("Called: ~p\n",[Called]),
			{Called,File,Line}
	end.

get_call_string(G,Vertex) ->
	{Vertex,{Label,_,File,Line}} = 
		digraph:vertex(G,Vertex),
	{ok,Toks,_} = 
		erl_scan:string(lists:flatten(Label)++"."),
	{ok,[Aexpr|_]} = 
		erl_parse:parse_exprs(Toks),
	{match,_,Call = {call,_,Called,_},_} = 
		Aexpr,
	case Called of 
		{remote,_,_,_} ->
			erl_prettypr:format(Call);
		_ ->
			{erl_prettypr:format(Call),File,Line}
	end.

get_call_value_string(G, Vertex) ->
	{Vertex,{Label,_,_,_}} = 
		digraph:vertex(G,Vertex),
	{ok,Toks,_} = 
		erl_scan:string(lists:flatten(Label)++"."),
	{ok,[Aexpr|_]} = 
		erl_parse:parse_exprs(Toks),
	{match,_,_,Value} = 
		Aexpr,
	{get_call_string(G, Vertex), erl_prettypr:format(Value)}.
	
get_ordinal(1) -> "first";
get_ordinal(2) -> "second";
get_ordinal(3) -> "third";
get_ordinal(4) -> "fourth";
get_ordinal(5) -> "fifth";
get_ordinal(6) -> "sixth";
get_ordinal(7) -> "seventh";
get_ordinal(N) ->
	integer_to_list(N)++"th".
	
translate_string_to_functions("\n") ->
	[];
translate_string_to_functions("") ->
	[];
translate_string_to_functions(List0 = [_|_]) ->
	List = string:strip(List0),
	case lists:splitwith(fun(C) -> C =/= $: end,List) of
	     {ModName,[_|Tail1]} ->
		case lists:splitwith(fun(C) -> C =/= $/ end,Tail1) of
		     {FunName,[_|Tail2]} ->
		     	case lists:splitwith(
		     	      fun(C) -> 
		     	         lists:member(C,[$0,$1,$2,$3,$4,$5,$6,$7,$8,$9]) 
		     	      end,Tail2) of
		     	     {FunArity,[_|Tail3]} ->
		     	       	case FunArity of
		     	       	     [] ->
		     	       	       	io: format("The format is not correct\n"),
	     				[];
	     			     _ -> 
	     			      [{list_to_atom(ModName),list_to_atom(FunName),
				  	list_to_integer(string:strip(FunArity))} |
	 			  	translate_string_to_functions(string:strip(Tail3))]
		     	       	end;
		     	     _ -> 
		     	     	io: format("The format is not correct\n"),
	     			[]
		     	end;
		     _ -> 
		     	io: format("The format is not correct\n"),
	     		[]
		end;
	     _ -> 
	     	io: format("The format is not correct\n"),
	     	[]
	end;
translate_string_to_functions(_) ->
	[].
	
look_for_root(G)->
	case digraph:no_vertices(G) of
	     0 -> no;
	     1 -> hd(digraph:vertices(G));
	     _ -> hd([V||V <- digraph:vertices(G), digraph:in_degree(G, V)==0])
	end.
	
	
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
	
dot_vertex({V,{L,_,File,Line}}) ->
	integer_to_list(V)++" "++"[shape=ellipse, label=\""
	++integer_to_list(V)++" .- " 
	++ changeNewLines(lists:flatten(
		transform_label(lists:flatten(L),[]))) 
	++ 
	case File of 
		none ->
			"";
		_ -> 
			[$\\,$l] ++ changeNewLines(io_lib:format("fun location: (~s, line ~p)",[File, Line]))
	end
	++ "\"];\n".     
	    
dot_edge({V1,V2}) -> 
	integer_to_list(V1)++" -> "++integer_to_list(V2)
	++" [color=black, penwidth=3];\n".	
	
changeNewLines([10|Chars]) ->
	[$\\,$l|changeNewLines(Chars)];
changeNewLines([$"|Chars]) ->
	[$\\,$"|changeNewLines(Chars)];
changeNewLines([Other|Chars]) ->
	[Other|changeNewLines(Chars)];
changeNewLines([]) ->
	[].

%%------------------------------------------------------------------------------
%% @doc Created a JSON representation of a given debugging tree
%% @end
%%------------------------------------------------------------------------------
-spec json_graph( G :: digraph:graph()) -> string().	   
json_graph(G)->
	Vertices = [digraph:vertex(G,V) || V <- digraph:vertices(G)],
	Edges = [{V1,V2} || V1 <- digraph:vertices(G),V2 <- digraph:out_neighbours(G, V1)],
	JSON_Erlang = 
		{struct, 
			[{"vertices", {array, (lists:map(fun json_vertex/1,Vertices))}},
			{"edges", {array, lists:flatten(lists:map(fun json_edge/1,Edges))}}]},
	io:format("JSON_Erlang: ~p\n", [JSON_Erlang]),
	lists:flatten(mochijson:encode(JSON_Erlang)).
	


json_vertex({V,Info = {L,_,File,Line}}) ->
	{struct, 
		[{"node",integer_to_list(V)} ,
		 {"question", 
			changeNewLines(lists:flatten(transform_label(lists:flatten(L),[]))) 
			++ 
			case File of 
				none ->
					"";
				_ -> 
					[$\\,$l] 
						++ changeNewLines(
							io_lib:format("fun location: (~s, line ~p)",[File, Line]))
			end},
		{"info", Info}]}.     
	    
json_edge({V1,V2}) -> 
	{struct, [{"from",integer_to_list(V1)}, {"to",integer_to_list(V2)}]}.

%%------------------------------------------------------------------------------
%% @doc Creates a tuple representation of a given debugging tree
%% @end
%%------------------------------------------------------------------------------
-spec tupled_graph( G :: digraph:graph()) -> tuple().	   
tupled_graph(G)->
	Vertices = [digraph:vertex(G,V) || V <- digraph:vertices(G)],
	Edges = [{V1,V2} || V1 <- digraph:vertices(G),V2 <- digraph:out_neighbours(G, V1)],
	Tupled_Erlang = 
		{
			{vertices, lists:map(fun(V) -> tupled_vertex(G,V) end,Vertices)},
		 	{edges,lists:map(fun tupled_edge/1,Edges)}
		},
	% io:format("Tupled_Erlang: ~p\n", [Tupled_Erlang]),
	Tupled_Erlang.
	


tupled_vertex(G, {V,Info = {L,_,File,Line}}) ->
	Question = 
		changeNewLines(lists:flatten(transform_label(lists:flatten(L),[]))) 
		++ 
		case File of 
			none ->
				"";
			_ -> 
				[$\\,$l] 
					++ changeNewLines(
						io_lib:format("fun location: (~s, line ~p)",[File, Line]))
		end,
	{
		{id, V},
		{question, Question},
		{info, Info},
		{mfa, get_MFA_Label(G, V)}
	}.     
	    
tupled_edge({V1,V2}) -> 
	{V1, V2}.


