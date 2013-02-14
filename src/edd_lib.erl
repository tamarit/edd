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

-export([parse_expr/1, dot_graph_file/2, ask/2, core_module/1]).

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
%% @doc Traverses the tree 'G' asking the programmer until it finds the buggy 
%%      node. The tree 'G' must be a digraph representing the abbreviated proof 
%%      tree of the evaluation of an expression that yields an incorrect value.
%%      When it finds the buggy node, shows the function rule responsible for
%%      the incorrect value. The strategy followed is indicated by its second
%%      argument.      
%% @end
%%------------------------------------------------------------------------------
-spec ask( G :: digraph(), Strategy :: top_down | divide_query) -> ok.
ask(G,Strategy)->
	STrustedFunctions = 
	  io:get_line("Please, insert a list of trusted functions [m1:f1/a1, m2:f2/a2 ...]: "),
	TrustedFunctions = translate_string_to_functions(STrustedFunctions),
	IniCorrect = [V || V <- digraph:vertices(G),
	                   lists:member(get_MFA_Label(G,V),TrustedFunctions)],
	Root = look_for_root(G),
	Vertices = digraph:vertices(G) -- [Root|IniCorrect],
	ask_about(G,Strategy,Vertices,IniCorrect,[Root]).
	

ask_about(G,Strategy,Vertices,Correct0,NotCorrect0) -> 
	{Correct,NotCorrect,Unknown,_,NStrategy} = 
	   asking_loop(G,Strategy,Vertices,Correct0,NotCorrect0,[],[]),
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
			     y -> ask_about(G,NStrategy,Maybe,Correct,NotCorrect);
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
	               	 "Call to a function that contains an error")
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
 
find_unknown_children(G,Unknown,[V|Vs]) ->
	OutNeighbours = digraph:out_neighbours(G, V),
	OutNeighboursUnknown = OutNeighbours -- (OutNeighbours -- Unknown),
	[V | find_unknown_children(G,Unknown,Vs ++ OutNeighboursUnknown)];
find_unknown_children(_,_,[]) ->
	[].
	 
 
print_buggy_node(G,NotCorrectVertex,Message) ->
	{NotCorrectVertex,{Label,Clause}} = digraph:vertex(G,NotCorrectVertex),
	io:format("~s:\n~s\n",[Message,Label]),
	print_clause(G,NotCorrectVertex,Clause).
   
print_clause(G,NotCorrectVertex,Clause) ->
	{ModName,FunName,Arity} = get_MFA_Label(G,NotCorrectVertex),
	{ok,M} = smerl:for_file(atom_to_list(ModName) ++ ".erl"),
	Clauses = hd([Clauses_ || 
	              	{function,_,FunName_,Arity_,Clauses_} <- smerl:get_forms(M),
			FunName_ =:= FunName, Arity_ =:= Arity]),
	case Clause > length(Clauses)  of
	     true -> 		     	
	     	io:format("There is no clause matching.\n");
	     false -> 
	     	io:format("Please, revise the ~s clause:\n",[get_ordinal(Clause)]),
		SelectedClause = lists:nth(Clause, Clauses),
		ClauseStr = 
		   erl_prettypr:format({function,1,FunName,Arity,[SelectedClause]}),
		io:format("~s\n",[ClauseStr])
	end.
	
	
get_MFA_Label(G,Vertex) ->
	{Vertex,{Label,_}} = digraph:vertex(G,Vertex),
	{ok,Toks,_} = erl_scan:string(lists:flatten(Label)++"."),
	{ok,[Aexpr|_]} = erl_parse:parse_exprs(Toks),
	{match,_,{call,_,{remote,_,{atom,_,ModName},{atom,_,FunName}},APars},_} = Aexpr,
	Arity = length(APars),
	{ModName,FunName,Arity}.
	
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
translate_string_to_functions(List0) ->
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
	end.
	
look_for_root(G)->
	case digraph:no_vertices(G) of
	     0 -> no;
	     1 -> hd(digraph:vertices(G));
	     _ -> hd([V||V <- digraph:vertices(G), digraph:in_degree(G, V)==0])
	end.

asking_loop(_,Strategy,[],Correct,NotCorrect,Unknown,State) -> 
	{Correct,NotCorrect,Unknown,State,Strategy};
asking_loop(_,Strategy,[-1],_,_,_,_) -> {[-1],[-1],[-1],[],Strategy};
asking_loop(G,Strategy,Vertices,Correct,NotCorrect,Unknown,State) ->
	VerticesWithValues = 
	  case Strategy of 
	       top_down ->
	        Children = digraph:out_neighbours(G, hd(NotCorrect)),
	        SelectableChildren = Children -- (Children -- Vertices), 
	          [{V, -length(digraph_utils:reachable([V], G))} 
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
	Selected = element(1,hd(SortedVertices)),
	NSortedVertices = [V || {V,_} <- tl(SortedVertices)],
	YesAnswer = begin
	             EqualToSeleceted = 
	                [V || V <- Vertices, begin {V,{L1,_}} = digraph:vertex(G,V),
	                                           {Selected,{L2,_}} = digraph:vertex(G,Selected),
	                                           L1 =:= L2
	                                     end],
	             {NSortedVertices -- digraph_utils:reachable(EqualToSeleceted,G),
	             EqualToSeleceted ++ Correct,NotCorrect,Unknown,
	             [{Vertices,Correct,NotCorrect,Unknown}|State],Strategy}
	            end, 
	Answer = ask_question(G,Selected),
	{NVertices,NCorrect,NNotCorrect,NUnknown,NState,NStrategy} = 
	   case Answer of
	        y -> YesAnswer;
	        i -> YesAnswer;
	        n -> {digraph_utils:reachable([Selected],G)
	              --([Selected|NotCorrect]++Correct++Unknown),
	              Correct,[Selected|NotCorrect],Unknown,
	              [{Vertices,Correct,NotCorrect,Unknown}|State],Strategy};
	        d -> %Hacer memoization?
	             {NSortedVertices -- [Selected],
	              Correct,NotCorrect,[Selected|Unknown],
	              [{Vertices,Correct,NotCorrect,Unknown}|State],Strategy};
	        u -> case State of
	                  [] ->
	                     io:format("Nothing to undo\n"),
	                     {Vertices,Correct,NotCorrect,Unknown,State};
	                  [{PVertices,PCorrect,PNotCorrect,PUnknown}|PState] ->
	                     {PVertices,PCorrect,PNotCorrect,PUnknown,PState,Strategy}
	             end;
	        t -> NewCorrect = 
	                lists:flatten([digraph_utils:reachable([V], G) 
	                               || V <- Vertices,
	                                  get_MFA_Label(G,V) =:= get_MFA_Label(G,Selected)]),
	             {Vertices -- NewCorrect,NewCorrect ++ Correct,NotCorrect,Unknown,
	              [{Vertices,Correct,NotCorrect,Unknown}|State],Strategy};
	        s -> case get_answer("Select a strategy (Didide & Query or "
	                  ++"Top Down): [d/t] ",[t,d]) of
	                  t -> 
	                     {Vertices,Correct,NotCorrect,Unknown,State,top_down};
	                  d -> 
	                     {Vertices,Correct,NotCorrect,Unknown,State,divide_query}
	             end;
	        a -> {[-1],Correct,NotCorrect,Unknown,State,Strategy};
	        _ -> {Vertices,Correct,NotCorrect,Unknown,State,Strategy}
	   end, 
	asking_loop(G,NStrategy,NVertices,NCorrect,NNotCorrect,NUnknown,NState).
	
ask_question(G,V)->
	{V,{Label,_}} = digraph:vertex(G,V),
	io:format("~s",[Label]),
	[_|Answer]=lists:reverse(io:get_line("? [y/n/t/d/i/s/u/a]: ")),
	list_to_atom(lists:reverse(Answer)).
	
	  
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
-spec dot_graph_file( G :: digraph(), Name :: string() ) -> string().	   
dot_graph_file(G,Name)->
	file:write_file(Name++".dot", list_to_binary("digraph PDG {\n"++dot_graph(G)++"}")),
	os:cmd("dot -Tpdf "++ Name ++".dot > "++ Name ++".pdf").	
	
dot_graph(G)->
	Vertices = [digraph:vertex(G,V)||V <- digraph:vertices(G)],
	Edges = [{V1,V2}||V1 <- digraph:vertices(G),V2 <- digraph:out_neighbours(G, V1)],
	lists:flatten(lists:map(fun dot_vertex/1,Vertices))++
	lists:flatten(lists:map(fun dot_edge/1,Edges)).
	
dot_vertex({V,{L,_}}) ->
	integer_to_list(V)++" "++"[shape=ellipse, label=\""
	++integer_to_list(V)++" .- " ++ changeNewLines(L) ++ "\"];\n".     
	    
dot_edge({V1,V2}) -> 
	integer_to_list(V1)++" -> "++integer_to_list(V2)
	++" [color=black, penwidth=3];\n".	
	
changeNewLines([10|Chars]) ->
	[$\\,$l|changeNewLines(Chars)];
changeNewLines([Other|Chars]) ->
	[Other|changeNewLines(Chars)];
changeNewLines([]) ->
	[].
