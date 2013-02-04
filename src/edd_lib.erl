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

%-export([parse_expr/1, dotGraphFile/2, ask/1,lookForRoot/1,core_module/1,getOrdinal/1]).
-export([parse_expr/1, dotGraphFile/2, ask/1, core_module/1]).

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
%%      the incorrect value.      
%% @end
%%------------------------------------------------------------------------------
-spec ask( G :: digraph() ) -> ok.
ask(G)->
	STrustedFunctions = 
	  io:get_line("Please, insert a list of trusted functions [m1:f1/a1, m2:f2/a2 ...]: "),
	TrustedFunctions = translate_string_to_functions(STrustedFunctions),
	%io:format("Trusted functions: ~p\n",[TrustedFunctions]),
	IniCorrect = [V || V <- digraph:vertices(G),
	                   lists:member(getMFALabel(G,V),TrustedFunctions)],
	Root = lookForRoot(G),
	Vertices = digraph:vertices(G) -- [Root|IniCorrect],
	ask_about(G,Vertices,IniCorrect,[Root]).
	

ask_about(G,Vertices,Correct0,NotCorrect0) -> 
	%io:format("IniCorrect: ~p\n",[IniCorrect]),
	{Correct,NotCorrect,Unknown,_} = askingLoop(G,Vertices,Correct0,NotCorrect0,[],[]),
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
			case get_unknown_answer() of
			     y -> ask_about(G,Maybe,Correct,NotCorrect);
			     n -> 
	                       [print_buggy_node(G,V,"Call to a function that could contain an error") 
	                        || V <- NotCorrectWithUnwnownVertexs],
	                       [print_buggy_node(G,V,
	                         "This call has not been answered and could contain an error") 
	                        || V <- Maybe]
			end;
	             [NotCorrectVertex|_] ->
	               	print_buggy_node(G,NotCorrectVertex,"Call to a function that contains an error")
	        end
	end,
	ok.
	
get_unknown_answer() ->
   [_|Answer] = 
     lists:reverse(io:get_line("Do you want to try to answer the needed information? [y/n]: ")),
   case list_to_atom(lists:reverse(Answer)) of
        y -> y;
        n -> n;
        _ -> get_unknown_answer()
   end.
 
find_unknown_children(G,Unknown,[V|Vs]) ->
	OutNeighbours = digraph:out_neighbours(G, V),
	OutNeighboursUnknown = OutNeighbours -- (OutNeighbours -- Unknown),
	[V | find_unknown_children(G,Unknown,Vs ++ OutNeighboursUnknown)];
find_unknown_children(_,_,[]) ->
	[].
	 
 
print_buggy_node(G,NotCorrectVertex,Message) ->
	{NotCorrectVertex,{Label,Clause}} = digraph:vertex(G,NotCorrectVertex),
	io:format("~s: ~s\n",[Message,Label]),
	print_clause(G,NotCorrectVertex,Clause).
   
print_clause(G,NotCorrectVertex,Clause) ->
	{ModName,FunName,Arity} = getMFALabel(G,NotCorrectVertex),
	{ok,M} = smerl:for_file(atom_to_list(ModName) ++ ".erl"),
	Clauses = hd([Clauses_ || 
	              	{function,_,FunName_,Arity_,Clauses_} <- smerl:get_forms(M),
			FunName_ =:= FunName, Arity_ =:= Arity]),
	case Clause > length(Clauses)  of
	     true -> 		     	
	     	io:format("There is no clause matching.\n");
	     false -> 
	     	io:format("Please, revise the ~s clause:\n",[getOrdinal(Clause)]),
		SelectedClause = lists:nth(Clause, Clauses),
		ClauseStr = 
		   erl_prettypr:format({function,1,FunName,Arity,[SelectedClause]}),
		io:format("~s\n",[ClauseStr])
	end.
	
	
getMFALabel(G,Vertex) ->
	{Vertex,{Label,_}} = digraph:vertex(G,Vertex),
	%io:format("Vertex: ~p\nLabel: ~p\n",[Vertex,lists:flatten(Label)]),
	{ok,Toks,_} = erl_scan:string(lists:flatten(Label)++"."),
	{ok,[Aexpr|_]} = erl_parse:parse_exprs(Toks),
	{match,1,{call,1,{remote,1,{atom,1,ModName},{atom,1,FunName}},APars},_} = Aexpr,
	Arity = length(APars),
	{ModName,FunName,Arity}.
	
getOrdinal(1) -> "first";
getOrdinal(2) -> "second";
getOrdinal(3) -> "third";
getOrdinal(4) -> "fourth";
getOrdinal(5) -> "fifth";
getOrdinal(6) -> "sixth";
getOrdinal(7) -> "seventh";
getOrdinal(N) ->
	integer_to_list(N)++"th".
	
translate_string_to_functions("\n") ->
	[];
translate_string_to_functions("") ->
	[];
translate_string_to_functions(List0) ->
	List = string:strip(List0),
	case lists:splitwith(fun(C) -> C =/= $: end,List) of
	     {ModName,[_|Tail1]} ->
	     	%io:format("ModName: ~p\n",[ModName]),
		case lists:splitwith(fun(C) -> C =/= $/ end,Tail1) of
		     {FunName,[_|Tail2]} ->
		     %io:format("FunName: ~p\n",[FunName]),
		     	case lists:splitwith(
		     	      fun(C) -> 
		     	         lists:member(C,[$0,$1,$2,$3,$4,$5,$6,$7,$8,$9]) 
		     	      end,Tail2) of
		     	     {FunArity,[_|Tail3]} ->
		     	       	%io:format("FunArity: ~p\n",[FunArity]),
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
	
lookForRoot(G)->
	case digraph:no_vertices(G) of
	     0 -> no;
	     1 -> hd(digraph:vertices(G));
	     _ -> hd([V||V <- digraph:vertices(G), digraph:in_degree(G, V)==0])
	end.

askingLoop(_,[],Correct,NotCorrect,Unknown,State) -> {Correct,NotCorrect,Unknown,State};
askingLoop(_,[-1],_,_,_,_) -> {[-1],[-1],[-1],[]};
askingLoop(G,Vertices,Correct,NotCorrect,Unknown,State) ->
	% Sorted according Divide & Query
	VerticesWithValues = 
	  [{V,begin
	         Reach = digraph_utils:reachable([V], G),
	         TotalReach = length(Reach) - (1 + length(Reach -- Vertices)),
	         Rest = (length(Vertices) - 1) - TotalReach,
	         abs(TotalReach - Rest)
	      end} || V <- Vertices],
	SortedVertices = lists:sort(fun({_,Value1},{_,Value2}) ->
	                                Value1 =< Value2
	                            end,VerticesWithValues),
	%io:format("SortedVertices: ~p\n",[SortedVertices]),
	{Selected,_} = hd(SortedVertices),
	NSortedVertices = [V || {V,_} <- tl(SortedVertices)],
	YesAnswer = begin
	             EqualToSeleceted = 
	                [V || V <- Vertices, begin {V,{L1,_}} = digraph:vertex(G,V),
	                                           {Selected,{L2,_}} = digraph:vertex(G,Selected),
	                                           L1 =:= L2
	                                     end],
	             {NSortedVertices -- digraph_utils:reachable(EqualToSeleceted,G),
	             EqualToSeleceted ++ Correct,NotCorrect,Unknown,
	             [{Vertices,Correct,NotCorrect,Unknown}|State]}
	            end, 
	Answer = askQuestion(G,Selected),
	{NVertices,NCorrect,NNotCorrect,NUnknown,NState} = 
	   case Answer of
	        y -> YesAnswer;
	        i -> YesAnswer;
	        n -> {digraph_utils:reachable([Selected],G)
	              --([Selected|NotCorrect]++Correct++Unknown),
	              Correct,[Selected|NotCorrect],Unknown,
	              [{Vertices,Correct,NotCorrect,Unknown}|State]};
	        d -> %Hacer memoization?
	             {NSortedVertices -- [Selected],
	              Correct,NotCorrect,[Selected|Unknown],
	              [{Vertices,Correct,NotCorrect,Unknown}|State]};
	        u -> case State of
	                  [] ->
	                     io:format("Nothing to undo\n"),
	                     {Vertices,Correct,NotCorrect,Unknown,State};
	                  [{PVertices,PCorrect,PNotCorrect,PUnknown}|PState] ->
	                     {PVertices,PCorrect,PNotCorrect,PUnknown,PState}
	             end;
	        t -> NewCorrect = 
	                lists:flatten([digraph_utils:reachable([V], G) 
	                               || V <- Vertices,
	                                  getMFALabel(G,V) =:= getMFALabel(G,Selected)]),
	             %io:format("NewCorrect: ~p\n",[NewCorrect]),
	             %io:format("All: ~p\n",[[{V,getMFALabel(G,V)} || V <- Vertices]]),
	             {Vertices -- NewCorrect,NewCorrect ++ Correct,NotCorrect,Unknown,
	              [{Vertices,Correct,NotCorrect,Unknown}|State]};
	        a -> {[-1],Correct,NotCorrect,Unknown,State};
	        _ -> {Vertices,Correct,NotCorrect,Unknown,State}
	   end, 
	askingLoop(G,NVertices,NCorrect,NNotCorrect,NUnknown,NState).
	
askQuestion(G,V)->
	{V,{Label,_}} = digraph:vertex(G,V),
	io:format("~s",[Label]),
	[_|Answer]=lists:reverse(io:get_line("? [y/n/t/d/i/u/a]: ")),
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
-spec dotGraphFile( G :: digraph(), Name :: string() ) -> string().	   
dotGraphFile(G,Name)->
	file:write_file(Name++".dot", list_to_binary("digraph PDG {\n"++dotGraph(G)++"}")),
	os:cmd("dot -Tpdf "++ Name ++".dot > "++ Name ++".pdf").	
	
dotGraph(G)->
	Vertices = [digraph:vertex(G,V)||V <- digraph:vertices(G)],
	Edges = [{V1,V2}||V1 <- digraph:vertices(G),V2 <- digraph:out_neighbours(G, V1)],
	lists:flatten(lists:map(fun dotVertex/1,Vertices))++
	lists:flatten(lists:map(fun dotEdge/1,Edges)).
	
dotVertex({V,{L,_}}) ->
	integer_to_list(V)++" "++"[shape=ellipse, label=\""++integer_to_list(V)++" .- " ++ L ++ "\"];\n".     
	    
dotEdge({V1,V2}) -> 
	integer_to_list(V1)++" -> "++integer_to_list(V2)++" [color=black, penwidth=3];\n".	
