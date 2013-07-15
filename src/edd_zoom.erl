-module(edd_zoom).

-export([zoom_graph/1]).


%Pending:
%CUIDAO: EL APPLY ES TRADUEIX A VOLTES A UN CALL. SI EL MODULO NO EXPORTA EIX FUNCIO ES UN ERROR
%% El shadowing provoca mal comportament per culpa del apply subsitution que substitueix variables en els patrons
%poner los cases y sobretodo sus argumentos de manera que al traducir a Core no haya mas de un case en la misma linea 

zoom_graph(Expr)->
	%Clause = 1, %Te que ser un parametro
    {ok,[AExpr|_]} = edd_zoom_lib:parse_expr(Expr++"."),
    FunOperator = erl_syntax:application_operator(AExpr),
    %io:format("FunOperator:~p\n",[FunOperator]),
    FunArity = length(erl_syntax:application_arguments(AExpr)),
    Env = ets:new(env,[set]),
    CoreArgs = [
    	try
    		cerl:abstract(erl_syntax:concrete(Arg)) 
    	catch
    		_:_ -> 
    			begin
    				M1 = smerl:new(foo),
					{ok, M2} = smerl:add_func(M1,"bar() ->" ++ erl_prettypr:format(Arg) ++ " ."),
					%Obtiene el CORE de la expresión Expr
					{ok,_,CoreP} = smerl:compile2(M2,[to_core,binary,no_copt]), 
					CoreFun = extract_call(CoreP),
					%io:format("CoreFun:~p\n",[CoreFun]),
					case cerl:type(CoreFun) of 
						call ->
							CoreFun;
						_ ->
							[{id,{_,_,FunName}},_,{file,File}] = cerl:get_ann(CoreFun),
							{anonymous_function,FunName,CoreFun,File,Arg,[]}
					end
    			end
    	end
                || Arg <- erl_syntax:application_arguments(AExpr)],
	{remote,_,{atom,_,ModName},{atom,_,FunName}} = FunOperator,
	Core = edd_zoom_lib:core_module(atom_to_list(ModName)++".erl"),
	% ONLY TO DEBUG
	compile:file(atom_to_list(ModName)++".erl",[to_core,no_copt]),
	ModuleDefs = cerl:module_defs(Core),
	[Definition|_] = 
	   [Def || {{c_var,[],{FunName_,FunArity_}},Def} <- ModuleDefs, 
	           FunName_ =:= FunName, FunArity_ =:= FunArity],
	CaseFun = cerl:fun_body(Definition),
	InitialCall = cerl:update_c_case(CaseFun, cerl:c_values(CoreArgs), 
	                                 cerl:case_clauses(CaseFun)), 
	%io:format("InitialCall: ~p\n",[InitialCall]), 
    {ok,M} = smerl:for_file(atom_to_list(ModName) ++ ".erl"),
	FunctionDef = 
	    hd([FunctionDef_ || 
              	FunctionDef_ = {function,_,FunName_,Arity_,_} <- smerl:get_forms(M),
				FunName_ =:= FunName, Arity_ =:= FunArity]),
    ets:insert(Env,{initial_case,true}),
    ets:insert(Env,{function_definition, FunctionDef}),
    ets:insert(Env,{core,Core}),
	{Value,FreeV,Graphs,LastExprInfo} = get_tree(InitialCall,Env,0), %{c_literal,[],1}
	%io:format("LastExprInfo:~p\n",[LastExprInfo]),
	ALastExpr = 
		case LastExprInfo of 
			[TypeLastExpr,Line,File] ->
				get_expression_from_abstract(File,Line,TypeLastExpr);
			_ -> 
				[]
		end,
	%io:format("ALastExpr:~p\n",[ALastExpr]),
	

	%io:format("Num. de grafos: ~p\n",[length(Graphs)]),
	%io:format("Roots: ~p\n",[[edd_zoom_lib:look_for_root(G_) || G_ <- Graphs]]),
	G = digraph:new([acyclic]),
	AValue = get_abstract_form(Value),
	% {FunDef,TotalClauses,VarsClause} = 	
	%    get_fundef_and_vars_clause(ModName,FunName,FunArity,Clause),
	%TotalClauses = 1,
	RootInfo = {root,{AExpr,AValue,ALastExpr},[]},
	% NFreeV = 
	%     case TotalClauses of
	%          1 -> 
	digraph:add_vertex(G,FreeV,RootInfo),
	 %         	FreeV;
	 %         _ -> 
		%         % digraph:add_vertex(G,FreeV,{function_clause, {FunDef,Clause,VarsClause}}),
		%         % digraph:add_vertex(G,FreeV + 1, RootInfo),
		%         % digraph:add_edge(G, FreeV + 1, FreeV),
		%         FreeV + 1
		% end,
	add_graphs_to_graph(G,Graphs),
	[digraph:add_edge(G,FreeV,edd_zoom_lib:look_for_root(G_)) || G_ <- Graphs],
	io:format("Total number of tree nodes: ~p\n",[length(digraph:vertices(G))]),
	edd_zoom_lib:dot_graph_file(G,"dbg_zoom"),
	edd_zoom_lib:ask(G,top_down),
	%io:format("Env final:\n~p\n",[ets:tab2list(Env)]),
	ets:delete(Env),
	ok.

check_errors(Value) -> 
    %possiblement falten mes tipos d'errors.
	case Value of
	     {c_literal,[],{error,_}} -> true;
	     _ -> false
	end.

extract_call(Core) ->
	{c_module,_,_,_,_,FunDefs} = Core,
	[{c_fun,_,_,FunBody}|_] = 
		[FunBody_ || {{c_var,_,FunName},FunBody_} <- FunDefs, FunName == {bar,0}],
	[{c_clause,_,_,_,Call}|_] = cerl:case_clauses(FunBody),
	Call.

% get_fundef_and_vars_clause(ModName,FunName,Arity,Clause) ->
% 	{ok,M} = smerl:for_file(atom_to_list(ModName) ++ ".erl"),
% 	FunctionDef = 
% 	    hd([FunctionDef_ || 
%               	FunctionDef_ = {function,_,FunName_,Arity_,_} <- smerl:get_forms(M),
% 		FunName_ =:= FunName, Arity_ =:= Arity]),
% 	{function,_,FunName,Arity,Clauses} = FunctionDef,
% 	%io:format("Clauses: ~p\nClause: ~p\n",[length(Clauses),Clause]),
% 	SelectedClause = lists:nth(Clause, Clauses),
% 	Patterns = erl_syntax:clause_patterns(SelectedClause),
% 	{FunctionDef,length(Clauses),
% 	 sets:to_list(sets:union([erl_syntax_lib:variables(Pattern) || Pattern <- Patterns]))}.

get_tree_list([E|Es],Env,FreeV) ->
	{NE,NFreeV,GraphsH,_} = get_tree(bind_vars(E,Env),Env,FreeV),
	{NEs,NNFreeV,GraphsT} = get_tree_list(Es,Env,NFreeV),
	{[NE|NEs],NNFreeV,GraphsH++GraphsT};
get_tree_list([],_,FreeV) ->
	{[],FreeV,[]}.

build_graphs_and_add_bindings([{Var,Value,_,_}|Bins],Env,FreeV,Deps,ALet,InfoAcc) ->
	{GraphsVar,_} = build_graphs_and_add_bindings([Var],Env,FreeV,Value,Deps,ALet,[]),
	NInfoAcc = 
		case GraphsVar of 
			[] ->
				InfoAcc;
			[G] ->
				{FreeV, {'let',{VarName,AValue,_},_}} = digraph:vertex(G,FreeV),
				[{VarName,AValue}|InfoAcc]
		end,
	build_graphs_and_add_bindings(Bins,Env,FreeV,Deps,ALet,NInfoAcc);
build_graphs_and_add_bindings([],_,FreeV,Deps,ALet,InfoAcc) ->
	case InfoAcc of 
		[] ->
			{[],FreeV};
		_ ->
			%RevInfoAcc = lists:reverse(InfoAcc),
			G = digraph:new([acyclic]),
			digraph:add_vertex(G,FreeV,{'let_multiple',{InfoAcc,ALet},Deps}),
			{[G],FreeV + 1 }
	end.

build_graphs_and_add_bindings([Var | Vars],Env,FreeV,Value,Deps,ALet,GraphsAcc) ->
	VarName = cerl:var_name(Var),
	case atom_to_list(VarName) of
	     [$c,$o,$r | _] -> 
	     	add_bindings_to_env([{Var,Value,Deps,null}],Env),
	     	build_graphs_and_add_bindings(Vars,Env,FreeV,Value,Deps,GraphsAcc); 
	     [$r,$e,$c | _] -> 
	     	add_bindings_to_env([{Var,Value,Deps,null}],Env),
	     	build_graphs_and_add_bindings(Vars,Env,FreeV,Value,Deps,GraphsAcc); 
	     _ -> 
	     	G = digraph:new([acyclic]),
	     	ConcreteValue = 
	     		case Value of
	     			{anonymous_function,_,_,_,_,_}  ->
	     				get_abstract_form(Value);
	     			{c_call,_,_,_,_}  ->
	     				get_abstract_form(Value);
	     			_ ->
	     				cerl:concrete(cerl:fold_literal(Value))
	     		end,
			digraph:add_vertex(G,FreeV,{'let',{VarName,ConcreteValue,ALet},Deps}),
		    add_bindings_to_env([{Var,Value,Deps,FreeV}],Env),
	        build_graphs_and_add_bindings(Vars,Env,FreeV + 1,Value,Deps,ALet,[G | GraphsAcc]) 
	end;
build_graphs_and_add_bindings([],_,FreeV,_,_,_,GraphsAcc) -> 
	{GraphsAcc,FreeV}.

add_bindings_to_env([{{c_var,_,VarName},Value,Deps,Node}| TailBindings],Env) ->
	ets:insert(Env,{VarName,{Value,Deps,Node}}),
	add_bindings_to_env(TailBindings,Env);
add_bindings_to_env([{VarName,Value,Deps,Node}| TailBindings],Env) ->
	ets:insert(Env,{VarName,{Value,Deps,Node}}),
	add_bindings_to_env(TailBindings,Env);
add_bindings_to_env([Other| TailBindings],Env) ->
	ets:insert(Env,Other),
	add_bindings_to_env(TailBindings,Env);
add_bindings_to_env([],_) -> ok.

get_dependences(Vars, Env) ->
	get_dependences(Vars, Env, []).

get_dependences([Var | Vars], Env, Acc) ->
	VarName = 
		case Var of
			{c_var,_,VarName_} ->
				VarName_;
			_ -> 
				Var
		end,
	%io:format("Variable: ~p\nValues: ~p\n",[VarName,ets:lookup(Env,VarName)]),
	Deps = 
		case ets:lookup(Env,VarName) of 
			[] -> [];
			[{_,{Value,Deps_,Node}}|_] ->
				case atom_to_list(VarName) of 
					[$c,$o,$r | _] -> 
						Deps_;
					[$r,$e,$c | _] -> 
						Deps_;
					_ -> 
						ConcreteValue = 
							case Value of 
								{anonymous_function,_,_,_,_,_} -> 
									get_abstract_form(Value);
								{c_call,_,_,_,_} -> 
									get_abstract_form(Value);
								_ -> 
									cerl:concrete(Value)
							end,
						[{VarName,{ConcreteValue,Node}}]
				end
		end,
	get_dependences(Vars, Env, Acc ++ Deps);
get_dependences([], _, Acc) ->
	lists:usort(lists:flatten(Acc)).

get_abstract_from_core_literal({c_literal,_,Lit_}) ->
	{ok,[ALit|_]} = edd_zoom_lib:parse_expr(lists:flatten(io_lib:format("~w",[Lit_])++".")),
	ALit.
	
get_abstract_form({anonymous_function,_,_,File,Line,Env}) -> 
	NEnv = ets:new(a_fun_temp,[set]),
	ets:insert(NEnv,Env),
	get_fun_from_file(File,Line,NEnv);
get_abstract_form(Par_) -> 
	Par = cerl:fold_literal(Par_),
	case cerl:is_literal(Par) of
		true -> 
			get_abstract_from_core_literal(Par);
		_ -> 
			{ModName_,FunName_,FunArity_} = get_MFA(Par),
			{'fun',1,
				{function,
				{atom,1,ModName_},
				{atom,1,FunName_},
				{integer,1,FunArity_}}}
	end. 

get_fun_from_file(File,Line,Env) -> 
	AnoFun = 
		case smerl:for_file(File) of
		     {ok,Abstract} ->
			hd(lists:flatten([get_funs_from_abstract(Form,Line) 
			                  || Form <- smerl:get_forms(Abstract)]));
		     {error, {invalid_module, _}} -> 
		     	Line
		end,
	PatternsClause = 
		[erl_syntax:clause_patterns(Clause) || Clause <- erl_syntax:fun_expr_clauses(AnoFun)],
	BodyClause = 
		[erl_syntax:clause_body(Clause) || Clause <- erl_syntax:fun_expr_clauses(AnoFun)],
	VariablesPat = 
		sets:union([erl_syntax_lib:variables(Pattern) 
						|| Patterns <- PatternsClause, Pattern <- Patterns]),
	VariablesBody = 
		sets:union([erl_syntax_lib:variables(BodyExpression) 
						|| Body <- BodyClause, BodyExpression <- Body]),
	VarsOnlyInBody = sets:to_list(sets:subtract(VariablesBody, VariablesPat)),
	FunChangeVar =
		fun(T) ->
			case erl_syntax:type(T) of 
				variable -> 
					VarName = erl_syntax:variable_name(T),
					case lists:member(VarName,VarsOnlyInBody) of 
						true ->
							BVars = bind_vars(cerl:c_var(VarName),Env),
							case is_atom(BVars) of
								true ->
									erl_syntax:variable(BVars);
								false ->
									get_abstract_form(BVars)
							end;
						false ->
							T
					end;
				_ ->
					T
			end
		end,
	erl_syntax:revert(erl_syntax_lib:map(FunChangeVar,AnoFun)).

get_funs_from_abstract(Abstract,Line) -> 
	erl_syntax_lib:fold(fun(Tree,Acc) -> 
	               		case Tree of 
	               	    	{'fun',Line,{clauses,_}} ->
	               	    		[Tree|Acc];
	               	        {'fun',_,{clauses,_}} when Line =:= -1 ->
	               	    		[Tree|Acc];
	               	    	_ -> 
	               	    		Acc
	               		end
		    	     end, [], Abstract).

get_MFA(Function) ->
	{c_call,_,{c_literal,_,erlang},{c_literal,_,make_fun},MFA} = Function,
	[{c_literal,_,ModName},{c_literal,_,FunName},{c_literal,_,FunArity}] = MFA,
	{ModName,FunName,FunArity}.

apply_substitution(Expr,Env,Bindings) ->
	cerl_trees:map(
		fun(T)->
			case cerl:type(T) of
			     'var' -> 
			     	case cerl:var_name(T) of
			     	     {_,_} -> T;
			     	     VN ->
			     	     	case ets:lookup(Env,VN) of
			     	     	     [] -> 
			     	     	     	Values = 
			     	     	     	  [Value || {{c_var,_,VarName},Value} <- Bindings,
			     	     	     	            VarName =:= VN],
			     	     	     	case Values of
			     	     	     	     [] -> T;
			     	     	     	     [Value|_] -> 
			     	     	     	     	Value
			     	     	     	end;
			     	     	     [{_,{Value,_,_}}|_] ->
			     	     	     	Value
			     	     	end
			     	end;
			     _ -> T
			end
		end,Expr).

bind_vars(Expr,Env) ->
	%io:format("Expr: ~p\nEnv: ~p\n",[Expr,ets:tab2list(Env)]),
	case Expr of
	     {anonymous_function,_,_,_,_,_} -> Expr;
	     _ ->
			case cerl:type(Expr) of
			     'var' -> 
			     	VarName = cerl:var_name(Expr),
			     	case VarName of
			     	     {FunName,Arity} -> 
			     	     	[_,{file,FileName},_] = cerl:get_ann(Expr),
			     	     	{ModName,_} = lists:split(length(FileName)-4,FileName),
			     	     	MFArgs = 
			     	     	    [{c_literal,[],list_to_atom(ModName)},
	           			     {c_literal,[],FunName},
	          		             {c_literal,[],Arity}],
			     	     	{c_call,[],{c_literal,[],erlang},{c_literal,[],make_fun},MFArgs};
			     	     _ ->
			     	     	case ets:lookup(Env,VarName) of 
			     	     		[{VarName,{Value,_,_}}|_] ->
					     			case Value of
					     	             {anonymous_function,_,_,_,_,_} -> Value;
					     	             _ ->
					     	             	cerl:unfold_literal(Value)
					     	        end;
					     	    _ ->
					     	    	%Esto se anyade porque get_fun_from_file no tiene en cuenta las variables que se declaran dentro del cuerpo de la función y intenta buscarlas
					     	    	VarName
					     	end
			     	end;
			     'values' -> 
			     	Values = cerl:values_es(Expr),
			     	BValues = lists:map(fun(Var) -> bind_vars(Var,Env) end,Values),
			     	{c_values,cerl:get_ann(Expr),BValues};
			    'tuple' -> 
			    	NExps = [bind_vars(E,Env) || E <- cerl:tuple_es(Expr)],
			    	cerl:c_tuple(NExps);
			    'cons' -> 
			    	[NHd,NTl] = [bind_vars(E,Env) || 
			    	             E <- [cerl:cons_hd(Expr),cerl:cons_tl(Expr)]],
			    	cerl:c_cons(NHd,NTl);
			     _ -> Expr
			 end
	 end.

% create_new_env([{c_var,_,VarName}|TailArgs],[Values|TailPars],Env) ->
% 	ets:insert(Env,{VarName,Values}),
% 	create_new_env(TailArgs,TailPars,Env);
% create_new_env([],[],_) -> ok.


get_expression_from_abstract(File,Line,Type) ->
	case smerl:for_file(File) of 
		{ok,Abstract} ->
			lists:flatten(
				[erl_syntax_lib:fold(
					fun(Tree,Acc) -> 
						%io:format("{Line,Tree}: ~p\n",[{Line,Tree}]),
			       		case Tree of 
			       			{'var',Line,_} when Type == 'var' ->
								[Tree|Acc];
			       			{'integer',Line,_} when Type == 'literal' ->
								[Tree|Acc];
							{'float',Line,_} when Type == 'literal' ->
								[Tree|Acc];
							{'string',Line,_} when Type == 'literal' ->
								[Tree|Acc];
							{'atom',Line,_} when Type == 'literal' ->
								[Tree|Acc];
							{'nil',Line} when Type == 'literal' ->
								[Tree|Acc];
							{'call',Line,_,_} when Type == 'call' ->
								[Tree|Acc];
							{'op',Line,_,_,_} when Type == 'call' ->
								[Tree|Acc];
							{'op',Line,_,_} when Type == 'call' ->
								[Tree|Acc];
			       			{'cons',Line,_,_} when Type == 'cons' ->
								[Tree|Acc];
							{'tuple',Line,_} when Type == 'tuple' ->
								[Tree|Acc];
							{'match',Line,_,_}  when Type == 'match' ->
								[Tree|Acc];
							{'case',Line,_,_} when Type == 'case' ->
								[{Tree,"case"}|Acc];
							{'if',Line,_} when Type == 'case' ->
								[{Tree,"if"}|Acc];
							{'lc',Line,_,_} when Type == 'lc' ->
								[Tree|Acc];
							_ -> 
								Acc
			       		end
				     end, [], Form) || Form<-smerl:get_forms(Abstract)]);
		_ -> 
			[]
	end.


get_try_from_abstract(File,Line,Type) -> 
	{ok,Abstract} = smerl:for_file(File),
	lists:flatten(
		[erl_syntax_lib:fold(
			fun(Tree,Acc) -> 
				%io:format("{Line,Tree}: ~p\n",[{Line,Tree}]),
	       		case Tree of 
					{'try',Line,_,_,_,_} ->
						[{Tree,Type}|Acc];
					_ -> 
						Acc
	       		end
		     end, [], Form) || Form<-smerl:get_forms(Abstract)]).

% get_match_from_abstract(File,Line) -> 
% 	case smerl:for_file(File) of 
% 		{ok,Abstract}  -> 
% 			lists:flatten(
% 				[erl_syntax_lib:fold(
% 					fun(Tree,Acc) -> 
% 						%io:format("{Line,Tree}: ~p\n",[{Line,Tree}]),
% 			       		case Tree of 
% 							{'match',Line,_,_} ->
% 								[Tree|Acc];
% 							_ -> 
% 								Acc
% 			       		end
% 				     end, [], Form) || Form<-smerl:get_forms(Abstract)]);
% 		_ ->
% 			[]
% 	end.

% get_case_from_abstract(File,Line) -> 
% 	case smerl:for_file(File) of 
% 		{ok,Abstract}  -> 
% 			lists:flatten(
% 				[erl_syntax_lib:fold(
% 					fun(Tree,Acc) -> 
% 						%io:format("{Line,Tree}: ~p\n",[{Line,Tree}]),
% 			       		case Tree of 
% 							{'case',Line,_,_} ->
% 								[{Tree,"Case"}|Acc];
% 							{'if',Line,_} ->
% 								[{Tree,"If"}|Acc];
% 							_ -> 
% 								Acc
% 			       		end
% 				     end, [], Form) || Form<-smerl:get_forms(Abstract)]);
% 		_ ->
% 			[]
% 	end.
		     


% get_tuple_from_abstract(File,Line) -> 
% 	{ok,Abstract} = smerl:for_file(File),
% 	lists:flatten(
% 		[erl_syntax_lib:fold(
% 			fun(Tree,Acc) -> 
% 				%io:format("{Line,Tree}: ~p\n",[{Line,Tree}]),
% 	       		case Tree of 
% 					{'tuple',Line,_} ->
% 						[Tree|Acc];
% 					_ -> 
% 						Acc
% 	       		end
% 		     end, [], Form) || Form<-smerl:get_forms(Abstract)]).
	
get_tree_case(Expr,Env,FreeV) -> 
	Args = cerl:case_arg(Expr),
	{ArgsValue,_,_,_} = get_tree(Args,Env,FreeV),
	%io:format("Args: ~p\n",[Args]),
	%io:format("Type: ~p\n",[cerl:type(Args)]),
	%io:format("Stored ~p\n",[ets:lookup(Env,let_graphs)]),
	GraphsArgs =
		case cerl:type(Args) of 
	    	'var' -> 
	    		VarLetArg = cerl:var_name(Args),
	    		ResLookUp = ets:lookup(Env,let_graphs),
	    		ets:delete(Env,let_graphs),
	    		case ResLookUp of 
	    			[{let_graphs,{VarLetArg,StoredGraphs}}] -> 
	    				NG = digraph:new([acyclic]),
						add_graphs_to_graph(NG,StoredGraphs),
			    		[NG];
			    	_ -> 
			    		[]
			    end;
			_ ->
				[]
		end,
	BArgs_ = bind_vars(ArgsValue,Env),
	BArgs = 
		case cerl:type(BArgs_) of
			values -> cerl:values_es(BArgs_);
			_ -> [BArgs_]
		end,
	Clauses = cerl:case_clauses(Expr),
	FunGetFromTry = 
		fun() ->
			case ets:lookup(Env,current_try) of
				[] ->
					[];
				[{current_try,{FileNameTry,LineTry,TryType}}|_] ->
					get_try_from_abstract(FileNameTry,LineTry,TryType)
			end
		end,
	ErrorCaseType = 
		try
			cerl:concrete(lists:nth(1,cerl:tuple_es(lists:nth(1,cerl:primop_args(cerl:clause_body(lists:last(Clauses)))))))
		catch
			_:_ -> none 
		end,
	AbstractCase = 
		case ErrorCaseType of 
			badmatch -> 
				[];
			_ ->
				case cerl:get_ann(Expr) of 
					[Line,{file,FileName}|_] ->
						case get_expression_from_abstract(FileName,Line,'case') of
							[] ->
								FunGetFromTry();
							AbstractCase_ ->
								AbstractCase_
						end;
					[] ->
						FunGetFromTry()
				end
		end,
	ets:delete(Env,current_try),
	HasAbstractOrIsInitial = 
		(length(AbstractCase) =/= 0) 
		 or 
		  ((length(ets:lookup(Env,initial_case)) =:= 1) 
		  	andalso (hd(ets:lookup(Env,initial_case)) == {initial_case,true})),
	ListArgs = 
		case cerl:type(Args) of
			values ->
				cerl:values_es(Args);
			_ ->
				[Args]
		end,
    %io:format("Args: ~p\n",[ListArgs]),
	Deps = lists:flatten([try get_dependences(cerl_trees:variables(ArgsElem),Env) 
						  catch _:_ -> []
						  end || ArgsElem <- ListArgs]),
	{ClauseNumber,ClauseBody,Bindings,NFreeV_,GraphsClauses} = 
		get_clause_body(Clauses,Clauses,HasAbstractOrIsInitial,BArgs,Env,FreeV,Deps,1),
	IsInitial = 
		case ets:lookup(Env,initial_case) of 
			[{initial_case,true}|_] ->
				ets:insert(Env,{initial_case,false}),
				true;
			_ ->
				false
		end,
	%io:format("Bindings: ~p\n", [Bindings]),
	VarsBody = 
		try 
			cerl_trees:variables(ClauseBody)
		catch _:_ -> []
		end,
	%io:format("VarsBody: ~p\n",[VarsBody]),
	DepsBody = get_dependences(VarsBody,Env),
	%io:format("DepsBody: ~p\n",[DepsBody]),
	{GraphsBindings,NFreeV} =
		case HasAbstractOrIsInitial of 
			false ->
				ALet = 
					case cerl:get_ann(Expr) of 
						[Line_,{file,File_}] ->
							get_expression_from_abstract(File_,Line_,'match');
						_ ->
							[]
					end,
				%io:format("ALetCase: ~p\n",[ALet]),
				InfoBinding = 
					build_graphs_and_add_bindings(Bindings,Env,NFreeV_,Deps,ALet,[]),
				case InfoBinding of 
					{[],_} -> 
						InfoBinding;
					{GraphsBindings_,NFreeVBindings} ->
							GBindings = digraph:new([acyclic]),
							add_graphs_to_graph(GBindings,GraphsBindings_),
							add_graphs_to_graph(GBindings,GraphsArgs),
							[digraph:add_edge(GBindings,NFreeV_,edd_zoom_lib:look_for_root(G_)) 
							 	    || G_ <-  GraphsArgs],
							{[GBindings],NFreeVBindings}
				end;
			_ ->
				add_bindings_to_env(Bindings,Env),
				{[],NFreeV_}
		end,
	%io:format("ClauseBody: ~p\n",[ClauseBody]),
	{Value,NNFreeV,GraphsBody,LastExprInfo} = get_tree(ClauseBody,Env,NFreeV),
	%io:format("Value: ~p\nLastExprInfo: ~p\n",[Value,LastExprInfo]),
	ALastExpr = 
		case LastExprInfo of 
			[TypeLastExpr,LineLastExpr,FileLastExpr] ->
				get_expression_from_abstract(FileLastExpr,LineLastExpr,TypeLastExpr);
			_ -> 
				[]
		end,
	{NNNFreeV, CaseGraphs} = 
		case AbstractCase of 
			[] -> 
				case IsInitial of 
					true ->
						{function_definition,FunctionDef} = 
						 	hd(ets:lookup(Env,function_definition)),
						% 	{function,_,FunName,Arity,Clauses} = FunctionDef,
						Gs =  
							[begin
								G = digraph:new([acyclic]),
								digraph:add_vertex(G,NumNode,
									{fun_clause,{FunctionDef,CaseClause,pattern,SuccFail},[]}),
								digraph:add_edge(G,NNFreeV,NumNode)	,
								case TotalNodes of 
									2 ->
										digraph:add_vertex(G,NumNode + 1,
											{fun_clause,{FunctionDef,CaseClause,guard,SuccFail},[]}),
										digraph:add_edge(G,NumNode, NumNode + 1);
									_ ->
										ok
								end,
								G
							 end || {NumNode,TotalNodes,CaseClause,_BindingsClause,_GuardsDeps,SuccFail} <- GraphsClauses],

						{NNFreeV, Gs ++ GraphsBody};
					_ -> 
						{NNFreeV, GraphsBody}
				end;
			[_|_] -> 
				G = digraph:new([acyclic]),
				ConcreteBArg = 
					case BArgs of 
						[] -> {};
						_ -> hd([cerl:concrete(BArg) || BArg <- BArgs])
					end,
				GuardsDepsClauses = 
					lists:flatten([GuardsDeps || {_,_,_,_,GuardsDeps,_} <- GraphsClauses]),
				InfoClausesCase = 
					[{NumNode,TotalNodes,CaseClause,SuccFail}  
					 || {NumNode,TotalNodes,CaseClause,_,_,SuccFail} <- GraphsClauses],
				NNFreeV_ = 
					case ClauseNumber =:= length(Clauses) of 
						true -> 
							digraph:add_vertex(G,NNFreeV,
								{case_if_failed,
								 {hd(AbstractCase),
								  ConcreteBArg,
								  cerl:concrete(cerl:fold_literal(Value)),
								  InfoClausesCase},
								 lists:usort(Deps ++ GuardsDepsClauses)}),
							NNFreeV;
						false ->
							Info = 								
							 	{case_if,
								 {hd(AbstractCase),
								  ConcreteBArg,
								  ClauseNumber,
								  cerl:concrete(cerl:fold_literal(Value)),
								  ALastExpr,
						      	  remove_core_vars_from_env([{BinVar, BinValue} || {BinVar, BinValue, _, _} <- Bindings]),
						      	  InfoClausesCase
						      	 },
								 lists:usort(Deps ++ GuardsDepsClauses ++ DepsBody)},
							digraph:add_vertex(G,NNFreeV,Info),
							digraph:add_vertex(G,NNFreeV+1,Info),
							digraph:add_edge(G,NNFreeV ,NNFreeV + 1), 
						    add_graphs_to_graph(G,GraphsArgs),
							[digraph:add_edge(G,NNFreeV,edd_zoom_lib:look_for_root(G_)) 
							 	    || G_ <-  GraphsArgs],
							NNFreeV + 1
					end,
				[begin
					digraph:add_vertex(G,NumNode,
						{case_if_clause,{hd(AbstractCase),ConcreteBArg,
						 CaseClause,pattern,SuccFail,BindingsClause,[]},Deps}),
					digraph:add_edge(G,NNFreeV_,NumNode)	,
					case TotalNodes of 
						2 ->
							digraph:add_vertex(G,NumNode + 1,
								{case_if_clause,{hd(AbstractCase),ConcreteBArg,
								 CaseClause,guard,SuccFail,BindingsClause,GuardsDeps},Deps}),
							digraph:add_edge(G,NumNode, NumNode + 1);
						_ ->
							ok
					end
				 end || {NumNode,TotalNodes,CaseClause,BindingsClause,GuardsDeps,SuccFail} <- GraphsClauses],
				add_graphs_to_graph(G,GraphsBody),
				[digraph:add_edge(G,NNFreeV_,edd_zoom_lib:look_for_root(G_)) 
				 	    || G_ <-  GraphsBody],
				{NNFreeV_ + 1, [G]}
		end,
	%io:format("AbstractCase: ~p\nCaseGraphs: ~p\n",[AbstractCase,CaseGraphs]),
	{Value, NNNFreeV, GraphsBindings ++ CaseGraphs,LastExprInfo}.

get_clause_body([Clause | Clauses],AllClauses,HasAbstractOrIsInitial,BArgs,Env,FreeV,Deps,ClauseNumber) ->
	NClause = cerl:c_clause(cerl:clause_pats(Clause), cerl:clause_body(Clause)),
	FunCreateFailedNode = 
		fun(Type,Bindings,DepsGuard) ->
			{NFreeV, GraphFailed} = 
				case HasAbstractOrIsInitial of
					false ->
				      {FreeV,[]};
				    true ->
				      case Type of 
				      	guard -> 
				      		{FreeV + 2,[{FreeV,2,ClauseNumber,Bindings,DepsGuard,failed}]};
				      	pattern -> 
				      		{FreeV + 1,[{FreeV,1,ClauseNumber,Bindings,[],failed}]}
				      end
				  end,
	       	{SelectedClause,NClauseBody,NBindings,NNFreeV,Graphs} =
	       	   get_clause_body(Clauses,AllClauses,HasAbstractOrIsInitial,
	       	                 BArgs,Env,NFreeV,Deps,ClauseNumber + 1),
	       	{SelectedClause,NClauseBody,NBindings,
	       	 NNFreeV,GraphFailed ++ Graphs}
		end,
	%BUArgs = [cerl:unfold_literal(BArg) || BArg <- BArgs],
	%io:format("BArgs: ~p\nNClause: ~p\n",[BArgs,NClause]),
	case cerl_clauses:reduce([NClause| Clauses], BArgs) of
		{true, {{c_clause, _, _, _, ClauseBody}, Bindings}} -> 
			case cerl:clause_body(Clause) of
				ClauseBody -> 
					TempEnv = ets:new(env_guard,[set]),
					ets:insert(TempEnv,ets:tab2list(Env)),
					%{VarsPattern,ValuesPattern} = lists:unzip(Bindings),
					add_bindings_to_env([ {Var, Value, [], null} || {Var,Value} <- Bindings],TempEnv),
					%create_new_env(VarsPattern, [{VP,[],null} , TempEnv),
					{GuardValue,FreeV,[],_} = 
						get_tree(cerl:clause_guard(Clause),TempEnv,FreeV),
					DepsGuard_ = 
					 	try get_dependences(cerl_trees:variables(cerl:clause_guard(Clause)),TempEnv) 
						catch _:_ -> []
						end,
					VarsBindings = [cerl:var_name(Var) || {Var,_} <- Bindings],
					DepsGuard = [Entry || Entry = {Var,{_,_}} <- DepsGuard_, not(lists:member(Var,VarsBindings))],
					%io:format("VarsBindings: ~p\nDepsGuard_: ~p\nDepsGuard: ~p\n",[VarsBindings,DepsGuard_,DepsGuard]),
					ets:delete(TempEnv),
					%io:format("Bindings: ~p\nRemoving: ~p\n", [Bindings,remove_core_vars_from_env(Bindings)]),
					case cerl:concrete(GuardValue) of
					     true -> 
					     	{NFreeV,GraphGuardSucceeds} = 
					      	   case HasAbstractOrIsInitial and (Clauses =/= []) of
					      	   		true ->
					      	   			%io:format("cerl:clause_guard(Clause): ~p\n",[cerl:clause_guard(Clause)]),
					      	   			case cerl:clause_guard(Clause) of 
					      	   				{c_literal,[],true} ->
							      	   			{FreeV + 1,
							      	   			 [{FreeV,1,ClauseNumber,remove_core_vars_from_env(Bindings),[],succeed}]};
							      	   		_ ->
							      	   			{FreeV + 2,
							      	   			 [{FreeV,2,ClauseNumber,remove_core_vars_from_env(Bindings),DepsGuard,succeed}]}
							      	   	end;
					      	        false ->
					      	          	{FreeV,[]}
					      	   end,
					      	Node = 
					      		case NFreeV of
					      			FreeV ->
					      				null;
					      			_ -> 
					      				FreeV
					      		end,
					      	%io:format("Bindings: ~p\nNode: ~p\n", [Bindings,Node]),
					      	NBindings =
					      	 [{BinVar, BinValue, Deps, Node} || {BinVar, BinValue} <- Bindings],
					     	{ClauseNumber,ClauseBody,NBindings,NFreeV,GraphGuardSucceeds};
					     false ->
					      	FunCreateFailedNode(guard,remove_core_vars_from_env(Bindings),DepsGuard)
					end;
				_ -> 
				 	FunCreateFailedNode(pattern,[],[])
			end;
		_ -> 
			FunCreateFailedNode(pattern,[],[])
	end;
get_clause_body([],_,_,_,_,_,_,_) -> 
	throw({error,"Non matching clause exists"}).

remove_core_vars_from_env([{Var,Value} | Tail]) ->
	VarName = cerl:var_name(Var),
	case atom_to_list(VarName) of
	    [$c,$o,$r | _] ->
	     	 remove_core_vars_from_env(Tail);
	    [$r,$e,$c | _] ->
	     	 remove_core_vars_from_env(Tail);
     	_ -> 
     		[{VarName,Value} | remove_core_vars_from_env(Tail)]
	end;
remove_core_vars_from_env([]) -> [].

add_graphs_to_graph(G,Graphs) ->
	[begin 
	  {V,Label} = digraph:vertex(G_,V),
%	  {StrLabel,_} = Label,
%	  NLabel = {StrLabel++ ", with: "++ Context,0},
	  digraph:add_vertex(G,V,Label) 
	 end || G_ <- Graphs,V <- digraph:vertices(G_)],
	[digraph:add_edge(G,V1,V2) || G_ <- Graphs,V1 <- digraph:vertices(G_),
	                              V2 <- digraph:out_neighbours(G_, V1)].
	

get_tree_call(Call,Env0,FreeV) -> 
	ModName = cerl:concrete(bind_vars(cerl:call_module(Call),Env0)),
	FunName = cerl:concrete(bind_vars(cerl:call_name(Call),Env0)),
	Args = cerl:call_args(Call),
	%Quitar funciones de esta lista
	%io:format("Args: ~p\n",[[cerl:call_module(Call),cerl:call_name(Call)|Args]]),
	%io:format("Env: ~p\n",[ets:tab2list(Env0)]),
	%io:format("GraphVars: ~p\n",[GraphVars]),
%	io:format("FUNCTION CALL ~p:~p/~p\n",[ModName,FunName,FunArity]),
    BArgs = lists:map(fun(Arg) -> bind_vars(Arg,Env0) end,Args),
    %io:format("BArgs: ~p\n",[BArgs]),
 	NoLits = 
     	case [BArg || BArg = {anonymous_function,_,_,_,_,_} <- BArgs] of
     	     [] ->
     	        [BArg || BArg <- BArgs,
     	                 not cerl:is_literal(cerl:fold_literal(BArg))];
     	     List -> List
     	end,
     	%io:format("NoLits: ~p\n",[NoLits]),
 	case NoLits of
 	     [] -> 
 	     	ABArgs = [get_abstract_from_core_literal(cerl: fold_literal(BArg)) || BArg <- BArgs],
 	     	%io:format("ABArgs: ~p\n",[ABArgs]),
 	     	Value =  
				try
		      	% Posible problema si el ya existe un módulo foo
				   M1 = smerl:new(foo),
				   {ok, M2} = 
				      smerl:add_func(M1, 
				          {function,1,bar,0,
			                    [{clause,1,[],[],
			                     [{call,1,{remote,1,{atom,1,ModName},
				                               {atom,1,FunName}},ABArgs}]}]}),
		            smerl:compile(M2,[nowarn_format]),
		            foo:bar() %Genera el valor utilizando el módulo creado
		                      %on de fly
				catch
				   Exception:Reason -> {Exception,Reason}
				end,
			CValue = cerl:abstract(Value),
			%io:format("AValue: ~p\n\n",[AValue]),
			{CValue,FreeV,[],[]};
	     _ -> 
	        case {ModName,FunName} of
	             {'erlang','is_function'} ->
	                case BArgs of
	                     [Function = {c_call,_,{c_literal,_,erlang},
	                      {c_literal,_,make_fun},_} | MayBeArity] ->
	                       {_,_,CFunArity} = get_MFA(Function),
	                       case lists:map(fun cerl:concrete/1,MayBeArity) of
     	                            [] ->  {{c_literal,[],true},FreeV,[],[]};
     	                            [CFunArity] -> {{c_literal,[],true},FreeV,[],[]};
     	                            _ -> {{c_literal,[],false},FreeV,[],[]}
     	                    end;
	                     [{anonymous_function,_,AFunCore,_,_,_} | MayBeArity] ->
    	                       AFunArity = cerl:fun_arity(AFunCore),
    	                       case lists:map(fun cerl:concrete/1,MayBeArity) of
    	                            [] ->  {{c_literal,[],true},FreeV,[],[]};
    	                            [AFunArity] -> {{c_literal,[],true},FreeV,[],[]};
    	                            _ -> {{c_literal,[],false},FreeV,[],[]}
    	                       end; 
	                     _ ->
	                       {{c_literal,[],false},FreeV,[],[]}
	                end;
	             _ ->
	               	FileAdress = code:where_is_file(atom_to_list(ModName)++".erl"),
					% Busca el erl. Si no está, busca el beam (libreria de sistema) y tunea la ruta
					% para apuntar al ebin/ correspondiente
					NFileAdress = 
					   case FileAdress of
					        non_existing -> 
					     		NFileAdress_ = 
					     		   code:where_is_file(atom_to_list(ModName)++".beam"),
					     		case NFileAdress_ of
					     		     non_existing -> 
					     		     	throw({error,"Non existing module",ModName});
					     		     _ -> 
					     		     	RelPath = "ebin/" ++ 
					     		     	           atom_to_list(ModName) ++ ".beam",
					     		     	NRelPath = "src/" ++ 
					     		     	           atom_to_list(ModName) ++ ".erl",
					     		     	PrevPath = 
					     		     	  lists:sublist(NFileAdress_,
					     		     	     1,length(NFileAdress_)-length(RelPath)),
					     		     	PrevPath ++ NRelPath
					     		end;
					     	_ -> FileAdress
					   end,
	               ModCore = edd_zoom_lib:core_module(NFileAdress),
	               EnvApply = ets:new(FunName, [set]),
	               ets:insert(EnvApply, {core,ModCore}),
	               add_bindings_to_env([ {Var, Value, [], null} || {Var,Value} <- lists:zip(Args,BArgs)],EnvApply),
	               FunArity = cerl:call_arity(Call),
	               get_tree_apply(cerl:ann_c_apply(cerl:get_ann(Call),
	                            cerl:c_var({FunName,FunArity}),Args),
	                     EnvApply,FreeV)
	        end
	        
	 end.


extract_module_from_ann([_,{file,File}]) ->
	[_,_,_,_|ModName_] = lists:reverse(File),
	list_to_atom(lists:reverse(ModName_)).

get_tree_apply(Apply,Env0,FreeV)->
	FunVar = cerl:apply_op(Apply),
	%io:format("Apply: ~p\nFunVar: ~p\n",[Apply,FunVar]),
	Pars = cerl:apply_args(Apply),
	NPars = lists:map(fun(Par) -> bind_vars(Par,Env0) end,Pars),
	%io:format("Env: ~p\n",[[ {I,L}|| {I,L} <- ets:tab2list(Env0),I=/='core']]),
	%io:format("Pars: ~p\nNPars: ~p\n",[Pars,NPars]),
	{core,Core} = hd(ets:lookup(Env0,core)),
	FunDefs = cerl:module_defs(Core),
	case FunVar of 
		{anonymous_function,_,{c_fun,_,Args,FunBody},_,_,EnvAFun} ->
			get_tree_apply_fun(Args,NPars,FreeV,FunBody,EnvAFun);
		_ -> 
			case cerl:type(FunVar) of
				'var' ->
					%io:format("ENTRA: ~p\n",[FunDefs]),
					case cerl:var_name(FunVar) of
					     {FunName,_} ->
							case [FunBody_ || {{c_var,_,FunName_},FunBody_} <- FunDefs, 
							                  FunName_ == cerl:var_name(FunVar)] of
							     [{c_fun,_,Args,FunBody}|_] -> % apply 'd'/1 (3)
							     	get_tree_apply_fun(Args,NPars,FreeV,FunBody,[{core,Core}]);
							     [{anonymous_function,_,{c_fun,_,Args,FunBody},_,_,EnvAFun}|_] -> %list comprehension
							     	NEnvAFun = [{core,Core} | [Item || Item <- EnvAFun, element(1,Item) =/= 'core']], 
				     				get_tree_apply_fun(Args,NPars,FreeV,FunBody,NEnvAFun);
							     _ ->
							     	% Esta en otro modulo
							     	get_tree_call(
							     	  cerl:ann_c_call(cerl:get_ann(Apply),
							     	              {c_literal,[],extract_module_from_ann(cerl:get_ann(Apply))},
							     	              {c_literal,[],FunName},Pars),Env0,FreeV)
							end;
					     _ -> % Apply de una variable
					     	BFunVar = bind_vars(FunVar,Env0),
					     	case BFunVar of
					     	     {anonymous_function,_,{c_fun,_,Args,FunBody},_,_,EnvAFun} -> % Se enlaza a una función anónima
					     	        % {anonymous,{c_fun,_,Args,FunBody},_,_,_} = 
					     	        %         get_anon_func(EnvAF,FunName,FunCore),
						           get_tree_apply_fun(Args,NPars,FreeV,FunBody,EnvAFun);
					     	     _ -> % Caso de un make_fun
							     	{ModName,FunName,_} = get_MFA(BFunVar),
							     	get_tree_call({c_call,cerl:get_ann(Apply),
							     	            	{c_literal,[],ModName},{c_literal,[],FunName},NPars},
						     	            		ets:new(env, [set]),FreeV)
							end
					end;
				_ -> 
					{ModName,FunName,_} = get_MFA(FunVar),
			     	get_tree_call({c_call,cerl:get_ann(Apply),
			     	            {c_literal,[],ModName},{c_literal,[],FunName},NPars},
			     	             ets:new(env, [set]),FreeV)
			end
		end.
	
get_tree_apply_fun(Args,NPars,FreeV,FunBody,Env0) ->
	%io:format("ENTRA: ~p\n",[NPars]),
	Env = ets:new(env_temp, [set]),
	add_bindings_to_env([ {Var, Value, [], null} || {Var,Value} <- lists:zip(Args,NPars)],Env),
	%create_new_env(Args, NPars, Env),
	%io:format("Env0: ~p\nEnv: ~p\n",[Env0,ets:tab2list(Env]),
	add_bindings_to_env(Env0,Env),
	{Value,NFreeV,_,FExpr} = get_tree(FunBody,Env,FreeV),
	ets:delete(Env),
	{Value,NFreeV,[],FExpr}.


get_tree(Expr,Env,FreeV) ->
	%io:format("Expr: ~p\n",[Expr]),
	%io:format("Type: ~p\n",[cerl:type(Expr)]),
	%io:format("Env: ~p\n",[ets:tab2list(Env)]),
	%io:format("Ann: ~p\n",[cerl:get_ann(Expr)]),
	LineFile =
		case cerl:get_ann(Expr) of 
			[_,Line_,{file,File_}] ->
				 [Line_,File_];
			[Line_,{file,File_}] ->
				[Line_,File_];
			[Line_,{file,File_}|_] ->
				[Line_,File_];
			[] ->
				[]
		end,
	case cerl:type(Expr) of
		'apply' ->
			{Value,NFreeV,Graph,_} = get_tree_apply(Expr,Env,FreeV),
			{Value,NFreeV,Graph,['call'|LineFile]};
		'case' ->
			get_tree_case(Expr,Env,FreeV);
		'let' ->
			Vars = cerl:let_vars(Expr),
			LetArg = cerl:let_arg(Expr),
			ALet = 
				case cerl:get_ann(hd(Vars)) of 
					[Line,{file,File}] ->
						get_expression_from_abstract(File,Line,'match');
					_ ->
						[]
				end,
			%io:format("ALet: ~p\n",[ALet]),
			% io:format("LetArg: ~p\nVars: ~p\n",
			% 	[LetArg,cerl_trees:variables(LetArg)]),

			{ValueArg,NFreeV_,GraphsArgRight,_} = 
		     	get_tree(LetArg,Env,FreeV),
		    Deps = get_dependences(cerl_trees:variables(LetArg),Env),

		    %io:format("Deps: ~p\n",[Deps]),

		    {GraphsArg,NFreeV} = 
		    	build_graphs_and_add_bindings(Vars,Env,NFreeV_,ValueArg,Deps,ALet,[]),
		    
		    FunCreateNGraphsArg = 
		    	fun() ->
				    case GraphsArg of 
				    	[] -> 
				    		ets:insert(Env,{let_graphs,{cerl:var_name(hd(Vars)),GraphsArgRight}}),
				    		[];
				    	_ ->
				    		NG = digraph:new([acyclic]),
							add_graphs_to_graph(NG,GraphsArg),
							add_graphs_to_graph(NG,GraphsArgRight),
				    		[digraph:add_edge(NG,NFreeV_,edd_zoom_lib:look_for_root(G_)) || G_ <- GraphsArgRight],
				    		[NG]
			    	end
			    end,
		    NGraphsArg =
			    case cerl:type(LetArg) of 
			    	'var' -> 
			    		VarLetArg = cerl:var_name(LetArg),
			    		ResLookUp = ets:lookup(Env,let_graphs),
			    		ets:delete(Env,let_graphs),
			    		case ResLookUp of 
			    			[{let_graphs,{VarLetArg,StoredGraphs}}] -> 
			    				NG = digraph:new([acyclic]),
								add_graphs_to_graph(NG,GraphsArg),
								add_graphs_to_graph(NG,StoredGraphs),
					    		[digraph:add_edge(NG,NFreeV_,edd_zoom_lib:look_for_root(G_)) || G_ <- StoredGraphs],
					    		[NG];
					    	_ -> 
					    		FunCreateNGraphsArg()
					    end;
					_ ->
						FunCreateNGraphsArg()
				end,
			case check_errors(ValueArg) of
			     true -> 
			     	{ValueArg,NFreeV,NGraphsArg,[]};
			     _ ->
					LetBody = cerl:let_body(Expr),
					{Value,NNFreeV,GraphBody,LastExprInfo} = 
					    get_tree(LetBody,Env,NFreeV),
					{Value,NNFreeV,NGraphsArg ++ GraphBody,LastExprInfo} 
			end;
		'letrec' ->
			%les definicions poden gastar variables declarades abans? Si es aixi hi hauria que tractaro
			NewDefs_ = cerl:letrec_defs(Expr),
			NewDefs = 
				[begin {Value,_,_,_} = get_tree(FunDef,Env,FreeV), {VarName,Value} end 
				 || {VarName,FunDef} <- NewDefs_],
			{core,Core} = hd(ets:lookup(Env,core)),
			NCore =	cerl:c_module(cerl:module_name(Core), 
					 	cerl:module_exports(Core),
					 	cerl:module_attrs(Core),
					 	NewDefs ++ cerl:module_defs(Core)),
			ets:insert(Env,{core,NCore}),
			{Value,NFreeV,Graph,_} = get_tree(cerl:letrec_body(Expr),Env,FreeV),
			{Value,NFreeV,Graph,['lc'|LineFile]};
			% Genero un nuevo CORE de un módulo que es igual a 'Core' pero que tiene
			% la función declarada en el letrec y genero el arbol del cuerpo del letrec
		'call' ->
			case Expr of
			     {c_call,_,{c_literal,_,erlang},{c_literal,_,make_fun},_} ->
			     	{Expr,FreeV,[],[]};
			     _ -> 
			     	{Value,NFreeV,Graph,_} = get_tree_call(Expr,Env,FreeV),
			     	{Value,NFreeV,Graph,['call'|LineFile]}
			end;
		'fun' -> 
			[Line,File] = LineFile,
			FunName =
				case cerl:get_ann(Expr) of 
					[{id,{_,_,FunName_}},_,_] ->
						 FunName_;
					[_,_] ->
						list_to_atom("let_rec_" ++ File ++ "_" ++ integer_to_list(Line))  
				end,
			%io:format("Previous expr: ~p\n",[Expr]),
			%io:format("\n\nVars: ~p\n\n\n",[cerl_trees:variables(Expr)]),
			NExpr = 
			   apply_substitution(Expr,Env,[]), % Sustituye las variables libres
			EnvFun = ets:new(FunName,[set]),
			ets:insert(EnvFun,ets:tab2list(Env)),
			%io:format("Env: ~p\nEnvFun: ~p\n",[ets:tab2list(Env),ets:tab2list(EnvFun)]),
			%io:format("NExpr: ~p\n",[NExpr]),
			% de la fun por sus valores
			%io:format("New expr: ~p\n",[NExpr]),
			{{anonymous_function,FunName,NExpr,File,Line,ets:tab2list(EnvFun)},FreeV,[],['fun'|LineFile]};
		'cons' ->
			{[NHd,NTl],NFreeV,Graphs} = 
				get_tree_list([cerl:cons_hd(Expr),cerl:cons_tl(Expr)],
				            Env,FreeV),
			{cerl:c_cons(NHd,NTl),NFreeV,Graphs,['cons'|LineFile]};
		'tuple' ->
			{NExps,NFreeV,Graphs} = 
			    get_tree_list(cerl:tuple_es(Expr),Env,FreeV),
			{cerl:c_tuple(NExps),NFreeV,Graphs,['tuple'|LineFile]};
		'try' ->
			{ValueArg,NFreeV,GraphsArg,_} = 
			   get_tree(cerl:try_arg(Expr),Env,FreeV),
			Deps = get_dependences(cerl_trees:variables(cerl:try_arg(Expr)),Env),
			%io:format("Try-Value: ~p\n",[ValueArg]),
			case ValueArg of
			     {c_literal,[],{error,TypeError}} -> 
			     	%io:format("ERROR\n"),
			     	AError = [cerl:abstract(Lit) || Lit <- [error,TypeError,[]]],
			     	EVarsError = lists:zip(cerl:try_evars(Expr),AError),
			     	add_bindings_to_env(
			     		[{EVar,EValue,Deps,null} || {EVar,EValue} <- EVarsError],Env),
			     	case cerl:get_ann(Expr) of 
						[Line,{file,FileName}] ->
							%io:format("INSERTA\n"),
							ets:insert(Env,{current_try,{FileName,Line,"catch of try"}});
						[] ->
							ok
					end, 
			     	{Value,NNFreeV,GraphsBody,LastExprInfo} = 
			     	    get_tree(cerl:try_handler(Expr),Env,NFreeV),
			     	{Value,NNFreeV,GraphsArg ++ GraphsBody,LastExprInfo};
			     _ ->
			        lists:map(fun(Var) -> 
			                    add_bindings_to_env([{Var,ValueArg,Deps,null}],Env) 
			                  end,cerl:try_vars(Expr)),
			       	case cerl:get_ann(Expr) of 
						[Line,{file,FileName}] ->
							ets:insert(Env,{current_try,{FileName,Line,"try"}});
						[] ->
							ok
					end, 
			     	{Value,NNFreeV,GraphsBody,LastExprInfo} = 
			     	   get_tree(cerl:try_body(Expr),Env,NFreeV),
			     	{Value,NNFreeV,GraphsArg ++ GraphsBody,LastExprInfo}
			end;
		'catch' ->
		  % Genera la expresión azucar sintáctico equivalente y computa su árbol
			TempVar = 
			   cerl:c_var(list_to_atom("_cor_catch"++integer_to_list(FreeV))),
			TempVar1 = 
			   cerl:c_var(list_to_atom("_cor_catch"++integer_to_list(FreeV+1))),
			TempVar2 = 
			   cerl:c_var(list_to_atom("_cor_catch"++integer_to_list(FreeV+2))),
			TempVar3 = 
			   cerl:c_var(list_to_atom("_cor_catch"++integer_to_list(FreeV+3))),
			EqTry = 
			  cerl:c_try(cerl:catch_body(Expr),
			             [TempVar],TempVar,[TempVar1,TempVar2,TempVar3], 
			             cerl:c_case(
			                TempVar1,
			           	[cerl:c_clause([cerl:abstract('throw')],TempVar2),
			                 cerl:c_clause([cerl:abstract('exit')],
			                 cerl:c_tuple([cerl:abstract('EXIT'),TempVar2])),
			           	 cerl:c_clause([cerl:abstract('error')],
			           	               cerl:c_tuple([cerl:abstract('EXIT'),
			           	                             cerl:c_tuple(
			           	                               [TempVar2,TempVar3])]))
			           	])),
			get_tree(EqTry,Env,FreeV);  
%		'bitstr' ->
%			{Value,NFreeV,Roots} = 
%			   getTree(cerl:bitstr_val(Expr),Env,G,Core,FreeV,EnvAF,Trusted),
%			{cerl:c_bitstr(Value,cerl:bitstr_size(Expr),
%			              cerl:bitstr_unit(Expr),cerl:bitstr_type(Expr),
%			              cerl:bitstr_flags(Expr)),
%			NFreeV,Roots};
%		'binary' ->
%			{NExps,NFreeV,Roots} = 
%			   getTreeList(cerl:binary_segments(Expr),Env,G,Core,FreeV,EnvAF,Trusted),
%			{cerl:c_binary(NExps),NFreeV,Roots};
		'seq' ->
		         {Value,NFreeV,Graphs,_} =
		           get_tree(cerl:seq_arg(Expr),Env,FreeV),
		         case check_errors(Value) of
		              true -> 
		              	{Value,NFreeV,Graphs,[]};
		              _ ->
		              	get_tree(cerl:seq_body(Expr),Env,FreeV)
		         end;
		'literal' ->
			%io:format("Entra: ~p\n",[['literal'|LineFile]]),
			{Expr,FreeV,[],['literal'|LineFile]};
		'var' -> 
			{bind_vars(Expr,Env),FreeV,[],['var'|LineFile]};
		'values' -> 
			{bind_vars(Expr,Env),FreeV,[],[]};
		'primop' ->
		        {{c_literal,[],
		          {error,cerl:concrete(cerl:primop_name(Expr)),
		          [
		          	try cerl:concrete(bind_vars(Arg,Env))
		          	catch _:_ -> bind_vars(Arg,Env)
		          	end
		          || Arg <- cerl:primop_args(Expr)]}},
		        FreeV,[],[]};
		_ -> throw({error,"Non treated expression",Expr})%,
		     %{Expr,FreeV,[],[]}	
	end.


	