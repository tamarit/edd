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
%%% @doc Erlang Declarative Debugger. This is the main module of the Erlang
%%%      Declarative Debugger (edd). The debugger asks to programmer questions 
%%%      about the expected results of function calls and points out the 
%%%      program fragment that is causing the bug. 
%%% @end
%%%-----------------------------------------------------------------------------

-module(edd).
-export([dd/1,dd/2,dd/3, dd_server/2, cdd/2, cdd/3, cdd/4]).


-record(edd_options, 
	{
	  strategy = divide_query
	, tree = false
	, load_tests = true
	, save_tests = true
	, test_files = []
	}).

%%------------------------------------------------------------------------------
%% @doc Starts the declarative debugger 'edd' with an initial expression 'Expr'
%%      whose evaluation yields an incorrect value. The arguments can be atom 
%%      top_down, indicating that a Top Down strategy will be followed, or atom 
%%      tree, indicating that the tree will be created.
%% @end
%%------------------------------------------------------------------------------
-spec dd(Expr::string(), StrategyGraph1 :: top_down | tree, 
         StrategyGraph2 :: top_down | tree) -> ok.
dd(Expr,top_down,tree) -> 
	dd_internal(Expr,#edd_options{strategy = top_down, tree = true});
dd(Expr,tree,top_down) -> 
	dd_internal(Expr,#edd_options{strategy = top_down, tree = true});
dd(Expr,tree,_) -> 
	dd(Expr,tree);
dd(Expr,_,tree) -> 
	dd(Expr,tree);
dd(Expr,top_down,_) -> 
	dd(Expr,top_down);
dd(Expr,_,top_down) -> 
	dd(Expr,top_down);
dd(Expr,_,_) -> 
	dd(Expr).

%%------------------------------------------------------------------------------
%% @doc Starts the declarative debugger 'edd' with an initial expression 'Expr'
%%      whose evaluation yields an incorrect value. If the second argument is 
%%      atom top_down, the strategy will be set to Top Down and the tree will
%%      not be created. Contrarily, if the second argument is atom tree, the
%%      tree will be created, but the strategy will be Divide and Query.
%%		An alternative is to use a list of options. This list can include 
%%		both atoms top_down and tree. Additionally there are three options 
%%		related with eunit tests. not_load_tests/not_save_tests avoids load/
%%		save tests from/to the debugged modules. Finally, given the tuple 
%%		{test_files, Files :: list()} edd will automatically load additional 
%%		eunit tests from the given files.
%% @end
%%------------------------------------------------------------------------------
-spec dd(Expr::string(), Options :: top_down | tree | list()) -> ok.	
dd(Expr,Options) when is_list(Options)->
	Strategy = 
		case lists:member(top_down, Options) of
			true -> 
				top_down;
			false ->
				divide_query
		end,
	TestFiles  =
		case [Files || {test_files, Files} <- Options ] of 
			[] ->
				[];
			[ListFiles] ->
				ListFiles
		end,
	dd_internal(Expr,
		#edd_options{
			  strategy = Strategy
			, tree = lists:member(tree, Options) 
			, load_tests = not(lists:member(not_load_tests, Options))
			, save_tests = not(lists:member(not_save_tests, Options))
			, test_files = TestFiles
		});
dd(Expr,top_down) ->
	dd_internal(Expr,#edd_options{strategy = top_down});
dd(Expr,tree) ->
	dd_internal(Expr,#edd_options{tree = true});
dd(Expr,_) ->
	dd(Expr).


%%------------------------------------------------------------------------------
%% @doc Starts the declarative debugger 'edd' with an initial expression 'Expr'
%%      whose evaluation yields an incorrect value. This function allow to debug
%%		concurrent programs. The second argument is the time (in miliseconds) to
%%		trace the program.
%% @end
%%------------------------------------------------------------------------------
-spec cdd(Expr::string(), TraceTimeout :: integer()) -> ok.	
cdd(Expr,TraceTimeout) ->
	cdd(Expr,TraceTimeout, []).

%%------------------------------------------------------------------------------
%% @doc Starts the declarative debugger 'edd' with an initial expression 'Expr'
%%      whose evaluation yields an incorrect value. This function allow to debug
%%		concurrent programs. The second argument is the time (in miliseconds) to
%%		trace the program.
%% @end
%%------------------------------------------------------------------------------
-spec cdd(Expr::string(), TraceTimeout :: integer(), info | list()) -> ok.	
cdd(Expr,TraceTimeout, info) ->
	cdd(Expr,TraceTimeout, info, []);
cdd(Expr,TraceTimeout, InstModules) when is_list(InstModules) ->
	put(print_session_info, false),
	put(modules_to_instrument, InstModules),
	edd_con:cdd(Expr,TraceTimeout).

%%------------------------------------------------------------------------------
%% @doc Starts the declarative debugger 'edd' with an initial expression 'Expr'
%%      whose evaluation yields an incorrect value. This function allow to debug
%%		concurrent programs. The second argument is the time (in miliseconds) to
%%		trace the program.
%% @end
%%------------------------------------------------------------------------------
-spec cdd(Expr::string(), TraceTimeout :: integer(), info, InstModules :: list()) -> ok.	
cdd(Expr,TraceTimeout, info, InstModules) ->
	put(print_session_info, true),
	put(modules_to_instrument, InstModules),
	edd_con:cdd(Expr,TraceTimeout).

%%------------------------------------------------------------------------------
%% @doc Starts the declarative debugger 'edd' server.
%% @end
%%------------------------------------------------------------------------------
-spec dd_server(Expr::string(), Dir :: string()) -> ok.	
dd_server(Expr, Dir) ->
	code:add_patha(Dir), 
	% io:format("PATHS: ~p\n",[code:get_path()]),
	dd_internal_core(Expr, fun(X) -> edd_lib:core_module(atom_to_list(X) ++ ".erl", Dir) end).


%%------------------------------------------------------------------------------
%% @doc Starts the declarative debugger 'edd' with an initial expression 'Expr'
%%      whose evaluation yields an incorrect value. It will use Divide and Query
%%      strategy and will not create the tree.
%% @end
%%------------------------------------------------------------------------------
-spec dd(Expr::string()) -> ok.	
dd(Expr) ->
	dd_internal(Expr,#edd_options{}).

dd_internal(Expr, 
	#edd_options{
		  strategy = Strategy 
		, tree = Graph
		, load_tests = LoadTest
		, save_tests = SaveTest
		, test_files = TestFiles}) ->
	G = dd_internal_core(Expr, fun(X) -> edd_lib:core_module(atom_to_list(X)++".erl") end),
	case Graph of
	     true ->
	       edd_lib:dot_graph_file(G,"dbg");
	     false -> 
	       ok
	end,
	%TO BENCHAMARK ONLY %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	io:format("Total number of tree nodes: ~p\n",[length(digraph:vertices(G))]),
	[_,{memory,Words},_] = digraph:info(G),
	io:format("Tree size:\n\t~p words\n\t~p bytes\n", [Words, Words * erlang:system_info(wordsize)]),
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	edd_lib:ask(G,Strategy,Graph, {LoadTest, SaveTest, TestFiles}).

dd_internal_core(Expr, FunCore) ->
	{ok,[AExpr|_]} = edd_lib:parse_expr(Expr++"."),
	M1 = smerl:new(foo),
	{ok, M2} = smerl:add_func(M1,"bar() ->" ++ Expr ++ " ."),
	%Obtiene el CORE de la expresión Expr
	{ok,_,CoreP} = smerl:compile2(M2,[to_core,binary,no_copt]), 
	InitialCall = extract_call(CoreP),
	% io:format("InitialCall: ~p\n",[InitialCall]),
	FunOperator = erl_syntax:application_operator(AExpr),
	{remote,_,{atom,_,ModName},_} = FunOperator,
	% io:format("ModName: ~p\n",[ModName]),
	Core = FunCore(ModName),
	% io:format("Core: ~p\n",[Core]),
	
	%to get the .core file. ONLY FOR DEBUGGING 
	%compile:file(atom_to_list(ModName)++".erl",[to_core,no_copt]),
	FunsInitialCall = get_funs_from_abstract(AExpr,-1),
	%TO CHANGE
	put(funs_initial_call,FunsInitialCall),
	put(case_id,0),
	%
	
	G = digraph:new([acyclic]),
	Env = ets:new(env,[bag]),
	{Value,FreeV,Roots} = 
	  get_tree(InitialCall,ets:new(env,[bag]),G,Core,0,false),
	ets:delete(Env),

	%Caso especial si la llamada inicial tiene funciones anidadas como f(g(3)), que
	%será traducido a CORE con un let x = g(3) in f(x)
	% io:format("InitialCall: ~p\n",[InitialCall]),
	%io:format("FreeV: ~p\n",[FreeV]),
 	%io:format("Roots: ~w\n",[Roots]),
	FunAddVertexAndEdges = 
		fun(Call) ->
			case cerl:concrete(cerl:call_module(Call)) of
 	     	     'erlang' -> 
 	     	     	ok;
 	     	     _ ->
 	     	       	digraph:add_vertex(G,FreeV,
	 	     	                          {Expr ++ " = "
	 	     	                           ++ io_lib:format("~p",[cerl:concrete(Value)]),
	 	     	                           get(0),none,1}),
					[digraph:add_edge(G,FreeV,Root) || Root <- Roots]
 	     	end
 	     end,
	case cerl:type(InitialCall) of 
	     'let' ->
	     	case cerl:type(cerl:let_arg(InitialCall)) of
	     	     'call' ->
	     	     	FunAddVertexAndEdges(cerl:let_arg(InitialCall));
	     	%      	case cerl:concrete(cerl:call_module(cerl:let_arg(InitialCall))) of
	     	%      	     'erlang' -> 
	     	%      	     	ok;
	     	%      	     _ ->
	     	%      	       	digraph:add_vertex(G,FreeV,
	     	%      	                          {Expr++" = "
	     	%      	                           ++io_lib:format("~p",[cerl:concrete(Value)]),
	     	%      	                           get(0),none,1}),
							% [digraph:add_edge(G,FreeV,Root) || Root <- Roots]
	     	%      	end;
	     	     _ -> ok
	     	end;
	     % 'call' ->
	     % 		io:format("ENTRA\n"),
	     % 		FunAddVertexAndEdges(InitialCall);
	     	% case cerl:concrete(cerl:call_module(cerl:let_arg(InitialCall))) of
	     	%      'erlang' -> 
	     	%      	ok;
	     	%      _ ->
	     	%        	digraph:add_vertex(G,FreeV,
	     	%                           {Expr++" = "
	     	%                            ++io_lib:format("~p",[cerl:concrete(Value)]),
	     	%                            get(0),none,1}),
	     _ -> 
	     	ok
	end,
    G.


get_tree(Expr,Env,G,Core,FreeV,Trusted) ->
	% io:format("Expr: ~p\n",[Expr]),
	% io:format("Env: ~p\n",[ets:tab2list(Env)]),
	case cerl:type(Expr) of
		'apply' ->
			get_tree_apply(Expr,Env,G,Core,FreeV,Trusted);
		'case' ->
			get_tree_case(Expr,Env,G,Core,FreeV,Trusted);
		'let' ->
			Vars = cerl:let_vars(Expr),
			LetArg = cerl:let_arg(Expr),
			{ValueArg,NFreeV,RootsArg} =
			   get_tree(LetArg,Env,G,Core,FreeV,Trusted),
			case check_errors(ValueArg) of
			     true -> 
			     	{ValueArg,NFreeV,RootsArg};
			     _ -> 
				lists:map(fun(Var) -> add_bindings_to_env([{Var,ValueArg}],Env) end,Vars),
				LetBody = cerl:let_body(Expr),
				{Value,NNFreeV,Roots} = 
				   get_tree(LetBody,Env,G,Core,NFreeV,Trusted),
				{Value,NNFreeV,RootsArg ++ Roots} 
			% Se devuelven las raices de los árboles de cómputo de cada expresión
			% en el argumento del let (RootArgs) además de las raíces de los árboles
			% de cómputo del cuerpo (RootArgs)
			end;
		'letrec' ->
			NewDefs_ = cerl:letrec_defs(Expr),
			NewDefs = 
			   [{V,apply_substitution(FunDef,Env,[])} || {V,FunDef} <- NewDefs_],
			NCore =	cerl:c_module(cerl:module_name(Core), 
					 	cerl:module_exports(Core),
					 	cerl:module_attrs(Core),
					 	NewDefs ++ cerl:module_defs(Core)),
			get_tree(cerl:letrec_body(Expr),Env,G,NCore,FreeV,Trusted);
			% Genero un nuevo CORE de un módulo que es igual a 'Core' pero que tiene
			% la función declarada en el letrec y genero el arbol del cuerpo del letrec
	    'fun' -> 
			[{id,{_,_,FunName}},Line,{file,File}] = cerl:get_ann(Expr),
			NExpr = apply_substitution(Expr,Env,[]), % Sustituye las variables libres
			% de la fun por sus valores
			% ets:insert(EnvAF,{FunName,{anonymous,NExpr,File,Line,Env}}),
			% % Guardo la función anónima en el entorno por si luego me aparece saber
			% % dónde estaba
			EnvFun = ets:new(FunName,[set]),
			ets:insert(EnvFun,ets:tab2list(Env)),
			{{anonymous_function,FunName,NExpr,File,Line,EnvFun},FreeV,[]};
		'call' ->
			case Expr of
			     {c_call,_,{c_literal,_,erlang},{c_literal,_,make_fun},_} ->
			     	{Expr,FreeV,[]};
			     _ -> 
			     	get_tree_call(Expr,Env,G,Core,FreeV)
			end;
		'cons' ->
			{[NHd,NTl],NFreeV,Roots} = 
				get_tree_list([cerl:cons_hd(Expr),cerl:cons_tl(Expr)],
				            Env,G,Core,FreeV,Trusted),
			{cerl:c_cons(NHd,NTl),NFreeV,Roots};
		'tuple' ->
			{NExps,NFreeV,Roots} = 
			   get_tree_list(cerl:tuple_es(Expr),Env,G,Core,FreeV,Trusted),
			{cerl:c_tuple(NExps),NFreeV,Roots};
		'try' -> 
			{ValueArg,NFreeV,ArgRoots} = 
			   get_tree(cerl:try_arg(Expr),Env,G,Core,FreeV,Trusted),
			case ValueArg of
			     {c_literal,[],{error,TypeError}} -> 
			     	add_bindings_to_env(
			     	   lists:zip(cerl:try_evars(Expr),
			     	             [cerl:abstract(Lit) 
			     	              || Lit <- [error,TypeError,[]]]),Env),
			     	{Value,NNFreeV,BodyRoots} = 
			     	   get_tree(cerl:try_handler(Expr),Env,G,Core,NFreeV,Trusted),
			     	{Value,NNFreeV,ArgRoots ++ BodyRoots};
			     _ ->
			        lists:map(fun(Var) -> 
			                    add_bindings_to_env([{Var,ValueArg}],Env) 
			                  end,cerl:try_vars(Expr)),
			     	{Value,NNFreeV,BodyRoots} = 
			     	   get_tree(cerl:try_body(Expr),Env,G,Core,NFreeV,Trusted),
			     	{Value,NNFreeV,ArgRoots ++ BodyRoots}
			end;
		'catch' ->
		  % Genera la expresión azucar sintáctico equivalente y computa su árbol
			TempVar = cerl:c_var(list_to_atom("_cor_catch"++integer_to_list(FreeV))),
			TempVar1 = cerl:c_var(list_to_atom("_cor_catch"++integer_to_list(FreeV+1))),
			TempVar2 = cerl:c_var(list_to_atom("_cor_catch"++integer_to_list(FreeV+2))),
			TempVar3 = cerl:c_var(list_to_atom("_cor_catch"++integer_to_list(FreeV+3))),
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
			get_tree(EqTry,Env,G,Core,FreeV,Trusted);  
		'bitstr' ->
			{Value,NFreeV,Roots} = get_tree(cerl:bitstr_val(Expr),Env,G,Core,FreeV,Trusted),
			{cerl:c_bitstr(Value,cerl:bitstr_size(Expr),
			              cerl:bitstr_unit(Expr),cerl:bitstr_type(Expr),
			              cerl:bitstr_flags(Expr)),
			NFreeV,Roots};
		'binary' ->
			{NExps,NFreeV,Roots} = get_tree_list(cerl:binary_segments(Expr),Env,G,Core,FreeV,Trusted),
			{cerl:c_binary(NExps),NFreeV,Roots};
		'seq' ->
		         {Value,NFreeV,Roots} =
		           get_tree(cerl:seq_arg(Expr),Env,G,Core,FreeV,Trusted),
		         case check_errors(Value) of
		              true -> 
		              	{Value,NFreeV,Roots};
		              _ ->
		              	{ValueB,NNFreeV,RootsB} = 
		              	   get_tree(cerl:seq_body(Expr),Env,G,Core,NFreeV,Trusted),
		              	{ValueB,NNFreeV,Roots ++ RootsB}
		         end;
		'literal' ->
			{Expr,FreeV,[]};
		'var' -> 
			{bind_vars(Expr,Env),FreeV,[]};
		'values' -> 
			{bind_vars(Expr,Env),FreeV,[]};
		'primop' ->
		        {{c_literal,[],{error,cerl:concrete(cerl:primop_name(Expr))}},FreeV,[]};
		_ -> throw({error,"Non treated expression",Expr}),
		     {Expr,FreeV,[]}	
	end.

get_tree_list([E|Es],Env,G,Core,FreeV,Trusted) ->
	{NE,NFreeV,RootsH} = get_tree(bind_vars(E,Env),Env,G,Core,FreeV,Trusted),
	{NEs,NNFreeV,RootsT} = get_tree_list(Es,Env,G,Core,NFreeV,Trusted),
	{[NE|NEs],NNFreeV,RootsH++RootsT};
get_tree_list([],_,_,_,FreeV,_) ->
	{[],FreeV,[]}.

get_tree_apply(Apply,Env0,G,Core,FreeV,Trusted)->
	FunVar = cerl:apply_op(Apply),
	%io:format("Apply: ~p\nFunVar: ~p\nModule: ~p\nEnv0: ~p\nTrusted: ~p\n",[Apply,FunVar,cerl:module_name(Core),ets:tab2list(Env0),Trusted]),
	Pars = cerl:apply_args(Apply),
	NPars = lists:map(fun(Par) -> bind_vars(Par,Env0) end,Pars),
	FunDefs = cerl:module_defs(Core),
	case FunVar of 
		{anonymous_function,_,{c_fun,_,Args,FunBody},_,_,_} ->
			get_tree_applyFun(Args,NPars,Core,FreeV,FunBody,
					          G,FunVar,FunVar,Trusted,[]);
		_ -> 
			case cerl:type(FunVar) of
				'var' ->
					case cerl:var_name(FunVar) of
					     {FunName,_} ->
							case [FunBody_ || {{c_var,_,FunName_},FunBody_} <- FunDefs, 
							                  FunName_ == cerl:var_name(FunVar)] of
							     [{c_fun,_,Args,FunBody}|_] -> % apply 'd'/1 (3)
							     	get_tree_applyFun(Args,NPars,Core,FreeV,
							     	                FunBody,G,FunVar,FunName,Trusted,[]);
							     _ -> % Apply de ¿?
							     	get_tree_call(
							     	  cerl:ann_c_call(cerl:get_ann(Apply),
							     	              {c_literal,[],extract_module_from_ann(cerl:get_ann(Apply))},
							     	              {c_literal,[],FunName},Pars),Env0,G,Core,FreeV)
							end;
					     _ -> % Apply de una variable
					     	BFunVar = bind_vars(FunVar,Env0),
					     	case BFunVar of
					     	     {anonymous_function,_,{c_fun,_,Args,FunBody},_,_,_} -> % Se enlaza a una función anónima
					     	        % {anonymous,{c_fun,_,Args,FunBody},_,_,_} = 
					     	        %         get_anon_func(EnvAF,FunName,FunCore),
					           get_tree_applyFun(Args,NPars,Core,FreeV,FunBody,
					                           G,FunVar,BFunVar,Trusted,[]);
					     	     _ -> % Caso de un make_fun
						     	{ModName,FunName,_} = get_MFA(BFunVar),
						     	get_tree_call({c_call,cerl:get_ann(Apply),
						     	            {c_literal,[],ModName},{c_literal,[],FunName},NPars},
						     	            ets:new(env, [bag]),G,Core,FreeV)
							end
					end;
				_ -> 
					{ModName,FunName,_} = get_MFA(FunVar),
			     	get_tree_call({c_call,cerl:get_ann(Apply),
			     	            {c_literal,[],ModName},{c_literal,[],FunName},NPars},
			     	             ets:new(env, [bag]),G,Core,FreeV)
			end
		end.
	
get_tree_applyFun(Args,NPars,Core,FreeV,FunBody,G,FunVar,FunName,Trusted,Env0) ->
	Env = ets:new(env_temp, [bag]),
	create_new_env(Args, NPars, Env),
	% io:format("Env0: ~p\nEnv: ~p\n",[Env0,ets:tab2list(Env)]),
	add_bindings_to_env(Env0,Env),
	CaseId = get(case_id),
	{Value,NFreeV,Roots} = get_tree(FunBody,Env,G,Core,FreeV,Trusted),
	% io:format("{Value,NFreeV,Roots}: ~p\n",[{Value,NFreeV,Roots} ]),
	IsLC = % Detecta si se trata de una list comprehension
		case FunVar of 
		  {anonymous_function,_,_,_,_,_} ->
		  	false;
		  _ ->
			  case cerl:var_name(FunVar) of
			       {FunName,_} ->
			     	  SFunName = atom_to_list(FunName), 
					  case SFunName of
						[$l,$c,$$,$^|_] -> true;
						_ -> false
					  end;
				       _ -> false
			  end
		end,
	[AValue|APars] = 
	  lists:map(fun get_abstract_form/1,[Value|NPars]),
	{AApply,FileCall,LineCall} =  
	  case FunVar of 
	     {anonymous_function,_,AFunCore,_,_,_} ->
	     	AApply_ = {call,1,get_abstract_form(FunName),APars},
			case cerl:get_ann(AFunCore) of 
				[_,Line,{file,File}] ->
					{AApply_,File,Line};
				_ ->
					{AApply_,none,1}
			end;
	     _ ->
			  case  cerl:var_name(FunVar) of
				{FunName,_} ->
					case IsLC of
					     true ->
					     	{"",none,1};
					     _ -> 
					     	case Trusted of
					     	     true -> 
					     	     	{"",none,1};
					     	     _ -> 
									{{call,1,{remote,1,
									          {atom,1,cerl:concrete(cerl:module_name(Core))},
									          {atom,1,FunName}},APars},
									 none,1}
						end
					end;
				_ -> 
					%io:format("~p\n",[cerl:get_ann(AFunCore)]),
					AApply_ = {call,1,get_abstract_form(FunName),APars},
					case FunName of 
						{anonymous_function,_,AFunCore,_,_,_} ->
							case cerl:get_ann(AFunCore) of 
								[_,Line,{file,File}] ->
									{AApply_,File,Line};
								_ ->
									{AApply_,none,1}
							end;
						_ ->
							{AApply_,none,1}
					end
		  	end
	   end,
	case AApply of 
	     "" -> 
	     	{Value,NFreeV,Roots};
	     _ -> 
			digraph:add_vertex(G,NFreeV,
			                   {erl_prettypr:format({match,1,AApply,AValue},
			                                       [{paper, 300},{ribbon, 300}]),
			                    get(CaseId),FileCall,LineCall}),
			[digraph:add_edge(G,NFreeV,Root) || Root <- Roots],
			{Value,NFreeV+1,[NFreeV]}
	end.

get_tree_call(Call,Env0,G,Core,FreeV) -> 
	ModName = cerl:concrete(bind_vars(cerl:call_module(Call),Env0)),
	FunName = cerl:concrete(bind_vars(cerl:call_name(Call),Env0)),
	FunArity = cerl:call_arity(Call),
	Args = cerl:call_args(Call),
	% io:format("FUNCTION CALL ~p:~p/~p\n",[ModName,FunName,FunArity]),
	% io:format("~p\n",[Call]),
	% io:format("~s\n",[atom_to_list(ModName) ++ ".erl"]),
	FileAdress = 
		case lists:member($/, atom_to_list(ModName)) of 
			true ->  
				atom_to_list(ModName);
			false -> 
				code:where_is_file(atom_to_list(ModName) ++ ".erl")
		end,
	% io:format("FileAdress: ~p\n", [FileAdress]), 
	% Busca el erl. Si no está, busca el beam (libreria de sistema) y tunea la ruta
	% para apuntar al ebin/ correspondiente
	NFileAdress = 
	   case FileAdress of
	        non_existing -> 
	     		NFileAdress_ = code:where_is_file(atom_to_list(ModName)++".beam"),
	     		case NFileAdress_ of
	     		     non_existing -> 
	     		     	io:format("PATHS: ~p\n",[code:get_path()]),
	     		     	throw({error,"Non existing module",ModName});
	     		     _ -> 
	     		     	RelPath = "ebin/" ++ atom_to_list(ModName) ++ ".beam",
	     		     	NRelPath = "src/" ++ atom_to_list(ModName) ++ ".erl",
	     		     	PrevPath = 
	     		     	   lists:sublist(NFileAdress_,1,
	     		     	                 length(NFileAdress_)-length(RelPath)),
	     		     	PrevPath ++ NRelPath
	     		end;
	     	_ -> FileAdress
	   end,
	LibDir = code:lib_dir(),
	Trusted = lists:prefix(LibDir, NFileAdress),
	% [FirstChar|_] = NFileAdress,
	% Trusted = 
	%    case FirstChar of
	%         $. -> false;
	%         _ -> true
	%    end,
	% io:format("Current module: ~p\n",[{cerl:concrete(cerl:module_name(Core)),Trusted}]),
	case {cerl:concrete(cerl:module_name(Core)),Trusted} of
	     {ModName,false} -> % Call de una función en el mismo módulo
	     	get_tree_apply(cerl:ann_c_apply(cerl:get_ann(Call),
	     	             cerl:c_var({FunName,FunArity}), Args),
	     	             Env0,G,Core,FreeV,false);
	     {_,false} -> % Call de una función en un módulo distinto
	     	ModCore = edd_lib:core_module(NFileAdress),
	        get_tree_apply(cerl:ann_c_apply(cerl:get_ann(Call),
	                     cerl:c_var({FunName,FunArity}),Args),
	                     Env0,G,ModCore,FreeV,false);
	     _ -> % Es trusted
	     	BArgs = lists:map(fun(Arg) -> bind_vars(Arg,Env0) end,Args),
	     	% io:format("Args: ~p\nBArgs: ~p\n",[Args, BArgs]),
	     	NoLits = 
		     	case [BArg || BArg = {anonymous_function,_,_,_,_,_} <- BArgs] of
		     	     [] ->
		     	        [BArg || BArg <- BArgs,
		     	                 not cerl:is_literal(cerl:fold_literal(BArg))];
		     	     List -> List
		     	end,
	     	case {NoLits} of
	     	    {[]} -> 
	     	     	ABArgs = 
	     	     	   [get_abstract_from_core_literal(cerl: fold_literal(BArg)) 
	     	     	    || BArg <- BArgs],
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
					AValue = cerl:abstract(Value),
					{AValue,FreeV,[]};
			     _ -> 
			        case {ModName,FunName} of
			             {'erlang','is_function'} ->
			                case BArgs of
			                     [Function = {c_call,_,{c_literal,_,erlang},
			                      {c_literal,_,make_fun},_} | MayBeArity] ->
			                       {_,_,CFunArity} = get_MFA(Function),
			                       case lists:map(fun cerl:concrete/1,MayBeArity) of
		     	                            [] ->  {{c_literal,[],true},FreeV,[]};
		     	                            [CFunArity] -> {{c_literal,[],true},FreeV,[]};
		     	                            _ -> {{c_literal,[],false},FreeV,[]}
		     	                       end;
			                     [{anonymous_function,_,{c_fun,_,AFunArgs,_},_,_,_} | MayBeArity] ->
			                       % {anonymous,{c_fun,_,AFunArgs,_},_,_,_} = 
		     	                  %         get_anon_func(EnvAF,AFunName,AFunCore),
		     	                       AFunArity = length(AFunArgs),
		     	                       case lists:map(fun cerl:concrete/1,MayBeArity) of
		     	                            [] ->  {{c_literal,[],true},FreeV,[]};
		     	                            [AFunArity] -> {{c_literal,[],true},FreeV,[]};
		     	                            _ -> {{c_literal,[],false},FreeV,[]}
		     	                       end; 
			                     _ ->
			                       {{c_literal,[],false},FreeV,[]}
			                end;
			             _ ->	
			               ModCore = edd_lib:core_module(NFileAdress),
			               get_tree_apply(cerl:ann_c_apply(cerl:get_ann(Call),
			                               cerl:c_var({FunName,FunArity}),Args),
			                     Env0,G,ModCore,
			                     FreeV,Trusted)

			        end
		        
		 end
	end.
	
get_tree_case(Expr,Env,G,Core,FreeV,Trusted) -> 
	Args = cerl:case_arg(Expr),
	{ArgsValue,NFreeV,RootsArgs} = get_tree(Args,Env,G,Core,FreeV,Trusted),
	BArgs_ = bind_vars(ArgsValue,Env),
	BArgs = 
		case BArgs_ of
			{anonymous_function,_,_,_,_,_} ->
				[BArgs_];
			_ ->
				case cerl:type(BArgs_) of
					values -> cerl:values_es(BArgs_);
					_ -> [BArgs_]
				end
		end,
    Clauses = cerl:case_clauses(Expr),
	{Clause,ClauseBody,Bindings} = get_clause_body(Clauses,BArgs,Env,G,Core,Trusted),
	put(get(case_id),get_clause_number(Clauses,Clause,1)),
	put(case_id,get(case_id)+1),
	add_bindings_to_env(Bindings,Env),
	{Value,NNFreeV,RootsValue} = get_tree(ClauseBody,Env,G,Core,NFreeV,Trusted),
	{Value,NNFreeV,RootsArgs++RootsValue}.
	
	
% 1.- Construye la 1ª cláusula sin guardas, el resto igual
% 2.- Mira si ha elegido la 1ª cláusula
% 3.- a) Si el la primera, comprueba que la guarda se cumple. Si es así, 
%        devuelve esa cláusula como la elegida. Si no, prueba con la siguiente.
% 3.- b) Si no, prueba con la siguiente
%% Explicación: cerl_clauses:reduce(), que devuelve la cláusula que se elige
%% en un case, no funciona con las guardas
get_clause_body([Clause | Clauses],BArgs,Env,G,Core,Trusted) ->
	NClause = cerl:c_clause(cerl:clause_pats(Clause), cerl:clause_body(Clause)),
	case cerl_clauses:reduce([NClause| Clauses], BArgs) of
	     {true, {{c_clause, _, _, _, ClauseBody}, Bindings}} -> 
		case cerl:clause_body(Clause) of
		     ClauseBody -> 
		     	{GuardValue,_,_} = 
		     		get_tree(apply_substitution(cerl:clause_guard(Clause),Env,Bindings),
		     		        Env,G,Core,0,Trusted),
		     	case cerl:concrete(GuardValue) of
		     	     true -> {Clause,ClauseBody,Bindings};
		     	     false -> get_clause_body(Clauses,BArgs,Env,G,Core,Trusted)
		     	end;
		     _ -> get_clause_body(Clauses,BArgs,Env,G,Core,Trusted)
		end;
	      _ -> get_clause_body(Clauses,BArgs,Env,G,Core,Trusted)
	end;
get_clause_body([],_,_,_,_,_) -> throw({error,"Non matching clause exists"}).
	
    
% Core es el CORE del módulo foo que tiene bar() -> Expr
% extract_call devuelve únicamente el CORE de Expr (o quizá del case que lo contiene)
extract_call(Core) ->
	{c_module,_,_,_,_,FunDefs} = Core,
	[{c_fun,_,_,FunBody}|_] = 
		[FunBody_ || {{c_var,_,FunName},FunBody_} <- FunDefs, FunName == {bar,0}],
	[{c_clause,_,_,_,Call}|_] = cerl:case_clauses(FunBody),
	Call.	


get_abstract_form({anonymous_function,_,_,File,Line,Env}) -> 
	get_fun_from_file(File,Line,Env);
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
		     	hd(get(funs_initial_call))
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
	% io:format("VarsOnlyInBody: ~p\n",[VarsOnlyInBody]),
	% io:format("VariablesPat: ~p\n",[sets:to_list(VariablesPat)]),
	% io:format("VariablesBody: ~p\n",[sets:to_list(VariablesBody)]),
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
	erl_syntax_lib:map(FunChangeVar,AnoFun).    	
    
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
		     	     		[{VarName,Value}|_] ->
				     			case Value of
				     	             {anonymous_function,_,_,_,_,_} -> Value;
				     	             _ -> cerl:unfold_literal(Value)
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
		    	[NHd,NTl] = 
		    	  [bind_vars(E,Env) || 
		    	   E <- [cerl:cons_hd(Expr),cerl:cons_tl(Expr)]],
		    	cerl:c_cons(NHd,NTl);
		     _ -> Expr
		 end
	 end.

get_abstract_from_core_literal(Lit) ->
	{c_literal,_,Lit_} = Lit, 
	% io:format("Lit: ~p\n",[Lit]),
	{ok,[ALit|_]} = edd_lib:parse_expr(lists:flatten(io_lib:format("~w",[Lit_])++".")),
	ALit.

get_MFA(Function) ->
	{c_call,_,{c_literal,_,erlang},{c_literal,_,make_fun},MFA} = Function,
	[{c_literal,_,ModName},{c_literal,_,FunName},{c_literal,_,FunArity}] = MFA,
	{ModName,FunName,FunArity}.
	      	
create_new_env([{c_var,_,VarName}|TailArgs],[Value|TailPars],Env) ->
	ets:insert(Env,{VarName,Value}),
	create_new_env(TailArgs,TailPars,Env);
create_new_env([],[],_) -> ok.

add_bindings_to_env([{{c_var,_,VarName},Value}| TailBindings],Env) ->
	ets:insert(Env,{VarName,Value}),
	add_bindings_to_env(TailBindings,Env);
add_bindings_to_env([{VarName,Value}| TailBindings],Env) ->
	ets:insert(Env,{VarName,Value}),
	add_bindings_to_env(TailBindings,Env);
add_bindings_to_env([],_) -> ok.
	
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
		    	    

extract_module_from_ann([_,{file,File}]) ->
	[_,_,_,_|ModName_] = lists:reverse(File),
	list_to_atom(lists:reverse(ModName_)).
	
check_errors(Value) -> 
        %possiblemente faltan mas tipos de errores.
	case Value of
	     {c_literal,[],{error,_}} -> true;
	     _ -> false
	end.
	
% get_anon_func(EnvAF,FunName,FunCore) ->
% 	get_anon_func(ets:lookup(EnvAF,FunName),FunCore).
	
% get_anon_func([{_,AF = {anonymous,FunCore,_,_,_}}|_],FunCore) -> AF;
% get_anon_func([_|AFs],FunCore) -> get_anon_func(AFs,FunCore);
% get_anon_func([],_)->{}.

get_clause_number([Clause|_],Clause,N) -> N;
get_clause_number([_|Clauses],Clause,N) -> get_clause_number(Clauses,Clause,N+1);
get_clause_number([],_,_) -> -1.

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
			     	     	     [{_,Value}|_] ->
			     	     	     	Value
			     	     	end
			     	end;
			     _ -> T
			end
		end,Expr).
