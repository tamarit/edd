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

%c(adbgc),c(edd_lib).
-module(edd).
-export([dd/1]).

%TODO
% - Fer que traga la clausula exacta que genera el error
% - Modificar els cases per a que funcionen en tots el casos i amb guardes. Evaluar la exprssio del case abans (ara considera q es un literal sempre i no es aixi).

%fer:
%
%let X = 3 in 
%   case X of
%        Z when Z>2 -> .... -> Z when Z>2 -> {1,[{Z,3}]}
%        ...

% tambe es poden evaluar abans les guardes i no donar suport a binaris <- FER ESTA


%fer una program transformation per a ficar funcions anononimes que estiguen en la mateixa linea en linees distintes. O etiquetar i buscar la fun amb la etiqueta concreta (no se si pot servir per al abstract, crec q no)
%fer que els binary funcionen al case. cerl_clauses no funciona amb binaris.
%pareix que les guardes q son crides a funcions no funcionen al cerl_clauses
%tractar de fer una transformacio del case de manera que torne la clausula evaluada y les variables dels patrons enllaçades. despres llançarlo amb smerl.
%%------------------------------------------------------------------------------
%% @doc Starts the declarative debugger 'edd' with an initial expression 'Expr'
%%      whose evaluation yields an incorrect value.
%% @end
%%------------------------------------------------------------------------------
-spec dd(Expr::string()) -> ok.
dd(Expr)->
	{ok,[AExpr|_]} = edd_lib:parse_expr(Expr++"."),
	M1 = smerl:new(foo),
	{ok, M2} = smerl:add_func(M1,"bar() ->" ++ Expr ++ " ."),
	%io:format("Forms: ~p\n",[smerl:get_forms(M2)]),
	%{ok,ModName,CoreP} = compile:forms(smerl:get_forms(M2), [to_core,binary,no_copt]),
	{ok,_,CoreP} = smerl:compile2(M2,[to_core,binary,no_copt]), %Obtiene el CORE de la expresión Expr
	InitialCall = extractCall(CoreP),
	%io:format("InitialCall: ~p\n",[InitialCall]),
	FunOperator = erl_syntax:application_operator(AExpr),
	{remote,_,{atom,_,ModName},_} = FunOperator,
	%{remote,_,_,{atom,_,FunName}} = FunOperator,
	Core = edd_lib:core_module(atom_to_list(ModName)++".erl"),
	%io:format("Core: ~p\n",[Core]),
	%PMCore = cerl_pmatch:expr(Core,rec_env:empty()),
	%io:format("PMCore: ~s\n",[cerl_prettypr:format(PMCore)]),
%	{LTree,_} = cerl_trees:label(Core,0),
%	{OutList, Outputs, Dependencies} = cerl_typean:analyze(LTree),
%	io:format("OutList: ~p\n",[OutList]),
%	io:format("Outputs: ~p\n",[dict:to_list(Outputs)]),
%	io:format("Dependencies: ~p\n",[dict:to_list(Dependencies)]),
	
	%to get the .core file. ONLY FOR DEBUGGING 
	compile:file(atom_to_list(ModName)++".erl",[to_core,no_copt]),
	
	%{ok,Abstract} = smerl:for_file(atom_to_list(ModName) ++ ".erl"),
	%smerl:compile(Abstract),
	
	%{CExpr,_} = createApply(ModName,FunName,AExpr,0),
	%io:format("CExpr: ~p\n",[CExpr]),
	
	put(case_id,0),
	G = digraph:new([acyclic]),
	{Value,FreeV,Roots} = getTree(InitialCall,ets:new(env,[bag]),G,Core,0,ets:new(env_anonymous_functions,[bag]),false),
	%Caso especial si la llamada inicial tiene funciones anidadas como f(g(3)), que
	%será traducido a CORE con un let x = g(3) in f(x)
	case cerl:type(InitialCall) of 
	     'let' ->
	     	case cerl:type(cerl:let_arg(InitialCall)) of
	     	     'call' ->
	     	     	case cerl:concrete(cerl:call_module(cerl:let_arg(InitialCall))) of
	     	     	     'erlang' -> 
	     	     	     	ok;
	     	     	     _ ->
	     	     	       	digraph:add_vertex(G,FreeV,
	     	     	                          {Expr++" = "
	     	     	                           ++io_lib:format("~p",[cerl:concrete(Value)]),
	     	     	                           get(0)}),
				[digraph:add_edge(G,FreeV,Root) || Root <- Roots]
	     	     	end;
	     	     _ -> ok
	     	end;
	     _ -> ok
	end,
%	edd_lib:dotGraphFile(G,"dbg"),
%	ok.
	edd_lib:dotGraphFile(G,"dbg"),
    	edd_lib:ask(G).


%FALTA QUE NO FAN FALTA
%alias -> Està als case, y ell al fer el select clause ja fa el binding correctament
%clause -> mai arribarà a cridarse
%module -> mai arribarà cridarse
%binary -> problema amb cerl_clauses
%bitstr -> problema amb cerl_clauses
%
%
%FALTA QUE JA SE FARAN MES TARD O NO
%primop -> tractarlo com si fora un error. Es veu afectada per el tractament de les guardes en cerl_clauses
%receive -> no s'utilitza al sequencial
% Expr: CORE de la expresión a depurar
% Env: (ETS) Mapping de variables (incluidos nombres de funciones) a valores
% G: Grafo digraph
% Core: CORE del módulo donde está la expresión a depurar
% FreeV: Próximo número a utilizar al crear un vértice
% EnvAF: Entorno con funciones anónimas
% Trusted: booleano que dice si el "módulo actual" es trusted o no
getTree(Expr,Env,G,Core,FreeV,EnvAF,Trusted) ->
	%io:format("Expr: ~p\n",[Expr]),
	case cerl:type(Expr) of
		'apply' ->
			getTreeApply(Expr,Env,G,Core,FreeV,EnvAF,Trusted);
		'case' ->
			getTreeCase(Expr,Env,G,Core,FreeV,EnvAF,Trusted);
		'let' ->
			Vars = cerl:let_vars(Expr),
			LetArg = cerl:let_arg(Expr),
			{ValueArg,NFreeV,RootsArg} = getTree(LetArg,Env,G,Core,FreeV,EnvAF,Trusted),
			case check_errors(ValueArg) of
			     true -> 
			     	{ValueArg,NFreeV,RootsArg};
			     _ -> 
				lists:map(fun(Var) -> addBindingsToEnv([{Var,ValueArg}],Env) end,Vars),
				LetBody = cerl:let_body(Expr),
				{Value,NNFreeV,Roots} = getTree(LetBody,Env,G,Core,NFreeV,EnvAF,Trusted),
				{Value,NNFreeV,RootsArg ++ Roots} 
				% Se devuelven las raices de los árboles de cómputo de cada expresión
				% en el argumento del let (RootArgs) además de las raíces de los árboles
				% de cómputo del cuerpo (RootArgs)
			end;
		'letrec' ->
			%les definicions poden gastar variables declarades abans? Si es aixi hi hauria que tractaro
			NewDefs = cerl:letrec_defs(Expr),
			NCore =	cerl:c_module(cerl:module_name(Core), 
					 	cerl:module_exports(Core),
					 	cerl:module_attrs(Core),
					 	NewDefs ++ cerl:module_defs(Core)),
			getTree(cerl:letrec_body(Expr),Env,G,NCore,FreeV,EnvAF,Trusted);
			% Genero un nuevo CORE de un módulo que es igual a 'Core' pero que tiene
			% la función declarada en el letrec y genero el arbol del cuerpo del letrec
		'call' ->
			case Expr of
			     {c_call,_,{c_literal,_,erlang},{c_literal,_,make_fun},_} ->
			     	{Expr,FreeV,[]};
			     _ -> 
			     	getTreeCall(Expr,Env,G,Core,FreeV,EnvAF)
			end;
		'fun' -> 
			[{id,{_,_,FunName}},Line,{file,File}] = cerl:get_ann(Expr),
			%io:format("\n\nVars: ~p\n\n\n",[cerl_trees:variables(Expr)]),
			NExpr = applySubstitution(Expr,Env,[]), % Sustituye las variables libres
			% de la fun por sus valores
%			cerl_trees:map(
%				fun(T)->
%					case cerl:type(T) of
%					     'var' -> 
%					     	case cerl:var_name(T) of
%					     	     {_,_} -> T;
%					     	     VN ->
%					     	     	case ets:lookup(Env,VN) of
%					     	     	     [] -> T;
%					     	     	     [{_,Value}|_] ->
%					     	     	     	Value
%					     	     	end
%					     	end;
%					     _ -> T
%					end
%				end,Expr),
			%io:format("\n\nNExpr: ~p\n\n\n",[NExpr]),
			ets:insert(EnvAF,{FunName,{anonymous,NExpr,File,Line,[]}}),%ets:tab2list(Env)
			% Guardo la función anónima en el entorno por si luego me aparece saber
			% dónde estaba
			%ets:insert(EnvAF,{FunName,{anonymous,Expr,File,Line,ets:tab2list(Env)}}),%ets:tab2list(Env)
			{{anonymous_function,FunName,NExpr},FreeV,[]};
		'cons' ->
			{[NHd,NTl],NFreeV,Roots} = 
				getTreeList([cerl:cons_hd(Expr),cerl:cons_tl(Expr)],
				            Env,G,Core,FreeV,EnvAF,Trusted),
			{cerl:c_cons(NHd,NTl),NFreeV,Roots};
		'tuple' ->
			{NExps,NFreeV,Roots} = getTreeList(cerl:tuple_es(Expr),Env,G,Core,FreeV,EnvAF,Trusted),
			{cerl:c_tuple(NExps),NFreeV,Roots};
		'try' -> 
			{ValueArg,NFreeV,ArgRoots} = getTree(cerl:try_arg(Expr),Env,G,Core,FreeV,EnvAF,Trusted),
			%io:format("Try-Value: ~p\n",[ValueArg]),
			case ValueArg of
			     {c_literal,[],{error,TypeError}} -> 
			     	addBindingsToEnv(
			     	   lists:zip(cerl:try_evars(Expr),
			     	             [cerl:abstract(Lit) || Lit <- [error,TypeError,[]]]),Env),
			     	%io:format("Env: ~p\n",[ets:tab2list(Env)]),
			     	%io:format("cerl:try_handler(Expr): ~p\n",[cerl:try_handler(Expr)]),
			     	{Value,NNFreeV,BodyRoots} = getTree(cerl:try_handler(Expr),Env,G,Core,NFreeV,EnvAF,Trusted),
			     	%io:format("Value: ~p\n",[Value]),
			     	{Value,NNFreeV,ArgRoots ++ BodyRoots};
			     _ ->
			        lists:map(fun(Var) -> 
			                    addBindingsToEnv([{Var,ValueArg}],Env) 
			                  end,cerl:try_vars(Expr)),
			     	{Value,NNFreeV,BodyRoots} = 
			     		getTree(cerl:try_body(Expr),Env,G,Core,NFreeV,EnvAF,Trusted),
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
			%io:format("EqTry: ~p\n",[EqTry]),
			getTree(EqTry,Env,G,Core,FreeV,EnvAF,Trusted);  
		'bitstr' ->
			{Value,NFreeV,Roots} = getTree(cerl:bitstr_val(Expr),Env,G,Core,FreeV,EnvAF,Trusted),
%			io:format("cerl:bitstr_size(Expr): ~p\ncerl:bitstr_bitsize(Expr): ~p\n",
%			          [cerl:bitstr_size(Expr),cerl:bitstr_bitsize(Expr)]),
			{cerl:c_bitstr(Value,cerl:bitstr_size(Expr),
			              cerl:bitstr_unit(Expr),cerl:bitstr_type(Expr),
			              cerl:bitstr_flags(Expr)),
			NFreeV,Roots};
		'binary' ->
			{NExps,NFreeV,Roots} = getTreeList(cerl:binary_segments(Expr),Env,G,Core,FreeV,EnvAF,Trusted),
			{cerl:c_binary(NExps),NFreeV,Roots};
		'seq' ->
		         {Value,NFreeV,Roots} =
		           getTree(cerl:seq_arg(Expr),Env,G,Core,FreeV,EnvAF,Trusted),
		         %io:format("VALUE: ~p\n",[Value]),
		         case check_errors(Value) of
		              true -> 
		              	{Value,NFreeV,Roots};
		              _ ->
		              	getTree(cerl:seq_body(Expr),Env,G,Core,FreeV,EnvAF,Trusted)
		         end;
		'literal' ->
			{Expr,FreeV,[]};
		'var' -> 
			{bindVars(Expr,Env),FreeV,[]};
		'values' -> 
			{bindVars(Expr,Env),FreeV,[]};
		'primop' ->
		        {{c_literal,[],{error,cerl:concrete(cerl:primop_name(Expr))}},FreeV,[]};
		_ -> throw({error,"Non treated expression",Expr}),
		     {Expr,FreeV,[]}	
	end.

getTreeApply(Apply,Env0,G,Core,FreeV,EnvAF,Trusted)->
	FunVar = cerl:apply_op(Apply),
%	io:format("Apply: ~p\nFunVar: ~p\nModule: ~p\nEnv0: ~p\nTrusted: ~p\n",[Apply,FunVar,cerl:module_name(Core),ets:tab2list(Env0),Trusted]),
	%io:format("Apply: ~p\nTrusted: ~p\n",[Apply,Trusted]),
	Pars = cerl:apply_args(Apply),
	NPars = lists:map(fun(Par) -> bindVars(Par,Env0) end,Pars),
	FunDefs = cerl:module_defs(Core),
	case cerl:var_name(FunVar) of
	     {FunName,_} ->
	     	%si este conjunt es buit entonces fer un throw diguent que la funció no esta en el modul
		case [FunBody_ || {{c_var,_,FunName_},FunBody_} <- FunDefs, 
		                  FunName_ == cerl:var_name(FunVar)] of
		     [{c_fun,_,Args,FunBody}|_] -> % apply 'd'/1 (3)
		     	getTreeApplyFun(Args,NPars,Core,EnvAF,FreeV,
		     	                FunBody,G,FunVar,FunName,Trusted,[]);
		     _ -> % Apply de ¿?
		     	getTreeCall(
		     	  cerl:ann_c_call(cerl:get_ann(Apply),
		     	              {c_literal,[],extractModuleFromAnn(cerl:get_ann(Apply))},
		     	              {c_literal,[],FunName},Pars),Env0,G,Core,FreeV,EnvAF)
		end;
	     _ -> % Apply de una variable
	     	BFunVar = bindVars(FunVar,Env0),
	     	case BFunVar of
	     	     {anonymous_function,FunName,FunCore} -> % Se enlaza a una función anónima
	     	        {anonymous,{c_fun,_,Args,FunBody},_,_,EnvAF0} = 
	     	                getAnonFunc(EnvAF,FunName,FunCore),
	           getTreeApplyFun(Args,NPars,Core,EnvAF,FreeV,FunBody,
	                           G,FunVar,FunName,Trusted,EnvAF0);
	     	     _ -> % Caso de un make_fun
		     	{ModName,FunName,_} = getMFA(BFunVar),
		     	getTreeCall({c_call,cerl:get_ann(Apply),
		     	            {c_literal,[],ModName},{c_literal,[],FunName},NPars},
		     	            ets:new(env, [bag]),G,Core,FreeV,EnvAF)
		end
	end.
	
getTreeApplyFun(Args,NPars,Core,EnvAF,FreeV,FunBody,G,FunVar,FunName,Trusted,Env0) ->
	%Comprovar que no genere problemes el fet de tindre tables anidades amb el mateix nom
	Env = ets:new(env_temp, [bag]),
	createNewEnv(Args, NPars, Env),
	addBindingsToEnv(Env0,Env),
	CaseId = get(case_id),
	{Value,NFreeV,Roots} = getTree(FunBody,Env,G,Core,FreeV,EnvAF,Trusted),
	IsLC = % Detecta si se trata de una list comprehension
	  case cerl:var_name(FunVar) of
	       {FunName,_} ->
	     	  SFunName = atom_to_list(FunName), 
		  case SFunName of
			[$l,$c,$$,$^|_] -> true;
			_ -> false
		  end;
	       _ -> false
	  end,
%	io:format("Env: ~p\n",[ets:tab2list(Env)]),
%	io:format("FunBody: ~p\n",[FunBody]),
%	io:format("[Value|NPars]: ~p\n",[[Value|NPars]]),
%	io:format("Trusted: ~p\n",[Trusted]),
	[AValue|APars] = 
	  lists:map(
	          fun ({anonymous_function,FunName_,FunCore}) -> 
	          	 %io:format("FunName_: ~p\n",[FunName_]),
			 {anonymous,_,File,Line,_} = getAnonFunc(EnvAF,FunName_,FunCore),
			 getFunFromAbstract(File,Line);
	              (Par_) -> 
	                Par = cerl:fold_literal(Par_),
	          	case cerl:is_literal(Par) of
	          	     true -> 
		    		getAbstractFromCoreLiteral(Par);
			      _ -> 
			      	{ModName_,FunName_,FunArity_} = getMFA(Par),
			      	{'fun',1,
			      	   {function,
			      	      {atom,1,ModName_},
			      	      {atom,1,FunName_},
			      	      {integer,1,FunArity_}}}
			 end
		  end,[Value|NPars]),
	%io:format("cerl:var_name(FunVar): ~p\n",[cerl:var_name(FunVar)]),
	AApply =  
	  case  cerl:var_name(FunVar) of
		{FunName,_} ->
			case IsLC of
			     true ->
			     	"";
			     _ -> 
			     	case Trusted of
			     	     true -> "";
			     	     _ -> 
					{call,1,{remote,1,
					          {atom,1,cerl:concrete(cerl:module_name(Core))},
					          {atom,1,FunName}},APars}
				end
			end;
		_ -> 
			""
%			[{_,{anonymous,_,File,Line}}] = ets:lookup(EnvAF,FunName),
%			{call,1,getFunFromAbstract(File,Line),APars}
	  end,
	%io:format("Vertex's label: ~s\n",[VLabel]),
	case AApply of 
	     "" -> {Value,NFreeV,Roots};
	     _ -> 
		digraph:add_vertex(G,NFreeV,
		                   {erl_prettypr:format({match,1,AApply,AValue},
		                                       [{paper, 300},{ribbon, 300}]),
		                    get(CaseId)}),
		[digraph:add_edge(G,NFreeV,Root) || Root <- Roots],
		{Value,NFreeV+1,[NFreeV]}
	end.

getTreeCall(Call,Env0,G,Core,FreeV,EnvAF) -> 
	ModName = cerl:concrete(bindVars(cerl:call_module(Call),Env0)),
	FunName = cerl:concrete(bindVars(cerl:call_name(Call),Env0)),
	FunArity = cerl:call_arity(Call),
	Args = cerl:call_args(Call),
%	io:format("FUNCTION CALL ~p:~p/~p\n",[ModName,FunName,FunArity]),
%	io:format("~p\n",[Call]),
	FileAdress = code:where_is_file(atom_to_list(ModName)++".erl"),
	% Busca el erl. Si no está, busca el beam (libreria de sistema) y tunea la ruta
	% para apuntar al ebin/ correspondiente
	NFileAdress = 
	   case FileAdress of
	        non_existing -> 
	     		NFileAdress_ = code:where_is_file(atom_to_list(ModName)++".beam"),
	     		case NFileAdress_ of
	     		     non_existing -> 
	     		     	throw({error,"Non existing module",ModName});
	     		     _ -> 
	     		     	RelPath = "ebin/" ++ atom_to_list(ModName) ++ ".beam",
	     		     	NRelPath = "src/" ++ atom_to_list(ModName) ++ ".erl",
	     		     	PrevPath = lists:sublist(NFileAdress_,1,length(NFileAdress_)-length(RelPath)),
	     		     	PrevPath ++ NRelPath
	     		end;
	     	_ -> FileAdress
	   end,
	[FirstChar|_] = NFileAdress,
	Trusted = 
	   case FirstChar of
	        $. -> false;
	        _ -> true
	   end,
	%io:format("~p\n~p\n",[NFileAdress,Trusted]),
	case {cerl:module_name(Core),Trusted} of
	     {ModName,false} -> % Call de una función en el mismo módulo
	     	getTreeApply(cerl:ann_c_apply(cerl:get_ann(Call),cerl:c_var({FunName,FunArity}), Args),Env0,G,Core,FreeV,EnvAF,false);
	     {_,false} -> % Call de una función en un módulo distinto
	     	ModCore = edd_lib:core_module(NFileAdress),
	        getTreeApply(cerl:ann_c_apply(cerl:get_ann(Call),cerl:c_var({FunName,FunArity}), Args),Env0,G,ModCore,FreeV,EnvAF,false);
	     _ -> % Es trusted
	     	BArgs = lists:map(fun(Arg) -> bindVars(Arg,Env0) end,Args),
	     	%io:format("BArgs: ~p\n",[BArgs]),
	     	NoLits = 
		     	case [BArg || BArg = {anonymous_function,_,_} <- BArgs] of
		     	     [] ->
		     	        [BArg || BArg <- BArgs,
		     	                 not cerl:is_literal(cerl:fold_literal(BArg))];
		     	     List -> List
		     	end,
	     	%io:format("NoLits: ~p\n",[NoLits]),
	     	case {NoLits} of
	     	     {[]} -> 
	     	     	ABArgs = [getAbstractFromCoreLiteral(cerl: fold_literal(BArg)) || BArg <- BArgs],
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
%				   {value,Value_,_} = 
%				       erl_eval:expr({call,1,{remote,1,{atom,1,ModName},
%				                             {atom,1,FunName}},ABArgs},[]),
%			            Value_
				catch
				   Exception:Reason -> {Exception,Reason}
				end,
			AValue = cerl:abstract(Value),
			%io:format("AValue: ~p\n\n",[AValue]),
			{AValue,FreeV,[]};
		     _ -> 
		        case {ModName,FunName} of
		             {'erlang','is_function'} ->
		                case BArgs of
		                     [Function = {c_call,_,{c_literal,_,erlang},
		                      {c_literal,_,make_fun},_} | MayBeArity] ->
		                       {_,_,CFunArity} = getMFA(Function),
		                       case lists:map(fun cerl:concrete/1,MayBeArity) of
	     	                            [] ->  {{c_literal,[],true},FreeV,[]};
	     	                            [CFunArity] -> {{c_literal,[],true},FreeV,[]};
	     	                            _ -> {{c_literal,[],false},FreeV,[]}
	     	                       end;
		                     [{anonymous_function,AFunName,AFunCore} | MayBeArity] ->
		                       {anonymous,{c_fun,_,AFunArgs,_},_,_,_} = 
	     	                          getAnonFunc(EnvAF,AFunName,AFunCore),
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
		               getTreeApply(cerl:ann_c_apply(cerl:get_ann(Call),
		                               cerl:c_var({FunName,FunArity}),Args),
		                     Env0,G,ModCore,
		                     FreeV,EnvAF,Trusted)
		        end
		        
		 end
	end.
	
getTreeCase(Expr,Env,G,Core,FreeV,EnvAF,Trusted) -> 
	Args = cerl:case_arg(Expr),
	{ArgsValue,NFreeV,RootsArgs} = getTree(Args,Env,G,Core,FreeV,EnvAF,Trusted),
	%io:format("Args: ~p\nEnv: ~p\n",[Args,ets:tab2list(Env)]),
	BArgs_ = bindVars(ArgsValue,Env),
	BArgs = 
		case cerl:type(BArgs_) of
			values -> cerl:values_es(BArgs_);
			_ -> [BArgs_]
		end,
        Clauses = cerl:case_clauses(Expr),
	{Clause,ClauseBody,Bindings} = getClauseBody(Clauses,BArgs,Env,G,Core,EnvAF,Trusted),
	put(get(case_id),getClauseNumber(Clauses,Clause,1)),
	put(case_id,get(case_id)+1),
	%io:format("Bindings: ~p\n",[Bindings]),
	addBindingsToEnv(Bindings,Env),
	{Value,NNFreeV,RootsValue} = getTree(ClauseBody,Env,G,Core,NFreeV,EnvAF,Trusted),
	{Value,NNFreeV,RootsArgs++RootsValue}.
	
getTreeList([E|Es],Env,G,Core,FreeV,EnvAF,Trusted) ->
	{NE,NFreeV,RootsH} = getTree(bindVars(E,Env),Env,G,Core,FreeV,EnvAF,Trusted),
	{NEs,NNFreeV,RootsT} = getTreeList(Es,Env,G,Core,NFreeV,EnvAF,Trusted),
	{[NE|NEs],NNFreeV,RootsH++RootsT};
getTreeList([],_,_,_,FreeV,_,_) ->
	{[],FreeV,[]}.
	
% 1.- Construye la 1ª cláusula sin guardas, el resto igual
% 2.- Mira si ha elegido la 1ª cláusula
% 3.- a) Si el la primera, comprueba que la guarda se cumple. Si es así, 
%        devuelve esa cláusula como la elegida. Si no, prueba con la siguiente.
% 3.- b) Si no, prueba con la siguiente
%% Explicación: cerl_clauses:reduce(), que devuelve la cláusula que se elige
%% en un case, no funciona con las guardas
getClauseBody([Clause | Clauses],BArgs,Env,G,Core,EnvAF,Trusted) ->
	NClause = cerl:c_clause(cerl:clause_pats(Clause), cerl:clause_body(Clause)),
	case cerl_clauses:reduce([NClause| Clauses], BArgs) of
	     {true, {{c_clause, _, _, _, ClauseBody}, Bindings}} -> 
		case cerl:clause_body(Clause) of
		     ClauseBody -> 
		        %io:format("cerl:clause_guard(Clause): ~p\n",[cerl:clause_guard(Clause)]),
		     	{GuardValue,_,_} = 
		     		getTree(applySubstitution(cerl:clause_guard(Clause),Env,Bindings),
		     		        Env,G,Core,0,EnvAF,Trusted),
		    	%io:format("GuardValue: ~p\n",[cerl:concrete(GuardValue)]),
		     	case cerl:concrete(GuardValue) of
		     	     true -> {Clause,ClauseBody,Bindings};
		     	     false -> getClauseBody(Clauses,BArgs,Env,G,Core,EnvAF,Trusted)
		     	end;
		     _ -> getClauseBody(Clauses,BArgs,Env,G,Core,EnvAF,Trusted)
		end;
	      _ -> getClauseBody(Clauses,BArgs,Env,G,Core,EnvAF,Trusted)
	end;
getClauseBody([],_,_,_,_,_,_) -> throw({error,"Non matching clause exists"}).
	
    
% Core es el CORE del módulo foo que tiene bar() -> Expr
% extractCall devuelve únicamente el CORE de Expr (o quizá del case que lo contiene)
extractCall(Core) ->
	{c_module,_,_,_,_,FunDefs} = Core,
	[{c_fun,_,_,FunBody}|_] = 
		[FunBody_ || {{c_var,_,FunName},FunBody_} <- FunDefs, FunName == {bar,0}],
	[{c_clause,_,_,_,Call}|_] = cerl:case_clauses(FunBody),
	Call.	     	
    
bindVars(Expr,Env) ->
	%io:format("Expr: ~p\nEnv: ~p\n",[Expr,ets:tab2list(Env)]),
	case Expr of
	     {anonymous_function,_,_} -> Expr;
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
		     		{VarName,Value} = hd(ets:lookup(Env,VarName)),
		     		case Value of
		     	             {anonymous_function,_,_} -> Value;
		     	             _ -> cerl:unfold_literal(Value)
		     	        end
		     	end;
		     'values' -> 
		     	Values = cerl:values_es(Expr),
		     	BValues = lists:map(fun(Var) -> bindVars(Var,Env) end,Values),
		     	{c_values,cerl:get_ann(Expr),BValues};
		    'tuple' -> 
		    	NExps = [bindVars(E,Env) || E <- cerl:tuple_es(Expr)],
		    	cerl:c_tuple(NExps);
		    'cons' -> 
		    	[NHd,NTl] = [bindVars(E,Env) || E <- [cerl:cons_hd(Expr),cerl:cons_tl(Expr)]],
		    	cerl:c_cons(NHd,NTl);
		     _ -> Expr
		 end
	 end.

getAbstractFromCoreLiteral(Lit) ->
	{c_literal,_,Lit_} = Lit, 
	{ok,[ALit|_]} = edd_lib:parse_expr(lists:flatten(io_lib:format("~w",[Lit_])++".")),
	ALit.

getMFA(Function) ->
	%io:format("Function: ~p",[Function]),
	{c_call,_,{c_literal,_,erlang},{c_literal,_,make_fun},MFA} = Function,
	[{c_literal,_,ModName},{c_literal,_,FunName},{c_literal,_,FunArity}] = MFA,
	{ModName,FunName,FunArity}.
	      	
createNewEnv([{c_var,_,VarName}|TailArgs],[Value|TailPars],Env) ->
	ets:insert(Env,{VarName,Value}),
	createNewEnv(TailArgs,TailPars,Env);
createNewEnv([],[],_) -> ok.

addBindingsToEnv([{{c_var,_,VarName},Value}| TailBindings],Env) ->
	ets:insert(Env,{VarName,Value}),
	addBindingsToEnv(TailBindings,Env);
addBindingsToEnv([{VarName,Value}| TailBindings],Env) ->
	ets:insert(Env,{VarName,Value}),
	addBindingsToEnv(TailBindings,Env);
addBindingsToEnv([],_) -> ok.

getFunFromAbstract(File,Line) -> 
	{ok,Abstract} = smerl:for_file(File),
	[AFun|_] =
	     lists:flatten(
		[erl_syntax_lib:fold(fun(Tree,Acc) -> 
					%io:format("{Line,Tree}: ~p\n",[{Line,Tree}]),
		               		case Tree of 
		               	    	     {'fun',Line,{clauses,_}}->
		               	    		[Tree|Acc];
		               	    	     _ -> 
		               	    		Acc
		               		end
			    	     end, [], Form)||Form<-smerl:get_forms(Abstract)]),		    
	AFun.

extractModuleFromAnn([_,{file,File}]) ->
	[_,_,_,_|ModName_] = lists:reverse(File),
	list_to_atom(lists:reverse(ModName_)).
	
check_errors(Value) -> 
         	%possiblement falten mes tipos d'errors.
	case Value of
	     {c_literal,[],{error,_}} -> true;
	     _ -> false
	end.
	
getAnonFunc(EnvAF,FunName,FunCore) ->
	getAnonFunc(ets:lookup(EnvAF,FunName),FunCore).
	
getAnonFunc([{_,AF = {anonymous,FunCore,_,_,_}}|_],FunCore) -> AF;
getAnonFunc([_|AFs],FunCore) -> getAnonFunc(AFs,FunCore);
getAnonFunc([],_)->{}.

getClauseNumber([Clause|_],Clause,N) -> N;
getClauseNumber([_|Clauses],Clause,N) -> getClauseNumber(Clauses,Clause,N+1);
getClauseNumber([],_,_) -> -1.

applySubstitution(Expr,Env,Bindings) ->
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

%createApply(_,FunName,AExpr,FVar) ->
%	{Args, NFVar, Context} = createArgs(erl_syntax:application_arguments(AExpr),FVar,no_context),
%	Apply = 
%		{c_apply,[],{c_var,[],{FunName,length(erl_syntax:application_arguments(AExpr))}},Args},
%	case Context of
%             {c_let,LetAnn,LetVars,LetArg,_} -> 
%		    {{c_let,LetAnn,LetVars,LetArg,Apply},NFVar};
%	     _ -> {Apply, NFVar}
%	end.
%
%createArgs([Arg|TailArgs],FVar,Context) ->
%	LetVar = {c_var,[],list_to_atom("Cor"++integer_to_list(FVar))},
%	{LetExpr,NFVar} =  
%	  case Arg of
%	     	{call,_,{remote,_,{atom,_,ModName},{atom,_,FunName}},_} ->
%	     	     {Apply,NFVar_} = createApply(ModName,FunName,Arg,FVar+1),
%	     	     {{c_let,[],[LetVar],Apply,-1},NFVar_};
%	     	_ -> {{c_let,[],[LetVar],cerl:abstract(erl_syntax:concrete(Arg)),-1},FVar+1}
%	   end,
%	 NContext = 
%	   case Context of
%		 {c_let,LetAnn,LetVars,LetArg,_} -> 
%		    {c_let,LetAnn,LetVars,LetArg,LetExpr};
%		  _ -> LetExpr
%	   end,
%	{NTailArgs, NNFVar, NNContext} = createArgs(TailArgs,NFVar,NContext),
%	{[LetVar | NTailArgs], NNFVar, NNContext};
%createArgs([], FVar, Context) -> {[], FVar, Context}.
