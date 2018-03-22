%%%    Copyright (C) 2013 Salvador Tamarit <stamarit@dsic.upv.es>
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
%%% @doc Erlang Declarative Debugger instrumentation for concurrent tracing
%%% @end
%%%-----------------------------------------------------------------------------

-module(edd_con_pt).

-export([parse_transform/2]).

% TODO: Treat correctly errors to be considered as a value


parse_transform(Forms, Opts) ->
	% io:format("Opts: ~p\n", [Opts]),
	put(modules_to_instrument, hd([InsMod0 || {inst_mod, InsMod0} <- Opts])),
	% io:format("Trans: ~p\n", [hd(Forms)]),
	put(free, 0),
	ModFileName = 
		lists:sort(
			lists:flatten(
				[
					erl_syntax_lib:fold(
						fun get_module_filename/2,
						[],
						Form)
				|| Form <- Forms])), 
	NForms =
		[
			erl_syntax_lib:map(
				 fun(CForm) -> 
				 	inst_fun(CForm, ModFileName) 
				 end,
				 Form) 
		|| Form <- Forms],
	% StrRevertedNForms = 
	% 	[ try 
	% 		erl_pp:form(Form)
	% 	 catch 
	% 	 _:_ -> 
	% 	 	try
	% 	 		erl_pp:function(Form)
	% 	 	catch 
	% 	 	_:_ ->
	% 	 		"error" ++ lists:flatten(io_lib:format("~p", [Form]))
	% 	 	end
	% 	 end || Form <- erl_syntax:revert_forms(NForms)],
	% [io:format("~s\n", [StrForm]) || StrForm <- StrRevertedNForms],
	erl_syntax:revert_forms(NForms).

get_module_filename(T, Acc) ->
	case erl_syntax:type(T) of 
		attribute -> 
			NameAttr = 
				erl_syntax:attribute_name(T),
			case erl_syntax:type(NameAttr) of
				atom ->  
					% io:format("~p\n", [erl_syntax:atom_value(NameAttr)]),
					case erl_syntax:atom_value(NameAttr) of
						% file -> 
						% 	[{file_name, hd(erl_syntax:attribute_arguments(T))} | Acc];
						module -> 
							ModName = 
								hd(erl_syntax:attribute_arguments(T)),
							[{file_name, erl_syntax:string(atom_to_list(erl_syntax:atom_value(ModName)) ++ ".erl") },
							 {module_name, ModName} | Acc];
						_ ->
							Acc
					end;
				_ ->
					Acc
			end;
		_ -> 
			Acc
	end.

inst_fun(T, ModFileName) ->
	case erl_syntax:type(T) of 
		function -> 
			NTAnn = 
				erl_syntax_lib:annotate_bindings(T,ordsets:new()),
			NTAnn2 = 
				erl_syntax_lib:map(
					fun(CT) -> 
						annotate_info_tree(CT,ModFileName) 
					end,
					NTAnn), 
			% io:format("~p\n", [NTAnn2]),
			Clauses = erl_syntax:function_clauses(NTAnn2),
			NClauses = inst_fun_clauses(Clauses, erl_syntax:function_name(T)),
			% NClauses = Clauses,
			erl_syntax:function(erl_syntax:function_name(T), NClauses);
			 % erl_syntax:function(erl_syntax:function_name(T), Clauses);
		% attribute -> 
		% 	NameAttr = 
		% 		erl_syntax:attribute_name(T),
		% 	case erl_syntax:type(NameAttr) of
		% 		atom ->  
		% 			case erl_syntax:atom_value(NameAttr) of
		% 				file -> 
		% 					% FileName = 
		% 					% 	erl_syntax:string_literal(
		% 					% 		hd(erl_syntax:attribute_arguments(T0))),
		% 					% io:format("~s\n",[FileName]),
		% 					put(file_name, hd(erl_syntax:attribute_arguments(T))),
		% 					T;
		% 				module -> 
		% 					% io:format("~p: ~p\n", [self(),erl_syntax:attribute_arguments(T0)]),
		% 					put(module_name, hd(erl_syntax:attribute_arguments(T))),
		% 					T;
		% 				_ ->
		% 					T
		% 			end;
		% 		_ ->
		% 			T
		% 	end,
		% 	T;
		_ -> 
			T
	end.

inst_expr(T) ->
	% io:format("~p\n", [erl_syntax:revert(T)]),
	% io:format("~p\n", [erl_syntax:type(T)]),
	NT = 
		case erl_syntax:type(T) of 
			receive_expr ->
				Clauses = erl_syntax:receive_expr_clauses(T),
				{NClauses,_} = 
					lists:mapfoldl(
						fun inst_receive_clause/2, 
						1,
						Clauses),
				% [io:format("~p\n", [erl_syntax:revert(NClause)]) || NClause <- NClauses ],
				NReceive = 
					erl_syntax:receive_expr(
						NClauses, 
						erl_syntax:receive_expr_timeout(T), 
						erl_syntax:receive_expr_action(T)),
				VarsContext = 
					get_ann_info(env, T),
				Context = 
					build_dict_var(VarsContext),
				% io:format("PP: ~p\n", [erl_syntax:tuple(pos_and_pp(T))]),
				SendReceive = 
						build_send_trace(
							receive_reached, 
							[Context,
							erl_syntax:tuple(pos_and_pp(T))]), 
							% pos_and_pp(T)),
				BlockReceive = 
					erl_syntax:block_expr([SendReceive, NReceive]),
				% io:format("~p\n",[BlockReceive]),
				BlockReceive;
			infix_expr -> 
				case erl_syntax:operator_name(erl_syntax:infix_expr_operator(T)) of 
					'!' ->
						inst_send(T, 
							[erl_syntax:infix_expr_left(T),
							 erl_syntax:infix_expr_right(T)]);
					_ ->
						T 
				end;
			application ->
				AppOper = 
					erl_syntax:application_operator(T),
				% io:format("~p\n", [AppOper]),
				NApp = 
					case erl_syntax:type(AppOper) of 
						% {_,_} ->
						% 	T;
						atom ->
							case erl_syntax:atom_value(AppOper)  of 
								spawn ->
									inst_spawn(T, erl_syntax:application_arguments(T));
								spawn_link ->
									inst_spawn(T, erl_syntax:application_arguments(T));
								spawn_monitor ->
									inst_spawn(T, erl_syntax:application_arguments(T));
								spawn_opt ->
									inst_spawn(T, erl_syntax:application_arguments(T));
								apply ->
									[ModName, _, _] = 
										erl_syntax:application_arguments(T),
									inst_call_loading(T, ModName);
								_ ->
									T
									% case lists:member(Other, ?BIFS) of 
									% 	true -> 
									% 		T;
									% 	false -> 
									% 		inst_call(T, erl_syntax:application_arguments(T))
									% end
							end;
						% {var,_,Name} ->
						% 	inst_spawn(T, erl_syntax:application_arguments(T));
						% _ -> 
						% 	case erl_syntax:type(AppOper) of 
						module_qualifier -> 
							ModName = 
								erl_syntax:module_qualifier_argument(AppOper),
							FunName = 
								erl_syntax:module_qualifier_body(AppOper),
							% try 
							% 	io:format("~p\n", [{erl_syntax:atom_value(ModName), erl_syntax:atom_value(FunName)}])
							% catch
							% 	_:_ ->
							% 		ok
							% end,
							try 
								case {erl_syntax:atom_value(ModName), erl_syntax:atom_value(FunName)} of 
									{erlang, send} ->
										inst_send(T, erl_syntax:application_arguments(T));
									{erlang, spawn} ->
										inst_spawn(T, erl_syntax:application_arguments(T));
									{erlang, spawn_link} ->
										inst_spawn(T, erl_syntax:application_arguments(T));
									{erlang, spawn_monitor} ->
										inst_spawn(T, erl_syntax:application_arguments(T));
									{erlang, spawn_opt} ->
										inst_spawn(T, erl_syntax:application_arguments(T));
									% {var,_,_} ->
									% 	T;
									_ ->
										% ModNameStr = erl_syntax:atom_literal(ModName),
										% {[VarCall], [StoreCall]} = 
										% 	args_assign("EDDCallResult", [T]),
										% io:format("~p\n", [{ModNameStr, FunName}]),
										inst_call_loading(T, ModName)
									% _ -> 
									% 	T
								end
							catch
								_:_ ->
									inst_call_loading(T, ModName)
							end;
						% _ ->
						% 	T
					% end;
						variable -> 
							T;
						fun_expr -> 
							T;
						implicit_fun -> 
							T;
						_ ->
							T
					end,		
				NApp;
			% macro ->
			% 	io:format("~p\n",[T]);
			fun_expr -> 
				try
					inst_fun_expr(T)
				catch
					_:_ ->
						T
				end;
			_ ->
				T
		end,
	Res = erl_syntax:set_ann(NT, erl_syntax:get_ann(T)),
	Res. 

inst_call_loading(T, ModName) ->
	erl_syntax:case_expr(
		erl_syntax:application(
			erl_syntax:atom(code), 
			erl_syntax:atom(where_is_file), 
			[erl_syntax:infix_expr(
				erl_syntax:application(
					erl_syntax:atom(erlang),
					erl_syntax:atom(atom_to_list),
					[ModName]), 
				erl_syntax:operator("++"), 
				erl_syntax:string(".erl"))]),
			% [erl_syntax:string(ModNameStr ++ ".erl")]),
		[
			%TODO: try to load also from src directory
			erl_syntax:clause(
				[erl_syntax:cons(
					erl_syntax:char($.), 
					erl_syntax:underscore())] ,
				[],
				[	
					% erl_syntax:application(
					% 	erl_syntax:atom(io) , 
					% 	erl_syntax:atom(format), 
					% 	[erl_syntax:string("entra desde " ++ erl_syntax:atom_literal(get(module_name)) ++ " : " ++ ModNameStr ++ " " ++ erl_syntax:atom_literal(FunName) ++ "\n")]),
					% erl_syntax:application(
					% 	erl_syntax:atom(edd_trace_new) , 
					% 	erl_syntax:atom(compile_and_reload), 
					% 	[ModName]),
					build_send_load(ModName),
					build_receive_load(),
					T
					% StoreCall,
					% erl_syntax:application(
					% 	erl_syntax:atom(edd_trace_new) , 
					% 	erl_syntax:atom(undo_compile_and_reload), 
					% 	[ModName]),
					% VarCall
				]),
			erl_syntax:clause(
				[erl_syntax:underscore()],
				[],
				[	
					erl_syntax:case_expr(
						erl_syntax:application(
							erl_syntax:atom(lists), 
							erl_syntax:atom(member), 
							[ModName,
							 lists_with_modules_to_instument()]),
						[
							erl_syntax:clause(
								[erl_syntax:atom(true)] ,
								[],
								[	
									% erl_syntax:application(
									% 	erl_syntax:atom(io) , 
									% 	erl_syntax:atom(format), 
									% 	[erl_syntax:application(
									% 			erl_syntax:atom(erlang),
									% 			erl_syntax:atom(atom_to_list),
									% 			[ModName])]),
									build_send_load(ModName),
									build_receive_load(),
									T
								]),
							erl_syntax:clause(
								[erl_syntax:atom(false)] ,
								[],
								[	
									T
								])
						])
				])
		]).

inst_fun_clauses(Clauses, FunId) ->
	% T = erl_syntax_lib:mapfold(fun annotate_vars/2,T0),
	% NClauses0 = 
	% 	[
	% 	begin
	% 		NBody = 
	% 			element(1, 
	% 				lists:mapfoldl(
	% 					fun ann_expr/2,
	% 					vars_patterns(erl_syntax:clause_patterns(Clause)),
	% 					erl_syntax:clause_body(Clause))),
	% 		erl_syntax:clause(
	% 			erl_syntax:clause_patterns(Clause), 
	% 			erl_syntax:clause_guard(Clause), 
	% 			NBody)
	% 	end
	% 	|| Clause <- Clauses],
	% io:format("~p\n", [erl_syntax:atom_literal(erl_syntax:function_name(T))]),
	[
		begin
			NBody0 = 
				erl_syntax:clause_body(
					erl_syntax_lib:map(
						fun inst_expr/1, Clause )),
			% io:format("~p: ~p\n", [self(),get(module_name)]),
			{ParValues, NPars} = 
				args_assign("EDDFunPar", erl_syntax:clause_patterns(Clause)),
			VarsContextStart = 
				get_ann_info(env, hd(erl_syntax:clause_body(Clause))),
			ContextStart = 
				build_dict_var(VarsContextStart),
			CallRep = 
					[get_ann_info(module_name, Clause),
					 FunId] 
					 % ), 
					 ++ [ erl_syntax:list(ParValues)], 
			SendCallStart = 
				build_send_trace(start_call, [erl_syntax:tuple(CallRep), ContextStart] ++ pos_and_pp(Clause)), 
			LastExpr = 
				lists:last(NBody0),
			BodyWOLast =
				lists:droplast(NBody0),
			VarFunResult = free_named_var("EDDResultFun"),
			VarsContextEnd = 
				get_ann_info(env, lists:last(erl_syntax:clause_body(Clause))),
			ContextEnd = 
				build_dict_var(VarsContextEnd),
			NLastExpr = 
				erl_syntax:match_expr(
					VarFunResult, 
					LastExpr),
			SendResult =
				build_send_trace(end_call, [erl_syntax:tuple(CallRep), VarFunResult, ContextEnd]), 
			NOldBody = 
				BodyWOLast ++ [NLastExpr, SendResult, VarFunResult],
			% Salida = 
			% 	erl_syntax:application(
			% 		erl_syntax:atom(io) , 
			% 		erl_syntax:atom(format), 
			% 		[erl_syntax:string("entra " ++ erl_syntax:atom_literal(get(module_name)) ++ " " ++ erl_syntax:atom_literal(erl_syntax:function_name(T)) ++ "\n")]),
			NBody = 
				[SendCallStart | NOldBody],
		 	erl_syntax:clause(
					NPars, 
					erl_syntax:clause_guard(Clause), 
					NBody)
		 end
	 || Clause <- Clauses].

inst_fun_expr(T) ->
			% 
			%  erl_syntax:function(erl_syntax:function_name(T), NClauses),
	% io:format("B1: ~p\n", [erl_syntax:fun_expr_clauses(T)]),
	Clauses = erl_syntax:fun_expr_clauses(T),
	NClauses = inst_fun_clauses(Clauses, hd(pos_and_pp(T))),
	NFunExpr = erl_syntax:fun_expr(NClauses),

	{[VarFun], [StoreFun]} = 
		args_assign("EDDFunResult", [NFunExpr]),

	SendFunInfo = 
		build_store_fun(VarFun, hd(pos_and_pp(T))),	

	BlockFun = 
		erl_syntax:block_expr([StoreFun, SendFunInfo, VarFun]),

	BlockFun.

inst_send(T, SendArgs) ->

	{VarArgs, StoreArgs} = 
		args_assign("EDDSendArg", SendArgs),

	VarArgsToSend = 
		[
			erl_syntax:case_expr(
				erl_syntax:application(
					erl_syntax:atom(erlang),
					erl_syntax:atom(is_atom),
					[hd(VarArgs)]),
				[
					erl_syntax:clause(
						[erl_syntax:atom(true)],
						[],
						[erl_syntax:application(
							erl_syntax:atom(erlang),
							erl_syntax:atom(whereis),
							[hd(VarArgs)])]),
					erl_syntax:clause(
						[erl_syntax:atom(false)],
						[],
						[hd(VarArgs)])
				])
			|
			tl(VarArgs)
		],

	SendSend = 
		build_send_trace(
			send_sent, 
			VarArgsToSend ++ pos_and_pp(T)), 	

	NT = 
		build_send_par(
			hd(VarArgs),
			tl(VarArgs)),
		% erl_syntax:infix_expr(
		% 	lists:nth(1, VarArgs), 
		% 	erl_syntax:operator('!'), 
		% 	lists:nth(2, VarArgs)),

	BlockSend = 
		erl_syntax:block_expr(StoreArgs ++ [NT, SendSend, lists:nth(2, VarArgs)]),

	BlockSend.

inst_spawn(T, SpawnArgs) ->
	% TODO: Be careful with this approach. Could not work for spwan with only one arg, i.e. spawn(fun() -> ... )
	% Maybe solved correcting SpwancCall or removing previous variables bindings for that case
	% io:format("INSTRUMENT spawn ~p\n", [erl_syntax:application_operator(T)]),
	VarReceiveResult = 
		free_named_var("EDDSpawnResult"),
	{VarArgs, StoreArgs} = 
		args_assign("EDDSpawnArg", SpawnArgs),

	SendSpawn = 
		build_send_trace(made_spawn, [erl_syntax:tuple(VarArgs) ,VarReceiveResult] ++ pos_and_pp(T)), 

	SpawnCall = 
		erl_syntax:application(erl_syntax:application_operator(T), VarArgs), 
	NT = 
		erl_syntax:match_expr(VarReceiveResult, SpawnCall), 

	BlockSpawn = 
		erl_syntax:block_expr(StoreArgs ++ [NT, SendSpawn, VarReceiveResult]),
	BlockSpawn.


inst_receive_clause(Clause, CurrentClause) ->
	{ [VarMsg], Patterns} = 
		args_assign("EDDMsg", erl_syntax:clause_patterns(Clause)),
	% io:format("~p\n", [hd(erl_syntax:clause_body(Clause))]),
	% io:format("~p\n", [erl_syntax:get_ann(
	% 		hd(erl_syntax:clause_body(Clause))) ]),
	% io:format("~p\n", [hd(erl_syntax:get_ann(
	% 		hd(erl_syntax:clause_body(Clause))) )]),
	% io:format("~p\n", [hd(Patterns)]),
	VarsContextStart = 
		get_ann_info(env, hd(erl_syntax:clause_body(Clause))),
	VarsBindings = 
		get_ann_info(bound, hd(erl_syntax:clause_patterns(Clause))),
		% case erl_syntax:get_ann(
		% 	hd(erl_syntax:clause_body(Clause))) of 
		% 	[] -> 
		% 		[];
		% 	[H|_] ->
		% 		H 
		% end,
	% io:format("~p\n", [Vars]),
	% SendContext =
	% 	build_send_trace(
	% 		receive_context, 
	% 		erl_syntax_zip(
 % 				[erl_syntax:string(V) || V <- Vars],
 % 				[erl_syntax:variable(V)  || V <- Vars])  ), 
	ContextStart = 
		build_dict_var(VarsContextStart),
	Bindings = 
		build_dict_var(VarsBindings),

	SendEvaluated =
		build_send_trace(
			receive_evaluated, 
			[VarMsg, ContextStart, Bindings, erl_syntax:integer(CurrentClause)]), 

	LastExpr = 
		lists:last(erl_syntax:clause_body(Clause)),
	BodyWOLast =
		lists:droplast(erl_syntax:clause_body(Clause)),
	VarReceiveResultName = free_named_var("EDDResultReceive"),
	NLastExpr = 
		erl_syntax:match_expr(
			VarReceiveResultName , 
			LastExpr),
	VarsContextEnd = 
		get_ann_info(env,lists:last(erl_syntax:clause_body(Clause))),
	ContextEnd = 
		build_dict_var(VarsContextEnd),
	SendResult =
		build_send_trace(
			receive_finished, 
			[VarReceiveResultName, ContextEnd]), 
	% NOldBody = 
	NBody = 
		[SendEvaluated] ++ BodyWOLast ++ [NLastExpr, SendResult, VarReceiveResultName],
	% NBody = 
	% 	[SendContext | NOldBody],
	NClause = 
		erl_syntax:clause(Patterns, erl_syntax:clause_guard(Clause), NBody),
	{erl_syntax:set_ann(NClause, erl_syntax:get_ann(Clause) ), CurrentClause + 1} .

erl_syntax_zip([], []) ->
	[];
erl_syntax_zip([H1 | T1], [H2 | T2]) ->
	[ erl_syntax:tuple([H1, H2]) | erl_syntax_zip(T1, T2)]. 

% ann_expr(T, VarAcc) -> 
% 	% io:format("~p\n", [erl_syntax:revert(T)]),
% 	% io:format("~p\n", [erl_syntax:type(T)]),
% 	case erl_syntax:type(T) of 
% 		receive_expr ->
% 			Clauses = erl_syntax:receive_expr_clauses(T),
% 			NClauses = 
% 				lists:map(
% 					fun(Clause) -> 
% 						ann_clause(Clause, VarAcc) 
% 					end, 
% 					Clauses),
% 			NReceive0 = 
% 				erl_syntax:receive_expr(
% 					NClauses, 
% 					erl_syntax:receive_expr_timeout(T), 
% 					erl_syntax:receive_expr_action(T)),
% 			NReceive = 
% 				erl_syntax:set_pos(NReceive0,erl_syntax:get_pos(T)),
% 			{erl_syntax:add_ann(lists:usort(VarAcc), NReceive),
% 			 VarAcc ++ sets:to_list(erl_syntax_lib:variables(T))};
% 		_ ->
% 			{erl_syntax:add_ann(lists:usort(VarAcc), T), 
% 			 VarAcc ++ sets:to_list(erl_syntax_lib:variables(T))}
% 	end.

% ann_clause(Clause, PrevVars) ->
% 	Patterns = 
% 		erl_syntax:clause_patterns(Clause),
% 	VarsBody = 
% 		lists:usort(
% 			PrevVars 
% 			++ vars_patterns(Patterns)),
% 	NBody = 
% 		element(1, 
% %TODO: I think that this is useless since the mapping is bottom-up, so it should be already annotated.
% 			lists:mapfoldl(
% 				fun ann_expr/2,
% 				VarsBody,
% 				erl_syntax:clause_body(Clause))),
% 	erl_syntax:clause(Patterns, erl_syntax:clause_guard(Clause), NBody).

build_dict_var(Vars) ->
	erl_syntax:list(
		erl_syntax_zip(
			[erl_syntax:string(V) || V <- Vars],
			[erl_syntax:variable(V)  || V <- Vars] )).

build_send_par(Dest, Pars) ->
	erl_syntax:application(
		erl_syntax:atom(erlang) , 
		erl_syntax:atom(send), 
		[Dest| Pars]).

build_send(Msg) ->
	build_send_par(
		erl_syntax:tuple([
			erl_syntax:atom(edd_tracer),
			erl_syntax:atom(node())
		]),
		[erl_syntax:tuple(Msg)]).

build_send_trace(Tag, Args) -> 
	build_send(
		[
	 		erl_syntax:atom(edd_trace),
	 		erl_syntax:atom(Tag),
	 		erl_syntax:application(
	 			erl_syntax:atom(erlang) , 
				erl_syntax:atom(self), 
				[]),
	 		erl_syntax:tuple(Args) 
	 	] ).

build_send_load(Module) -> 
	build_send(
		[
	 		erl_syntax:atom(edd_load_module),
	 		Module,
	 		erl_syntax:application(
	 			erl_syntax:atom(erlang) , 
				erl_syntax:atom(self), 
				[])
	 	] ).

build_store_fun(Name, FunInfo) -> 
	build_send(
		[
	 		erl_syntax:atom(edd_store_fun),
	 		Name,
	 		FunInfo
	 	] ).

build_receive_load() -> 
	erl_syntax:receive_expr
	(
		[
			erl_syntax:clause
			(
				[erl_syntax:atom(loaded)],
				[],
				[erl_syntax:atom(ok)
				% erl_syntax:application(
				% 	erl_syntax:atom(io) , 
				% 	erl_syntax:atom(format), 
				% 	[erl_syntax:string("RECIBIDO \n")])
			 	]
			)
		]
	) .

pos_and_pp(T, [{file_name,Fname}, {module_name,MName}]) ->
	% io:format("~p\n", [erl_syntax:get_attrs(T)]),
	[erl_syntax:tuple(
		[
			erl_syntax:atom(pos_info),
			erl_syntax:tuple(
				[
					MName,
					Fname,
					erl_syntax:integer(erl_syntax:get_pos(T)),
					% erl_syntax:string(
					% 	io_lib:format("~p", [erl_syntax:get_pos(T)]) ),
					erl_syntax:string(erl_prettypr:format(T)) 
				])
	 	])].

pos_and_pp(T) ->
	% io:format("~p\n", [erl_syntax:get_attrs(T)]),
	get_ann_info(info_tree, T).

	% [erl_syntax:tuple(
	% 	[
	% 		erl_syntax:atom(pos_info),
	% 		erl_syntax:tuple(
	% 			[
	% 				erl_syntax:integer(erl_syntax:get_pos(T)),
	% 				% erl_syntax:string(
	% 				% 	io_lib:format("~p", [erl_syntax:get_pos(T)]) ),
	% 				erl_syntax:string(erl_prettypr:format(T)) 
	% 			])
	%  	])].

annotate_info_tree(T, ModFileName = [_, {module_name,MName}]) ->
	% T.
	NT = 
		erl_syntax:add_ann(
			{info_tree, pos_and_pp(T, ModFileName)},
			T),
	erl_syntax:add_ann(
		{module_name, MName},
		NT).

% vars_patterns(Patterns) ->
% 	lists:foldl(
% 		fun(Pattern, Acc) -> 
% 			sets:to_list(erl_syntax_lib:variables(Pattern)) ++ Acc 
% 		end, 
% 		[], Patterns).

get_free() ->
	Free = get(free),
	put(free, Free + 1),
	Free.

free_named_var(NameRoot) ->
	erl_syntax:variable("_" ++ NameRoot ++ integer_to_list(get_free()) ).

args_assign(NameRoot, Args) -> 
	lists:unzip( 
		[ begin
			VarArg = 
				free_named_var(NameRoot),
			StoreArg = 
				erl_syntax:match_expr(VarArg, Arg),
			{VarArg, StoreArg}
		 end
		|| Arg <- Args] ).

get_ann_info(Tag, T) ->
	% io:format("~p\n~p: ~p\n", [T, Tag, erl_syntax:get_ann(T)]),
	case [Info 
		|| {Tag_, Info} <- erl_syntax:get_ann(T), 
			Tag_ == Tag ] of 
		[] -> 
			[];
		[H|_] ->
			H 
	end.

lists_with_modules_to_instument() ->	
	erl_syntax:list(
		[erl_syntax:atom(M) 
		|| 
		M <- get(modules_to_instrument), is_atom(M)]).

	% hd(
	% 	[Info 
	% 	|| {Tag_, Info} <- erl_syntax:get_ann(T), 
	% 		Tag_ == Tag ]).

% get_var_names(T) ->
% 	lists:usort(
% 		erl_syntax_lib:fold(
% 			fun(V, Acc) -> 
% 				case erl_syntax:type(V) of 
% 					variable ->
% 						[erl_syntax:variable_name(V) | Acc];
% 					_ ->
% 						Acc
% 				end 
% 			end, [], T) ).