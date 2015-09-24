-module(edd_test_reader).

-export([read/1]).

read(Module) -> 
	read_file(atom_to_list(Module) ++ ".erl").

read_file(File) -> 
	{ok,Forms} = epp:parse_file(File, [], []), 
	% Res = epp:scan_erl_form(File),
	% io:format("~p\n", [Forms]),
	Module = hd([Mod || {attribute,1,module,Mod} <- Forms]),
	put(module, Module),
	% io:format("~p\n", [Res]),

	F = 
		fun (Form, Acc) ->
			form(Form, Acc)
		end,
	lists:reverse(lists:foldl(F, [], Forms)).

form({function, _L, Name, 0, Clauses}=Form, Acc) ->
	% TODO: Poner nombre del test del que se ha extraido
    N = atom_to_list(Name),
    case lists:suffix("_test", N) of
		true ->
			get_form(Clauses) ++ Acc;
		false ->
	    	Acc
    end;
form(Form, Acc) ->
    Acc.

get_form(Clauses) ->
	F = 
		fun (Clause, Acc) ->
			clause(Clause, Acc)
		end,
	% lists:reverse(lists:foldl(F, [], Clauses)).
	lists:foldl(F, [], Clauses).

clause(Clause, Acc) -> 
	Res = lists:foldl(fun extract_assertEqual/2, Acc, erl_syntax:clause_body(Clause)),
	Res.
	% [extract_assertEqual(Block) 
	%  || Block <- erl_syntax:clause_body(Clause)] ++ Acc.

extract_assertEqual(Block, Acc) ->
	try
		[Application] = erl_syntax:block_expr_body(Block),
		[Argument] = erl_syntax:application_arguments(Application),
		Fun = erl_syntax:application_operator(Application),
		[ClauseFun] = erl_syntax:fun_expr_clauses(Fun),
		[ClauseFunBody] = erl_syntax:clause_body(ClauseFun),
		CaseClauses = erl_syntax:case_expr_clauses(ClauseFunBody),
		CaseExpr = erl_syntax:case_expr_argument(ClauseFunBody),
		Type = assert_type_assertEqual(CaseClauses),
		{App0, Res} = decide_app_res(Argument, CaseExpr),
		App = remotize_app(App0),
		[{erl_prettypr:format(App), erl_prettypr:format(Res), Type} | Acc]
	catch 
		_:_ -> 
			extract_assert(Block, Acc)
	end.

decide_app_res(Cand1, Cand2) ->
	case erl_syntax:type(Cand1) of 
		application -> 
			{Cand1, Cand2};
		_ ->
			{Cand2, Cand1}
	end.

extract_assert(Block, Acc) ->
	try
		[Application] = erl_syntax:block_expr_body(Block),
		Fun = erl_syntax:application_operator(Application),
		[ClauseFun] = erl_syntax:fun_expr_clauses(Fun),
		[ClauseFunBody] = erl_syntax:clause_body(ClauseFun),
		CaseClauses = erl_syntax:case_expr_clauses(ClauseFunBody),
		CaseExpr0 = erl_syntax:case_expr_argument(ClauseFunBody),
		{Type, CaseExpr} = assert_type_assert(CaseExpr0),
		{App, Res, NType} = 
			try 
				OpL = erl_syntax:infix_expr_left(CaseExpr),
				OpR = erl_syntax:infix_expr_right(CaseExpr),
				{App_0, Res_} = decide_app_res(OpL, OpR),
				App_ = remotize_app(App_0),
				case 
					erl_syntax:operator_name(
						erl_syntax:infix_expr_operator(CaseExpr)) of
					'=:=' ->
						{App_, Res_, Type};
					'==' ->
						{App_, Res_, Type};
					'/=' ->
						{App_, Res_, not(Type)};
					'=/=' ->
						{App_, Res_, not(Type)}
				end
			catch 
				_:_ ->
					{CaseExpr, erl_syntax:atom(true), Type}
			end,
		[{erl_prettypr:format(App), erl_prettypr:format(Res), NType} | Acc]
	catch 
		_:_ -> 
			Acc
	end.


assert_type_assert(Expr) ->
	try 
		case erl_syntax:operator_name(erl_syntax:prefix_expr_operator(Expr)) of 
			'not' -> 
				{not_equal, erl_syntax:prefix_expr_argument(Expr)}	
		end
	catch 
		_:_ ->
			{equal, Expr}
	end.

assert_type_assertEqual(CaseClauses) ->
	try 
		[FirstClauseBody] = 
			erl_syntax:clause_body(lists:nth(1, CaseClauses)),
		case erl_syntax:type(FirstClauseBody) of
			atom -> 
				ok = erl_syntax:atom_value(FirstClauseBody),
				equal;
			_ ->
				not_equal
		end
	catch 
		_:_ ->
			none
	end.

remotize_app(App) -> 
	try
		Operator = erl_syntax:application_operator(App),
		case erl_syntax:type(Operator) of 
			atom ->				
				Module = erl_syntax:atom(get(module)),
				% NOperator = 
				% 	erl_syntax:module_qualifier(Module, Operator),
				erl_syntax:application(
					Module, Operator, 
					erl_syntax:application_arguments(App)) 
		end 
	catch 
		_:_ ->
			App
	end.
