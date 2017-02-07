-module(edd_proper_reader).

-export([read/1, read_file/1, read_from_clause/1, put_attributes/1]).

% Test check: proper:check(rat_eqc:floor_1(),[{Arg1, Arg2}]). 
% Type check: proper_typeserver:demo_is_instance(1, rat, "integer()").

read(Module) -> 
	read_file(atom_to_list(Module) ++ ".erl").

read_file(File) -> 
	{ok,Forms} = 
		epp:parse_file(File, [], []), 
	put_attributes(Forms),
	F = 
		fun (Form, Acc) ->
			form(Form, Acc)
		end,
	AllTestsCandidates = 
		lists:reverse(lists:foldl(F, [], Forms)),
	Res = 
		lists:foldl(
			fun separeteUsefulDiscarded/2,
			{[], []},
			AllTestsCandidates),
	{ok, IODeviceW} = 
		file:open("result.txt",[write]),
	file:write(IODeviceW, list_to_binary(lists:flatten(io_lib:format("~p\n", [Res])))),
	file:close(IODeviceW).

separeteUsefulDiscarded({Fun, _, []}, {U, NU}) ->
	{U, [Fun | NU]};
separeteUsefulDiscarded(Fun, {U, NU}) ->
	{[Fun | U], NU}.

put_attributes(Forms) ->
	Module = hd([Mod || {attribute,_,module,Mod} <- Forms]),
	put(module, Module),
	Exports = lists:concat([Exports || {attribute,_,export,Exports} <- Forms]),
	put(exports, Exports).

form({function, _, Name, 0, Clauses}, Acc) ->
	Mod = get(module),
	SuffixComplete = 
		"_complete",
	StrName = 
		atom_to_list(Name),
	IsComplete = 
		case length(StrName) < length(SuffixComplete) of 
			true -> 
				false;
			false -> 
				case string:sub_string(StrName, length(StrName) - length(SuffixComplete) + 1,  length(StrName)) of 
					SuffixComplete ->
						true;
					_ -> 
						false

				end
		end,
	case Clauses of 
		[Clause] -> 
			[{{Mod,Name,0}, IsComplete, read_from_clause(Clause)}] ++ Acc;
		_ -> 
			Acc 
	end;
form(_, Acc) ->
    Acc.

read_from_clause(Clause) ->
	read_from_clause(Clause, []).

read_from_clause(Clause0, Acc) -> 
	Clause = 
		% erl_syntax_lib:map(fun remotize/1, Clause0),
		Clause0,
	lists:foldl(
		fun extract_forAll/2, 
		Acc, 
		erl_syntax:clause_body(Clause)).

extract_forAll(Application, Acc) ->
	try
		ForAllFun = 
			erl_syntax:application_operator(Application),
		[TypeDescAST, FunTest] = 
			erl_syntax:application_arguments(Application),
		proper = 
			erl_syntax:atom_value(
				erl_syntax:module_qualifier_argument(ForAllFun)),
		forall = 
			erl_syntax:atom_value(
				erl_syntax:module_qualifier_body(ForAllFun)),
		% The first parameter (the type descriptors) can only be a call or a tuples of calls
		TypeDesc = 
			get_type_descriptors(TypeDescAST),
		[ClauseFunTest] = 
			erl_syntax:fun_expr_clauses(FunTest),
		[PatternFunTest] = 
			erl_syntax:clause_patterns(ClauseFunTest),
		VarsPattern = 
			[VarName || {var, _, VarName} <- get_vars_test(PatternFunTest)],
		TypesArgs = 
			lists:zip(VarsPattern, TypeDesc),
		[BodyTest] = 
			erl_syntax:clause_body(ClauseFunTest),
		Calls = 
			extract_calls(BodyTest),
		UsableCalls = 
			lists:foldl(
				fun(Call, CurrentUsableCalls) -> 
					usable_calls(Call, CurrentUsableCalls, VarsPattern) 
				end, 
				[], 
				Calls),
		AllFunsCalles = 
			[erl_syntax:application_operator(C) || C <- Calls],
		case UsableCalls of 
			[] -> 
				Acc;
			_ ->
				[{TypesArgs, UsableCalls, AllFunsCalles}| Acc]
		end
	catch 
		_:_ -> 
			Acc
	end.

get_type_descriptors(N) ->
	case erl_syntax:type(N) of 
		tuple ->
			lists:concat( 
				lists:map(
					fun get_type_descriptors/1,
					erl_syntax:tuple_elements(N)));
		application ->
			[N]
	end.

get_vars_test(N) ->
	case erl_syntax:type(N) of 
		tuple ->
			lists:concat( 
				lists:map(
					fun get_vars_test/1,
					erl_syntax:tuple_elements(N)));
		variable ->
			[N]
	end.

extract_calls(Node) -> 
	erl_syntax_lib:fold(fun add_if_call/2, [], Node).

add_if_call(Node, Acc) -> 
	case erl_syntax:type(Node) of 
		application -> 
			[Node | Acc];
		_ ->
			% io:format("Node: ~p\nType: ~p\n", [Node, erl_syntax:type(Node)]), 
			Acc
	end.

usable_calls(Call, Acc, VarsPattern) ->
	Args = 
		erl_syntax:application_arguments(Call),
	OnlyVarsArgs = 
		[V || {var,_,V} <- Args],
	WithoutVarsArgs = 
		lists:foldl(
			fun add_if_does_not_have_vars/2,
			[],
			Args),
	case length(Args) =:= (length(OnlyVarsArgs) + length(WithoutVarsArgs)) of 
		true ->
			case lists:usort(VarsPattern) == lists:usort(OnlyVarsArgs) of 
				true -> 
					[{erl_syntax:application_operator(Call), OnlyVarsArgs} | Acc];
				false -> 
					Acc 
			end;
		false -> 
			Acc
	end.

add_if_does_not_have_vars(N, Acc) ->
	VarsInN = 
		erl_syntax_lib:fold(
			fun get_vars/2,
			[],
			N),
	case VarsInN of 
		[] ->
			[N | Acc];
		_ -> 
			Acc
	end.

get_vars(N, Acc) -> 
	case erl_syntax:type(N) of 
		variable ->
			[N | Acc];
		_ ->
			Acc
	end.