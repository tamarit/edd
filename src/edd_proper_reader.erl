-module(edd_proper_reader).

-export([
		read/1, read_file/1, 
		read_from_clause/1, put_attributes/1, 
		get_initial_set_of_nodes/4]).

% Test check: proper:check(rat_eqc:floor_1(),[{Arg1, Arg2}]). 
% Type check: proper_typeserver:demo_is_instance(1, rat, "integer()").

read(Module) -> 
	read_file(atom_to_list(Module) ++ ".erl").

read_file(File) -> 
	io:format("File: ~p\n", [File]),
	{ok,Forms} = 
		epp:parse_file(File, [], []), 
	put_attributes(Forms),
	F = 
		fun (Form, Acc) ->
			form(Form, Acc)
		end,
	AllTestsCandidates = 
		lists:reverse(lists:foldl(F, [], Forms)),
	ProperTest = 
		lists:foldl(
			fun separeteUsefulDiscarded/2,
			{[], []},
			AllTestsCandidates),
	% {ok, IODeviceW} = 
	% 	file:open("result.txt",[write]),
	% file:write(IODeviceW, list_to_binary(lists:flatten(io_lib:format("~p\n", [ProperTest])))),
	% file:close(IODeviceW),
	ProperTest.

separeteUsefulDiscarded({Fun, _, []}, {U, NU}) ->
	{U, [Fun | NU]};
separeteUsefulDiscarded(Fun, {U, NU}) ->
	{[Fun | U], NU}.

put_attributes(Forms) ->
	Module = 
		hd([Mod || {attribute,_,module,Mod} <- Forms]),
	put(module, Module),
	Exports = 
		lists:concat([Exports || {attribute,_,export,Exports} <- Forms]),
	put(exports, Exports),
	AllFuns = 
		[{Name, Arity} || {function, _, Name, Arity, _} <-  Forms],
	put(all_funs, AllFuns).

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
		AllFunsCalled = 
			[{erl_syntax:application_operator(C), length(erl_syntax:application_arguments(C))} 
			|| C <- Calls, not(is_trusted_fun({erl_syntax:application_operator(C), length(erl_syntax:application_arguments(C))}))],
		UsableCalls = 
			lists:foldl(
				fun(Call, CurrentUsableCalls) -> 
					usable_calls(Call, CurrentUsableCalls, VarsPattern, AllFunsCalled) 
				end, 
				[], 
				Calls),
		case UsableCalls of 
			[] -> 
				Acc;
			_ ->
				[{TypesArgs, UsableCalls}| Acc]
		end
	catch 
		_:_ -> 
			% io:format("Failing: ~p\n", [Application]),
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

usable_calls(Call, Acc, VarsPattern, AllFunsCalled) ->
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
					Op = 
						erl_syntax:application_operator(Call),
					% io:format("~p\n~p\n",[Op, is_trusted_fun({Op, length(Args)})]),
					case is_trusted_fun({Op, length(Args)}) of 
						true -> 
							Acc;
						false ->
							RestOfFuns =  
								lists:usort(AllFunsCalled -- [{Op, length(Args)}]),
							[{Op, Args, RestOfFuns} | Acc]
					end;
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

is_trusted_fun({FunName, Arity}) -> 
	case erl_syntax:type(FunName) of 
		atom -> 
			FunNameAtom = 
				erl_syntax:atom_value(FunName),
			AllFuns = 
				get(all_funs),
			case lists:member({FunNameAtom, Arity}, AllFuns) of 
				true -> 
					false;
				false -> 
					true 
			end;
		module_qualifier ->
			ModNameAtom = 
				erl_syntax:atom_value(
					erl_syntax:module_qualifier_argument(FunName)),
			ModName = 
				atom_to_list(ModNameAtom),
			FileAdress = 
				code:where_is_file(ModName ++ ".erl"),
			NFileAdress = 
			   	case FileAdress of
			        non_existing -> 
			     		NFileAdress_ = 
			     			code:where_is_file(ModName ++ ".beam"),
			     		case NFileAdress_ of
			     		     non_existing -> 
			     		     	% io:format("PATHS: ~p\n",[code:get_path()]),
			     		     	throw({error,"Non existing module", ModName});
			     		     _ -> 
			     		     	RelPath = 
			     		     		"ebin/" ++ ModName ++ ".beam",
			     		     	NRelPath = 
			     		     		"src/" ++ ModName ++ ".erl",
			     		     	PrevPath = 
			     		     	   lists:sublist(
			     		     	   		NFileAdress_,1,
										length(NFileAdress_) - length(RelPath)),
			     		     	PrevPath ++ NRelPath
			     		end;
			     	_ -> 
			     		FileAdress
			   end,
			lists:prefix(code:lib_dir(), NFileAdress)
	end.

get_initial_set_of_nodes(G, TrustedFunctions, Root, TestFiles) -> 
	Modules = 
		lists:usort(
			[element(1, edd_lib:get_MFA_Label(G,V)) 
		 	 || V <- digraph:vertices(G)]) -- 
		lists:usort(
			[case element(1, edd_lib:get_MFA_Label(G,V)) of 
				Elem = {'fun',_,_} -> 
					Elem; 
				_ ->
					ok 
			 end
		 	 || V <- digraph:vertices(G)]),
	% io:format("Loading from files ~p and modules ~p\n", [TestFiles, Modules]),
	Tests = 
		lists:flatten(
			[element(1, edd_proper_reader:read(Module) )
			 || Module <- Modules]
			++ [ element(1, edd_proper_reader:read_file(File))
			 || File <- TestFiles]),
	io:format("Tests : ~p\n", [Tests]),
	io:format("TrustedFunctions : ~p\n", [TrustedFunctions]),
	VerticesInTests = 
		lists:flatten(
			[
				begin 
					case get_call_vertex(G, V) of 
						none -> 
							[];
						FunAndArgs -> 
							io:format("V:~p\n", [V]),
							find_usable_tests(FunAndArgs, TrustedFunctions, Tests)
					end
				end 
			|| V <- digraph:vertices(G)]),
	% % io:format("VerticesInTests : ~p\n", [VerticesInTests]),
	% VerticesNotValidFromPositiveTests = 
	% 	lists:flatten([begin 
	% 		{CallV,ValueV} = edd_lib:get_call_value_string(G,V),
	% 		[V 
	% 			|| 	{CallT, ValueT, equal} <- Tests,
	% 			 	CallV == CallT, ValueV /= ValueT] 
	% 	 end || V <- digraph:vertices(G)]),
	% % io:format("VerticesNotValidFromPositiveTests : ~p\n", [VerticesNotValidFromPositiveTests]),
	% ValidFromTest = 
	% 	[V || {V, equal} <- VerticesInTests],
	% NotValidFromTest = 
	% 	[V || {V, not_equal} <- VerticesInTests] 
	% 	++ VerticesNotValidFromPositiveTests,
	% % io:format("{ValidFromTest,NotValidFromTest} : ~p\n", [{ValidFromTest,NotValidFromTest}]),
	% IniValid_ = lists:usort(ValidFromTest ++ ValidTrusted),
	% IniNotValid_ = lists:usort([Root | NotValidFromTest]),
	% NewValidTests = 
	% 	case lists:usort(ValidFromTest) of 
	% 		IniValid_ ->
	% 			% Trusted nodes were already as test cases
	% 			[];
	% 		_ ->
	% 			lists:usort(ValidTrusted) -- lists:usort(ValidFromTest)
	% 	end,
	% NewNotValidTests = 
	% 	case lists:usort(NotValidFromTest) of 
	% 		IniNotValid_ ->
	% 			% Root was already as a test case
	% 			[];
	% 		_ ->
	% 			[Root]
	% 	end,
	% put(test_to_NOT_store, {IniValid_ -- NewValidTests, IniNotValid_ -- NewNotValidTests}),
	% {IniValid_, IniNotValid_}.
	ok.

find_usable_tests(FunAndArgs, TrustedFunctions, Tests) -> 
	lists:foldl(
		fun(T, Acc) -> 
			get_compatible_calls(FunAndArgs, T, TrustedFunctions, Acc)
		end,
		[],
		Tests).

get_compatible_calls(
		FunAndArgs, 
		{{ModuleTest,FunTest,0}, IsComplete, [{Pars, UsableCalls}]}, 
		TrustedFunctions,
		Acc) -> 
	lists:foldl(
		fun(UsableCall, CAcc) -> 
			get_compatible_usable_calls(ModuleTest, UsableCall, FunAndArgs, TrustedFunctions, CAcc)
		end,
		[],
		UsableCalls),
	Acc.


get_compatible_usable_calls(
		ModuleTest, 
		{FunUsable, ArgsUsable, RestOfFuns}, 
		{FunVertex, ArgsVertex}, 
		TrustedFunctions, 
		Acc) -> 
	% 	io:format("A: ~p\nB: ~p\n", [{FunVertex, ArgsVertex}, {FunUsable, ArgsUsable}]),
	case same_fun(ModuleTest, FunVertex, FunUsable) of 
		true ->
			io:format("Equal_A: ~p\nEqual_B: ~p\n", [FunVertex, FunUsable]),
			io:format("RestOfFuns; ~p\n", [RestOfFuns]),
			TupledRestOfFuns = 
				[tuple_function(ModuleTest, NeededFun, Arity) 
				 || {NeededFun, Arity} <- RestOfFuns],
			case (TupledRestOfFuns -- TrustedFunctions) of 
				[] -> 
					io:format("Valid test\n"),
					case compatible_args(ArgsVertex, ArgsUsable) of 
						false -> 
							Acc;
						{true, Dict} -> 
							Acc
					end;
				_ ->
					io:format("Not valid test\n"),
					Acc 
			end;
		false ->
			Acc 
	end.

same_fun(ModuleTest, FunVertex, FunUsable) -> 
	ModuleVertex = 
		erl_syntax:atom_value(
			erl_syntax:module_qualifier_argument(FunVertex)),
	FunNameVertex = 
		erl_syntax:atom_value(
			erl_syntax:module_qualifier_body(FunVertex)),		
	case {erl_syntax:type(FunUsable), ModuleVertex == ModuleTest} of 
		{atom, true} -> 
			FunNameUsable = 
				erl_syntax:atom_value(FunUsable),
			FunNameVertex == FunNameUsable;
		{module_qualifier, _} -> 
			ModuleUsable = 
				erl_syntax:atom_value(
					erl_syntax:module_qualifier_argument(FunUsable)),
			FunNameUsable = 
				erl_syntax:atom_value(
					erl_syntax:module_qualifier_body(FunUsable)),
			(ModuleVertex  == ModuleUsable) and (FunNameVertex == FunNameUsable)
	end. 

tuple_function(ModuleTest, NeededFun, Arity) -> 
	case erl_syntax:type(NeededFun) of 
		atom -> 
			FunNameNeeded = 
				erl_syntax:atom_value(NeededFun),
			{ModuleTest, FunNameNeeded, Arity};
		module_qualifier -> 
			ModuleNeeded = 
				erl_syntax:atom_value(
					erl_syntax:module_qualifier_argument(NeededFun)),
			FunNameNeeded = 
				erl_syntax:atom_value(
					erl_syntax:module_qualifier_body(NeededFun)),
			{ModuleNeeded, FunNameNeeded, Arity}
	end.	

get_call_vertex(G, V) -> 
	{Vertex,{Label,_,File,Line}} = 
		digraph:vertex(G, V),
	{ok,Toks,_} = 
		erl_scan:string(lists:flatten(Label)++"."),
	{ok,[Aexpr|_]} = 
		erl_parse:parse_exprs(Toks),
	{match,_,Call = {call, _, Called, Args},_} = 
		Aexpr,
	case Called of 
		{remote,_,_,_} ->
			{Called, Args};
		_ ->
			none
	end.
