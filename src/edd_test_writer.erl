-module(edd_test_writer).

-export([write/4]).

write(G, NewCorrect, NewNotCorrect, OtherTests) ->
	% io:format("~p\n", [{NewCorrect, NewNotCorrect}]),
	% [begin
	% 	io:format("~p\n", [V]),
	% 	{ok,Toks,_} = erl_scan:string(V ++ "."),
	% 	{ok,[Aexpr|_]} = erl_parse:parse_exprs(Toks),
	% 	case Aexpr of 
	% 		{call,_,{remote,_,{atom,_,_},_},APars} ->
	% 			io:format("~p\n", [APars]) ;
	% 		_ ->
	% 			[]
	% 	end	
	% end 
	% 	|| V <- NewCorrect],
	% [io:format("~p\n", [edd_lib:get_MFA_Label(G,V)]) 
	% 		|| V <- NewNotCorrect],
	LabeledVertex = 
			[{V, element(1, edd_lib:get_MFA_Label(G,V)), equal} 
			 || V <- NewCorrect] 
		++ 	[{V, element(1, edd_lib:get_MFA_Label(G,V)), not_equal} 
		 	 || V <- NewNotCorrect],
	% io:format("~p\n", [LabeledVertex]),
	LabeledOtherTests = 
		lists:flatten(
			[begin
				{ok,Toks,_} = erl_scan:string(lists:flatten(Call) ++ "."),
				{ok,[Aexpr|_]} = erl_parse:parse_exprs(Toks),
				case Aexpr of 
					{call,_,{remote,_,{atom,_,ModName},_},APars} ->
						[{{Call, Value}, ModName, Type}];
					_ ->
						[]
				end	
			 end 
			|| {Call, Value, Type} <- OtherTests]),
	ModulesDict = 
		create_modules_dict(
			LabeledVertex ++ LabeledOtherTests, 
			dict:new()),
	% OtherTestsDict = create_modules_dict(LabeledOtherTests, dict:new()),
	% io:format("~p\n", [dict:to_list(ModulesDict)]),
	[begin
		% TestsTree = 
		% 	case dict:is_key(K, ModulesDict) of 
		% 		true -> 
		% 			dict:fetch(K, ModulesDict);
		% 		false ->
		% 			[]
		% 	end,
		% TestsOther =
		% 	case dict:is_key(K, ModulesDict) of 
		% 		true -> 
		% 			dict:fetch(K, OtherTestsDict);
		% 		false ->
		% 			[]
		% 	end,
		% write_in_file(K, TestsTree, TestsOther, G) 
		write_in_file(K, dict:fetch(K, ModulesDict), G) 
	 end
	 || K <- lists:usort(dict:fetch_keys(ModulesDict))].


create_modules_dict([], Dict) ->
	Dict;
create_modules_dict([{V, Module, Type} | Tail], Dict) ->
	NDict = dict:append(Module, {V, Type}, Dict),
	create_modules_dict(Tail, NDict).

% create_modules_dict_test([], Dict) ->
% 	Dict;
% create_modules_dict_test([{Module, Call, Value, Type} | Tail], Dict) ->
% 	NDict = dict:append(Module, {Call, Value, Type}, Dict),
% 	create_modules_dict_test(Tail, NDict).

write_in_file(Module, Tests, G) ->
	{ok,Forms} = 
		epp:parse_file(atom_to_list(Module) ++ ".erl", [], []),
	edd_test_reader:put_attributes(Forms),
	% io:format("Forms: ~p\n", [Forms]),
	PreviousTests0 =
		[ 	edd_test_reader:read_from_clause(Clause) 
		||  {function, _, edd_test, _, [Clause]} <- Forms],
	PreviousTests = 
		case PreviousTests0 of 
			[] -> 
				[];
			[PT] -> 
				PT 
		end,
	% io:format("PreviousTests: ~p\n", [PreviousTests]),
		% case lists:last(Forms -- [lists:last(Forms)]) of 
		% 	{function, _, edd_test, _, [Clause]} ->
		% 		edd_test_reader:read_from_clause(Clause);
		% 	_ ->
		% 		[]
		% end,
	StrTests1 = 
		[ build_test_case(Type, Call, Value) 
		 || {Call, Value, Type} <- PreviousTests],
	put(complexity_total, []),
	io:format("******* COMPLEXITY DETAILS *******\n", []),  
	StrTests2 = 
		build_tests(Tests, G, []),
	io:format("**********************************\n", []), 
	io:format("******* COMPLEXITY SUMMARY *******\n", []),  
	% io:format("~p\n", [get(complexity_total)]),
	case sets:is_element(unknown, sets:from_list(get(complexity_total))) of
		true -> 
			io:format("WARNING: Some complexity are unknown");
		false -> 
			ok 
	end,
	WithoutUnknowns = [ComFun || ComFun <- get(complexity_total), ComFun =/= unknown ],
	SumComplexities = lists:sum(WithoutUnknowns),
	case SumComplexities of 
		0 -> 
			io:format("Total complexity unknown\n", []);
		_ ->
			MaxComplexities = lists:max(WithoutUnknowns),
			MinComplexities = lists:min(WithoutUnknowns),
			MeanComplexity = SumComplexities/length(WithoutUnknowns),
			io:format("Total complexity: ~p\n", [SumComplexities]),
			io:format("Max complexity: ~p\n", [MaxComplexities]),
			io:format("Min complexity: ~p\n", [MinComplexities]),
			io:format("Average complexity: ~p\n", [MeanComplexity])
	end,
	io:format("**********************************\n", []), 
	StrTests = 
		StrTests1 ++ StrTests2,
	StrTestFun = 
		str_test_fun(StrTests),
	% io:format("~s\n", [StrTestFun]),
	StrModule = 
		atom_to_list(Module) ++ ".erl", 
	{ok, IODeviceR} = 
		file:open(StrModule, [read]),
	Read0 = 
		read_file(IODeviceR),
	file:close(IODeviceR),
	Read = 
		remove_previous_test(Read0, []),
	StrEunitInclude = 
		case is_previous_include(Read) of 
			true ->
				"";
			false ->
				"\n-include_lib(\"eunit/include/eunit.hrl\").\n"
		end,
	{ok, IODeviceW} = 
		file:open(StrModule, [write]),
	StrIni = 
		"\n% begin edd test\n",
	StrEnd = 
		"\n% end edd test\n",
	StrProgram = 
		Read ++ StrIni ++ StrEunitInclude ++ StrTestFun ++ StrEnd,
	file:write(IODeviceW, list_to_binary(StrProgram)),
	file:close(IODeviceW),
	ok.


remove_previous_test([$\n,$%,$ ,$b,$e,$g,$i,$n,$ ,$e,$d,$d,$ ,$t,$e,$s,$t | Tail], Acc) ->
	remove_previous_test_until(Tail, Acc);
remove_previous_test([Other | Content],Acc) ->
	remove_previous_test(Content, [Other|Acc]);
remove_previous_test([],Acc) ->
	lists:reverse(Acc).

remove_previous_test_until([$\n,$%,$ ,$e,$n,$d,$ ,$e,$d,$d,$ ,$t,$e,$s,$t | Tail], Acc) ->
	lists:reverse(lists:reverse(Tail) ++ Acc);
remove_previous_test_until([_ | Content],Acc) ->
	remove_previous_test_until(Content, Acc);
remove_previous_test_until([],Acc) ->
	lists:reverse(Acc).

is_previous_include([$e,$u,$n,$i,$t,$/,$i,$n,$c,$l,$u,$d,$e,$/,$e,$u,$n,$i,$t,$.,$h,$r,$l | _]) ->
	true;
is_previous_include([Other | Content]) ->
	is_previous_include(Content);
is_previous_include([]) ->
	false.

read_file(IODevice) -> 
	read_file(IODevice,[],[]).
	
read_file(IODevice,PrevData,Acc) ->
	% io:format("A: ~p\nB: ~p\n",[PrevData,Acc]),
	case io:request(IODevice, {get_line, ''}) of
	     eof -> Acc++PrevData;
	     Data -> read_file(IODevice,Data,Acc++PrevData)
	     
	end.

str_test_fun(Tests) ->
	"\nedd_test() ->\n" ++ str_test_fun_body(Tests).

str_test_fun_body([]) ->
	"\tok.\n";
str_test_fun_body([Test]) ->
	"\t" ++ Test ++ ".\n";
str_test_fun_body([Test | Tests]) ->
		"\t" ++ Test ++ ",\n"
	++	str_test_fun_body(Tests).

build_tests([{V, Type}|Tail], G, Acc) ->
	try 
		{StrCall, StrValue} = 
			case V of 
				{StrCall_, StrValue_} ->
					{StrCall_, StrValue_};
				_ ->
					{V,{Test,_,_,_}} = digraph:vertex(G,V),
					{ok,Toks,_} = erl_scan:string(lists:flatten(Test)++"."),
					{ok,[Aexpr|_]} = erl_parse:parse_exprs(Toks),
					Call = erl_syntax:match_expr_pattern(Aexpr),
					Value = erl_syntax:match_expr_body(Aexpr),
					{erl_prettypr:format(Call), erl_prettypr:format(Value)}
			end,
		StrTest = build_test_case(Type, StrCall, StrValue),
		build_tests(Tail, G, [StrTest | Acc])
	catch
		_:_ ->
			build_tests(Tail, G, Acc)	
	end;
build_tests([], _, Acc) ->
	Acc.

str_assert(equal) ->
	"?assertEqual";
str_assert(not_equal) ->
	"?assertNotEqual".

build_test_case(Type, StrCall0, StrValue) ->
	% io:format("Question: " ++ StrCall0 ++ "\n"),
	{ok,Toks,_} = erl_scan:string(lists:flatten(StrCall0)++"."),
	{ok,[Aexpr|_]} = erl_parse:parse_exprs(Toks),
	NAexp = disremotize(Aexpr),
	StrCall = erl_prettypr:format(NAexp),
	io:format(
		"Question:\n\t~s = ~s\n", 
		[StrCall, StrValue]),
	try
		{ok,ToksValue,_} = erl_scan:string(lists:flatten(StrValue)++"."),
		{ok,[AexprValue|_]} = erl_parse:parse_exprs(ToksValue),
		% io:format("v: ~p\n", [AexprValue]),
		{value, ValueTerm, []} = erl_eval:expr(AexprValue, []),
		ComplexityValue = edd_con:complexity_term(ValueTerm),
		% io:format("ComplexityValue: ~p\n", [ComplexityValue]),
		ComplexityCall = get(complexity),
		% io:format("ComplexityCall: ~p\n", [ComplexityCall]),
		ComplexityQuestion = 1 + ComplexityCall + ComplexityValue,
		io:format("Complexity:\n\t~p\n", [ComplexityQuestion]),
		put(complexity_total, [ComplexityQuestion | get(complexity_total)])
	catch
		_:_ ->
			put(complexity_total, [unknown | get(complexity_total)]),
			io:format("Complexity:\n\t~p\n", [unknown])
	end,
	lists:flatten(
		io_lib:format(
			"~s(~s, ~s)", 
			[str_assert(Type), StrCall, StrValue])).
	

disremotize(Node) ->
	erl_syntax_lib:map(fun disremotize_node/1, Node).

disremotize_node(Node) -> 
	try
		% io:format("~p\n", [erl_syntax:application_arguments(Node)]),
		% io:format("~p\n", [erl_syntax:concrete(lists:nth(1, erl_syntax:application_arguments(Node)))]),
		% io:format("~p\n", [edd_con:complexity_term(erl_syntax:concrete(lists:nth(1, erl_syntax:application_arguments(Node))))]),
		try
			Complexity = 
				lists:sum(
					lists:map(
						fun(AppArg) -> 
							edd_con:complexity_term(
								erl_syntax:concrete(AppArg)
							) 
						end, 
						erl_syntax:application_arguments(Node)
					)
				),
			% io:format("ComplexityCall: ~p\n", [Complexity]),
			put(complexity, Complexity)
		catch
			_:_ ->
				put(complexity, unknown)
		end,
		Operator = erl_syntax:application_operator(Node),
		Module = get(module),
		Module = 
			erl_syntax:atom_value( 
				erl_syntax:module_qualifier_argument(Operator)),
		erl_syntax:application(
			erl_syntax:module_qualifier_body(Operator), 
			erl_syntax:application_arguments(Node))
	catch 
		_:_ ->
			put(complexity, unknown),
			disremotize_implicit_fun(Node)
	end.

disremotize_implicit_fun(Node) -> 
	try
		implicit_fun = 
			erl_syntax:type(Node), 
		Name = 
			erl_syntax:implicit_fun_name(Node),
		module_qualifier = 
			erl_syntax:type(Name),
		Module = get(module),
		Module = 
			erl_syntax:atom_value(
				erl_syntax:module_qualifier_argument(Name)),
		erl_syntax:implicit_fun(
			erl_syntax:module_qualifier_body(Name))
	catch 
		_:_ ->
			Node
	end.