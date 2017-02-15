-module(edd_test_writer).

-export([write/4]).

write(G, NewCorrect, NewNotCorrect, OtherTests) ->
	% io:format("~p\n", [{NewCorrect, NewNotCorrect}]),
	LabeledVertex = 
			[{V, element(1, edd_lib:get_MFA_Label(G,V)), equal} 
			 || V <- NewCorrect] 
		++ 	[{V, element(1, edd_lib:get_MFA_Label(G,V)), not_equal} 
		 	 || V <- NewNotCorrect],
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
	io:format("PreviousTests: ~p\n", [PreviousTests]),
		% case lists:last(Forms -- [lists:last(Forms)]) of 
		% 	{function, _, edd_test, _, [Clause]} ->
		% 		edd_test_reader:read_from_clause(Clause);
		% 	_ ->
		% 		[]
		% end,
	StrTests1 = 
		[ build_test_case(Type, Call, Value) 
		 || {Call, Value, Type} <- PreviousTests],
	StrTests2 = 
		build_tests(Tests, G, []),
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


remove_previous_test([$%,$ ,$b,$e,$g,$i,$n,$ ,$e,$d,$d,$ ,$t,$e,$s,$t | Tail], Acc) ->
	remove_previous_test_until(Tail, Acc);
remove_previous_test([Other | Content],Acc) ->
	remove_previous_test(Content, [Other|Acc]);
remove_previous_test([],Acc) ->
	lists:reverse(Acc).

remove_previous_test_until([$%,$ ,$e,$n,$d,$ ,$e,$d,$d,$ ,$t,$e,$s,$t | Tail], Acc) ->
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
	{ok,Toks,_} = erl_scan:string(lists:flatten(StrCall0)++"."),
	{ok,[Aexpr|_]} = erl_parse:parse_exprs(Toks),
	NAexp = disremotize(Aexpr),
	StrCall = erl_prettypr:format(NAexp),
	lists:flatten(
		io_lib:format(
			"~s(~s, ~s)", 
			[str_assert(Type), StrCall, StrValue])).


disremotize(Node) ->
	erl_syntax_lib:map(fun disremotize_node/1, Node).

disremotize_node(Node) -> 
	try
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