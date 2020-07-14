%edd:dd("align_columns:align_left()").
-module (align_columns).
-export([align_left/0, align_right/0, align_center/0, prepare_line/3]).

-include_lib("eunit/include/eunit.hrl").

align_left()-> align_columns(left).
align_right()-> align_columns(right).
align_center()-> align_columns(centre).
align_columns(Alignment) ->
    Lines =
         ["Given\$a\$text\$file\$of\$many\$lines\$where\$fields\$within\$a\$line\$",
          "are\$delineated\$by\$a\$single\$'dollar'\$character,\$write\$a\$program",
          "that\$aligns\$each\$column\$of\$fields\$by\$ensuring\$that\$words\$in\$each\$",
          "column\$are\$separated\$by\$at\$least\$one\$space.",
          "Further,\$allow\$for\$each\$word\$in\$a\$column\$to\$be\$either\$left\$",
          "justified,\$right\$justified,\$or\$center\$justified\$within\$its\$column."],
    Words = [ string:tokens(Line, "\$") || Line <- Lines ],
    Words_length  = lists:foldl( fun max_length/2, [], Words),
    Result = [prepare_line(Words_line, Words_length, Alignment) || Words_line <- Words],
    %[ io:fwrite("~s~n", [lists:flatten(Line)]) || Line <- Result],
    %ok.
    Result.

max_length(Words_of_a_line, Acc_maxlength) ->
    Line_lengths = [length(W) || W <- Words_of_a_line ],
    Max_nb_of_length = lists:max([length(Acc_maxlength), length(Line_lengths)]),
    Line_lengths_prepared = adjust_list (Line_lengths, Max_nb_of_length, 0),
    Acc_maxlength_prepared = adjust_list(Acc_maxlength, Max_nb_of_length, 0),
    Two_lengths =lists:zip(Line_lengths_prepared, Acc_maxlength_prepared),
    [ lists:max([A, B]) || {A, B} <- Two_lengths].

adjust_list(L, Desired_length, Elem) ->
    L++lists:duplicate(Desired_length - length(L), Elem).

prepare_line(Words_line, Words_length, Alignment) ->
    All_words = adjust_list(Words_line, length(Words_length), ""),
    Zipped = lists:zip (All_words, Words_length),
    [ apply(string, Alignment, [Word, Length-1, $\s]) %WRONG
    %[ apply(string, Alignment, [Word, Length + 1, $\s]) %RIGHT
      || {Word, Length} <- Zipped].
      
      
%% Tests %%
prepare_last_line_test() ->
	?assertNotEqual(prepare_line([[106,
               117,
               115,
               116,
               105,
               102,
               105,
               101,
               100,
               44],
              [114, 105, 103, 104, 116],
              [106, 117, 115, 116, 105, 102, 105, 101, 100, 44],
              [111, 114],
              [99, 101, 110, 116, 101, 114],
              [106, 117, 115, 116, 105, 102, 105, 101, 100],
              [119, 105, 116, 104, 105, 110],
              [105, 116, 115],
              [99, 111, 108, 117, 109, 110, 46]],
             [10, 10, 10, 6, 6, 9, 10, 8, 7, 7, 6, 4],
             left), [[106, 117, 115, 116, 105, 102, 105, 101, 100],
 [114, 105, 103, 104, 116, 32, 32, 32, 32],
 [106, 117, 115, 116, 105, 102, 105, 101, 100],
 [111, 114, 32, 32, 32],
 [99, 101, 110, 116, 101],
 [106, 117, 115, 116, 105, 102, 105, 101],
 [119, 105, 116, 104, 105, 110, 32, 32, 32],
 [105, 116, 115, 32, 32, 32, 32],
 [99, 111, 108, 117, 109, 110],
 [32, 32, 32, 32, 32, 32],
 [32, 32, 32, 32, 32],
 [32, 32, 32]]).


