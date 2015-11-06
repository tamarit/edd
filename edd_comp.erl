%%%    Copyright (C) 2013 Enrique Martin-Martin <emartinm@fdi.ucm.es>
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
%%% @author Enrique Martin-Martin <emartinm@fdi.ucm.es>
%%% @copyright 2013 Enrique Martin-Martin
%%% @version 0.1
%%% @doc Erlang Declarative Debugger installer.
%%% @end
%%%-----------------------------------------------------------------------------

-module(edd_comp).
-export([compile/0, load/0]).

files() ->
  [
    "edd","edd_lib","smerl"
  , "edd_zoom", "edd_zoom_lib"
  , "edd_con", "edd_con_lib", "edd_trace", "edd_tcp"
  , "edd_server", "edd_client", "edd_jserver"
  , "edd_test_reader", "edd_test_writer"
  , "edd_con_pt"
  % TO BE REMOVED WHEN INTEGRATED
  , "edd_trace_new"
  , "mochijson", "smerl"
  ].

%%------------------------------------------------------------------------------
%% @doc Compiles all the files of the Erlang Declarative Debugger (edd) and 
%% generates the edoc documentation.
%% @end
%%------------------------------------------------------------------------------
-spec compile() -> ok.
compile() ->
  AtomizedFiles = 
    [list_to_atom("src/" ++ F ++ ".erl") 
     || F <- files()],
  lists:map(fun comp_aux/1, AtomizedFiles),
  edoc:files(['edd_comp.erl' | AtomizedFiles], [{dir,doc}]).

%%------------------------------------------------------------------------------
%% @doc Load all the files of the Erlang Declarative Debugger (edd).
%% @end
%%------------------------------------------------------------------------------
-spec load() -> ok.
load() ->
  [code:load_abs("ebin/" ++ F) || F <- files()],
  ok.

comp_aux( File ) ->
  case compile:file( File, [{outdir, ebin}]) of
    {ok, _} -> ok;
    Error   -> io:format("Error compiling ~p:~n~p~n", [File, Error])
  end,
  ok.
