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
-export([compile/0]).

%%------------------------------------------------------------------------------
%% @doc Compiles all the files of the Erlang Declarative Debugger (edd) and 
%% generates the edoc documentation.
%% @end
%%------------------------------------------------------------------------------
-spec compile() -> ok.
compile() ->
  comp_aux( 'src/smerl.erl' ),
  comp_aux( 'src/edd_lib.erl' ),
  comp_aux( 'src/edd.erl' ),
  comp_aux( 'src/edd_zoom_lib.erl'),
  comp_aux( 'src/edd_zoom.erl'),
  edoc:files(['src/edd.erl','src/edd_lib.erl','src/smerl.erl', 'edd_comp.erl',
  'src/edd_zoom.erl', 'src/edd_zoom_lib.erl'],[{dir,doc}]).

comp_aux( File ) ->
  case compile:file( File, [{outdir, ebin}]) of
    {ok, _} -> ok;
    Error   -> io:format("Error compiling ~p:~n~p~n", [File, Error])
  end,
  ok.
