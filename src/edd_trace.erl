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
%%% @author Salvador Tamarit <stamarit@dsic.upv.es>
%%% @copyright 2013 Salvador Tamarit
%%% @version 0.1
%%% @doc Erlang Declarative Debugger tracer
%%% @end
%%%-----------------------------------------------------------------------------
-module(edd_trace).

-export([trace/2]).

trace(Call,Timeout) -> 
  Self = self(),
	Pid = execute_call(Call,Self),
  dbg:tracer(process,{fun handler/2, Self}),
  %dbg:tracer(),
  dbg:p(Pid,[p,sos,m]),
  %dbg:tp(tcp,cx),
  %dbg:tpl(tcp,cx),
  Pid!start,
  receive 
    {result,Result} ->
      %ok
      io:format("Result: ~p\n",[Result])
  after 
    Timeout ->
      io:format("Tracing timeout\n")
  end,
  dbg:stop_clear(),
  Traces0 = lists:reverse(receive_loop([])),
  Trace = clean_trace(Traces0,[Pid,Self],[],1),
  %io:format("~p\n",[Trace]),
  Dict = dict:new(),
  NDict = separate_by_pid(Trace,Dict),
  % FirstPid = 
  %   case Trace of 
  %     [{trace,1,FirstPid_,_,_}|_] ->
  %       FirstPid_;
  %     [{trace,1,FirstPid_,_,_,_}|_] ->
  %       FirstPid_;
  %     _ ->
  %       no_pid
  %   end,
  [exit(Key, kill) || Key <- dict:fetch_keys(NDict)],
  %io:format("~p\n",[dict:to_list(NDict)]),
  %io:format("First PID: ~p\n",[FirstPid]),
  %io:format("First PID: ~p\n",[Pid]),
  {{first_pid,Pid},{traces,NDict}}.

handler(M,Pid) ->
  Pid!M,
  Pid.

receive_loop(Res) ->
	receive
		M -> 
			%io:format("~p\n",[M]),
			receive_loop([M|Res])
	after 
		100 -> Res
	end.

clean_trace([],_,Res,_) ->
  Res;
clean_trace([{trace,Main,'receive',start}|T],Pids = [Main,_],Acc,CurrentId) ->
  clean_trace(T,Pids,Acc,CurrentId);
clean_trace([{trace,Main,send,{result,_},Parent}|T],Pids = [Main,Parent],Acc,CurrentId) ->
  clean_trace(T,Pids,Acc,CurrentId);
clean_trace([{trace,Parent,_,_}|T],Pids = [_,Parent],Acc,CurrentId) ->
  clean_trace(T,Pids,Acc,CurrentId);
clean_trace([{trace,Parent,_,_,_}|T],Pids = [_,Parent],Acc,CurrentId) ->
  clean_trace(T,Pids,Acc,CurrentId);
clean_trace([{trace,_,'receive',{code_server,_}}|T],Pids,Acc,CurrentId) ->
  clean_trace(T,Pids,Acc,CurrentId);
clean_trace([{trace,_,send,{io_request,_,_,_},_}|T],Pids,Acc,CurrentId) ->
  clean_trace(T,Pids,Acc,CurrentId);
clean_trace([{trace,_,'receive',{io_reply,_,ok}}, {trace,_,'receive',timeout} |T],Pids,Acc,CurrentId) ->
  clean_trace(T,Pids,Acc,CurrentId);
clean_trace([{trace,_,'receive',{io_reply,_,ok}} |T],Pids,Acc,CurrentId) ->
  clean_trace(T,Pids,Acc,CurrentId);
clean_trace([{trace,Pid,Info1,Info2}|T],Pids,Acc,CurrentId) ->
  clean_trace(T,Pids, Acc ++ [{trace,CurrentId,Pid,Info1,Info2}],CurrentId + 1);
clean_trace([{trace,Pid,Info1,Info2,Info3}|T],Pids,Acc,CurrentId) ->
  clean_trace(T,Pids, Acc ++ [{trace,CurrentId,Pid,Info1,Info2,Info3}],CurrentId + 1).

separate_by_pid([],Dict) ->
  Dict;
separate_by_pid([Trace = {trace,_,Pid,_,_}|T],Dict) ->
  NDict = get_new_dict(Pid,Dict,Trace),
  separate_by_pid(T,NDict);
separate_by_pid([Trace = {trace,_,Pid,_,_,_}|T],Dict) ->
  NDict = get_new_dict(Pid,Dict,Trace),
  separate_by_pid(T,NDict).


get_new_dict(Pid,Dict,Trace) ->
  case dict:find(Pid,Dict) of
    {ok, PidTrace} ->
      dict:store(Pid, PidTrace ++ [Trace], Dict);
    error ->
      dict:store(Pid, [Trace], Dict) 
  end.

execute_call(Call,PidParent) ->
	M1 = smerl:new(foo),
	{ok, M2} = 
	  smerl:add_func(M1, "bar() ->" ++ Call ++ " ."),
	smerl:compile(M2,[nowarn_format]),
	spawn(fun() -> receive start -> ok end,Res = foo:bar(), PidParent!{result,Res} end).
