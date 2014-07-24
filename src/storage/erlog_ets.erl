%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. июн 2014 18:00
%%%-------------------------------------------------------------------

-module(erlog_ets).

-behaviour(erlog_storage).

%% erlog callbacks
-export([new/0, new/1,
	add_built_in/2,
	add_compiled_proc/2,
	assertz_clause/2,
	asserta_clause/2,
	retract_clause/2,
	abolish_clauses/2,
	get_procedure/2,
	get_procedure_type/2,
	get_interp_functors/1,
	findall/2, %TODO remove me
	raw_store/2,
	raw_fetch/2,
	raw_append/2,
	raw_erase/2]).

new() -> {ok, ets:new(eets, [])}.

new(_) -> {ok, ets:new(eets, [])}.

add_built_in(Db, {Functor}) ->
	true = ets:insert(Db, {Functor, built_in}),
	{ok, Db}.

add_compiled_proc(Db, {{Functor, M, F}}) ->
	case ets:lookup(Db, Functor) of
		[{_, built_in}] ->
			erlog_errors:permission_error(modify, static_procedure, ec_support:pred_ind(Functor), Db);
		[_] -> ets:insert(Db, {Functor, code, {M, F}});
		[] -> ets:insert(Db, {Functor, code, {M, F}})
	end,
	{ok, Db}.

assertz_clause(Db, {Collection, Head, Body0}) ->
	Ets = ets_db_storage:get_db(Collection),
	{Res, _} = assertz_clause(Ets, {Head, Body0}),
	{Res, Db};
assertz_clause(Db, {Head, Body0}) ->
	clause(Head, Body0, Db,
		fun(Functor, Tag, Cs, Body) ->
			ets:insert(Db, {Functor, clauses, Tag + 1, Cs ++ [{Tag, Head, Body}]})
		end),
	{ok, Db}.

asserta_clause(Db, {Collection, Head, Body0}) ->
	Ets = ets_db_storage:get_db(Collection),
	{Res, _} = asserta_clause(Ets, {Head, Body0}),
	{Res, Db};
asserta_clause(Db, {Head, Body0}) ->
	clause(Head, Body0, Db,
		fun(Functor, Tag, Cs, Body) ->
			ets:insert(Db, {Functor, clauses, Tag + 1, [{Tag, Head, Body} | Cs]})
		end),
	{ok, Db}.

retract_clause(Db, {Collection, Functor, Ct}) ->
	Ets = ets_db_storage:get_db(Collection),
	{Res, _} = retract_clause(Ets, {Functor, Ct}),
	{Res, Db};
retract_clause(Db, {Functor, Ct}) ->
	case ets:lookup(Db, Functor) of
		[{_, built_in}] ->
			erlog_errors:permission_error(modify, static_procedure, ec_support:pred_ind(Functor), Db);
		[{_, code, _}] ->
			erlog_errors:permission_error(modify, static_procedure, ec_support:pred_ind(Functor), Db);
		[{_, clauses, Nt, Cs}] ->
			ets:insert(Db, {Functor, clauses, Nt, lists:keydelete(Ct, 1, Cs)});
		[] -> ok        %Do nothing
	end,
	{ok, Db}.

abolish_clauses(Db, {Collection, Functor}) ->
	Ets = ets_db_storage:get_db(Collection),
	{Res, _} = abolish_clauses(Ets, {Functor}),
	{Res, Db};
abolish_clauses(Db, {Functor}) ->
	case ets:lookup(Db, Functor) of
		[{_, built_in}] ->
			erlog_errors:permission_error(modify, static_procedure, ec_support:pred_ind(Functor), Db);
		[{_, code, _}] -> ets:delete(Db, Functor);
		[{_, clauses, _, _}] -> ets:delete(Db, Functor);
		[] -> ok        %Do nothing
	end,
	{ok, Db}.

findall(Db, {Collection, Functor}) ->
	Ets = ets_db_storage:get_db(Collection),
	Res = case findall(Ets, {Functor}) of
		      {[], _} -> [];
		      {[{_, Result, _}], _} -> Result
	      end,
	{Res, Db};
findall(Db, {Functor}) ->
	Params = tuple_to_list(Functor),
	Fun = hd(Params),
	Len = length(Params) - 1,
	case ets:lookup(Db, {Fun, Len}) of
		[{_, clauses, _, Body}] -> {Body, Db};
		[{_, code, Body}] -> {Body, Db};
		[{Body, built_in}] -> {Body, Db};
		[] -> {[], Db}
	end.

get_procedure(Db, {Collection, Functor}) ->
	Ets = ets_db_storage:get_db(Collection),
	{Res, _} = get_procedure(Ets, {Functor}),
	{Res, Db};
get_procedure(Db, {Functor}) ->
	{case ets:lookup(Db, Functor) of
		 [{_, built_in}] -> built_in;
		 [{_, code, C}] -> {code, C};
		 [{_, clauses, _, Cs}] -> {clauses, Cs};
		 [] -> undefined
	 end, Db}.

get_procedure_type(Db, {Functor}) ->
	{case ets:lookup(Db, Functor) of
		 [{_, built_in}] -> built_in;    %A built-in
		 [{_, code, _C}] -> compiled;    %Compiled (perhaps someday)
		 [{_, clauses, _, _Cs}] -> interpreted;  %Interpreted clauses
		 [] -> undefined        %Undefined
	 end, Db}.

get_interp_functors(Db) ->
	{ets:foldl(fun({_, built_in}, Fs) -> Fs;
		({Func, code, _}, Fs) -> [Func | Fs];
		({Func, clauses, _, _}, Fs) -> [Func | Fs]
	end, [], Db), Db}.

clause(Head, Body0, Db, ClauseFun) ->
	{Functor, Body} = case catch {ok, ec_support:functor(Head), ec_body:well_form_body(Body0, false, sture)} of
		                  {erlog_error, E} -> erlog_errors:erlog_error(E, Db);
		                  {ok, F, B} -> {F, B}
	                  end,
	case ets:lookup(Db, Functor) of
		[{_, built_in}] -> erlog_errors:permission_error(modify, static_procedure, ec_support:pred_ind(Functor), Db);
		[{_, code, _}] -> erlog_errors:permission_error(modify, static_procedure, ec_support:pred_ind(Functor), Db);
		[{_, clauses, Tag, Cs}] -> ClauseFun(Functor, Tag, Cs, Body);
		[] -> ets:insert(Db, {Functor, clauses, 1, [{0, Head, Body}]})
	end.

raw_store(Db, {Key, Value}) ->
	ets:insert(Db, {Key, Value}),
	{ok, Db}.

raw_fetch(Db, {Key}) ->
	Res = case ets:lookup(Db, Key) of
		      [] -> [];
		      [{_, Value}] -> Value
	      end,
	{Res, Db}.

raw_append(Db, {Key, AppendValue}) ->
	{Value, _} = raw_fetch(Db, {Key}),
	raw_store(Db, {Key, [AppendValue | Value]}),
	{ok, Db}.

raw_erase(Db, {Key}) ->
	ets:delete(Db, Key),
	{ok, Db}.