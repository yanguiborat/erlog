%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc erlog standart library interface
%%%
%%% @end
%%% Created : 12. Авг. 2014 18:31
%%%-------------------------------------------------------------------
-module(erlog_stdlib).
-author("tihon").

-include("erlog.hrl").
-include("erlog_core.hrl").

%% load database to kernel space
-callback load(Db :: #db_state{}) -> #db_state{}.

%% proves goal Goal
-callback prove_goal(Params :: #param{}) -> ok. %TODO what return value?