%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Июль 2014 11:19
%%%-------------------------------------------------------------------
-author("tihon").

-define(ERLOG_TIME,
	[
		{{localtime, 1}, ?MODULE, localtime_1},
		{{date_diff, 4}, ?MODULE, datediff_4},
		{{add_time, 4}, ?MODULE, add_time_4},
		{{date_print, 4}, ?MODULE, dateprint_2},
		{{date_parse, 4}, ?MODULE, dateparse_2},
		{{date, 2}, ?MODULE, date_2},
		{{date, 4}, ?MODULE, date_4},
		{{time, 2}, ?MODULE, time_2},
		{{time, 4}, ?MODULE, time_4}
	]).