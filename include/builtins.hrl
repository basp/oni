%%%----------------------------------------------------------------------------
%%% @author Bas Pennings [http://themeticulousgeek.com/]
%%% @copyright 2013 TMG
%%% @end
%%%----------------------------------------------------------------------------

%% Builtin properties
-define(NAME,               <<"name">>).
-define(LOCATION,           <<"location">>).
-define(PARENT,             <<"parent">>).
-define(OWNER,              <<"owner">>).
-define(PLAYER,             <<"player">>).
-define(WIZARD,             <<"wizard">>).
-define(PROGRAMMER,         <<"programmer">>).
-define(READ,               <<"r">>).
-define(WRITE,              <<"w">>).
-define(FERTILE,            <<"f">>).

%% Flags for (some) builtin properites
-define(WIZARD_FLAG,        2#100000).
-define(PROGRAMMER_FLAG,    2#010000).
-define(READ_FLAG,          2#001000).
-define(WRITE_FLAG,         2#000100).
-define(FERTILE_FLAG,       2#000010).
-define(PLAYER_FLAG,        2#000001).