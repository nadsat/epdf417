-module(epdf417).
-on_load(init/0).

%% API exports
-export([raw_code/1]).
-export([png/1]).

-define(APPNAME, epdf417).
-define(LIBNAME, epdf417).

%%====================================================================
%% API functions
%%====================================================================

raw_code (_)->
  not_loaded(?LINE).

png (_)->
  not_loaded(?LINE).
%%====================================================================
%% Internal functions
%%====================================================================
init() ->
    SoName = case code:priv_dir(?APPNAME) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, ?LIBNAME]);
                _ ->
                    filename:join([priv, ?LIBNAME])
            end;
        Dir ->
            filename:join(Dir, ?LIBNAME)
    end,
    erlang:load_nif(SoName, 0).

not_loaded(Line) ->
    exit({not_loaded, [{module, ?MODULE}, {line, Line}]}).

