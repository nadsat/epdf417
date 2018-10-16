-module(epdf417).
-on_load(init/0).

%% API exports
-export([raw_code/1]).
-export([png/1]).

-define(APPNAME, epdf417).
-define(LIBNAME, epdf417).
-define(BLACK, {0,0,0}).
-define(WHITE, {255,255,255}).

%%====================================================================
%% API functions
%%====================================================================

raw_code (_)->
  not_loaded(?LINE).

png (Text)->
  {ok, Payload} = raw_code(Text),
  Raw = proplists:get_value("raw_data",Payload),
  DataLen = proplists:get_value("len_data",Payload),
  Columns = proplists:get_value("len_columns",Payload),
  Width = Columns*8,
  Heigth = proplists:get_value("len_rows",Payload),
  ColorMode = indexed,
  Bits = 8,
  Palette = {ColorMode, Bits, [?BLACK,?WHITE]},
  PngConfig = #png_config{size = {Width, Height},
                          mode = {indexed, 8}},
  Rows = get_png_rows(Columns, Raw),
  Data = {rows, Rows},
  IoData = [png:header(),
            png:chunk('IHDR', PngConfig),
            png:chunk('PLTE', Pallette),
            png:chunk('IDAT', Data),
            png:chunk('IEND')],
  {ok, IoData}.

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

get_png_rows(Cols,Data) ->
  get_png_rows(Cols,Data,[]).


get_png_rows(Cols, <<>>, L) ->
  lists:reverse(L);
get_png_rows(Cols, Data, Acc) ->
  {Row, Rest} = get_row(Cols, Data),
  get_png_rows(Cols, Rest, [Row|Acc]).

get_row(Cols, Data) ->
  get_row(Cols, Data,[] ).

get_row(0, Rest, Row) ->
  R = lists:reverse(Row),
  {R, Rest};
get_row(Col, Data, P) ->
  << B:8, Rest/binary >> = Data,
  P = get_pixels(B)
  get_row(Col-1, Rest, Acc).

get_pixels(B) ->
  get_pixels(B, []).

get_pixels(<<>>, L) ->
  lists:reverse(L);
get_pixels(Bits, L) ->
  <<B:1, Rest/bitstring>> = Bits,
  C = get_color(B),
  get_pixels(Rest, [C|L]).

get_color(0) ->
  <<0>>;
get_color(1) ->
  <<1>>.
