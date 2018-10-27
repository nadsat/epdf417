-module(epdf417).
-on_load(init/0).

-include_lib("png/include/png.hrl").

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
  Raw = proplists:get_value(raw_data,Payload),
  BitColumns = proplists:get_value(bit_columns,Payload),
  Width = BitColumns,
  Height = 3*proplists:get_value(len_rows,Payload),
  ColorMode = indexed,
  Bits = 8,
  Palette = {rgb, Bits, [?BLACK,?WHITE]},
  PngConfig = #png_config{size = {Width, Height},
                          mode = {ColorMode, 8}},
  Rows = get_png_rows(BitColumns, Raw),
  Data = {rows, Rows},
  IoData = [png:header(),
            png:chunk('IHDR', PngConfig),
            png:chunk('PLTE', Palette),
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
               timer:sleep(2000),
               filename:join(Dir, ?LIBNAME)
           end,
  erlang:load_nif(SoName, 0).

not_loaded(Line) ->
  exit({not_loaded, [{module, ?MODULE}, {line, Line}]}).

get_png_rows(Cols,Data) ->
  get_png_rows(Cols,Data,[]).


get_png_rows(Cols, Data, L) when bit_size(Data) < Cols ->
  lists:reverse(L);
get_png_rows(Cols, Data, Acc) when bit_size(Data) >= Cols->
  {Row, Rest} = get_row(Cols, Data),
  One = [Row|Acc],
  Two = [Row|One],
  Tree = [Row|Two],
  get_png_rows(Cols, Rest, Tree).

get_row(Cols, Data) ->
  get_row(Cols, Data,[] ).

get_row(0, Rest, Row) ->
  R = lists:reverse(Row),
  {R, Rest};
get_row(Col, Data, Acc) when Col < 8 ->
  Left = 8-Col,
  << B:Col, Rem/bitstring >> = Data,
  << _:Left, Rest/bitstring >> = Rem,
  P = get_pixels(<< B:Col >>),
  get_row(0 , Rest, [P|Acc]);
get_row(Col, Data, Acc) when Col >= 8 ->
  << B:8, Rest/binary >> = Data,
  %<< B:8, Rest/bitstring >> = Data,
  P = get_pixels(<<B>>),
  get_row(Col-8, Rest, [P|Acc]).

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
