-module(visualization).

-include("map_elites.hrl").

-export([static/2]).

%% Issue a plot command to GNUPlot
command(GNUPlot, Command) ->
    GNUPlot ! {self(), {command, Command}}.

to_data_string([]) -> [];
to_data_string([Row|Rest]) -> 
    to_row_string(Row) ++ to_data_string(Rest).

to_row_string([]) -> "\n";
to_row_string([[X, Y, Z]|Rest]) -> 
    [ZStr] = io_lib:format("~.10f", [Z]),
    integer_to_list(X) ++ " " ++ integer_to_list(Y) ++ " " ++ ZStr ++ "\n"
	++ to_row_string(Rest).

send_data(GNUPlot, Data) ->
    command(GNUPlot, to_data_string(Data)),
    command(GNUPlot, "e\n").

plot_map(GNUPlot, Data) ->
    command(GNUPlot, <<"plot '-' with image\n">>),
    send_data(GNUPlot, Data),
    ok.

%%% This is sort of like the "jet" palette found here:
%%% https://github.com/Gnuplotting/gnuplot-palettes/blob/master/jet.pal
set_palette(jet, GNUPlot) ->
    command(GNUPlot, <<"set palette defined (0 '#000080', 12 '#0000ff', 24 '#0080ff', 36 '#00ffff', 48 '#80ff80', 60 '#ffff00', 72 '#ff8000', 84 '#ff0000', 100 '#800000', 101 '#3f3f3f')\n">>).

get_fitness_range(Callbacks, Map) ->
    {[{_, Phenotype}], _} = ets:match_object(Map, {['_', '_'], '$1'}, 1),
    Init = Callbacks:objective(Phenotype),
    ets:foldl(fun({[_, _], P}, {Min, Max}) ->
		      Obj = Callbacks:objective(P),
		      if Obj > Max ->			  
			      {Min, Obj};
			 Obj < Min ->
			      {Obj, Max};
			 (Obj =< Max) and (Obj >= Min)->
			      {Min, Max}
		      end;
		 (_, Acc) -> Acc %% Ignore extra data stored in the table
	      end, {Init, Init}, Map).

map_to_list(Callbacks, 
	    #mape{map=Map, granularity=Granularity},
	    UndefinedValue) ->
    [[case ets:lookup(Map, [X, Y]) of
	  [{[X, Y], Phenotype}] ->
	      [X, Y, Callbacks:objective(Phenotype)];
	  [] -> [X, Y, UndefinedValue]
      end 
      || Y <- lists:seq(0, Granularity-1)] 
     || X <- lists:seq(0, Granularity-1)].

num_to_list(Num) when is_integer(Num) ->
    integer_to_list(Num);
num_to_list(Num) when is_float(Num)   ->
    io_lib:format("~.10f", [Num]).

static(Callbacks, M=#mape{map=Map, granularity=Granularity}) ->
    io:format("VIZ: building static viz~n"),
    GNUPlot = open_port({spawn, "gnuplot"}, []),
    {MinFitness, MaxFitness} = get_fitness_range(Callbacks, Map),
    G = integer_to_list(Granularity - 1),
    Min = num_to_list(MinFitness),
    Max = num_to_list(MaxFitness),
    command(GNUPlot, "set xrange [0:"++G++"]\n"),
    command(GNUPlot, "set yrange [0:"++G++"]\n"),
    command(GNUPlot, "set cbrange ["++Min++":"++Max++"]\n"),
    set_palette(jet, GNUPlot),
    io:format("VIZ: converting map to list~n"),
    FullMap = map_to_list(Callbacks, M, MaxFitness + 10),
    plot_map(GNUPlot, FullMap).
