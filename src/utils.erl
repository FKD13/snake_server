-module(utils).

%% API
-export([droplast/1, coords_to_list/1]).

droplast([]) -> [];
droplast(A) -> lists:droplast(A).

coords_to_list(L) -> lists:map(fun({X, Y}) -> [X, Y] end, L).