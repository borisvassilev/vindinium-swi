% Copyright 2014, Boris Vassilev
%
% This file is part of vindinium-swi.
%
% Vindinium-swi is free software: you can redistribute it and/or modify
% it under the terms of the GNU General Public License as published by
% the Free Software Foundation, either version 3 of the License, or
% (at your option) any later version.
%
% Vindinium-swi is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
%
% You should have received a copy of the GNU General Public License
% along with vindinium-swi.  If not, see <http://www.gnu.org/licenses/>.
%
:- module(bot, [bot_init/2, bot_move/4]).

:- dynamic hero/1.          % hero's ID this game
:- dynamic w/2.             % accessible tiles
:- dynamic tavern/2.        % taverns
:- dynamic hero_spawn/2.    % spawn location of each hero
:- dynamic gold/2.          % gold mines

clean_db :-
    retractall(hero(_)),
    retractall(w(_,_)),
    retractall(tavern(_,_)),
    retractall(hero_spawn(_,_)),
    retractall(gold(_,_)).

bot_init(Game, deliberating) :-
    clean_db,
    assertz(hero(Game.hero.id)),
    atom_chars(Game.game.board.tiles, Tiles),
    phrase(tiles(Game.game.board.size, 0, 0), Tiles).

tiles(Size, X, Y) -->
    [C],
    tile(C, X, Y),
    {   next_xy(Size, X, Y, X1, Y1), !
    },
    tiles(Size, X1, Y1).
tiles(_, _, _) --> [].

next_xy(Size, X, Y, X, Y1) :- Y < Size - 1,
    succ(Y, Y1).
next_xy(Size, X, Y, X1, 0) :- Y =:= Size - 1,
    succ(X, X1).

tile('#', _, _) --> ['#'].
tile(' ', X, Y) --> [' '],
    {   assertz(w(X, Y))
    }.
tile('[', X, Y) --> [']'],
    {   assertz(tavern(X, Y))
    }.
tile('@', X, Y) --> [H],
    {   atom_number(H, N),
        assertz(hero_spawn(N, w(X, Y))),
        assertz(w(X, Y))
    }.
tile('$', X, Y) --> ['-'],
    {   assertz(gold(X, Y))
    }.

:- use_module(library(heaps)).
:- use_module(library(assoc)).

path_to_gold(Game, Path) :-
    not_my_gold_list(Game.game.board, Golds),
    HX = Game.hero.pos.x,
    HY = Game.hero.pos.y,
    maplist(gold_seed(HX, HY), Golds, Seeds),
    list_to_heap(Seeds, Openset),
    find_path(HX, HY, Openset, Path).

gold_seed(HX, HY, gold(X, Y), H-path(X, Y, [], Visited)) :-
    manhattan_distance(HX, HY, X, Y, H),
    empty_assoc(Visited).

manhattan_distance(X1, Y1, X2, Y2, D) :-
    D is abs(X1 - X2) + abs(Y1 - Y2).

not_my_gold_list(Board, Golds) :-
    hero(N),
    atom_number(Hero, N),
    findall(G, not_my_gold(Board.tiles, Board.size, Hero, G), Golds).

not_my_gold(Tiles, Size, Hero, gold(X, Y)) :-
    gold(X, Y),
    Before is 2 * (X*Size + Y) + 1,
    \+ sub_atom(Tiles, Before, 1, _, Hero).

find_path(HX, HY, Open0, Path) :-
    get_from_heap(Open0, _H, path(X, Y, Dirs, Visited), Open),
    (   HX == X, HY == Y
    ->  Path = Dirs
    ;   findall(n(NX, NY, Dir), n(X, Y, NX, NY, Dir), Ns),
        add_nodes(Ns, HX, HY, Dirs, Visited, Open, Open1),
        find_path(HX, HY, Open1, Path)
    ).

n(X, Y, X, NY, 'East') :- NY is Y - 1, w(X, NY).
n(X, Y, X, NY, 'West') :- NY is Y + 1, w(X, NY).
n(X, Y, NX, Y, 'North') :- NX is X + 1, w(NX, Y).
n(X, Y, NX, Y, 'South') :- NX is X - 1, w(NX, Y).

add_nodes([], _, _, _, _, Final, Final).
add_nodes([n(X, Y, Dir)|Rest], HX, HY, Dirs, Visited, Open, Final) :-
    (   get_assoc(w(X, Y), Visited, 0)
    ->  Open = Open1
    ;   put_assoc(w(X, Y), Visited, 0, Visited1),
        manhattan_distance(HX, HY, X, Y, H),
        length(Dirs, D),
        NH is D + 1 + H,
        add_to_heap(Open, NH, path(X, Y, [Dir|Dirs], Visited1), Open1)
    ),
    add_nodes(Rest, HX, HY, Dirs, Visited, Open1, Final).

:- use_module(library(random)).

bot_move(deliberating, Game, New_state_of_mind, Dir) :-
    (   path_to_gold(Game, [First|Rest])
    ->  New_state_of_mind = get_rich(Rest),
        Dir = First
    ;   bot_move(confused, Game, _, Dir),
        New_state_of_mind = deliberating
    ).

bot_move(get_rich(Path), Game, New_state_of_mind, Dir) :-
    (   Path = [Dir|Rest]
    ->  New_state_of_mind = get_rich(Rest)
    ;   bot_move(deliberating, Game, New_state_of_mind, Dir)
    ).

bot_move(confused, _, _, Dir) :-
    random_member(Dir, ['Stay', 'East', 'West', 'North', 'South']).

