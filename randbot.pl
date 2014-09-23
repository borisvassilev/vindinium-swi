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

:- dynamic w/2.             % accessible tiles
:- dynamic tavern/2.        % taverns
:- dynamic hero_spawn/2.    % spawn location of each hero
:- dynamic gold/2.          % gold mines

clean_db :-
    retractall(w(_,_)),
    retractall(tavern(_,_)),
    retractall(hero_spawn(_,_)),
    retractall(gold(_,_)).

bot_init(Game, walking_around) :-
    clean_db,
    atom_chars(Game.game.board.tiles, Tiles),
    length(Tiles, Len),
    format("Size ~d, length ~d~n", [Game.game.board.size, Len]),
    phrase(tiles(Game.game.board.size, 0), Tiles).

% The tiles are initially organized into a vector by rows.
% Coordinates start at 0 and go up to Size - 1 (Last).
% X grows to the south and Y grows to the east.
% Each tile contains two chars.
tiles(Size, I) -->
    [C], !,
    {   X is I div Size,
        Y is I mod Size,
        I1 is I + 1
    },
    tile(C, X, Y),
    tiles(Size, I1).
tiles(_, _) --> [].

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

% Tile A lies to the direction D of tile B.
n(X, Y1, 'East', X, Y) :- succ(Y, Y1).
n(X, Y, 'West', X, Y1) :- succ(Y, Y1).
n(X, Y, 'North', X1, Y) :- succ(X, X1).
n(X1, Y, 'South', X, Y) :- succ(X, X1).

:- use_module(library(random)).

% Walk around randomly. Ignore other heroes.
bot_move(walking_around, Game, walking_around, Dir) :-
    HX = Game.hero.pos.x,
    HY = Game.hero.pos.y,
    findall(D, (n(X, Y, D, HX, HY), once(good_tile(Game, X, Y))), Dirs),
    (   Dirs = [] % all paths are blocked
    ->  Dir = 'Stay'
    ;   random_member(Dir, Dirs)
    ).

% good_tile(+Game, +X, +Y)
% Decide if tile at X, Y is a good move depending on Game state
%
% The hero is next to a tavern and he can use and afford a drink
good_tile(Game, X, Y) :-
    tavern(X, Y),
    Game.hero.life < 75,
    Game.hero.gold >= 2,
    format("You could have a drink at tavern ~d, ~d~n", [X, Y]).
% The hero is next to a gold mine that does not belong to him,
% and has enough health to fight for it
good_tile(Game, X, Y) :-
    gold(X, Y),
    atom_number(Hero, Game.hero.id),
    Before is 2 * (X * Game.game.board.size + Y) + 1,
    sub_atom(Game.game.board.tiles, Before, 1, _, Mine),
    Mine \== Hero,
    Game.hero.life > 20,
    format("You could take mine owned by ~a at ~d, ~d~n", [Mine, X, Y]).
% Just walk around aimlessly.
good_tile(_Game, X, Y) :-
    w(X, Y).

