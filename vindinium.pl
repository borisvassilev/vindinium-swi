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
key_filename(userkey). % change if necessary
:- dynamic key/1.
:- initialization(set_key).

set_key :-
    abolish(key/1),
    key_filename(Filename),
    setup_call_cleanup(
        open(Filename, read, File),
        read_string(File, "\n", "\n",  _, Key),
        close(File)
    ),
    atom_string(Key_atom, Key),
    assertz(key(Key_atom)).

game_url(training, "http://vindinium.org/api/training").
game_url(arena, "http://vindinium.org/api/arena").

train(Options) :-
    game_url(training, Url),
    play(Url, Options).
fight :-
    game_url(arena, Url),
    play(Url, []).

:- use_module(library(www_browser)).
play(Url, More_options) :-
    key(Key),
    init_game(Url, [key=Key|More_options], Game, State_of_mind),
    format("~w~n", [Game.viewUrl]),
    www_browser:www_open_url(Game.viewUrl),
    take_turn(State_of_mind, Game).


:- use_module(library(http/http_client)).
:- use_module(library(http/json)).
:- use_module('greedy.pl'). % select your bot

init_game(Url, Form, Game, State_of_mind) :-
    % wait 10 min, time is in seconds
    Wait is 10 * 60,
    http_client:http_post(Url, form(Form), Reply, [timeout(Wait)]),
    json:atom_json_dict(Reply, Game, []),
    bot:init(Game, State_of_mind).

take_turn(State_of_mind, Game) :-
    (   Game.game.finished == false
    ->  bot:move(State_of_mind, Game, New_state_of_mind, Dir),
        key(Key),
        http_client:http_post(
            Game.playUrl,
            form([key=Key, dir=Dir]),
            Reply,
            % keep alive, wait 15 sec
            [connection('Keep-Alive'), timeout(15)]
        ),
        json:atom_json_dict(Reply, Game_next, []),
        take_turn(New_state_of_mind, Game_next)
    ;   http_client:http_disconnect(all)
    ).

