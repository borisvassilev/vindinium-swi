This is a simple bot for http://vindinium.org/ implemented in SWI-Prolog.

This works with SWI-Prolog 7 or later, with HTTP support installed.
For most Linux distributions, this means it is easiest to build SWI-Prolog from source. See http://www.swi-prolog.org/build/ for details.

The main program is contained in a single file, `vindinium.pl`. Currently, there is a single, ``greedy'' bot implemented in `greedy.pl`. The user key is read from a plain text file containing nothing but the key, called by default `userkey` (you can change this: see the first line of `vindinium.pl`).

To run the bot, start SWI-Prolog in the directory with the three files, and load the program:

~~~
$ swipl
Welcome to SWI-Prolog (Multi-threaded, 64 bits, Version 7.1.23)
Copyright (c) 1990-2014 University of Amsterdam, VU Amsterdam
SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software,
and you are welcome to redistribute it under certain conditions.
Please visit http://www.swi-prolog.org for details.

For help, use ?- help(Topic). or ?- apropos(Word).

?- [vindinium].
true.
~~~

At that point, you can either fight at the arena:

~~~
?- fight.
~~~

... or train:

~~~
?- train([turns=50]).
~~~

You can leave the list of options empty, or supply number of turns and a map (see the Vindinium documentation).

The bot implemented in `greedy.pl` is... greedy. He will always rush to the closest gold mine not owned by him, and try to take it. He will get rich or die trying.

If everything goes well, both `fight/0` and `train/1` should succeed once the game has finished. Failure most probably indicates an error (a bug in the program) that I haven't noticed yet.

Enjoy!
