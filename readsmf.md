readsmf
=======

Read a MIDI file and output its content as text.


Usage
-----

It is a sort of interface to the `parse/1` function of the module
*ermidi*; hence beyond the CLI usage, indeed this text talks about
`ermidi:parse`.

To run `readsmf`, you first need to compile `ermidi.erl` into a BEAM
file:

    erlc ermidi.erl

Once you have `ermidi.beam`, you can run `readsmf` like this:

    ./readsmf path/to/a/midi/file.mid

and likely it will work (on GNU/Linux systems and alike), provided it
is executable and you are in the same folder where `ermidi.beam` is.


Why
---

It's easier to look at such a dump rather than at the output of
`hexdump -C` when you want to see what's inside a SMF that sounds
good.

Moreover I wanted to try the Erlang's binary syntax and this was an
opportunity.


Dump, no context
----------------

The `ermidi:parse/1` function *parses* the data (which likely is read
from a file) as they come. The only “status” which is passed around is
needed to handle the MIDI *running status* (hopefully well done, but
don't bet on it); beyond this, each *event* is on its own world.

This is the reason why the code can't prented to know if it is better
to call the same note `C#` rather than `Db` according to a *key
meta-event* parsed earlier. (Which could be wrong, by the way, if
whoever sequenced the file didn't pay attention to this detail.)

It means also that the program can't interpret deltas so to give a
better idea of when a note starts playing, nor it looks ahead to say
how long a note is.

Basically, it is a (dumb) plain dump of the events as they appear in
the file, with a little bit of interpretation of few informations when
it is feasable.


Bugs
----

The `ermidi` module is a quick-and-dirty code, where by *quick* I mean
I've done it as quick as it was possible for me, and by *dirty* I mean
I haven't designed it: I've built several “(allegedly) working
patches” over a small initial PoC test which parsed just the header.

During the development I have done several mistakes (due to my being
no more than an Erlang novice); then, trying the tool with some files
made me discover new issues, thus **expect problems** with the files
you want to try it with.


Future
------

This tool hasn't a future of its own. *Maybe* the `ermidi` module will
change, or part of its code will be replicated in new modules.

