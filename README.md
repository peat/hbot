### HIRC

This is a personal project, wherein I play around with building IRC bots and an IRC game with Haskell.

## Running

Clone this repo, and:

    $ cabal configure
    $ cabal build
    $ dist/build/HBot/HBot

In it's current configuration, this app will:

* Connect to irc.freenode.net as user 'HBot'
* Join channel '#hbot-test'
* Print out a hello world message 
* Echo other users' public messages sent to the channel

To change this behavior, check out the `src/Main.hs` file.

## Code

There are currently three files:

* `Main.hs` which contains the connection and message handling code.
* `Message.hs` which is a reasonably complete IRC message parser and generator.
* `Game.hs` which doesn't do much at the moment.

Have fun, and let me know if you have any questions!
