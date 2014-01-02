## Oni
Oni is an experiment to see how feasible it is to write a LambaMOO clone in Erlang. The aim of Oni is to depend on Erlang/OTP infrastructure as much as possible to enable most if not not all of the world to be written in those terms. 

Currently, everything is pure Erlang. There is no plan to implement a custom programming language like there is in LambdaMOO. Athough we do share a lot of common concepts and a bit of API with LambdaMOO, the whole implementation is completely different. Things like `fork` and `suspend` work completely differently in Oni. In fact, we don't actually have those concepts because they are built into the Erlang VM. In Oni, those are handled with continuations and the use of Erlang functions and processes.

## Important
Currently there are no checks on who can evaluate expressions directly so every player can evaluate Erlang code with the `;` prefix. Keep this in mind if you put the listener on an public open port. Oni doesn't really care about flags or permissions just yet but it will eventually.

Also, Oni is not very persistent yet. You might be able to backup the Mnesia database (especially the `object` table) or dump it in some way but there are no guarantees about compatibility. If you are gonna create something based on Oni, make sure you can recreate it with Erlang code using the Oni API. Also, don't plan to let any random players in just yet and make sure you trust anyone you allow to hack around using the `;` prefix.

## Just Remember this

    * Don't put this on a public port
    * Only allow trusted people to hack with `;`
    * The database will not be compatible going forward
    * That means you need to utilize that API
    * And also that you cannot invite players
    * The world will not be persisted beyond your initialization scripts.

## Building
Just use the `make.ps1` file if you are on Windows and have PowerShell. If not, make sure everything from the `src` directory is compiled to the `ebin` directory using your method of choice.

## Running
In the Erlang shell, type `oni:start().` and all the moving parts of Oni will be initialized (note that this will also start Mnesia):

    1> oni:start().
    ok
    2>

Anything else but a simple `ok` means that something went horribly wrong. If you get the `ok` though that means that Oni is running. The default `listener` port is `7777` so boot up your favorite telnet client and connect to this port.

    Oni [Little Nugget]

    Welcome! Please login.
    That doesn't seem right.

Note: I'm using Mudlet and after connecting it immediately sends a bunch of garbage (probably capability info) to the server. Oni doesn't really recognize this as valid input so that's why we get the `That doesn't seem right.` output.

During the call to `oni:start` we initialized one player object called `Wizard`. We can connect to this object:

    > connect Wizard
    *** Connected (Wizard) ***

At this point we are connected. The Oni world is not very rich yet and there are no verbs to execute but we can evaluate Erlang code by prefixing the Erlang expression list with a semicolon:

    > ;[1,2,3] ++ [foo, {1 + 3, quux}, xazar].
    => [1,2,3,foo,{4,quux},xazar]

But we are not limited to simple expressions though. We can use te Oni object database to create a new player (here, `nothing` is the parent object but Oni doesn't do anything with this yet):

    > ;object:create(nothing).
    => 3

In this case the server will respond with `3`, the id of our newly created object. We can make this object a player object:

    > ;object:set_player_flag(3, true).
    => ok

But we need to set a name too otherwise we could never login:

    > ;object:set_property(3, <<"name">>, "Mistress").
    => ok

At this point you can boot up another telnet client and `connect Mistress`.

### Inspecting Oni state
For fun, try executing `;tv:start().` from your telnet client (when connected). This will bootup the table viewer on the server! You can use the table viewer to view the ETS `connections` table that holds all the active players and their connections. Or you can use it to view the Mnesia `object` table to view all objects Oni knows about.

## Basic verbs
Oni supports a few basic verbs (in the `test` module) that you can use right now. If you are in the same room as another player you can `say` stuff:

    > say How are you doing?
    Wizard asks, "How are you doing?"

There is also a shortcut with the `'' prefix:

    > 'Looking good!
    Wizard exclaims, "Looking good!"
    > 'Glad to hear that.
    Wizard says, "Glad to hear that."

Note that the `say` command checks whether you're asking, exclaiming or just saying something depending on the final character of your message.

You can also `emote` things:

    > :grins and dances around the room, "we can do this!"
    Wizard grins and dances around the room, "we can do this!"

Or:
    
    > emote smiles, "hell yeah!"
    Mistress smiles, "hell yeah!"

It works like `say` but it doesn't prefix the output like a quotation.

### Moving stuff
You can easily move an `object` to any other `object` using the `object:move(Target)` function. If you created another player (and assuming Wizard is still in the genesis room) you can move this player to that room. But what is the id of that room? We can find out quite easily. From the Wizard telnet terminal execute the follwing:

    > ;object:get_property(2, <<"location">>).
    => 1

We can be 200% sure by executing the `object:contents` function:

    > ;object:contents(1).
    => [2]

The object with id `#2` is Wizard and that should be us.

So now we know that the location of Wizard is #1 or "The First Room" (we set this up in the `oni_app:genesis` function). We can move any other player to this room. Let's assume Mistress has object id `#3`. We can move her to Wizard's room like this:

    > ;object:move(3, 1).

Or even:
    
    > ;object:move(3, object:get_property(2, <<"location")).

It's unfortunate we need to use _magic numbers_ for now but Oni will support a more reasonable alternative soon. The design is just not very polished yet. We can double check that everybody is here by executing the `object:contents` function again:

    > ;object:contents(1).
    => [2, 3]

And now we have two people here. If you are connected to both players (using two telnet sessions) you should be able to `say` stuff and use Oni like a simple chat server. 

## Future Ideas
Of course it doesn't have to stay a simple chat server. You can create other rooms and make exits from one room to another. Each exit could take some time to traverse (like in HellMOO). This could be supported by the `aq` (action queue) that is not yet fully stable but almost ready. We also need to implement combat and crafting stuff and who knows what will happen if two players fight the same creature? What problems will arise? 