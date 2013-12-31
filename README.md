## Oni
Oni is an experiment to see how feasible it is to write a LambaMOO clone in Erlang. The aim of Oni is to depend on Erlang/OTP infrastructure as much as possible to enable most if not not all of the world to be written in those terms. 

Currently, everything is pure Erlang. There is no plan to implement a custom programming language like there is in LambdaMOO. Athough we do share a lot of common concepts and a bit of API with LambdaMOO, the whole implementation is completely different. Things like `fork` and `suspend` work completely differently in Oni. In fact, we don't actually have those concepts. In Oni, those are handled with continuations.

Thanks to the fact that Erlang itself comes with a lot of tools especially suited to our purpose we can delegate a lot of the infrastructure to the Erlang scheduler and make use of a lot of well suited libraries to implement a LambdaMOO-like server. Maybe it's more like a MFO (Mud-Functional-Oriented) but Oni sounds nicer.

## Setup
Compile everything from the `src` directory to the `ebin` directory with either the `make.ps1` file (if you are on Windows and have PowerShell) or by utilizing some form of `erlc` and some or your own shell magic (`make.ps1` is a one line script, even if you don't know PowerShell you should be able to understand what it does).

Next, startup Erlang referencing the `ebin` directory using `erl -pa ebin` (or `werl -pa ebin` if you are on Windows).

## Running
In the Erlang shell, type `oni:init().` and all the moving parts of Oni will be initialized. 

    1> oni:init().
    ok
    2>

Anything else but a simple `ok` means that something went horribly wrong. If you get the `ok` though that means that Oni is running. The default `listener` port is `7777` so boot up your favorite telnet client and connect to this port.

    Oni [Little Nugget]

    Welcome! Please login.
    That doesn't seem right.

Note: I'm using Mudlet and after connecting it immediately sends a bunch of garbage (probably capability info) to the server. Oni doesn't really recognize this as valid input so that's why we get the `That doesn't seem right.` output.

During the call to `oni:init` we initialized one player object called `Wizard`. We can connect to this object:

    > connect Wizard
    *** Connected (Wizard) ***

At this point we are connected. The Oni world is not very rich yet and there are no verbs to execute but we can evaluate Erlang code by prefixing the Erlang expression list with a semicolon:

    > ;[1,2,3] ++ [foo, {1 + 3, quux}, xazar].
    => [1,2,3,foo,{4,quux},xazar]

But we are not limited to simple expressions though. We can use te Oni object database to create a new player:

    > ;object:create(nothing).
    => 3

In this case the server will respond with `3`, the id of our newly created object. We can make this object a player object:

    > ;object:set_player_flag(3, true).
    => ok

But we need to set a name too otherwise we could never login:

    > ;object:set_property(3, <<"name">>, "Mistress").
    => ok

At this point you can boot up another telnet client and `connect Mistress`. Also, you can boot up the Erlang table viewer with `tv:start()` and inspect the ETS `connections` table to see the active connections and/or the Mnesia `object` table to examine all the objects that Oni knows about. This will contain both the `Wizard` and `Mistress` objects too.