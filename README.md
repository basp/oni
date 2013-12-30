## Oni
Oni is an experiment to see how feasible it is to write a LambaMOO server in Erlang. The aim of Oni is to depend on Erlang infrastructure as much as possible to enable most if not not all of the world to be written in Erlang terms.

### History
During development of Aki it became quite clear that a lot of effort was put into a somewhat compatible LambdaMOO VM. To that end, Aki was built around a byte code interpreter that was heavily inspired by the original by Pavel Curtis. All this effort was quite interresting from an implementation point of view but did raise the question: isn't there an easier way? 

### Vision
It turns out there might be an easier way. The main use case for implementing a custom VM is to be able to load and change code while the server is running. We want to have people building the game and people playing the game at the same time. Our MOO would be useless if we would have to stop and restart the server for every code change. Erlang is quite capable of changing code on the fly.

LambdaMOO is also attractive for game programming because it features a very solid object oriented programming environment that is quite elegant. It shouldn't be too hard to provide similar object features with a functional language engine. Especially not with Erlang and Mnesia for storage.

## What's Here
Oni is not really useful yet but you can boot up the server and have some kind of interaction with it. The things that are implemented right now are basically there to get a feel for how to design and implement the fundamental interactions between the main components of the system (player service, object database and tasks). 

### Setup
Note that everything below assumes you are using PowerShell. If you are not, I'm quite sure you can figure this out anyway (the commands might not match but the procedure is still the same).

Make sure your <code>erl/bin</code> is in your path. The clone the code into a directory of your choice:

    > git clone https://github.com/basp/oni.git
    > cd oni

After that just run the <code>make.ps1</code> file:

    > .\make.ps1

The `make.ps1` is not that spectaculair. It compiles the files in `src` and outputs the `beam` files to `ebin` like this:

    > ls src | foreach { erlc -o ebin $_.fullname }

This is also why it's recommended to have `erl/bin` stuff in your path so you can access it easily. If you are on *nix you're probably safe but if you use Windows you might have to add this directory to your path manually.

After that it's just a simple matter of booting up Erlang:

    > werl -pa ebin

Or `erl -pa ebin` (if you are on *nix).

### Running
After launching the Erlang shell you can boot Oni with the `oni:init()` command. It should look something like this:


    Erlang R16B02 (erts-5.10.3) [smp:4:4] [async-threads:10]

    Eshell V5.10.3  (abort with ^G)
    1> oni:init().
    ok
    2>

Next, startup your favorite telnet client and connect to `127.0.0.1:7777` and the Erlang console should output this:

    =INFO REPORT==== 30-Dec-2013::21:37:38 ===
    Client {{127,0,0,1},52922} connected

    =INFO REPORT==== 30-Dec-2013::21:37:38 ===
    Login attempt with {[255,252,3,255,250,201,67,111,114,101,46,83,117,112,112,
                         111,114,116,115,46,83,101,116],
                        ["[]ÿð"],
                        "[]ÿð"} from {{127,0,0,1},52922}

You might not have the second `INFO REPORT`. That is just because my client (Mudlet) is sending some kind of garbage to the server. It might have meaning but I've seen other servers choke on it too so I'm ignoring it for now. I guess it is trying to send its client capabalities (color, MXP, etc). 

Anyway, if you get the `Client bla connected` output things are working as they should.

### Basic Interaction
You cannot do much at this point but you can login from your telnet client:

    > connect Wizard
    *** Connected (Wizard) ***

At this point you are in an echo server so everything you type will be printed back to you. To disconnect you can type `@quit`.

### Notes
Basic interaction might not seem like much but the login seems pretty solid. You can try to enter bullshit and it will print back some kind of meaningful message. Yes, there is a secuirty risk by having a different message for unknown command and unknown login results but it's convenient for debugging and we are not a security agency anyway.

Also, from a server point of view we can observe client states of connecting and disconnecting which are most important to us right now.

Next step is to have a runtime process that will execute '''verbs''' that are executed by players. Eventually we might be able to program the login sequence using Oni itself instead of the kind of hacks that we have now.

Oni uses an object database that is facilitated by Mnesia and consists of a single table. The structure of Oni objects can be found in the `include/records.hrl` file. It is really quite simple.

For now, we are storing Oni objects as a single Erlang term in Mnesia. Currenty, we are only have memory tables and are `dirty_read` and `dirty_write` as much as we can. We are using `qlc` for selections so we have to do those in transactions but every other object query should be dirty if possible. This is because we are going to execute `program` objecjts synchronously anyway via a `rt` process (todo). 

This means that all tasks from all actors will be queued up to a central process. Yes, this will be a huge bottleneck but if we don't do this it will be a nightmare to synchronize everything and still keep a consistent outcome for all participants involved. Just the simple thing of two players executing a `get` action on the same object is involved. If we don't synchronize these actions we essentially put the burden of synchronizing everything on the `action` and `verb` implementors. I don't think that's the way to go. We just have to make sure our `programs` are effecient.