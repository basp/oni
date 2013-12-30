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

Make sure your <code>erl/bin</code> is in your path. Then, clone the code into a directory of your choice:

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

After connecting with your telnet client the Erlang server shell should output the following:

    =INFO REPORT==== 30-Dec-2013::22:55:45 ===
    Wizard logged in from {{127,0,0,1},56823}

Or someething quite like it.

### Creating and Manipulating Objects
Say you want to login with `Monster` but you can't because there is no player object that corresponds to that player name. Below is a step by step guide on how to create a new player object and logging in connecting to it.

Start with booting an Erlang shell with the `oni/ebin` files. Make sure they are built, with something like this:

    > cd oni
    > ./make.ps1
    > werl -pa ebin

Now from your Erlang shell you should see something like this:

    Erlang R16B02 (erts-5.10.3) [smp:4:4] [async-threads:10]

    Eshell V5.10.3  (abort with ^G)
    1> 

Boot oni by executing `oni:init()`, it looks like this in the prompt:

    1> oni:init()
    ok
    2>

The `ok` tells us that everything is fine. We now have `oni` running. We can create a new object easily:

    2> object:create(nothing).
    3
    3>

The `nothing` argument is the value of the `parent` property for our new object. In this case, we just specify `nothing` which means it is a '''root''' object; it has no parent. The result from call the `object:create` function is `3`. This is the object id of our newly created object. Let's remember this value:

    3> Obj3 = v(2).
    3

That is `Obj3`. Make sure we are on the same page:

    4> Obj3 =:= 3.
    true

Make sure that is `true`. We can check if this object is valid:

    5> object:valid(Obj3).
    true

Ok, after all those sanity checks, we have this valid object but it's not a player object yet. For that we have to set the `player` flag:

    6> object:set_player_flag(Obj3, true).
    ok

The `player` flag is kinda special but you can also set it like any other property:

    6> object:set_property(Obj3, <<"player">>, true).
    ok

By the way, remember you can inspect your objects at any time by using the table viewer. We are just using Mnesia for our storage and ETS for our active connections. For our connections in ETS we use the `connections` table. Our storage for objects in Mnesia is a single table and it's called `object`. Try booting up the ''table viewer'' (`tv:start()`) and inspect the tables. You can switch which kind of tables you want to view using the menu.

We are not done yet. Each object has some builtin properties. One of them is `name`. Before we can login, making a connection to a known object, we need to assign it a name:

    7> object:set_property(Obj3, <<"name">>, "Monster").
    ok

Let's double check it worked, we can ask for the `name` property value:

    8> object:get_property(Obj3, <<"name">>).
    "Monster"

That seems fine. Now this object is `player` and it has a `name` so let's try logging in. Start up a telnet client and connect to `127.0.0.1:7777` (if you have the default settings):

    Oni - Steampunk Dreams

    Welcome! Please login.

Now try to login with your new `player` object:

    connect Monster
    *** Connected (Monster) ***

It should say `*** Connected (Monster) ***`. If it doesn't say that you either get nothing or some horrible pile of damage (in your server shell probably). Either way, don't freak - it's probably something simple but Erlang can blow up in spectacular ways.

There's a lot of more stuff we can do with objects but if you can follow along this far you should be able to figure out the rest of the API unless the lack of documentation. More to come though.

### Final Notes
Basic interaction might not seem like much but the login seems pretty solid. You can try to enter bullshit and it will print back some kind of meaningful message. Yes, there is a secuirty risk by having a different message for unknown command and unknown login results but it's convenient for debugging and we are not a security agency anyway.

Also, from a server point of view we can observe client states of connecting and disconnecting which are most important to us right now.

Next step is to have a runtime process that will execute '''verbs''' that are executed by players. Eventually we might be able to program the login sequence using Oni itself instead of the kind of hacks that we have now.

Oni uses an object database that is facilitated by Mnesia and consists of a single table. The structure of Oni objects can be found in the `include/records.hrl` file. It is really quite simple.

For now, we are storing Oni objects as a single Erlang term in Mnesia. Currenty, we are only have memory tables and are `dirty_read` and `dirty_write` as much as we can. We are using `qlc` for selections so we have to do those in transactions but every other object query should be dirty if possible. This is because we are going to execute `program` objecjts synchronously anyway via a `rt` process (todo). 

This means that all tasks from all actors will be queued up to a central process. Yes, this will be a huge bottleneck but if we don't do this it will be a nightmare to synchronize everything and still keep a consistent outcome for all participants involved. Just the simple thing of two players executing a `get` action on the same object is involved. If we don't synchronize these actions we essentially put the burden of synchronizing everything on the `action` and `verb` implementors. I don't think that's the way to go. We just have to make sure our `programs` are effecient.