## Oni
Oni is an experiment to see how feasible it is to write a LambaMOO server in Erlang. The aim of Oni is to depend on Erlang infrastructure as much as possible to enable most if not not all of the world to be written in Erlang terms.

### History
During development of Aki it became quite clear that a lot of effort was put into a somewhat compatible LambdaMOO VM. To that end, Aki was built around a byte code interpreter that was heavily inspired by the original by Pavel Curtis. All this effort was quite interresting from an implementation point of view but did raise the question: isn't there an easier way? 

### Vision
It turns out there might be an easier way. The main use case for implementing a custom VM is to be able to load and change code while the server is running. We want to have people building the game and people playing the game at the same time. Our MOO would be useless if we would have to stop and restart the server for any code change. Erlang is quite capable of changing code on the fly.

Of course, LambdaMOO is also attractive for game programming because it features a very solid object oriented programming environment. It shouldn't be too hard to provide similar object features with a functional language running underneath. Especially not with Erlang and Mnesia for storage.