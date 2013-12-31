## Oni
Oni is an experiment to see how feasible it is to write a LambaMOO clone in Erlang. The aim of Oni is to depend on Erlang/OTP infrastructure as much as possible to enable most if not not all of the world to be written in those terms. 

Currently, everything is pure Erlang. There is no plan to implement a custom programming language like there is in LambdaMOO. Athough we do share a lot of common concepts and a bit of API with LambdaMOO, the whole implementation is completely different. Things like `fork` and `suspend` work completely differently in Oni. In fact, we don't actually have those concepts.

Thanks to the fact that Erlang itself comes with a lot of tools especially suited to our purpose we can delegate a lot of the infrastructure to the Erlang scheduler and make use of a lot of well suited libraries to implement a LambdaMOO-like server. Maybe it's more like a MFO (Mud-Functional-Oriented) but Oni sounds nicer.