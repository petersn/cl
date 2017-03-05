Cl
==

Cl stand for Crappy Language, and is a crappy toy interpreted language I am designing.
There is no particular reason this project should be of interest to you unless you are me, Peter Schmidt-Nielsen.

This entire project is licensed under CC0 (that is, released into the public domain).
You are free to do anything you want whatsoever with any of it.

TODO
----

These are largely notes for myself to remind myself of what I want to do.
In no particular order, I want to do the following. (Many of these are huge stretch goals, especially given that the language doesn't work at all right now.)

* Implement mark and sweep in addition to current ref counting (that get tricked by cycles).
* Maybe mark and sweep in a separate thread, with some synchronization? To aid this migration I've been careful to inc on target side before decing on source side on a move. Double check C++ memory model, and where I need to insert fences...
* Add mechanism for rtsc-style compilation into binary with bytecode attached.
* Once the above is done, then the language could potentially get a self-hosting bytecode compiler, because the inner-interpreter is in C++. This would be very exciting.
* Very carefully review performance of the parser. The CYK n^3 behavior is a little depressing.
* Carefully review all the n^2 things in the bytecode parser (like when functions are nested).
* At some point, maybe a JIT could be designed in?

