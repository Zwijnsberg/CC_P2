# INFOB3CC Assignment 2: Netchange

http://www.cs.uu.nl/docs/vakken/b3cc/assignments.html

Authors:
- Floris de Kruijff: 6359248
- Derek de Jonge   : 

Problems with our solution:
In our design we initially choose to make a routing table consist of a list of entries. 
Instead of locking the whole table, which would be slow, we decided to lock each entry with a TMVar.
While this is a good idea, the implementation is a different thing. We tried a lot of things but we could
not manage to pass the table down to functions, without making it a state variable such as IORef or MVar. But 
by making it such a variable you have to write (replace) it as whole, and not a single entry. Which kind of
defeats our single TMVar entry purpose we initially thought of. 
While implementing such a program in an imperative language, rather than a functional language, would be more
intuative for us. We did try our best to make to program work with this malfunctioning 'backbone'.