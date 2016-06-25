# GraphPlay
Polymorphic graph ideas - work in progress (just starting to play with this stuff)

My play-project to do graph things using Haskell and higher level polymorphism.
This is my first non-'hello world' Haskell project.

Why this topic?
Typical graph libraries think about a graph as a 'data structure'.
This maybe important for performance but is limiting. I want to investigate more polymorphic representations of graphs.
Data-structure like graph designs are forced to make stuctual decisions about what is a vertex and what is an edge. 
I find it less than optimal.

Examples: 
* Text is a graph with words as vertices and edges indicating if 2 words have been used together in a sentence.
* You can define DAG as a type.  If <- represents inheritance can you do that so Graph <- DGraph <- DAG, hard to do if type definitions are very data-structure like.

Hope to make this project grow over this summer.



