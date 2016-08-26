# GraphPlay

Library which uses directed and undirected graphs as an excuse to play with Haskell polymorphism.

GraphPlay uses CamelCase to purposefully not follow hackage-package naming convention.
It is intended as learning and experimentation playground with cabal let's have it all attitude. It is not a production library.

Here is what is in the library:

* Type classes with functional dependencies defining
  - read only graphs
  - buildable graphs
  - adjustable graphs
* Some graph folding and traversal algorithms.
* Examples with polymorphic data production, polymorphic morphisms from one type into another etc. Just because it is so much different than OO
* Examples of Free DSL-Interpreter design pattern (Traversals, Walks)
  - using monadic interpreters
  - composable Free-Cofree pattern

And there are lots of plans for the future:
* More Free DSL-Interpreter Stuff, prisms etc.
* Play with Effects
* GADT 

__Play Folder__ is separate from standard 'src' and 'test'.  
That folder contain files that demo the library and my experiments. I am re-working these as literate programs (lhs).

__Wiki__ Each lhs program is markdown to a file and checked in to this project wiki pages for easy reading.  
I think of them as code versions of blog posts. I love FP and Haskell and maybe my affection will become more contagious with these little program examples. The goal is to make these play folder examples *readable without 
knowing much about Haskell* with focus on what can be done more than how it is done.

To play with this project:

```
git clone https://github.com/rpeszek/GraphPlay.git
cd GraphPlay
stack build
stack build --test
```
or
```
stack ghci
stack ghci --test
```
and 
```
:l various play/test files.
```

My first more sizable Haskell project!  Learning a TON and having lots of fun.
