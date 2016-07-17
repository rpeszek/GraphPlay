# GraphPlay

WORK IN PROGRESS. I made it a public project for my colleagues to follow along.

GraphPlay uses CamelCase to purposefully not follow hackage-package naming convention.
It is intended as learning and experimentation playground, not a production package.

The goal is to play and experiment with polymorphism in Haskell language and use graphs more as an excuse.  My focus is on polymorphic data production combined with polymorphic or type specific consumption of that data. Stuff like this is exiting because is not possible in OO.

I started plying with polymorphism by using class types with functional dependencies.
When I get to it, I will want to try other approaches like using Free. Doing polymorphism without
much language support is just so intriguing.

Project uses somewhat unusual folder structure:  
* src - contains PolyGraph.xyz modules with mostly polymorphic type class level
* examples - contain Play.xyz modules for ghci use to play with the PolyGraph
* test - is a TODO

My first non-'hello world' Haskell project!  Learning a TON and having lots of fun.
