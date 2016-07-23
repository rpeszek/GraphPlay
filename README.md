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
* play - contain files that demo the library
* test - is a TODO

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
and :l various play/test files.

My first non-'hello world' Haskell project!  Learning a TON and having lots of fun.
