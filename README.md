# Lorax

Compiler and editor for a language with extensible syntax and typographical features. Proof of 
concept of a very small, declarative kernal language for execution, and a simple target 
presentation language with multiple source languages reduced to each, and an editor which 
directly renders the presentation language while editing the source AST.

The kernel language is a subset of the forms of Clojure (verion 1.1 or maybe 1.2) and is 
compiled and executed by the Clojure runtime, and the editor uses Java's AWT/Swing. The Batik 
library is required only for exporting PDF snapshots from the editor.

Mostly untouched since 2010. It probably would not be too hard to get this to run under Clojure 
1.2; might require significant rework for current versions of Clojure.

This repository also contains the TeX source of my Master's [thesis](thesis.pdf) submitted in 
Dec. 2010, and the [slides](presentation/copies/Lorax.pdf) from my thesis defense.

Posted 8/2018 in the hopes that somebody finds some of this useful or interesting.
