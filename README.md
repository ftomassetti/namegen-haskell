namegen-haskell
===============

A name generator written in Haskell

Usage:
```haskell
samples <- loadSamples path -- samples is just a list of Strings
let language = fromSamples samples
let myGeneratedName = generateName language seed -- seed could be ontained from a Random Generator
```

Obtaining data
==============

This name generator analyze a set of samples and derive the probability of a certain sequences of characters. For example, in Italian the sequence 'mp' is always followed by a vowel. By analyzing a samples of Italian names, the library will derive this rule and provide a name generator which have a probability equal to 0.0 for all the sequences 'mp' -> consonant.

A set of samples for different categories of names in different languages is provided in the related project [namegen-data](http://github.com/ftomassetti/namegen-data). 

New, original languages can be obtaines simply by using a set of samples obtained by mixing different sets. If you take 100 Elvish male names, 200 names of Japanese cities and 70 Spanish female names you will obtain a pretty unique new name generator which you can use in you fantasy world.
