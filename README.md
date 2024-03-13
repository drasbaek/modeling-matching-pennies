# Cognitive Modeling of the Matching Pennies Game (Portfolio 2, ACM)
This repository contains the code for the `portfolio 2 assignment` in the course `Advanced Cognitive Modeling` at the Cognitive Science MSc. (F24).

Code was produced jointly by the group members:
* Milena Cholozynska (@milenacholo)
* Daniel Blumenkranz (@daniblu)
* Anton Drasbæk Schiønning (@drasbaek)
* Mina Almasi (@MinaAlmasi)

## Overview 
The repository contains four folders: 
1. `src` - all R scripts 
2. `data` - simulated data and data resulting from fitting models (predictive checks & recovery)
3. `stan` - the RL model coded in Stan (git ignores the C++ compiled version)
4. `plots` - all plots

For the code in `src`, see the seperate [src/README.md](https://github.com/drasbaek/modeling-matching-pennies/blob/main/src/README.md) for a detailed description of its contents.

## Usage 
### Setup
To use the code, ensure that you have `R` and `RScript` installed. All code was developed using `R` version `4.3.2` and was primarily tested on a MacBook.

Furthermore, please install the package `pacman`. Within your `R console`, this can be done as such: 
```
install.packages("pacman")
```
### Run Code 
Code can be run by using `RScript` in your `bash` terminal
```bash
RScript src/illustrate_priors.R
```
