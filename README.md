# Project FEAR

Flaming Eagle Acrobatic Raptor

# Installing info

# Information for the fall 2024 group

## Getting setup

The project uses the R language and shiny. 

## Getting started

The main file is app.R this is the main file that gets run and is where 90% of the work is done. The first thing to looks at is the dashboardPage function (line 28 unless it changes). This function is what setup the GUI. The second function to look into is server function (line 99). This is what sets up all of the functionality of the program. The main part of this function is the observeEvent method (line 359) which calls the trigger functions which are the ones that make and display the graphs. observeEvent is called whenever the user makes a change on screen. To get a understanding of how all of this stuff works look at the Call for Service tab (line 748) as it is the simplest and what everything elseis theoretically based off of.

## Tabs

### Nameing

Most of the tabs are the dataset turned into a three letter id so call for service is called CFS in the code.

### UOF and CI tabs

These two tabs do not follow the normal layout for the tabs (code wise) and should be updated to matach the CFS tab.

## Group A/L split

There was two groups working on this project and so a lot of the code was divided into group A and group L stuff just to keep work area seprate.

## Contact info

If you need help getting started with this project you can contact Nathan (deggerfire) at nathan-tiegiser@oulook.com and I can help you get started just make sure you include that you are the capstone group working on the norman PD graphs.
