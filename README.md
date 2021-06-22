# Simulation of an Epidemic
This code simulates the spread of an epidemic using the [SIR model](https://en.wikipedia.org/wiki/Compartmental_models_in_epidemiology#The_SIR_model "The SIR Model") over a [network](https://en.wikipedia.org/wiki/Graph_theory#Graph "Networks") of nodes connected with each other by undirected links. Infected nodes have a chance of recovering, becoming immune to the infeciton, or infecting one of their susceptible neighbors.

## Usage
There are three main commands.
1. In order to compile the program (if needed), execute the program and plot the results use `make sim dir_net=net1000.dat N_samples=1000 offset=0` changing the variables as needed. 
* The network file must be saved in the `networks` directory. The results will be saved in a directory with the name of the network (using the example command, the results will be saved in a directory called `net1000.dat/`).
* `N_samples` indicates how many runs will be performed for each condition.
* `offset` indicates how many header lines must be skipped by the program when reading the network file.

2. In order to just compile use `make`.
3. In order to create the plots of a previously run simulation use `make plots dir_net=net1000.dat`, where net1000.dat referes now to the folder where the results are stored. The plots will be saved in a subfolder called `net1000.dat/plots/`.

## Example
For the simulation of the network named `net50000.dat` (stored in the `networks` directory) we use `make sim dir_net=net50000.dat N_samples=1000 offset=0`. This command will generate the results in the folder `net50000.dat/` and the plots in the folder `net50000.dat/plots/`.

The plots obtained are:

#TODO

## About
This code was developed by the authors for the subject of _Complex Systems_ of the _Computational Modelling_ Master in 2021.

## Authors & Contact

Arnau Jurado, arnau.jurado.romero@gmail.com

Eloi Sanchez, eloisanchez16@gmail.com