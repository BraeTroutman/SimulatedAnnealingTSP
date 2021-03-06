# The Travelling Salesman Problem with Simulated Annealing

This project is an application of the simulated annealing heuristic technique to
the Travelling Salesman Problem.  

This project was for my Algorithm Design and Analysis class, where we were asked to choose a problem and some heuristic or exact algorithm to apply to it. I chose to implement simulated annealing, as it has NLP applications such as in paraphrasing sentences, and I chose the TSP because it was a problem whose solutions can be modelled combinatorially as simulated annealing expects (i.e. each Hamiltonian Cycle through a given map/graph can be represented as a list of each node in the graph, ordered by when it was visited)

## Usage

   You can find the uberjar executable in target/uberjar/

    $ java -jar tsp-0.1.0-standalone.jar [args]

## Layout

Four Namespaces:
- tsp.core: the core functions of the project, including the entry point "-main"
for the compiler as well as the wrapper functions for use by the user in creating
maps and generating/visualizing solutions.
- tsp.san: the functions pertaining directly to simulated annealing. ie the fitness,
acceptance, and central algorithm functions.
- tsp.map: the functions for generating the map and running queries on them
- tsp.vis: the functions for visualizing solutions in svg files
