# The Travelling Salesman Problem with Simulated Annealing

This project is an application of the simulated annealing heuristic technique to
the Travelling Salesman Problem.  

## Usage

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