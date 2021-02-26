#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main (int argc, char* argv[]) {
	int i;
	srand (time (0));
	int node;
	int x, y;
	FILE* map;
	int num_nodes;
	sscanf (argv[1], "%d", &num_nodes);
	map = fopen ("map.txt", "w");
	for (i = 0; i < num_nodes; i++){
		node = i;
		x = rand() % 1000;
		y = rand() % 1000;
		fprintf (map, "%d %d %d\n", node, x, y);		
	}
	printf ("File created!\n");
	exit (EXIT_SUCCESS);	
}
