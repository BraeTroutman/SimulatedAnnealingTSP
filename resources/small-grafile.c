#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main () {
	int i;
	srand (time (0));
	char node;
	int x, y;
	FILE* map;
	map = fopen ("./resources/map.txt", "w");
	for (i = 0; i < 5; i++){
		node = 'A' + i;
		x = rand() % 100;
		y = rand() % 100;
		fprintf (map, "%c %d %d\n", node, x, y);		
	}
	printf ("File created!\n");
	exit (EXIT_SUCCESS);	
}
