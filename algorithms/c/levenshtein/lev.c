#include <stdlib.h>

#include "lev.h"

/* just to try the algorithm, substitute with
 * unsigned int *buffer;
 * unsigned int **distance;
 * initialized and destructed at the beginning and end of lev.
 */

unsigned int distance[24][24];


static inline
unsigned int min(unsigned int a, unsigned int b) {
	return (a < b) ? a : b;
}

static inline
unsigned int minimum(unsigned int a, unsigned int b, unsigned int c) {
	return min(min(a, b), c);
}


unsigned int levenshtein(const char* u, const size_t n, const char* v, const size_t m) {
	for(int i = 0; i <= n; ++i) {
		distance[i][0] = i;
	}
	for(int j = 0; j <= m; ++j) {
		distance[0][j] = j;
	}

	for(int i = 1; i <= n; ++i) {
		for(int j = 1; j <= m; ++j) {
			if(u[i-1] == v[j-1]) {
				distance[i][j] = distance[i-1][j-1];
			} else {
				distance[i][j] = 1 + minimum(
						distance[i-1][j],
						distance[i][j-1],
						distance[i-1][j-1]
				);
			}
		}
	}

	return distance[n][m];
    
}
