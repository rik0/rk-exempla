#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#define SZ 4


int solve(int m, int *Y, int n, int *R);

void pretty_print(int n, int* R, FILE* fh) {
    size_t i;
    for(i=0; i<n; ++i) {
        fputs(R[i] ? "T" : "F", fh);
    }
    fputs("\n", fh);
}

int eval(int n, int*Y, int*R) {
    int acc = 0, i;
    for(i = 0; i < n; ++i) {
        acc += (R[i] ? Y[i] : 0);
    }
    return acc;
}

int main() {
    const int M = 13;
    int buffer[SZ];
    int successes[][SZ] = {
        {5, 7, 3, 1},
        {5, 7, 1, 3},
        {8, 2, 2, 1},
        {13, 1, 1, 1},
        {13, 0, 0, 0},
        {0, 10, 0, 3},
        {15, -5, 3, 4},
        {-8, -5, 13, 1},
        {-8, -5, 13, 0},
        {15, -5, 4, 3}
    };

    int failures[][SZ] = {
        {1, 2, 3, 4},
        {5, 5, 5, 2},
        {3, 3, 3, 3},
        {0, 14, 1, 3},
        {-1, -2, -3, 5},
        {-13, 10, 1, 1}
    };

    const int no_successes = sizeof(successes)/sizeof(int[SZ]);
    const int no_failures = sizeof(failures)/sizeof(int[SZ]);

    size_t i = 0;
    for(i=0; i < no_successes; ++i) {
        memset(buffer, 0, SZ * sizeof(int));
        {
            int res = solve(M, successes[i], SZ, buffer);
            if(res) {
                assert(eval(SZ, successes[i], buffer) == M);
                pretty_print(SZ, buffer, stdout);
            } else {
                puts("ERROR");
            }
        }
    }

    for(i=0; i < no_failures; ++i) {
        memset(buffer, 0, SZ * sizeof(int));
        {
            int res = solve(M, failures[i], SZ, buffer);
            if(res) {
                printf("ERROR: %d", eval(SZ, failures[i], buffer));
                pretty_print(SZ, buffer, stdout);
            } else {
                puts("FAILURE EXPECTED.");
            }
        }
    }
    return 0;
}

int solve(int m, int *Y, int n, int *R) {
    if(m == 0) return 1;
    if(n == 0) return 0;

    *R = !*R;
    if(solve(m-*Y, Y+1, n-1, R+1)) {
        return 1;
    } else {
        *R = !*R;
        return solve(m, Y+1, n-1, R+1);
    }
}
