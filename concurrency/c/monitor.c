#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>

#include <sys/types.h>
#include <sys/event.h>
#include <sys/time.h>

long my_index(pid_t* pids, size_t max_index) {
    long i = 0;
    for(; i < max_index; ++i) {
        if(pids[i] == 0) {
            return i;
        }
    }
    return -1;
}

int main(int argc, char* argv[]) {
    char* process_arguments[2][2] = {
        {"/bin/date",  0},
        {"/bin/date",  0}
    };

    size_t n_processes = 2;

    pid_t pids[argc];
    {
        size_t i = 0;
        for(; i < n_processes; ++i) {
            if((pids[i] = fork())) {
                /* father */ 
                printf("Spawned child: %d\n", pids[i]);
            } else {
                /* child */
                srand(time(NULL) + getpid() * 17 * 13 * 31);
                unsigned int time_sleep = rand() / 1000;
                printf("me name's %u\n", getpid());
                printf("my index's %ld\n", my_index(pids, n_processes));
                printf("sleeping for %u microseconds.\n", time_sleep);
                usleep(time_sleep);
                close(0);
                if(execv(process_arguments[i][0], process_arguments[i]) < 0) {
                    perror("[SHIT]");
                    exit(255);
                }
            }
        }
    }


}

