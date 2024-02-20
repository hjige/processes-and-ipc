/**
 * n_barrier_test.c
 *
 * Test of the n_barrier implementation. A number of treads executing 
 * chunks of work in lockstep enforced by a barrier.
 *
 * History
 * 
 * 2023-11-04
 * 
 * First version by Karl Marklund <karl.markund@it.uu.se>.
 *
 */

#define _GNU_SOURCE

#include <stdio.h>     // printf()
#include <stdlib.h>    // abort(), srand(), rand()
#include <unistd.h>    // sleep()
#include <pthread.h>   // pthread_...()
#include <stdbool.h>   // true, false

#include "n_barrier.h" // The barrier API
#include "psem.h"      // Portable semaphores API for Linux and macOS

#define ITERATIONS      3  // The number of iterations executed by each thread.
#define NTHREADS        5  // Number of threads.
#define MAX_SLEEP_TIME  3  // Max sleep time (seconds) for each thread. 

/**
 * Function used to trace the execution of the threads and detect
 * invalid barrier synchronization.
 */
void trace(unsigned int id) {
    static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
    static unsigned int n_arrived = 0;
    static bool arrived[NTHREADS];
    static int iterations = 0;

    pthread_mutex_lock(&mutex);

    if (n_arrived == 0) {
        for (int i = 0; i < NTHREADS; i ++) {
            arrived[i] = false;
        }

        printf("Iteration %d\n\n", iterations);
    }

    if (arrived[id]) {
        printf("  T%d <=== ERROR: Jumped over the barrier to early!\n\n", id);

        exit(EXIT_FAILURE);
    } else {
        printf("  T%d\n", id);
        arrived[id] = true;
        n_arrived++;
    }
    if (n_arrived == NTHREADS) {
        printf("\n");
        n_arrived = 0;
        iterations++;

        if (iterations == ITERATIONS) {
            printf("SUCCESS: All iterations done in lockstep!\n\n");
        }
    }
    pthread_mutex_unlock(&mutex);
}

/**
 * This struct is used as argument to a thread. 
*/
typedef struct {
    int id;              // Numeric id of the thread. 
    barrier_t *barrier;  // Pointer to the barrier used by the threads. 
} thread_arg_t;


/**
 * This function is executed by all threads. 
*/
void *thread(void *arg) {
    thread_arg_t targ = *(thread_arg_t*) (arg);
    
    for (int i = 0; i < ITERATIONS; i++) {
        trace(targ.id);
        sleep(rand() % MAX_SLEEP_TIME);
        barrier_wait(targ.barrier);
    }

    pthread_exit(NULL);
    return NULL;
}

int main() {
    pthread_t threads[NTHREADS];

    srand(time(NULL));
    pthread_setconcurrency(NTHREADS);

    barrier_t barrier;

    barrier_init(&barrier, NTHREADS);
    thread_arg_t args[NTHREADS];

    printf("\n%d threads T0, ..., T%d doing %d iterations each in lockstep.\n\n", 
        NTHREADS,
        NTHREADS - 1,
        ITERATIONS);

    for (int i = 0; i < NTHREADS; i ++) {
        args[i].id = i;
        args[i].barrier = &barrier;
        pthread_create(&threads[i], NULL, thread, &args[i]);
    }

    for (int i = 0; i < NTHREADS; i ++) {
        pthread_join(threads[i], NULL);
    }

    return 0;
}
