/**
 * two_thread_barrier.c
 *  
 *   Two threads executing chunks of work in lockstep by having a rendezvous
 *   at a barrier.
 *
 * Author(s) 
 * 
 * Nikos Nikoleris <nikos.nikoleris@it.uu.se>
 * 
 *   Original version 
 *
 * Karl Marklund <karl.markund@it.uu.se> 
 * 
 *   Added the trace() function with automatic error detection.
 */

#include <stdio.h>      // printf() 
#include <stdlib.h>     // abort(), srand(), rand()
#include <unistd.h>     // sleep()
#include <pthread.h>    // pthread_...()

#include "psem.h"       // Portable semaphores API for Linux and macOS.

#define ITERATIONS      5   // Number of iterations each thread will execute. 
#define MAX_SLEEP_TIME  3   // Max sleep time (seconds) for each thread. 

/**
 *  Declare global semaphore variables. Note, they must be initialized, for 
 *  example in main() before use. 
 */
psem_t *sem; 

/**
 * next()
 * 
 * Helper function used by trace.
 */
char next(char x) {
    switch (x) {
        case 'A': 
            return 'B';
        case 'B':
            return 'A';
        default: 
             return '?';
    }
}

/**
 * trace()
 * 
 * Function used to trace the execution of the threads and detect
 * invalid barrier synchronization.
 */
void trace(char id) {
    static int iteration = 0;
    static char nxt = '?';
    static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
    
    pthread_mutex_lock(&mutex);

    if (nxt == '?') {
        printf("Iteration %d\n\n", iteration);
        printf("  %c\n", id);
        nxt = next(id);
    } else {
        if (id == nxt) {
            nxt = '?';
            printf("  %c\n\n", id);
            iteration++;

            if (iteration == ITERATIONS) {
                printf("SUCCESS: All iterations done!\n\n");
            }
        } else {
            printf("  %c <===== ERROR: should have been %c\n\n", id, nxt);
            exit(EXIT_FAILURE);
        }
    }

    pthread_mutex_unlock(&mutex);
}

/**
 * threadA()
 * 
 * Executed by thread A. 
 * 
 * TODO
 *
 * Make the two threads A and B perform their iterations in lockstep by having a
 * rendezvous at the barrier. 
 * 
 * */
void *threadA(void *param __attribute__((unused))) {
    int i;

    for (i = 0; i < ITERATIONS; i++) {
        trace('A');
        sleep(rand() % MAX_SLEEP_TIME);
    }

    pthread_exit(0);
}

/**
 * threadB()
 * 
 * Executed by thread B. 
 * 
 * TODO
 *
 * Make the two threads A and B perform their iterations in lockstep by having a
 * rendezvous at the barrier. 
 * 
 * */
void *threadB(void *param  __attribute__((unused))) {
    int i;

    for (i = 0; i < ITERATIONS; i++) {
        trace('B');
        sleep(rand() % MAX_SLEEP_TIME);
    }

    pthread_exit(0);
}

/**
 * main()
 * 
 * Initializes semaphores. 
 * 
 * Creates the two threads. 
 * 
 * Wait for the two threads to terminate (join).
 * 
 * Deallocates semaphores. 
*/
int main() {
    pthread_t tidA, tidB;

    /**
     * Todo: Initialize semaphores.
     */

    sem = psem_init(666);
    
    srand(time(NULL));
    pthread_setconcurrency(3);
    
    printf("\nTwo threads A and B doing %d iterations each in lockstep.\n\n", ITERATIONS);

    if (pthread_create(&tidA, NULL, threadA, NULL) ||
        pthread_create(&tidB, NULL, threadB, NULL)) {
        perror("pthread_create");
        abort();
    }
    if (pthread_join(tidA, NULL) != 0 ||
        pthread_join(tidB, NULL) != 0) {
        perror("pthread_join");
        abort();
    }

    /**
     * Todo: Destroy semaphores.
     */

    psem_destroy(sem);

    return 0;
}
