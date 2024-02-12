/**
 * n_barrier.c
 *
 * The implementation of the barrier API defined in n_barrier.h.
 *
 * Author
 *   Karl Marklund <karl.marklund@it.uu.se>
 *
 */

#include <stdio.h>      // perror()
#include <stdlib.h>     // exit(), EXIT_FAILURE

#include "n_barrier.h"   // The barrier API
 
void barrier_init(barrier_t *barrier, int count) {
    if (pthread_mutex_init(&barrier->mutex, NULL) < 0) {
        perror("Init mutex lock");
        exit(EXIT_FAILURE);
    }

    barrier->count      = count;
    barrier->arrived    = 0;
    barrier->semaphore  = psem_init(0);
}

void barrier_destroy(barrier_t *barrier) {
       psem_destroy(barrier->semaphore);
       pthread_mutex_destroy(&barrier->mutex);
}

void barrier_wait(barrier_t *barrier) {
    pthread_mutex_lock(&barrier->mutex);
    
    barrier->arrived++;
    
    if (barrier->arrived  == barrier->count) {
        for (unsigned int i = 0; i < barrier->count - 1; i ++) {
            psem_signal(barrier->semaphore);
        }

        barrier->arrived = 0;

        pthread_mutex_unlock(&barrier->mutex);
    } else {
        pthread_mutex_unlock(&barrier->mutex);
        psem_wait(barrier->semaphore);
    }
}