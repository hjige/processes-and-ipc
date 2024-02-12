/**
 * n_barrier.h
 * 
 * Defines the API to a synchronization barrier.
 * 
 * 2023-11-04
 * 
 * First version by Karl Marklund <karl.markund@it.uu.se>.
 * 
*/

#ifndef N_BARRIER_H
#define N_BARRIER_H

#include <pthread.h>   // pthread_...()
#include "psem.h"      // Portable semaphores API for Linux and macOS.

/**
 *  A barrier is represented by the following C structure. 
 */
typedef struct {
    // Number of threads participating in the barrier. 
    unsigned int count;  

    // Number of threads that have reached the barrier.  
    unsigned int arrived;

    // Mutex lock used to protect the critical section inside barrier_wait().
    pthread_mutex_t mutex;

    // Semaphore used to block and unblock threads at the barrier. 
    psem_t *semaphore;
} barrier_t;

/**
 * barrier_init()
 *   Creates a new barrier. 
 * 
 * barrier 
 *   Pointer to a barrier. 
 * 
 * count
 *   Number of threads participating in the barrier. 
*/
void barrier_init(barrier_t *barrier, int count);

/**
 * barrier_destroy()
 *   Destroys a barrier, freeing the resources it might hold.     
 * 
 * barrier: 
 *   Pointer to a barrier. 
 * 
 * */
void barrier_destroy(barrier_t *barrier);

/**
 *  barrier_wait()
 *    Will block the calling thread until all other threads also 
 *    executed barrier_wait().
 * 
 *  barrier: 
 *   Pointer to a barrier. 
 *  
*/
void barrier_wait(barrier_t *barrier);

#endif