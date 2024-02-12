/**
 * Critical sections and mutual exclusion.
 *
 * Multiple threads incrementing and decrementing the same shared variable.
 *
 * History:
 *
 * 2013 - Original version by Nikos Nikoleris <nikos.nikoleris@it.uu.se>.
 *
 * 2019 - Refactor and added stats summary by Karl Marklund <karl.marklund@it.uu.se>.
 */

#include <stdio.h>     // printf(), fprintf()
#include <stdlib.h>    // abort()
#include <pthread.h>   // pthread_...
#include <stdbool.h>   // true, false

#include "timing.h"    // timing_start(), timing_stop()

/* Shared variable */
volatile int counter;

/* Pthread mutex lock */
pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;

/* Shared variable used to implement a spinlock */
volatile int lock = false;

/* Number of threads that will increment the shared variable */
#define INC_THREADS 5
/* Value by which the threads increment the shared variable */
#define INCREMENT 2
/* Iterations performed incrementing the shared variable */
#define INC_ITERATIONS 20000
/* Number of threads that will try to decrement the shared variable */
#define DEC_THREADS 4
/* Value by which the threads increment the shared variable */
#define DECREMENT 2
/* Iterations performed decrementing the shared variable.*/
#define DEC_ITERATIONS (INC_ITERATIONS * INC_THREADS * INCREMENT / DEC_THREADS / DECREMENT)

/*******************************************************************************
                          Test 0 - No synchronization
*******************************************************************************/

/* Unsynchronized increments of the shared counter variable */
void *
inc_no_sync(void *arg __attribute__((unused)))
{
    int i;

    for (i = 0; i < INC_ITERATIONS; i++) {
        counter += INCREMENT;
    }

    return NULL;
}

/* Unsynchronized decrements of the shared counter variable */
void *
dec_no_sync(void *arg __attribute__((unused)))
{
    int i;

    for (i = 0; i < DEC_ITERATIONS; i++) {
        counter -= DECREMENT;
    }

    return NULL;
}

/*******************************************************************************
                          Test 1 - Pthread mutex lock
*******************************************************************************/

/* Increments of the shared counter should be protected by a mutex */
void *
inc_mutex(void *arg __attribute__((unused)))
{
    int i;

    for (i = 0; i < INC_ITERATIONS; i++) {
        /* TODO: Protect access to the shared variable counter with a mutex lock
         * inside the loop. */
        counter += INCREMENT;
    }

    return NULL;
}

/* Decrements of the shared counter should be protected by a mutex */
void *
dec_mutex(void *arg __attribute__((unused)))
{
    int i;

    for (i = 0; i < DEC_ITERATIONS; i++) {
        /* TODO: Protect access to the shared variable counter with a mutex lock
         * inside the loop. */
        counter -= DECREMENT;
    }

    return NULL;
}


/*******************************************************************************
                      Test 2 - Spinlock with test-and-set
*******************************************************************************/

void spin_lock() {
    /* TODO: Implement the lock operation for a test-and-set spinlock. */
}

void spin_unlock() {
    /* TODO: Implement the unlock operation for a test-and-set spinlock. */
}

/* Increments of the shared counter should be protected by a test-and-set spinlock */
void *
inc_tas_spinlock(void *arg __attribute__((unused)))
{
    int i;

    for (i = 0; i < INC_ITERATIONS; i++) {
        /* TODO: Add the spin_lock() and spin_unlock() operations inside the loop. */
        counter += INCREMENT;
    }

    return NULL;
}

/* Decrements of the shared counter should be protected by a test-and-set spinlock */
void *
dec_tas_spinlock(void *arg __attribute__((unused)))
{
    int i;

    for (i = 0; i < DEC_ITERATIONS; i++) {
        /* TODO: Add the spin_lock() and spin_unlock() operations inside the loop. */
        counter -= DECREMENT;
    }

    return NULL;
}


/*******************************************************************************
                      Tes 3 - Atomic addition/subtraction
*******************************************************************************/

/* Increment the shared counter using an atomic increment instruction */
void *
inc_atomic(void *arg __attribute__((unused)))
{
    int i;

    for (i = 0; i < INC_ITERATIONS; i++) {
        /* TODO: Use atomic addition to increment the shared counter */

        counter += INCREMENT; // You need to replace this.
    }

    return NULL;
}

/* Decrement the shared counter using an atomic increment instruction */
void *
dec_atomic(void *arg __attribute__((unused)))
{
    int i;

    for (i = 0; i < DEC_ITERATIONS; i++) {
        /* TODO: Use atomic subtraction to increment the shared counter */

        counter -= DECREMENT; // You need to replace this.
    }

    return NULL;
}

/*******************************************************************************
 *******************************************************************************
            NOTE: You don't need to modify anything below this line
 *******************************************************************************
 ******************************************************************************/

/* Each test case is represented by the following struct. */

typedef struct {
    char *name;            // Test case name.
    void * (*inc)(void *); // Increment function.
    void * (*dec)(void *); // Decrement function.
    double total_time;     // Total runtime;
    double average_time;   // Average execution time per thread.
    int counter;           // Final value of the shared counter.
} test_t;

test_t tests[] = {
    { .inc = inc_no_sync,      .dec = dec_no_sync,      .name = "No synchronization"},
    { .inc = inc_mutex,        .dec = dec_mutex,        .name = "Pthread mutex"},
    { .inc = inc_tas_spinlock, .dec = dec_tas_spinlock, .name = "Spinlock"},
    { .inc = inc_atomic,       .dec = dec_atomic,       .name = "Atomic add/sub"},
    { .inc = NULL,             .dec = NULL,             .name = NULL}
};


// Information about each thread will be kept in the following struct.

typedef struct {
    // Pthread ID (tid) of the created thread will be stored here after calling pthread_create().
    pthread_t tid;
    // Numeric thread ID.
    int id;
    // Type of thread (increment or decrement).
    enum type {inc, dec} type;
    // The created thread will start to execute in the start_routine function ...
    void *(*start_routine)(void *);
    // ... with arg as its sole argument.
    void *arg;
    // Total runtime of the thread.
    double run_time;
} thread_t;

char * type2string(enum type type) {
    switch (type) {
    case inc: return "inc";
    case dec: return  "dec";
    default: return "???";
    }
}

/* The startroutine used by both increment and decrement threads. */
void *
generic_thread(void *_conf)
{
    struct timespec ts;
    thread_t *conf = (thread_t *)_conf;

    timing_start(&ts);

    conf->start_routine(conf->arg);

    conf->run_time = timing_stop(&ts);

    pthread_exit(0);
}



double
print_stats(thread_t *threads, int nthreads, int niterations, test_t *test)
{
    double run_time_sum = 0;
    double average_execution_time = 0;

    printf("\nStatistics:\n\n");
    for (int i = 0; i < nthreads; i++) {
        thread_t *t = &threads[i];
        printf("Thread %i (%s): %.4f sec (%.4e iterations/s)\n",
               i, type2string(t->type), t->run_time,
               niterations / nthreads / t->run_time);
        run_time_sum += t->run_time;
    }

    average_execution_time = run_time_sum /nthreads;

    printf("\nAverage execution time: %.4f s/thread\n"
           "\nAvergage iterations/second: %.4e iterations/s\n",
           average_execution_time,
           niterations / nthreads / run_time_sum);

    test->total_time = run_time_sum;
    return average_execution_time;
}

char *successOrFailure(int counter) {
    return (counter == 0) ? "success" : "failure";
}
void print_stats_summary(test_t tests[]) {
    test_t *test = tests;
    int width = 20;

    printf("\n\n=========================================================================================\n\n");
    printf("                                       SUMMARY\n\n\n");

    printf("%*s                             Total run      Average execution time\n", width, "");
    printf("%*s     Counter     Result      time (sec)     per thread (sec/thread)\n", width, "Test Case");
    printf("-----------------------------------------------------------------------------------------\n");

    while (test->inc && test->dec) {
        printf("%*s     %-10d  %s     %f       %f\n",
               width,
               test->name,
               test->counter,
               successOrFailure(test->counter),
               test->total_time,
               test->average_time);
        test++;
    }
}

void run_test(test_t *test) {
    int i, nthreads = 0;
    thread_t threads[INC_THREADS + DEC_THREADS];
    double average_execution_time = 0;

    pthread_setconcurrency(INC_THREADS + DEC_THREADS);

    counter = 0;

    pthread_setconcurrency(INC_THREADS + DEC_THREADS + 1);

    /* Create the incrementing threads */

    for (i = 0; i < INC_THREADS; i++) {
        thread_t *thread = &threads[nthreads];
        thread->id = nthreads;
        thread->type = inc;
        thread->start_routine = test->inc;
        if (pthread_create(&thread->tid, NULL, generic_thread, thread) != 0) {
            perror("pthread_create");
            abort();
        }
        nthreads++;
    }

    /* Create the decrementing threads */

    for (i = 0; i < DEC_THREADS; i++) {
        thread_t *thread = &threads[nthreads];
        thread->id = nthreads;
        thread->type = dec;
        thread->start_routine = test->dec;
        if (pthread_create(&thread->tid, NULL, generic_thread, thread) != 0) {
            perror("pthread_create");
            abort();
        }
        nthreads++;
    }

    /* Wait for all threads to terminate */

    for (i = 0; i < nthreads; i++)
        if (pthread_join(threads[i].tid, NULL) != 0) {
            perror("pthread_join");
            abort();
        }
    printf("\n==========================================================================\n");
    printf("%s\n\n", test->name);
    printf("Counter expected value:%10d\n", 0);
    printf("Counter actual value:  %10d\n", counter);

    test -> counter = counter;

    if (counter != 0) {
        printf("\nFAILURE :-(\n");
    } else {
        printf("\nSUCCES :-)\n");
    }

    average_execution_time = print_stats(threads, nthreads, INC_ITERATIONS + DEC_ITERATIONS, test);
    test -> average_time = average_execution_time;

}

int
main()
{
    test_t *test = tests;

    while (test->inc && test->dec) {
        run_test(test);
        test++;
    }

    print_stats_summary(tests);

    exit(EXIT_SUCCESS);
}
