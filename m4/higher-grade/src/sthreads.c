/* On Mac OS (aka OS X) the ucontext.h functions are deprecated and requires the
   following define.
*/
#define _XOPEN_SOURCE 700

/* On Mac OS when compiling with gcc (clang) the -Wno-deprecated-declarations
   flag must also be used to suppress compiler warnings.
*/

#include <signal.h>   /* SIGSTKSZ (default stack size), MINDIGSTKSZ (minimal
                         stack size) */
#include <stdio.h>    /* puts(), printf(), fprintf(), perror(), setvbuf(), _IOLBF,
                         stdout, stderr */
#include <stdlib.h>   /* exit(), EXIT_SUCCESS, EXIT_FAILURE, malloc(), free() */
#include <ucontext.h> /* ucontext_t, getcontext(), makecontext(),
                         setcontext(), swapcontext() */
#include <stdbool.h>  /* true, false */
#include <string.h>   // memset()
#include <sys/time.h>
#include "sthreads.h"

/* Stack size for each context. */
#define STACK_SIZE SIGSTKSZ*100

#define TIME_SLICE_MS 50
#define TIMER_TYPE ITIMER_REAL

#define PAUSE_TIMER() setitimer(TIMER_TYPE, &pause_timer, &old_time)
#define RESUME_TIMER() setitimer(TIMER_TYPE, &old_time, NULL)
#define RESET_TIMER() setitimer(TIMER_TYPE, &new_time_slice, NULL)


/*******************************************************************************
                             Global data structures

                Add data structures to manage the threads here.
********************************************************************************/

thread_t *running_thread = NULL;
thread_t *ready_queue = NULL;
thread_t *waiting_queue = NULL;
tid_t thread_id = 0;
ucontext_t thread_manager_ctx;

struct itimerval old_time;
struct itimerval pause_timer;
struct itimerval new_time_slice;


/*******************************************************************************
                             Auxiliary functions

                      Add internal helper functions here.
********************************************************************************/



/// @brief pops the first element in the queue, removing it from the queue.
/// @param queue ptr to queue to pop from.
/// @return ptr to thread.
thread_t *pop(thread_t **queue) {
  thread_t *thread_to_return = NULL;
  if (*queue != NULL) {
    thread_to_return = *queue;
    *queue = (*queue)->next;
  }

  return thread_to_return;
}

/// @brief Appends a thread to the end of the queue, setting its next field to NULL.
/// @param queue ptr to queue to append to.
/// @param thread_to_append thread to append to queue.
void append(thread_t **queue, thread_t *thread_to_append) {
  if (thread_to_append == NULL) {
    perror("dereference nullpointer (thread)");
    exit(EXIT_FAILURE);
  }

  thread_to_append->next = NULL;
  
  if (queue == NULL || *queue == NULL) {
    *queue = thread_to_append;
    return;
  }

  thread_t *cursor = *queue;
  while (cursor->next != NULL) {
    cursor = cursor->next;
  }
  cursor->next = thread_to_append;
}

bool contains_thread(thread_t **queue, tid_t thread_to_find) {
  thread_t **cursor = queue;
  while (*cursor != NULL) {
    if ((*cursor)->tid == thread_to_find) {
      return true;
    }

    cursor = &(*cursor)->next;
  }
  return false;
}

/// @brief Creates a new unique thread id
/// @return a unique thread id
tid_t create_thread_id() {
  // Global variable
  return thread_id++;
}


/// @brief Initialize a context.
/// @param ctxt context to initialize.
/// @param next successor context to activate when ctx returns. If NULL, the thread
///             exits when ctx returns.
/// @return 1 on successful init, else -1
int init_context(ucontext_t *ctx, ucontext_t *next) {
  /* Allocate memory to be used as the stack for the context. */
  void *stack = malloc(STACK_SIZE);

  if (stack == NULL) {
    return -1;
  }

  if (getcontext(ctx) < 0) {
    return -1;
  }

  /* Before invoking makecontext(ctx), the caller must allocate a new stack for
     this context and assign its address to ctx->uc_stack, and define a successor
     context and assigns address to ctx->uc_link.
  */

  ctx->uc_link           = next;
  ctx->uc_stack.ss_sp    = stack;
  ctx->uc_stack.ss_size  = STACK_SIZE;
  ctx->uc_stack.ss_flags = 0;

  return 1;
}

/// @brief initialises a thread.
/// @param thread thread struct to initialize.
/// @param func function to start thread in.
/// @return the tid of the initialized thread,
///         -1 if thread context could not be initialised.
int init_thread(thread_t* thread, void (*func)()) {
  thread->tid = create_thread_id();
  thread->state = ready;
  thread->next = NULL;

  if (init_context(&(thread->ctx), &thread_manager_ctx) == -1){
    return -1;
  } 
  
  makecontext(&(thread->ctx), func, 0);
  
  return (int) thread->tid;
}

/// @brief Sets all threads that are waiting for terminated thread to ready. 
/// @param terminated_thread tid of terminated thread.
void set_waiting_threads_to_ready(tid_t terminated_thread){
  if (waiting_queue == NULL){
    return;
  }
  thread_t **cursor = &waiting_queue;
  while (*cursor != NULL) {
    if (terminated_thread == (*cursor)->waiting_for){
      thread_t *thread_to_rdy = *cursor;

      // Remember the next thread in waiting queue
      thread_t *next = thread_to_rdy->next;
      // Unlink from waiting
      thread_to_rdy->next = NULL;
      
      // Append thread finished to wait to ready
      thread_to_rdy->state = ready;
      append(&ready_queue, thread_to_rdy);

      *cursor = next;
    } else {
      cursor = &(*cursor)->next;
    }
  }
}

/// @brief Destroys a thread allocated on the heap.
/// @param thread thread to destroy
void destroy_thread(thread_t *thread) {
  free(thread->ctx.uc_stack.ss_sp);
  free(thread);
}

/// @brief Terminates the running thread, 
void manage_threads() {
  PAUSE_TIMER();

  thread_t *terminated_thread = running_thread;
  terminated_thread->state = terminated;
  
  set_waiting_threads_to_ready(terminated_thread->tid);

  destroy_thread(terminated_thread);

  // Set next ready thread to running
  running_thread = pop(&ready_queue);
  if (running_thread == NULL) {
    // No more threads, we can exit
    exit(EXIT_SUCCESS);
  }
  running_thread->state = running;
  running_thread->next = NULL;

  // Resume thread execution
  RESET_TIMER();
  setcontext(&running_thread->ctx);
}


/* The three types of timers causes different signals.

   type: type of timer, one of ITIMER_REAL, ITIMER_VIRTUAL, or ITIMER_PROF.

   return value: the signal generated by the timer.

 */
int timer_signal(int timer_type) {
  int sig;

  switch (timer_type) {
    case ITIMER_REAL:
      sig = SIGALRM;
      break;
    case ITIMER_VIRTUAL:
      sig = SIGVTALRM;
      break;
    case ITIMER_PROF:
      sig = SIGPROF;
      break;
    default:
      fprintf(stderr, "ERROR: unknown timer type %d!\n", timer_type);
      exit(EXIT_FAILURE);
  }

  return sig;
}


/* Set a timer and a handler for the timer.

   Arguments

   type: type of timer, one of ITIMER_REAL, ITIMER_VIRTUAL, or ITIMER_PROF.

   handler: timer signal handler.

   ms: time in ms for the timer. 

 */
void set_timer(int type, void (*handler) (int), int ms) {
  struct itimerval timer;
  struct sigaction sa;

  /* Install signal handler for the timer. */
  memset (&sa, 0, sizeof (sa));
  sa.sa_handler =  handler;
  sigaction (timer_signal(type), &sa, NULL);

  /* Configure the timer to expire after ms msec... */
  timer.it_value.tv_sec = 0;
  timer.it_value.tv_usec = ms*1000;
  timer.it_interval.tv_sec = 0;
  timer.it_interval.tv_usec = ms*1000;

  // Initiate global time variables
  new_time_slice.it_value.tv_sec = 0;
  new_time_slice.it_value.tv_usec = ms*1000;
  new_time_slice.it_interval.tv_sec = 0;
  new_time_slice.it_interval.tv_usec = ms*1000;

  old_time.it_value.tv_sec = 0;
  old_time.it_value.tv_usec = ms*1000;
  old_time.it_interval.tv_sec = 0;
  old_time.it_interval.tv_usec = ms*1000;

  pause_timer.it_value.tv_sec = 0;
  pause_timer.it_value.tv_usec = 0;
  pause_timer.it_interval.tv_sec = 0;
  pause_timer.it_interval.tv_usec = 0;

  if (setitimer (type, &timer, NULL) < 0) {
    perror("Setting timer");
    exit(EXIT_FAILURE);
  };
}

/* Timer signal handler. */
void timer_context_switch(int signum){
  printf("TIMEHANDLER: thread_id '%d' preemted! Exceeded allowed timeslice.\n", running_thread->tid);
  yield();
}


/*******************************************************************************
                    Implementation of the Simple Threads API
********************************************************************************/


int init(){
  // Create thread manager, to handle threads that terminate
  if (init_context(&thread_manager_ctx, NULL) == -1){
    return -1;
  }
  makecontext(&thread_manager_ctx, manage_threads, 0);

  // Create the main thread.
  thread_t *main_thread = calloc(1, sizeof(thread_t));
  main_thread->tid = create_thread_id();
  main_thread->next = NULL;
  if (init_context(&(main_thread->ctx), &thread_manager_ctx) == -1){
    return -1;
  } 

  // Set main process as the currently running
  main_thread->state = running;
  running_thread = main_thread;

  // Start timer for time slice pre-emption
  set_timer(TIMER_TYPE, timer_context_switch, TIME_SLICE_MS);

  return 1;
}

tid_t spawn(void (*start)()){
  thread_t *new_thread = calloc(1, sizeof(thread_t));
  if (new_thread == NULL) {
    return -1;
  }

  if (init_thread(new_thread, start) == -1){
    return -1;
  }

  append(&ready_queue, new_thread);

  return new_thread->tid;
}

void yield(){
  PAUSE_TIMER();

  if (ready_queue == NULL) {
    RESUME_TIMER();
    return;
  } else {
    thread_t *old_running_thread = running_thread;
    old_running_thread->state = ready;
    
    append(&ready_queue, old_running_thread);
    
    running_thread = pop(&ready_queue);
    running_thread->state = running;

    printf("thread: %d -> ready\n", (int) old_running_thread->tid);
    printf("thread: %d -> running\n", (int) running_thread->tid);

    RESET_TIMER();
    swapcontext(&old_running_thread->ctx, &running_thread->ctx);
  }
}

void  done(){
  setcontext(&thread_manager_ctx);
}

tid_t join(tid_t thread_id) {
  // Avoid timer expiring during join.
  PAUSE_TIMER();

  if (running_thread->tid != thread_id &&
      !contains_thread(&ready_queue, thread_id) &&
      !contains_thread(&waiting_queue, thread_id)) {
    // Thread is not running, waiting nor ready. It must be dead.
    
    RESUME_TIMER();
    return thread_id;
  }


  thread_t *thread_to_wait = running_thread;
  thread_to_wait->waiting_for = thread_id;
  thread_to_wait->state = waiting;
  append(&waiting_queue, thread_to_wait);

  running_thread = pop(&ready_queue);
  running_thread->state = running;
  
  printf("thread: '%d' waiting for thread '%d' to terminate\n", thread_to_wait->tid, thread_to_wait->waiting_for);

  RESET_TIMER();
  swapcontext(&thread_to_wait->ctx, &running_thread->ctx);

  return thread_to_wait->waiting_for;
}
