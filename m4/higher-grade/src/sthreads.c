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

#include "sthreads.h"

/* Stack size for each context. */
#define STACK_SIZE SIGSTKSZ*100

/*******************************************************************************
                             Global data structures

                Add data structures to manage the threads here.
********************************************************************************/

thread_t *ready_queue = NULL;
tid_t thread_id = 0;  // main thread has id 0


/*******************************************************************************
                             Auxiliary functions

                      Add internal helper functions here.
********************************************************************************/

/// @brief pops the first element in the queue, removing it from the queue.
/// @param queue linked list to pop from.
/// @return ptr to thread
thread_t *pop(thread_t **queue) {
  thread_t *thread_to_return = NULL;
  if (*queue != NULL) {
    thread_to_return = *queue;
    *queue = (*queue)->next; 
  }

  return thread_to_return;
}

/// @brief Appends a thread to the end of the queue
/// @param queue 
/// @param thread_to_append 
void append(thread_t **queue, thread_t *thread_to_append) {
  while (*queue != NULL) {
    queue = &(*queue)->next;
  }
  *queue = thread_to_append;
}

/// @brief Creates a new unique thread id
/// @return a unique thread id
tid_t create_thread_id() {
  thread_id++;
  return thread_id;
}

init_thread(thread_t* thread) {
  thread->tid = create_thread_id();
  thread->state = ready;
  thread->next = NULL;
  // TODO: initialize context
}

/* Initialize a context.

   ctxt - context to initialize.

   next - successor context to activate when ctx returns. If NULL, the thread
          exits when ctx returns.
 */
void init_context(ucontext_t *ctx, ucontext_t *next) {
  /* Allocate memory to be used as the stack for the context. */
  void *stack = malloc(STACK_SIZE);

  if (stack == NULL) {
    perror("Allocating stack");
    exit(EXIT_FAILURE);
  }

  if (getcontext(ctx) < 0) {
    perror("getcontext");
    exit(EXIT_FAILURE);
  }

  /* Before invoking makecontext(ctx), the caller must allocate a new stack for
     this context and assign its address to ctx->uc_stack, and define a successor
     context and assigns address to ctx->uc_link.
  */

  ctx->uc_link           = next;
  ctx->uc_stack.ss_sp    = stack;
  ctx->uc_stack.ss_size  = STACK_SIZE;
  ctx->uc_stack.ss_flags = 0;
}

/*******************************************************************************
                    Implementation of the Simple Threads API
********************************************************************************/


int  init(){
  // Create a thread manager thread?
  return 1;
}

tid_t spawn(void (*start)()){
  // TODO: Implement function
  thread_t *new_thread = calloc(1, sizeof(thread_t));
  init_thread(new_thread);
  return -1;
}

void yield(){
  // TODO: Implement function
}

void  done(){
}

tid_t join(tid_t thread) {
  return -1;
}
