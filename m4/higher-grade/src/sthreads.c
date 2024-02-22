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

thread_t *running_thread = NULL;
thread_t *ready_queue = NULL;
thread_t *waiting_queue = NULL;
tid_t thread_id = 0;
ucontext_t thread_manager_ctx;


/*******************************************************************************
                             Auxiliary functions

                      Add internal helper functions here.
********************************************************************************/



/// @brief pops the first element in the queue, removing it from the queue.
/// @param queue ptr to queue to pop from.
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
/// @param queue ptr to queue to append to
/// @param thread_to_append 
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

/// @brief Creates a new unique thread id
/// @return a unique thread id
tid_t create_thread_id() {
  thread_id++;  // Global variable
  return thread_id;
}

/* Initialize a context.

   ctxt - context to initialize.

   next - successor context to activate when ctx returns. If NULL, the thread
          exits when ctx returns.
 */
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

/// @brief 
/// @param thread 
/// @param func 
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

/// @brief 
void manage_threads() {
  // TODO:
}

void find_waiting_threads(tid_t terminated_thread){
  if (waiting_queue == NULL){
    return;
  }
  
  thread_t *current = waiting_queue;
  while(current != NULL){
    if (terminated_thread == current->waiting_for){
      // TODO: unlink from waiting


      // TODO: append to ready
    }
    
  }
  
}

void destroy_thread(thread_t *thread) {
  free(thread->ctx.uc_stack.ss_sp);
  free(thread);
}

/*******************************************************************************
                    Implementation of the Simple Threads API
********************************************************************************/


int init(){
  if (init_context(&thread_manager_ctx, NULL) == -1){
    return -1;
  } 

  return 1;
  makecontext(&thread_manager_ctx, manage_threads, 0);
}

tid_t spawn(void (*start)()){
  // TODO: Implement function
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
  if (ready_queue == NULL) {
    return;
  } else if (running_thread == NULL) {
    // Currently running in main thread.
    running_thread = pop(&(ready_queue));
    running_thread->state = running;
    
    printf("thread: %d -> running\n", (int) running_thread->tid);
    setcontext(&(running_thread->ctx));
  } else {
    thread_t *old_running_thread = running_thread;
    old_running_thread->state = ready;
    
    append(&ready_queue, old_running_thread);
    
    running_thread = pop(&ready_queue);
    running_thread->state = running;

    printf("thread: %d -> ready\n", (int) old_running_thread->tid);
    printf("thread: %d -> running\n", (int) running_thread->tid);

    swapcontext(&old_running_thread->ctx, &running_thread->ctx);
  }
}

void  done(){
  // TODO: handle thread to terminate.
  thread_t *terminated_thread = running_thread;
  terminated_thread->state = terminated;
  
  find_waiting_threads(terminated_thread->tid);
  //TODO: find all threads in waiting list that waits for this thread,
  //TODO: set them to ready and move from waiting list to ready list.

  destroy_thread(terminated_thread);  
}

tid_t join(tid_t thread_id) {
  thread_t *thread_to_wait = running_thread;
  thread_to_wait->waiting_for = thread_id;
  thread_to_wait->state = waiting;
  append(&waiting_queue, thread_to_wait);

  running_thread = pop(&ready_queue);
  running_thread->state = running;
  
  printf("thread: '%d' waiting for thread '%d' to terminate\n", thread_to_wait->tid, thread_to_wait->waiting_for);

  swapcontext(&thread_to_wait->ctx, &running_thread->ctx);

  return thread_to_wait->waiting_for;
}
