/*
  A generic API for semaphores than can be used both on Linux and macOS
  (aka OS X).
  
  This API provides a simplified version of the underlying platform dependent
  semaphore implementation.

  On error all functions in the API prints an error message an terminates the
  program.

  2018-03-01

  First version by Karl Marklund <karl.marklund@it.uu.se>.
*/

#ifndef PSEM_H
#define PSEM_H

/* Platform dependent definition of the psem_t data type. */
#include "platform_specifics.h"

/*******************************************************************************
                                 Semaphore API
********************************************************************************/

/* psem_init(value)

   Initializes a new semaphore with semaphore counter set to value.

   Return value

   On success a pointer to a new semaphore is returned. On failure, an error
   message is printed and the program terminated.
*/
psem_t *psem_init(unsigned int value);

/* psem_wait(sem)

  Atomically decrements the counter of the semaphore pointed to by sem. If the
  semaphore's counter value is greater than zero, then the decrement proceeds,
  and the function returns, immediately. If the semaphore counter currently has
  the value zero, then the call blocks until another process or thread signal the
  semaphore.
*/
void psem_wait(psem_t *sem);

/* psem_signal(sem)

   Atomically increments the counter of the semaphore pointed to by sem.  If
   the semaphore's counter value consequently becomes greater than zero, then
   another process or thread blocked in a psem_wait() call will be
   woken up and proceed to lock the semaphore.
*/
void psem_signal(psem_t *sem);

/* psem_destroy(sem)

   Destroys the semaphore pointed to by sem. Only a semaphore that has been
   initialized by psem_init() should be destroyed using psem_destroy().
 */
void psem_destroy(psem_t *sem);

#endif
