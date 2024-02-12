#include <unistd.h> // mkstemp()
#include <string.h> // strcpy()
#include <stdio.h>	// perror()
#include <stdlib.h>	// malloc()

#include "psem.h"

/*
  Unnamed POSIX semaphores are not implemented on macOS (aka OS X).
  Use named semaphores from sempahore.h to implement a generic API to
  un-named semaphores.
*/

void cleanup(psem_t *sem) {
  if (sem_close(sem->sem) == -1) {
    perror("sem_close()");
    abort();
  }

  if (sem_unlink(sem->name) == -1) {
    perror("sem_unlink()");
    abort();
  }

  free(sem->name);
  free(sem);
  sem = NULL;
}

void perror_and_abort(psem_t *sem, const char* msg) {
  perror(msg);
  cleanup(sem);
  abort();
}

psem_t *psem_init(unsigned int value) {
  psem_t *sem = malloc(sizeof(psem_t));

  sem->name = strdup("/tmp/semaphore.XXXXXX");
  mktemp(sem->name);

  sem->sem = sem_open(sem->name, O_CREAT | O_EXCL, 0, value);

  if (sem->sem == SEM_FAILED) {
    perror_and_abort(sem, "sem_open()");
  }
  return sem;
}


void psem_wait(psem_t *sem) {
  if (sem_wait(sem->sem) == -1) {
    perror_and_abort(sem, "sem_wait()");
  }
}

void psem_signal(psem_t *sem) {
  if (sem_post(sem->sem) == -1) {
    perror_and_abort(sem, "sem_post()");
  }
}

void psem_destroy(psem_t *sem) {
  cleanup(sem);
}
