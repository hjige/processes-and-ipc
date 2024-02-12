#include <stdio.h> // perror()
#include <stdlib.h> // malloc()

#include "psem.h"

psem_t *psem_init(unsigned int value) {
  psem_t *sem = malloc(sizeof(sem_t));

 if (sem_init(sem, 0, value) == -1) {
   perror("Initializing new semaphore");
   abort();
 }
 return sem;
}

void psem_wait(psem_t *sem) {
  if (sem_wait(sem) == -1) {
    perror("Wating on sempahore failed");
    abort();
  }
}

void psem_signal(psem_t *sem) {
  if (sem_post(sem) == -1) {
    perror("Signaling on semaphore failed");
    abort();
  }
}

void psem_destroy(psem_t *sem) {
  if (sem_destroy(sem) == -1) {
    perror("Destroying semaphore failed");
    abort();
  }
}
