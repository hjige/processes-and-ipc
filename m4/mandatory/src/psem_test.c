#include <stdlib.h>  // abort()
#include <unistd.h>  // sleep()
#include <stdio.h>   // printf()
#include <pthread.h> // pthread_create()

#include "psem.h"    // init_sem(), wait_sem(), signal_sem(), destroy_sem()

#define N     5      // Number of iterations for both the main thread and the pthread.
#define SLEEP 1      // Number of seconds the pthread will sleep in each iteration.

psem_t *sem;         // Semaphore used to synchronize the main thread and the pthread.

void *thread() {
  for (int i = 0; i < N; i++) {
    sleep(SLEEP);
    psem_signal(sem);
  }
  pthread_exit(0);
}

int main(void) {
  pthread_t tid;
  sem = psem_init(0);

  if (pthread_create(&tid, NULL, thread, NULL) != 0) {
    perror("pthread_create()");
    abort();
  }

  for (int i = 0; i < N; i++) {
    printf("  main(%d) waiting on a semaphore ...\n", i);
    psem_wait(sem);
    printf("  main(%d)     semaphore signaled by thread.\n", i);
  }

  psem_destroy(sem);

}
