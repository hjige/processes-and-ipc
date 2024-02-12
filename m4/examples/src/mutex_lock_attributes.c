
#include <stdio.h>   // printf(), fprintf()
#include <stdlib.h>  // abort()
#include <pthread.h> // pthread_...
#include <stdbool.h> // true, false

int main()
{
  pthread_mutex_t mutex;
  pthread_mutexattr_t attr;

  /* Create mutex attribute */

  if (pthread_mutexattr_init(&attr) != 0)
  {
    perror("pthread_mutex_attr_init() error");
    exit(EXIT_FAILURE);
  }

  /*

  When using he PTHREAD_MUTEX_ERRORCHECK attribute:

  A thread attempting to relock this mutex without first unlocking it will return with an error.
  A thread attempting to unlock a mutex which another thread has locked will return with an error.
  A thread attempting to unlock an unlocked mutex will return with an error.

  */

  if (pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_ERRORCHECK) != 0)
  {
    perror("pthread_mutexattr_settype() error");
    exit(EXIT_FAILURE);
  }

  if (pthread_mutex_init(&mutex, &attr) != 0)
  {
    perror("mutex_lock");
    exit(EXIT_FAILURE);
  }

  /* If we dont first lock the mutex ... */

  /*
  if (pthread_mutex_lock(&mutex) != 0) {
    perror("pthread_mutex_lock()");
    exit(4);
  };
  */

  /* ... unlocking the mutex will now result in an error. */

  if (pthread_mutex_unlock(&mutex) != 0)
  {
    perror("pthread_mutex_unlock()");
    exit(EXIT_FAILURE);
  }

  exit(EXIT_SUCCESS);
}
