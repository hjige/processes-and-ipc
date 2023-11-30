#include <stdio.h>    // puts(), printf(), perror(), getchar()
#include <stdlib.h>   // exit(), EXIT_SUCCESS, EXIT_FAILURE
#include <unistd.h>   // getpid(), getppid(), fork()
#include <sys/wait.h> // wait()
#include <time.h>     // time_t, time()
#include <sys/time.h> // gettimeofday()

#define NUM_CHILDREN 10
#define RAND_SIZE    1000

void child() {
  printf(" CHILD <%ld> My random number is:        %d\n", (long) getpid(), rand() % RAND_SIZE);
  exit(EXIT_SUCCESS);
}


int main(void) {
  pid_t pid;
  struct timeval t;

  /* Intializes random number generator */
  gettimeofday(&t, NULL); // Current time in microseconds.

  /* A simple seed: the current time in seconds. */

  // srand(time(NULL));

  /* A better seed: the current time in microsecons XOR the current tinme in seconds XOR the PID */

  srand((unsigned int) t.tv_usec ^ (unsigned int) t.tv_sec ^ (unsigned int) getpid());

  printf("PARENT <%ld> My first random number is:  %d\n", (long) getpid(), rand() % RAND_SIZE);

  for (int i = 0; i < NUM_CHILDREN; i++) {
    switch (pid = fork()) {
    case -1:
      // On error fork() returns -1.
      perror("fork failed");
      exit(EXIT_FAILURE);
    case 0:
      // On success fork() returns 0 in the child.
      child();
    default:
      // On success fork() returns the pid of the child to the parent.
      ; // Do nothing
    }
  }

  printf("PARENT <%ld> My second random number is: %d\n", (long) getpid(), rand() % RAND_SIZE);

  /* The parent waits for all the children */

  for (int i = 0; i < NUM_CHILDREN; i++) {
    wait(NULL);
  }

  exit(EXIT_SUCCESS);

}
