#include <stdio.h>    // puts(), printf(), perror(), getchar()
#include <stdlib.h>   // exit(), EXIT_SUCCESS, EXIT_FAILURE
#include <unistd.h>   // getpid(), getppid(),fork()
#include <sys/wait.h> // wait()

#define READ 0
#define WRITE 1
#define CHILD 0
#define ERROR -1
#define CHILD_COUNT 2
#define STDIN 0
#define STDOUT 1

void child_a(int fd[])
{
  dup2(fd[WRITE], STDOUT);
  close(fd[READ]);
  execlp("ls", "ls", "-F", "-1", NULL);

  perror("Return from execlp() not expected");
  exit(EXIT_FAILURE);
}

void child_b(int fd[])
{
  dup2(fd[READ], STDIN);
  close(fd[WRITE]);
  execlp("nl", "nl", NULL);

  perror("Return from execlp() not expected");
  exit(EXIT_FAILURE);
}

int main(void)
{
  int fd[2];
  if (pipe(fd) == ERROR)
  {
    perror("unexpected failure of pipe()");
    exit(EXIT_FAILURE);
  }

  pid_t child_a_id;
  pid_t child_b_id;

  // Create first child
  pid_t pid = fork();
  if (pid == ERROR)
  {
    perror("unexpected failure of first call to fork()");
    exit(EXIT_FAILURE);
  }
  else if (pid == CHILD)
  {
    child_a(fd);
  }

  child_a_id = pid;

  // Create second child
  pid = fork();
  if (pid == ERROR)
  {
    perror("unexpected failure of second call to fork()");
    exit(EXIT_FAILURE);
  }
  else if (pid == CHILD)
  {
    child_b(fd);
  }

  child_b_id = pid;

  int child_a_status = __INT_MAX__;
  int child_b_status = __INT_MAX__;

  // Wait for 2 children to terminate.
  // NOTE: As child b waits for child a input, child b will always terminate first.
  for (int i = 0; i < CHILD_COUNT; ++i)
  {
    int status = __INT_MAX__;
    int child_pid = wait(&status);

    // If-statement to determine which child is terminated.
    if ((long)child_pid == child_a_id)
    {
      child_a_status = status;
      close(fd[WRITE]); // Close after writing to avoid block of reader
    }
    else if ((long)child_pid == child_b_id)
    {
      // Since no writer attached, the pipe will return EOF on read, terminating nl.
      child_b_status = status;
      close(fd[READ]);
    }
    else if ((long)child_pid == ERROR)
    {
      perror("unexpected failure of call to wait()");
      exit(EXIT_FAILURE);
    }
    else
    {
      perror("unexpected pid");
      exit(EXIT_FAILURE);
    }
  }

  return child_a_status == 0 && child_b_status == 0;
}
