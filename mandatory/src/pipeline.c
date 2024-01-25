#include <stdio.h>    // puts(), printf(), perror(), getchar()
#include <stdlib.h>   // exit(), EXIT_SUCCESS, EXIT_FAILURE
#include <unistd.h>   // getpid(), getppid(),fork()
#include <sys/wait.h> // wait()

#define READ 0
#define WRITE 1
#define CHILD 0
#define ERROR -1

void child_a(int fd[])
{
  // TODO: Add code here.

  execlp("ls", "ls", "-F", "-1", NULL);
}

void child_b(int fd[])
{

  // TODO: Add code here.
}

int main(void)
{
  int fd[2];
  // TODO: create pipe

  pid_t child_a_id;
  pid_t child_b_id;

  pid_t pid = fork();
  if (pid == ERROR)
  {
    // TODO: Error
  }
  else if (pid == CHILD)
  {
    child_a(fd);
    exit(EXIT_SUCCESS);
  }

  child_a_id = pid;
  // Create second child
  pid = fork();
  if (pid == ERROR)
  {
    // Error
  }
  else if (pid == CHILD)
  {
    child_b(fd);
    exit(EXIT_SUCCESS);
  }

  child_b_id = pid;

  // TODO: fix
  int child_a_status = __INT_MAX__;
  waitpid(child_a_id, &child_a_status);

  int child_b_status = __INT_MAX__;
  waitpid(child_b_id, &child_b_status);
}