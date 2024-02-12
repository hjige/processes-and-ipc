/*

  When a new pipe is created or a file is opened, new file descriptors will be
  added to the file descriptor table. The kernel always allocates the lowest
  unused file descriptor when a new descriptor is added to the file descriptor
  table.

  As long as there is at least one open file descriptor to a pipe, the pipe will
  not be deallocated by the kernel. When all descriptors to a pipe has been
  closed, the pipe can be deallocated by the kernel.

  In this example two pipes A and B are created. Experiment by closing none, one
  or both of the pipe A descriptors before creating pipe B and try to predict
  the pipe B file descriptor values.

  2021-02-11

  First version by Karl Marklund <karl.marklund@it.uu.se>.

*/

#include <stdio.h>  // printf()
#include <stdlib.h> // EXIT_SUCCESS, EXIT_FAILURE
#include <unistd.h> // STDIN_FILENO, STDOUT_FILENO, STDERR_FILENO, pipe()



void open_pipe(const char* name, int fd[2]) {
    if (pipe(fd) < 0) {
      perror("Creating pipe");
      exit(EXIT_FAILURE);
    }
    printf("pipe(%s)\n", name);
    printf("%s[0] = %d (read)\n%s[1] = %d (write)\n", name, fd[0], name, fd[1]);
}

void close_descriptor(const char* type, int fd) {
  printf("close(%d)      (%s)\n", fd, type);

  if (close(fd) < 0){
      perror("close");
      exit(EXIT_FAILURE);
  }
}

void std_info(){
  printf("stdin     = %d (read)\n",  STDIN_FILENO);
  printf("stdout    = %d (write)\n", STDOUT_FILENO);
  printf("stderr    = %d (write)\n", STDERR_FILENO);
}

int main(){

  int pipe_a[2];
  int pipe_b[2];

  std_info();

  open_pipe("pipe_a", pipe_a);

  // TODO: Uncomment one or both of the following close operations. Try to
  // predict the pipe B descriptor values and see if your predictions matches
  // the printouts.

  // close_descriptor("read",  pipe_a[0]);
  // close_descriptor("write", pipe_a[1]);

  open_pipe("pipe_b", pipe_b);
}
