#include "parser.h" // cmd_t, position_t, parse_commands()

#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <stdlib.h>
#include <stdbool.h>
#include <fcntl.h> //fcntl(), F_GETFL

#define READ 0
#define WRITE 1
#define ERROR -1

/**
 * For simplicitiy we use a global array to store data of each command in a
 * command pipeline .
 */
cmd_t commands[MAX_COMMANDS];

/**
 *  Debug printout of the commands array.
 */
void print_commands(int n)
{
  for (int i = 0; i < n; i++)
  {
    printf("==> commands[%d]\n", i);
    printf("  pos = %s\n", position_to_string(commands[i].pos));
    printf("  in  = %d\n", commands[i].in);
    printf("  out = %d\n", commands[i].out);

    print_argv(commands[i].argv);
  }
}

/**
 * Returns true if file descriptor fd is open. Otherwise returns false.
 */
int is_open(int fd)
{
  return fcntl(fd, F_GETFL) != -1 || errno != EBADF;
}

void fork_error()
{
  perror("fork() failed)");
  exit(EXIT_FAILURE);
}

/**
 *  Fork a proccess for command with index i in the command pipeline. If needed,
 *  create a new pipe and update the in and out members for the command..
 */
void fork_cmd(int i)
{
  pid_t pid;

  int fd[2];
  bool is_first_or_mid = commands[i].pos == first || commands[i].pos == middle;
  if (is_first_or_mid)
  {
    // Create pipe
    if (pipe(fd) == ERROR)
    {
      perror("Unexpected failure when creating pipe");
      exit(EXIT_FAILURE);
    }
    else
    {
      // Save creaded pipe's fd's table indexes.
      commands[i].out = fd[WRITE];
      commands[i + 1].in = fd[READ];
    }
  }

  switch (pid = fork())
  {
  case -1:
    fork_error();
  case 0:
    // Child process after a successful fork().

    // Set stdout to pipe producer end.
    if (commands[i].out != STDOUT_FILENO)
    {
      dup2(commands[i].out, STDOUT_FILENO);
      // Close the read end of the pipe, not used by this process.
      close(fd[READ]);
    }

    // Set stdin to pipe consumer end.
    if (commands[i].in != STDIN_FILENO)
    {
      dup2(commands[i].in, STDIN_FILENO);
      // Write end of this pipe already closed by parent on previous iteration.
    }

    // Execute the command in the contex of the child process.
    execvp(commands[i].argv[0], commands[i].argv);

    // If execvp() succeeds, this code should never be reached.
    fprintf(stderr, "shell: command not found: %s\n", commands[i].argv[0]);
    exit(EXIT_FAILURE);

  default:
    // Parent process after a successful fork().

    if (is_first_or_mid)
    {
      // Already created child for write end, so producer fd can be safely closed.
      close(fd[WRITE]);
    }

    break;
  }
}

/**
 *  Fork one child process for each command in the command pipeline.
 */
void fork_commands(int n)
{
  for (int i = 0; i < n; i++)
  {
    fork_cmd(i);
  }

  // Close all pipe read fds (if any).
  for (int i = 0; i < n; i++)
  {
    if (commands[i].in != STDIN_FILENO)
    {
      close(commands[i].in);
    }
  }
}

/**
 *  Reads a command line from the user and stores the string in the provided
 *  buffer.
 */
void get_line(char *buffer, size_t size)
{
  // if (getline(&buffer, &size, stdin) != ERROR)
  getline(&buffer, &size, stdin);
  buffer[strlen(buffer) - 1] = '\0';
}

/**
 * Make the parents wait for all the child processes.
 */
void wait_for_all_cmds(int n)
{
  for (int i = 0; i < n; i++)
  {
    int status;
    pid_t child_pid = wait(&status);

    if ((long)child_pid == ERROR)
    {
      perror("Unexpected exit of child process");
      exit(EXIT_FAILURE);      
    }

    if (WIFEXITED(status) && WEXITSTATUS(status) != EXIT_SUCCESS)
    {
      // grep exits with code "1" if no line was found. 
      printf("Exit status: %d\n", WEXITSTATUS(status));
      perror("Unexpected exit of child process");
      exit(EXIT_FAILURE);
    }
  }
}

int main()
{
  int n;             // Number of commands in a command pipeline.
  size_t size = 128; // Max size of a command line string.
  char line[size];   // Buffer for a command line string.

  while (true)
  {
    printf(" >>> ");

    get_line(line, size);

    n = parse_commands(line, commands);

    fork_commands(n);

    // print_commands(n); // TODO: Remove this debug line

    wait_for_all_cmds(n);
  }

  exit(EXIT_SUCCESS);
}
