#include "parser.h"

#include <unistd.h>  // STDIN_FILENO, STDOUT_FILENO
#include <stdlib.h>  // EXIT_FAILURE
#include <stdio.h>   // printf()
#include <string.h>  // strlen()
#include <ctype.h>   // isspace(),
#include <string.h>  // strtok_r()
#include <stdbool.h> // true, false

/**
 * Returns true if a string only contains white space characters. Otherwise
 * returns false.
 */
bool empty(char* str) {
  while (*str) {
    if (!isspace(*str++))
      return false;
  }
  return true;
}

/**
 *  Strip whitespace (tabs, spaces, etc) from the beginning of a
 *  string.
 */
char* ltrim(char* s) {
  while (isspace((int)*s)) ++s;
  return s;
}

/**
 *  Strip whitespace (tabs, spaces, etc) from the end of a
 *  string.
 */
char* rtrim(char* str) {
  if(*str == 0 )
    return str;
  else {
    char* back = str + strlen(str)-1;  // Pointer to last char in str.

    while( isspace( *back) ) back--;   // While trailing whitespace move pointer-

    *(back+1) = 0; // Done, add terminating '\0'.

    return str;

  }
}

/**
 *  Strip whitespace (tabs, spaces, etc) from the beginning and the
 *  end of a string.
 */
char* trim(char* str) {
  if (str != NULL)
    return(ltrim(rtrim(str)));
  else return NULL;
}

/**
 *  Replaces the delim character in str with NULL and populates the argv array
 *  with pointers to each token. Each such token pointer points to a sub string
 *  in str.
 */
void get_argv(char* str, const char* delim, char* argv[]) {

  char *token;
  int i = 0;

  /* get the first token */
  token = strtok(str, delim);

  /* walk through other tokens */
  while( token != NULL ) {
    argv[i] = token;
    token = strtok(NULL, delim);
    i++;
  }
  argv[i] = NULL;
}

/**
 * Prints out the elements of the argument vector argv.
 */
void print_argv(char* argv[]) {
  int i = 0;
  char* s;

  while ((s = argv[i])) {
    printf("  argv[%d] = %s\n", i, s);
    i++;
  }
}

/**
 * Splits the string str on each occurence of the delim character by replacing
 * each delim character with NULL. Populates the cmds array with pointers to
 * each sub command. Each such pointer is a pointer to a sub string in str.
 */
void parse_cmds(char* str, const char* delim, char* cmds[]) {

  char *token;
  int i = 0;

  /* get the first token */
  token = strtok(str, delim);

  /* walk through other tokens */
  while( token != NULL ) {
    if (empty(token)) {
      fprintf(stderr, "Parser error: EMPTY command!\n");
      exit(EXIT_FAILURE);
    }

    cmds[i] = trim(token);
    token = strtok(NULL, delim);
    i++;
  }
  cmds[i] = NULL;
}

/**
 * Converts a pos_t position (single, first, middle or last) to a string
 * ("single", "first", "middle", "last").
 */
char* position_to_string(position_t pos) {

  switch (pos) {
  case single:
    return "single";
  case first:
    return "first";
  case middle:
    return "middle";
  case last:
    return "last";
  case unknown:
    return "unknown";
  default:
    return "?????";
  }
}

/**
 * n - number of commands.
 * i - index of a command (the first command has index 0).
 *
 * Returns the position (single, first, middle or last) of the command at index
 * i.
 */
position_t cmd_position(int i, int n) {
  if (i == 0 && n == 1) return single;

  // TODO: Add more cases ...

  return unknown;
}

/**
 *  Populates the array commands with argument vectors for all commands in the string str.
 *
 *  Initializes each commands:
 *     - position            to unknown
 *     - in file descriptor  to STDIN
 *     - out file descriptor to STDOUT
 *
 *  Returns the numnber of parsed commands.
 */
int first_pass(char *str, cmd_t* commands) {
  char* cmds[MAX_COMMANDS];

  parse_cmds(str, "|", cmds);

  int i = 0;

  while (cmds[i]) {
    commands[i].pos = unknown;
    get_argv(cmds[i], " ", commands[i].argv);
    commands[i].in   = STDIN_FILENO;
    commands[i].out  = STDOUT_FILENO;
    i++;
  }

  return i;
}

/**
 * After the first pass, we can find out each commands position (single, first,
 * middle or last).
 */
void second_pass(cmd_t* commands, int n){
  for (int i = 0; i < n; i++) {
    commands[i].pos = cmd_position(i, n);
  }
}

/**
 *  Parses the string str and populates the commands array with data for each
 *  command.
 */
int parse_commands(char *str, cmd_t* commands) {
  int n = first_pass(str, commands);
  second_pass(commands, n);
  return n;
}
