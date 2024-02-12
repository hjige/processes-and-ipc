#ifndef PARSER_H_
#define PARSER_H_

#define MAX_COMMANDS 16
#define MAX_ARGV     16

/**
 *  The position of a command within a commd pipeline.
 */
typedef enum {single, first, middle, last, unknown} position_t;

/**
 * Structure holding all data for a single command in a command pipeline.
 */
typedef struct {
  char*  argv[MAX_ARGV];  // Argument vector.
  position_t pos;         // Position within the pipeline (single, first, middle or last).
  int in;                 // Input file descriptor.
  int out;                // Output file descriptor.
} cmd_t;

/**
 *  Parses the string str and populates the commands array with data for each
 *  command.
 */
int parse_commands(char *str, cmd_t* commands);

/**
 * Prints out the elements of the argument vector argv.
 */
void print_argv(char* argv[]);

/**
 * Converts a pos_t position (single, first, middle or last) to a string
 * ("single", "first", "middle" or "last").
 */
char* position_to_string(position_t pos);

#endif // PARSER_H_
