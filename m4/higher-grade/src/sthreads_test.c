#include <stdlib.h>   // exit(), EXIT_FAILURE, EXIT_SUCCESS
#include <stdio.h>    // printf(), fprintf(), stdout, stderr, perror(), _IOLBF
#include <stdbool.h>  // true, false
#include <limits.h>   // INT_MAX
#include <unistd.h>   // sleep()

#include "sthreads.h" // init(), spawn(), yield(), done()

/*******************************************************************************
                   Functions to be used together with spawn()

    You may add your own functions or change these functions to your liking.
********************************************************************************/

/* Prints the sequence 0, 1, 2, .... INT_MAX over and over again.
 */
void numbers() {
  int n = 0;
  while (true) {
    printf(" n = %d\n", n);
    n = (n + 1) % (INT_MAX);
    if (n > 3) done();
  }
}

/* Prints the sequence a, b, c, ..., z over and over again.
 */
void letters() {
  char c = 'a';

  while (true) {
      printf(" c = %c\n", c);
      if (c == 'f') done();
      c = (c == 'z') ? 'a' : c + 1;
    }
}

/* Calculates the nth Fibonacci number using recursion.
 */
int fib(int n) {
  switch (n) {
  case 0:
    return 0;
  case 1:
    return 1;
  default:
    return fib(n-1) + fib(n-2);
  }
}

/* Print the Fibonacci number sequence over and over again.

   https://en.wikipedia.org/wiki/Fibonacci_number

   This is deliberately an unnecessary slow and CPU intensive
   implementation where each number in the sequence is calculated recursively
   from scratch.
*/

void fibonacci_slow() {
  int n = 0;
  int f;
  while (true) {
    f = fib(n);
    if (f < 0) {
      // Restart on overflow.
      n = 0;
    }
    printf(" fib(%02d) = %d\n", n, fib(n));
    n = (n + 1) % INT_MAX;
    // yield();
  }
}

/* Print the Fibonacci number sequence over and over again.

   https://en.wikipedia.org/wiki/Fibonacci_number

   This implementation is much faster than fibonacci().
*/
void fibonacci_fast() {
  int a = 0;
  int b = 1;
  int n = 0;
  int next = a + b;

  while(n != 20) {
    printf(" fib(%02d) = %d\n", n, a);
    next = a + b;
    a = b;
    b = next;
    n++;
    if (a < 0) {
      // Restart on overflow.
      a = 0;
      b = 1;
      n = 0;
    }
    yield();
  }
  done();
}

/* Prints the sequence of magic constants over and over again.

   https://en.wikipedia.org/wiki/Magic_square
*/
void magic_numbers() {
  int n = 3;
  int m = 0;
  while (m > 10000) {
    m = (n*(n*n+1)/2);
    if (m > 0) {
      printf(" magic(%d) = %d\n", n, m);
      n = (n+1) % INT_MAX;
    } else {
      // Start over when m overflows.
      n = 3;
    }
    // yield();
  }

  done();
}

void straight_a() {
  while (true) {
      puts("a");
      yield();
  }
}

void straight_b() {
  while (true) {
      puts("b");
      yield();
  }
}

void straight_c() {
  while (true) {
      puts("c");
      yield();
  }
}

void straight_d() {
  while (true) {
      puts("d");
      yield();
  }
}

void i_will_yield() {
  while(true) {
    puts("I am thread");
    yield();
  }
}

void i_am_done_a() {
  while(true) {
    puts("a is done");
    done();
  }
}

void i_am_done_b() {
  while(true) {
    puts("b is done");
    done();
  }
}

void i_am_done_c() {
  while(true) {
    puts("c is done");
    done();
  }
}

void waiting_magic_numbers(tid_t thread){
  while(true){
    tid_t magic_numbers_thread = spawn(magic_numbers);
    puts("waiting for magic_numbers");
    join(magic_numbers_thread);
    puts("magic_numbers done!");
    
    done();
  }
}

void waiting_several(tid_t thread){
  while(true){
    tid_t magic_numbers_thread = spawn(waiting_magic_numbers);
    tid_t fibonacci_fast_thread = spawn(fibonacci_fast);
    
    puts("waiting for waiting_magic_numbers");
    join(magic_numbers_thread);
    puts("waiting_magic_numbers done!");
    
    puts("waiting for fibonacci fast");
    join(fibonacci_fast_thread);
    puts("fibonacci fast done!");

    done();
  }
}

/*******************************************************************************
                                     main()

            Here you should add code to test the Simple Threads API.
********************************************************************************/


int main(){
  puts("\n==== Test program for the Simple Threads API ====\n");

  init(); // Initialization

  // spawn(straight_a);
  // spawn(straight_b);
  // spawn(straight_c);
  // spawn(straight_d);
  // spawn(magic_numbers);
  // spawn(fibonacci_fast);
  // spawn(fibonacci_slow);
  // yield();

  // tid_t a = spawn(i_am_done_a);
  // tid_t b = spawn(i_am_done_b);
  // tid_t c = spawn(i_am_done_c);

  // puts("in MAIN");
  
  // puts("waiting for a");
  // join(a);
  // puts("successfully waited on a");
  
  // puts("waiting for b");
  // join(b);
  // puts("successfully waited on b");
  
  // puts("waiting for c");
  // join(c);
  // puts("successfully waited on c");
  
  // tid_t waiting_thread_magic_numbers = spawn(waiting_magic_numbers);
  // tid_t waiting_several_threads = spawn(waiting_several);

  // join(waiting_thread_magic_numbers);
  // puts("in main: waiting for magic numbers done:");
  // join(waiting_several_threads);
  // puts("in main: waiting for several threads done:");

  spawn(fibonacci_slow);
  spawn(fibonacci_slow);
  spawn(fibonacci_slow);

  while(true) {
    sleep(1);
  }

  printf("back in main\n");
}
