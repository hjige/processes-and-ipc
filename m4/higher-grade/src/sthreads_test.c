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
  int counter = 0;
  int n = 0;
  int f;
  while (counter < 35) {
    f = fib(n);
    if (f < 0) {
      // Restart on overflow.
      n = 0;
    }
    printf(" fib(%02d) = %d\n", n, fib(n));
    n = (n + 1) % INT_MAX;
    // yield();
    counter++;
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

void done_a() {
  int n = 0;
  while(n < 4) {
    puts(" a");
    n++;
    sleep(1);
  }
  puts(" a is done");
  done();
}

void done_b() {
  int n = 0;
  while(n < 7) {
    puts(" b");
    n++;
    sleep(1);
  }
  puts(" b returns");
  return;
}

void done_c() {
  int n = 0;
  while(n < 9) {
    puts(" c");
    n++;
    sleep(1);
  }
  puts(" c is done");
  done();
}



// FOREVER /W SLEEP
void forever_a() {
  while(true) {
    puts(" a");
    sleep(1);
  }
}
void forever_b() {
  while(true) {
    puts(" b");
    sleep(1);
  }
}
void forever_c() {
  while(true) {
    puts(" c");
    sleep(1);
  }
}

// YIELD
void yield_a() {
  while(true) {
    puts(" a");
    yield();
  }
}
void yield_b() {
  while(true) {
    puts(" b");
    yield();
  }
}
void yield_c() {
  while(true) {
    puts(" c");
    yield();
  }
}

void waiting_magic_numbers(tid_t thread){
  while(true){
    tid_t magic_numbers_thread = spawn(magic_numbers);
    puts(" waiting for magic_numbers");
    join(magic_numbers_thread);
    puts(" magic_numbers done!");
    
    done();
  }
}

void waiting_several(tid_t thread){
  while(true){
    tid_t magic_numbers_thread = spawn(waiting_magic_numbers);
    tid_t fibonacci_slow_thread = spawn(fibonacci_slow);
    
    puts(" waiting for waiting_magic_numbers");
    join(magic_numbers_thread);
    puts(" waiting_magic_numbers done!");
    
    puts(" waiting for fibonacci slow");
    join(fibonacci_slow_thread);
    puts(" fibonacci slow done!");

    done();
  }
}

/// @brief Non-terminating threads sleeping after each print to exceed time slice.
void test_preemtion() {
  init(999);

  spawn(forever_a);
  spawn(forever_b);
  spawn(forever_c);

  while(true){
    puts(" main");
    sleep(1);
  }
}

/// @brief Non-terminating threads, yielding after each print.
void test_yield() {
  init(999);

  spawn(yield_a);
  spawn(yield_b);
  spawn(yield_c);

  while(true){
    puts(" main, slowing things down..");
    // main sleeps whole timeslice, so terminal doesnt get spammed.
    sleep(1);
  }
}

void test_terminate_by_done() {
  init(999);

  tid_t thread_a = spawn(done_a);
  tid_t thread_b = spawn(done_b);
  tid_t thread_c = spawn(done_c);

  while(true){
    puts(" main");
    join(thread_a);
    join(thread_b);
    join(thread_c);
    puts(" main is done");
    done();
  }
}

void test_waiting_recursive(){
  init(50);

  tid_t waiting_thread_magic_numbers = spawn(waiting_magic_numbers);
  tid_t waiting_several_threads = spawn(waiting_several);

  join(waiting_thread_magic_numbers);
  puts(" in main: waiting for magic numbers done:");
  join(waiting_several_threads);
  puts(" in main: waiting for several threads done:");

}

// TODO: test_terminate_by_return()

/*******************************************************************************
                                     main()

            Here you should add code to test the Simple Threads API.
********************************************************************************/


int main(){
  puts("\n==== Test program for the Simple Threads API ====\n");

  // test_preemtion();
  // test_yield();
  // test_terminate_by_done();
  test_waiting_recursive();

  printf("back in main\n");
}
