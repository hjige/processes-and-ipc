/**
 * Unit test for the bounded buffer.
 *
 * History:
 *
 * 2020 - Original version by Karl Marklund <karl.marklund@it.uu.se>.
 *
 * 2021 - Updated behaviour for Ubuntu machines <bjorn.safsten.5186@student.uu.se>.
 */

#include "bounded_buffer.h"

#include <stdio.h>   // printf(), fprintf, srand(), rand(), setbuf(), stdin, stdout, stderr
#include <stdlib.h>  // EXIT_SUCCESS, EXIT_FAILURE
#include <time.h>    // time()
#include <stddef.h>  // NULL
#include <unistd.h>  // usleep(), sleep()
#include <pthread.h> // pthread_.. 
#include <assert.h>  // assert()

#define TEST_HEADER printf("\n==== %s ====\n\n", __FUNCTION__)

void success() {
  printf("\nTest SUCCESSFUL :-)\n\n");
}

void init_test() {
  TEST_HEADER;
  buffer_t buffer;
  
  buffer.array = NULL;
  buffer.mutex = NULL;
  buffer.data = NULL;
  buffer.empty = NULL;
  

  buffer_init(&buffer, 10);

  assert(buffer.size == 10);
  assert(buffer.in == 0);
  assert(buffer.out == 0);
  assert(buffer.array != NULL);
  assert(buffer.mutex != NULL);
  assert(buffer.data  != NULL);
  assert(buffer.empty != NULL);

  success();
}

void destroy_test() {
  TEST_HEADER;

  buffer_t buffer;

  buffer_init(&buffer, 10);
  buffer_destroy(&buffer);

  assert(buffer.array == NULL);
  assert(buffer.mutex == NULL);
  assert(buffer.data  == NULL);
  assert(buffer.empty == NULL);

  success();
}

void print_test() {
  TEST_HEADER;

  buffer_t buffer_a, buffer_b;

  buffer_init(&buffer_a, 5);
  buffer_init(&buffer_b, 10);

  buffer_print(&buffer_a);
  buffer_print(&buffer_b);

  buffer_destroy(&buffer_a);
  buffer_destroy(&buffer_b);

  success();
}

void produce(buffer_t *buffer, int producer, int value) {
  buffer_put(buffer, producer, value);
  printf("Produced (%d, %d)\n", producer, value);
}

void put_test() {
  TEST_HEADER;

  buffer_t buffer;

  buffer_init(&buffer, 3);

  produce(&buffer, 1, 111);
  produce(&buffer, 2, 222);
  produce(&buffer, 3, 333);

  buffer_print(&buffer);

  assert(buffer.array[0].a == 1 && buffer.array[0].b == 111);
  assert(buffer.array[1].a == 2 && buffer.array[1].b == 222);
  assert(buffer.array[2].a == 3 && buffer.array[2].b == 333);

  success();
}


void consume(buffer_t *buffer, tuple_t *tuple) {
  buffer_get(buffer, tuple);
  printf("Consumed (%d, %d)\n", tuple->a, tuple->b);
}

void get_test() {
  TEST_HEADER;

  buffer_t buffer;
  tuple_t tuple;


  buffer_init(&buffer, 3);

  produce(&buffer, 1, 111);
  produce(&buffer, 2, 222);
  produce(&buffer, 3, 333);

  consume(&buffer, &tuple);
  assert(tuple.a == 1 && tuple.b == 111);

  consume(&buffer, &tuple);
  assert(tuple.a == 2 && tuple.b == 222);

  produce(&buffer, 4, 444);

  consume(&buffer, &tuple);
  assert(tuple.a == 3 && tuple.b == 333);

  consume(&buffer, &tuple);
  assert(tuple.a == 4 && tuple.b == 444);

  buffer_destroy(&buffer);

  success();
}

void random_ms_sleep(int min, int max) {
  usleep(1000 * (rand() % (max + 1 - min) + min));
}

void random_fast_or_slow_sleep() {
  int min, max;

  if (rand() % 2 == 0) {
    min = 100;
    max = 300;
  } else {
    min = 300;
    max = 600;
  }

  random_ms_sleep(min, max);
}

void *producer(void *arg) {
  buffer_t *buffer = (buffer_t*) arg;
  for (int i = 0; i < 10; i++) {
    random_fast_or_slow_sleep();
    produce(buffer, i, i*i) ;
  }
  pthread_exit(NULL);
}
void *consumer(void *arg) {
  buffer_t *buffer = (buffer_t*) arg;
  tuple_t tuple;

  for (int i = 0; i < 10; i++) {
    random_fast_or_slow_sleep();
    consume(buffer, &tuple);
    assert(tuple.a == i && tuple.b == i*i);
  }
  pthread_exit(NULL);
}


void concurrent_put_get_test() {
  TEST_HEADER;

  pthread_t producer_tid, consumer_tid;
  buffer_t buffer;

  buffer_init(&buffer, 5);

  // Use current time XOR pid as random seed.
  srand(time(NULL) ^ getpid());

  if (pthread_create(&producer_tid, NULL, producer, &buffer) != 0) {
    perror("pthread_create()");
    exit(EXIT_FAILURE);
  }

  if (pthread_create(&consumer_tid, NULL, consumer, &buffer) != 0) {
    perror("pthread_create()");
    exit(EXIT_FAILURE);
  }


  pthread_join(producer_tid, NULL);
  pthread_join(consumer_tid, NULL);


  success();
}

int main(void) {
  setbuf(stdout, NULL);

  // Run tests.

  init_test();
  destroy_test();
  print_test();
  put_test();
  get_test();
  concurrent_put_get_test();
}
