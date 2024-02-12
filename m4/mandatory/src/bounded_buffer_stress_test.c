#include "bounded_buffer.h"

#include <string.h>  // strncmp()
#include <stdbool.h> // true, false
#include <assert.h>  // assert()
#include <ctype.h>   // isprint()
#include <stddef.h>  // NULL
#include <stdio.h>   // printf(), fprintf()
#include <stdlib.h>  // [s]rand()
#include <unistd.h>  // usleep(), sleep()
#include <pthread.h> // pthread_...

typedef struct {
  int id;
  int n;
  buffer_t *buffer;
} producer_arg_t;

typedef struct {
  int n;
  int last_value;
} stat_t;

typedef struct {
  int id;
  int n;
  buffer_t *buffer;
  int num_producers;
  int *tuple_counters;
} consumer_arg_t;

bool verbose = false;

void *producer(void *arg) {
  producer_arg_t *a = (producer_arg_t *) arg;

  for (int i = 0; i < a -> n; i++) {
    if (verbose) printf("P%03d (%d, %d)\n", a->id, a->id, i);
    usleep(100);
    buffer_put(a -> buffer, a->id, i);
  }

  pthread_exit(0);
}

void *consumer(void *arg) {

  consumer_arg_t *a = (consumer_arg_t *) arg;

  stat_t *stats = malloc(a->num_producers*sizeof(stat_t));

  for (int i = 0; i < a->num_producers; i++) {
    stats[i].n = 0;
    stats[i].last_value = -1;
  }

  tuple_t tuple;

  for (int i = 0; i < a->n; i++) {
    usleep(100);
    buffer_get(a->buffer, &tuple);

    if (verbose) printf("C%03d (%d, %d)\n", a->id, tuple.a, tuple.b);

    if (stats[tuple.a].last_value < tuple.b) {
      stats[tuple.a].n = stats[tuple.a].n + 1;
      stats[tuple.a].last_value = tuple.b;

    } else {
      printf("C%03d (%d, %d) when expecting (%d, X > %d)  ==> ERROR out of sequence\n",
             a -> id,
             tuple.a,
             tuple.b,
             tuple.a,
             stats[tuple.a].last_value);
      exit(EXIT_FAILURE);
    }

  }

  int tuple_count = 0;

  for (int i = 0; i < a->num_producers; i++) {
    tuple_count += stats[i].n;
  }

  assert(tuple_count == a->n);
  pthread_exit(0);
}


void test(int buffer_size, int num_producers, int n, int num_consumers, int m){
  pthread_t *producers, *consumers;

  buffer_t buffer;
  buffer_init(&buffer, buffer_size);


  producers = malloc(num_producers * sizeof(pthread_t));
  int *tuple_counters = malloc(num_consumers*sizeof(int));

  if (producers == NULL) {
    perror("malloc()");
    exit(EXIT_FAILURE);
  }

  consumers = malloc(num_consumers * sizeof(pthread_t));

  if (consumers == NULL) {
    perror("malloc()");
    exit(EXIT_FAILURE);
  }

  producer_arg_t arg[num_producers];

  for (int i = 0; i < num_producers; i++) {

    arg[i].id = i;
    arg[i].n    = n;
    arg[i].buffer = &buffer;

    if (pthread_create(&producers[i], NULL, producer, &arg[i]) != 0) {
      perror("pthread_create()");
      abort();
    }
  }

  consumer_arg_t carg[num_consumers];

  for (int i = 0; i < num_consumers; i++) {
    carg[i].id = i;
    carg[i].n  = m;
    carg[i].buffer = &buffer;
    carg[i].num_producers = num_producers;
    carg[i].tuple_counters = tuple_counters;

    if (pthread_create(&consumers[i], NULL, consumer, &carg[i]) != 0) {
      perror("pthread_create()");
      abort();
    }
  }



  for (int i = 0; i < num_producers; i++) {
    if ( pthread_join(producers[i], NULL) != 0 ) {
    perror("couldn't join with  thread");
    exit(EXIT_FAILURE);
    }
  }

  for (int i = 0; i < num_consumers; i++) {
    if ( pthread_join(consumers[i], NULL) != 0 ) {
      perror("couldn't join with  thread");
      exit(EXIT_FAILURE);
    }

  }


  assert(num_producers*n % buffer.size == buffer.in);
  assert(num_consumers*m % buffer.size == buffer.out);
  assert(buffer.in == buffer.out);

  printf("\nThe buffer when the test ends.\n");

  buffer_print(&buffer);

  puts("\n====> TEST SUCCESS <====\n");
}

int optvalue(char opt, char *optarg, int default_value) {
  int tmp = atoi(optarg);

  if (tmp == 0) {
    printf("Option -%c: invalid value %s, will use default %d.\n", opt, optarg, default_value);
  }
  return (tmp != 0) ? tmp : default_value;
}

int num_of_digits(int num) {
  int n = 0;
  while(num != 0)
    {
      /* Increment digit count */
      n++;

      /* Remove last digit of 'num' */
      num /= 10;
    }
  return n;
}

int main(int argc, char *argv[]) {

  int s = 10, p = 20, n = 10000, c = 20, m = 10000;

  int opt;

  while((opt = getopt(argc, argv, ":s:p:n:c:m:v")) != -1)
    {
      switch(opt)
        {
        case 'v':
          verbose = true;
          break;
        case 's':
          s = optvalue(opt, optarg, s);
          break;
        case 'p':
          p = optvalue(opt, optarg, p);
          break;
        case 'n':
          n = optvalue(opt, optarg, n);
          break;
        case 'c':
          c = optvalue(opt, optarg, c);
          break;
        case 'm':
          m = optvalue(opt, optarg, m);
          break;
         case ':':
          printf("option %c needs a value\n", opt);
          break;
        case '?':
          printf("unknown option: %c\n", optopt);
        break;
        }
    }

  int wp = num_of_digits(p);
  int wc = num_of_digits(c);

  int wn = num_of_digits(n);
  int wm = num_of_digits(m);

  int w1 = (wp > wc) ? wp : wc;
  int w2 = (wn > wm) ? wn : wm;

  printf("Test buffer of size %d with: \n\n", s);
  printf(" %*d producers, each producing %*d items.\n", w1, p, w2, n);
  printf(" %*d consumers, each consuming %*d items.\n", w1, c, w2, m);

  if (p*n != c*m) {
    printf("\nWarning: total number of produced items (%d*%d = %d) not equal to the\n", p, n, p*n);
    printf("         total number of consumed items (%d*%d = %d).\n", c, m, c*m);
  }

  printf("\nVerbose: %s\n", verbose ? "true" : "false");

  test(s, p, n, c, m);

}
