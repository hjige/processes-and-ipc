#include "bounded_buffer.h"

#include <string.h>         // strncmp()
#include <stdbool.h>        // true, false
#include <assert.h>         // assert()
#include <ctype.h>          // isprint()
#include <stddef.h>         // NULL
#include <stdio.h>          // printf(), fprintf()
#include <stdlib.h>         // [s]rand()
#include <unistd.h>         // usleep(), sleep()
#include <pthread.h>        // pthread_...

#define EMPTY 0

void buffer_init(buffer_t *buffer, int size) {

  // Allocate the buffer array.
  tuple_t *array = malloc(size * sizeof(tuple_t));
  buffer->array = array;

  if (array == NULL) {
    perror("Could not allocate buffer array");
    exit(EXIT_FAILURE);
 }

  // Initialize the binary mutex semaphore.
  buffer->mutex = psem_init(1);

  // TODO: initialize the rest of the buffer struct members.
  buffer->size = size;
  buffer->in = 0;
  buffer->out = 0;

  buffer->data = psem_init(EMPTY);
  buffer->empty = psem_init(buffer->size);
}

void buffer_destroy(buffer_t *buffer) {

  // Dealloacte the array.
  free(buffer->array);
  buffer->array = NULL;

  // Deallocate the mutex semaphore.
  psem_destroy(buffer->mutex);
  buffer->mutex = NULL;

  // TODO: Deallocate any other resources allocated to the bounded buffer.
  psem_destroy(buffer->empty);
  buffer->empty = NULL;
  psem_destroy(buffer->data);
  buffer->data = NULL;

  buffer->size = 0;
  buffer->in = 0;
  buffer->out = 0;
}

void buffer_print(buffer_t *buffer) {
  puts("");
  puts("---- Bounded Buffer ----");
  puts("");

  printf("size: %d\n", buffer -> size);
  printf("  in: %d\n", buffer -> in);
  printf(" out: %d\n", buffer -> out);
  puts("");

  int i = 0;

  // TODO: print all elements of the array.
  for (i = 0; i < buffer->size; i++) {
    // Print element i of the array. 
    printf("array[%d]: (%d, %d)\n", i, buffer->array[i].a, buffer->array[i].b);
  }

  puts("");
  puts("------------------------");
  puts("");
}


void buffer_put(buffer_t *buffer, int a, int b) {
  // TODO: Add the needed synchronization.
  psem_wait(buffer->empty); // Decrease buffer's empty count.
  
  // If we take the lock before we have a spot in the buffer, deadlock.
  psem_wait(buffer->mutex); // Ensures only we can access buffer.

  // Insert the tuple (a, b) into the buffer.
  buffer->array[buffer->in].a = a;
  buffer->array[buffer->in].b = b;

  // TODO: Update buffer->in and make sure it wraps around.
  buffer->in = (buffer->in + 1) % buffer->size;

  // TODO: Add the needed synchronization.
  psem_signal(buffer->data); // Increase buffer's data count
  psem_signal(buffer->mutex); // Releases "ownership" of buffer
}

void buffer_get(buffer_t *buffer, tuple_t *tuple) {
  // TODO: Add the needed synchronization.
  psem_wait(buffer->data); // Decreases "data" buffer

  psem_wait(buffer->mutex); // Ensures only we can access buffer.
  
  // Read the tuple (a, b) from the buffer.
  tuple->a = buffer->array[buffer->out].a;
  tuple->b = buffer->array[buffer->out].b;

  // TODO: Update buffer->out and make sure it wraps around.
  buffer->out = (buffer->out + 1) % buffer->size;

  // TODO: Add the needed synchronization.
  psem_signal(buffer->empty); // Increases the "empty" buffer
  psem_signal(buffer->mutex); // Releases "ownership" of buffer
}
