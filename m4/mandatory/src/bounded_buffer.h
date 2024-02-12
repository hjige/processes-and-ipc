#include "psem.h" // init_sem(), wait_sem(), signal_sem(), destroy_sem()

typedef struct {
  int a;
  int b;
} tuple_t;

typedef struct {
  tuple_t *array;
  int     size;
  int     in;
  int     out;
  psem_t  *mutex;
  psem_t  *data;
  psem_t  *empty;
} buffer_t;


void buffer_print(buffer_t *buffer);
void buffer_init(buffer_t *buffer, int size);
void buffer_destroy(buffer_t *buffer);
void buffer_put(buffer_t *buffer, int a, int b);
void buffer_get(buffer_t *buffer, tuple_t *tuple);
