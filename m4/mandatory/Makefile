# Change to y to enable debugging support
DEBUG:=

CC=gcc
OS := $(shell uname)

CFLAGS=-std=c99 -D_XOPEN_SOURCE=600 -Wall -Wextra

ifeq ($(DEBUG), y)
	CFLAGS += -g
	LDFLAGS += -g
else
	CFLAGS += -O2
	LDFLAGS += -O2
endif

ifeq ($(OS), Linux)
	CFLAGS += -pthread
	LDLIBS += -pthread -lrt
endif

CFLAGS += -I ../psem

.PHONY: all clean


all: $(addprefix bin/, \
	mutex \
	psem_test \
	two_thread_barrier \
	bounded_buffer_test \
	bounded_buffer_stress_test \
	)

../psem/psem.o:
	cd ../psem; make

bin/mutex: obj/mutex.o obj/timing.o
	$(CC) $(LDFLAGS) $(LDLIBS) $^ -o $@

bin/psem_test: ../psem/psem.o obj/psem_test.o
	$(CC) $(LDFLAGS) $(LDLIBS) $^ -o $@

bin/two_thread_barrier: ../psem/psem.o obj/two_thread_barrier.o
	$(CC) $(LDFLAGS) $(LDLIBS) $^ -o $@

bin/bounded_buffer_test: ../psem/psem.o obj/bounded_buffer.o obj/bounded_buffer_test.o
	$(CC) $(LDFLAGS) $(LDLIBS) $^ -o $@

bin/bounded_buffer_stress_test: ../psem/psem.o obj/bounded_buffer.o obj/bounded_buffer_stress_test.o
	$(CC) $(LDFLAGS) $(LDLIBS) $^ -o $@

obj/%.o: src/%.c
	$(CC) -c $(CFLAGS) $^ -o $@

bin/%: obj/%.o
	$(CC) $(LDFLAGS) $(LDLIBS) $^ -o $@

clean:
	$(RM) obj/*.o bin/*
	cd ../psem; make clean

