DEBUG   := n
CC      := gcc
OS      := $(shell uname)
CFLAGS  := -std=gnu99 -Werror -Wall  -Wno-deprecated-declarations -I ../psem
LDFLAGS :=

ifeq ($(OS), Linux)
	CFLAGS += -pthread
	LDLIBS += -pthread -lrt
endif

ifeq ($(DEBUG), y	)
	CFLAGS += -DDEBUG -g
endif

.PHONY: all clean

all: bin/n_barrier_test bin/sthreads_test bin/bank_test

../psem/psem.o:
	cd ../psem; make 

bin/bank_test:  obj/bank_test.o obj/bank.o src/bank.h
	$(CC) $(CFLAGS) $(LDFLAGS) $(LDLIBS) $(filter %.o, $^) -o $@

bin/n_barrier_test: ../psem/psem.o obj/n_barrier.o obj/n_barrier_test.o
	$(CC) $(CFLAGS) $(LDFLAGS) $(LDLIBS)  $^ -o $@

bin/kaplan: obj/kaplan.o
	$(CC) $(CFLAGS) $(LDLIBS) $(filter-out src/sthreads.h, $^) -o $@

bin/sthreads_test: obj/sthreads_test.o obj/sthreads.o src/sthreads.h
	$(CC) $(CFLAGS) $(LDLIBS) $(filter-out src/sthreads.h, $^) -o $@

obj/sthreads.o: src/sthreads.c src/sthreads.h
	$(CC) $(CFLAGS) -c  $(filter-out src/sthreads.h, $^) -o $@

obj/%.o: src/%.c
	$(CC) $(CFLAGS) -c  $< -o $@

clean:
	$(RM) *~ src/*~ src/#* obj/*.o bin/*
	$(RM) -rf bin/*.dSYM
	cd ../psem; make clean

