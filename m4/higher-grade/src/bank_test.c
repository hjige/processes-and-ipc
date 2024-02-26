/**
 * bank_test.c
 *
 * Test of the simple bank API implementation. A number of treads attempting to
 * do random money transfers between bank accounts.
 *
 * History
 *
 * 2024-02-23
 *
 * First version by Karl Marklund <karl.markund@it.uu.se>.
 *
 */

#include "bank.h"  // The bank API.

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#define N_ACCOUNTS 10
#define N_THREADS 5
#define ROUNDS 100

account_t *accounts[N_ACCOUNTS];

int sum() {
  int sum = 0;

  for (int id = 0; id < N_ACCOUNTS; id++) {
    sum += accounts[id]->balance;
  }
  return sum;
}

void print_accounts() {
  printf("\nAccount Amount\n");
  printf("------------------\n");

  account_t *account;
  int balance;

  for (int id = 0; id < N_ACCOUNTS; id++) {
    account = accounts[id];
    balance = account->balance;

    printf("%d\t%d\n", id, balance);
  }

  printf("------------------\n");
  printf("   Sum: %d\n", sum());
}

void *thread(void *arg) {
  int from, to, amount;

  from = rand() % N_ACCOUNTS;
  to = rand() % N_ACCOUNTS;

  while (to == from) {
    to = rand() % N_ACCOUNTS;
  }

  amount = 50 * (1 + rand() % 5);

  if (transfer(amount, accounts[from], accounts[to]) == 0) {
    printf("%d ---- %03d ---> %d   Ok\n", from, amount, to);
  } else {
    printf("%d ---- %03d ---> %d   Insufficient funds\n", from, amount, to);
  };

  return NULL;
}

bool negative_balance() {
  for (int id = 0; id < N_ACCOUNTS; id++) {
    if (accounts[id]->balance < 0) return true;
  }
  return false;
}

void sanity_check(int init_sum) {
  printf("\n");

  bool error = false;

  // Due to data races a balance may become negative.

  if (negative_balance()) {
    printf("RACE CONDITION: Negative balance detected!\n\n");
    error = true;
  }

  int end_sum = sum();

  printf("Total amount of money was initially %d and is now %d.\n", init_sum,
         end_sum);
  if (end_sum == init_sum) {
    printf("\nSystem invariant (conservation of money) not broken.\n");
  } else {
    error = true;
    printf(
        "\nRACE CONDITION: System invariant (conservation of "
        "money) broken!\n\n");
  }

  if (error) exit(EXIT_FAILURE);
}

int main(void) {
  pthread_t threads[N_THREADS];

  // Seed the pseudo random number generator.
  srand(time(NULL));

  // Create the bank accounts.

  for (int id = 0; id < N_ACCOUNTS; id++) {
    accounts[id] = account_new(0);
  }

  accounts[0]->balance = 500;
  accounts[3]->balance = 200;

  print_accounts();

  // Save the initial total sum of money. This will be used a system invariant.
  int init_sum = sum();

  // Do ROUNDS number of tests. In each round N_THREADS attempts do one random
  // money transfer each.

  for (int i = 1; i <= ROUNDS; i++) {
    printf("\nRound %d of %d\n\n", i, ROUNDS);

    printf("From  Amount    To  Result\n\n");

    for (int i = 0; i < N_THREADS; i++) {
      pthread_create(&threads[i], NULL, thread, NULL);
    }

    for (int i = 0; i < N_THREADS; i++) {
      pthread_join(threads[i], NULL);
    }

    print_accounts();
    sanity_check(init_sum);
  }
}