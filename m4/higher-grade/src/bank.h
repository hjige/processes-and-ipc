/**
 * bank.h
 *
 * Defines the API for a simple bank.
 *
 * 2024-02-23
 *
 * First version by Karl Marklund <karl.markund@it.uu.se>.
 *
 */

#ifndef BANK_H
#define BANK_H

#include <pthread.h>  // pthread_...()

/**
 *  A bank account is represented by the following C structure.
 *
 *  NOTE: You may need to add members to this structure.
 */
typedef struct {
  int balance;

} account_t;

/**
 * account_new()
 *   Creates a new bank account.
 *
 * balance
 *   The initial balance for the new account.
 *
 * NOTE: You may need to add more parameters.
 *
 */
account_t* account_new(unsigned int balance);

/**
 * account_destroy()
 *   Destroys a bank account, freeing the resources it might hold.
 *
 * account:
 *   Pointer to a bank account.
 *
 */
void account_destroy(account_t* account);

/**
 * transfer()
 *   Attempts to transfer money from one account to another.
 *
 * amount:
 *   Amount to transfer.
 *
 * from:
 *   The account to transfer money from. Money should only be transferred if
 *   thee are sufficient funds.
 *
 * to:
 *   The account to transfer the money to.
 *
 * return value:
 *   Return -1 if not sufficient funds in the from account. Otherwise returns 0.
 *
 */
int transfer(int amount, account_t* from, account_t* to);

#endif