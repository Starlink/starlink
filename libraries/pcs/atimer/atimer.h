/*+  ATIMER_CANTIM - remove an event from the timer queue */

void atimer_cantim
(
int timerid,    /* timeout identifier (given) */
int *status     /* global status (given and returned) */
);

/*+  ATIMER_SETTIMR - add an event to the timer queue */

void atimer_settimr
(
int delay,             /* time in millisecs (given) */
int timerid,           /* timer request identifier (given) */
void (*func)(),        /* address of associated routine (given) */
int *status            /* global status (given and returned) */
);

