/*=  ATIMER_HANDLER - signal handler for timer system */

static void atimer_handler
(
int signo                   /* signal number (given) */
);

/*=  ATIMER_INSERT - insert an entry in the event list */

static void atimer_insert
(
struct timer_q *new_entry,  /* pointer to item to be added (given) */
int *status                 /* global status (given and returned) */
);

