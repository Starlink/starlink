struct task
  {
  struct task *next, *prev;
  char taskname [MESSYS__TNAME];
  char *filename;
  int unix_pid;
  int detached;
 };

/*
 * Routine prototypes
 */
extern void adam_stop(void);
extern value adam_control
(
char *name,			/* task name or "CACHED" (given) */
char *action,			/* action for control context (given) */
char *message			/* message to send (given) */
);
extern value init_adam(void);
