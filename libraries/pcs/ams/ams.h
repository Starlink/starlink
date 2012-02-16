/*+  AMS_ASTINT - send an ASTINT message from a signal handler */

void ams_astint
(
int *status         /* global status (given and returned) */
);

/*+  AMS_ASTMSG - send an ASTMSG from a signal handler */

void ams_astmsg
(
const char *name, /* name of the action to be rescheduled (given) */
int length,       /* number of significant bytes in value (given) */
const char *value,/* message to be passed to main-line code (given) */
int *status       /* global status (given and returned) */
);

/*=  AMS_EXIT - ams exit handler */

void ams_exit
(
void
);

/*+  AMS_EXTINT - send an EXTINT message from a signal handler */

void ams_extint
(
int *status         /* global status (given and returned) */
);

/*+  AMS_GETREPLY - receive a message on a specified path, messid */

void ams_getreply
(
int timeout,              /* timeout time in milliseconds (given) */
int path,                 /* pointer to the path (given) */
int messid,               /* message number of incoming message (given) */
int message_name_s,       /* space for name (given) */
int message_value_s,      /* space for value (given) */
int *message_status,      /* message status (returned) */
int *message_context,     /* message context (returned) */
char *message_name,       /* message name (returned) */
int *message_length,      /* length of value (returned) */
char *message_value,      /* message value (returned) */
int *status               /* global status (given and returned) */
);

/*+  AMS_INIT - initialise ams */

void ams_init
(
const char *own_name,     /* name of this task (given) */
int *status
);

/*+  AMS_INITEH - initialise ams with or without exit handler */

void ams_initeh
(
const char *own_name,/* name of this task (given) */
int eh,              /* register exit handler */
int *status
);

/*+  AMS_KICK - send a message to this task's kick queue */
void ams_kick
(
const char *name, /* name of the action to be rescheduled (given) */
int length,       /* number of significant bytes in value (given) */
const char *value,/* message to be passed to application code (given) */
int *status       /* global status (given and returned) */
);

/*+  AMS_PATH - get a communications path to another task */

void ams_path
(
const char *other_task_name,  /* name of task to which path is required (given)
                           */
int *path,              /* pointer to the path (returned) */
int *status             /* global status (given and returned) */
);

/*+  AMS_PLOOKUP - look up a taskname given a path to it */

void ams_plookup
(
int path,             /* the path number (given) */
char *name,           /* the task name (returned) */
int *status           /* global status (given and returned) */
);

/*+  AMS_RECEIVE - receive any incoming message */

void ams_receive
(
int timeout,              /* timeout time in milliseconds (given) */
int message_name_s,       /* space for name (given) */
int message_value_s,      /* space for value (given) */
int *message_status,      /* message status (returned) */
int *message_context,     /* message context (returned) */
char *message_name,       /* message name (returned) */
int *message_length,      /* length of value (returned) */
char *message_value,      /* message value (returned) */
int *path,                /* path on which message received (returned) */
int *messid,              /* message number of incoming message (returned) */
int *status               /* global status (given and returned) */
);

/*+  AMS_REPLY - send a message on a given path, messid */

void ams_reply
(
int path,               /* the path number for communicating with the other
                           task (given) */
int messid,             /* the number identifying the transaction (given) */
int message_function,   /* message function (given) */
int message_status,     /* message status (given) */
int message_context,    /* message context (given) */
const char *message_name,/* message name (given) */
int message_length,     /* length of value (given) */
const char *message_value,/* message value (given) */
int *status             /* global status (given and returned) */
);

/*+  AMS_RESMSG - send a message to this task's  reschedule queue */

void ams_resmsg
(
int length,        /* number of significant bytes in value (given) */
const char *value, /* message to be passed to main-line code (given) */
int *status        /* global status (given and returned) */
);

/*+  AMS_SEND - send a message on a given path */

void ams_send
(
int path,               /* pointer to the path (given) */
int message_function,   /* message function (given) */
int message_status,     /* message status (given) */
int message_context,    /* message context (given) */
const char *message_name,/* message name (given) */
int message_length,     /* length of value (given) */
const char *message_value,/* message value (given) */
int *messid,            /* message number issued by this task (returned) */
int *status             /* global status (given and returned) */
);

