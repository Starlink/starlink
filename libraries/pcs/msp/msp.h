/*+  MSP_CLOSE_TASK_QUEUE - close communications with another task */

void msp_close_task_queue
(
sendq_type qid,      /* a send queue to the other task */
int *status          /* global status (given and returned) */
);

/*+  MSP_CREATE_LOCALQ - create a queue for local messages */

void msp_create_localq
(
sendq_type *sendq,   /* created send queue (returned) */
receiveq_type *qid,  /* created receive queue (returned) */
int *status          /* global status (given and returned) */
);

/*+  MSP_CREATE_RECEIVEQ - create a queue for receiving messages */

void msp_create_receiveq
(
receiveq_type *qid,  /* created queue identifier (returned) */
int *status          /* global status (given and returned) */
);

/*+  MSP_DELETE_QUEUE - delete a queue */

void msp_delete_queue
(
receiveq_type qid,  /* identifier of queue to be deleted (given) */
int *status         /* global status (given and returned) */
);

/*+  MSP_ENTER_TASK - register this task with MSP */

void msp_enter_task
(
const char *task_name,   /* name of this task (given) */
receiveq_type *commandq, /* command queue for this task (returned) */
int *status              /* global status (given and returned) */
);

/*+  MSP_EXIT - exit handler */

void msp_exit
(
void
);

/*+  MSP_GET_TASK_QUEUE - get the command queue of a named task */

void msp_get_task_queue
(
const char *task_name, /* name of task (given) */
sendq_type *qid,       /* task command queue (returned) */
int *status            /* global status (given and returned) */
);

/*+  MSP_MKCOMQ - return a task's command q given any send q */

void msp_mkcomq
(
sendq_type replyq,    /* a reply queue for a task (given) */
sendq_type *commandq, /* command queue for the task (returned) */
int *status           /* global status (given and returned) */
);

/*+  MSP_MKNUMQ - return a numbered send q given any send q */

void msp_mknumq
(
sendq_type replyq,    /* a reply queue for a task (given) */
int number,           /* other task's reply number (given) */
sendq_type *numq,     /* reply queue for given number (returned) */
int *status           /* global status (given and returned) */
);

/*+  MSP_RECEIVE_MESSAGE - receive a message on one of a list of queues */

void msp_receive_message
(
const receiveq_type *qarr, /* array of queue identifiers (given) */
int nqueues,          /* number of queues (given) */
int waitflag,         /* wait flag (given) */
int maxlen,           /* maximum length of message (given) */
char msgbody[],       /* received message (returned) */
int *actlen,          /* size of received message (returned) */
receiveq_type *qid,   /* identifier of queue used (returned) */
sendq_type *replyq,   /* reply queue for message (returned) */
int *status           /* global status (given and returned) */
);

/*+  MSP_SEND_MESSAGE - send a message on a queue */

void msp_send_message
(
const char msgbody[],  /* message to be sent (given) */
int msglen,            /* length of message to be sent (given) */
sendq_type sendq,      /* queue identifer to be used (given) */
receiveq_type replyq,  /* reply queue to be associated with the message
                          (given) */
int *status            /* global status (given and returned) */
);

