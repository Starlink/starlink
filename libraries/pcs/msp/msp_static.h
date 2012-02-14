/*=  MSP_ACCEPT - accept an incoming connection request */

static void msp_accept
(
int *status          /* global status (given and returned) */
);

/*=  MSP_CHECKSOCK - check a socket number is in use */

void msp_checksock
(
int sock,            /* a socket number (given) */
int *status          /* global status (given and returned) */
);

/*=  MSP_GET_TASKPORT - get task identifier */

static void msp_get_taskport
(
const char *filedir,  /* name of directory for rendezvous files (given) */
const char *taskname, /* task name (given) */
int *taskport,        /* port number of task (returned) */
int *status           /* global status (given and returned) */
);

/*=  MSP_INPUT - get a message from any of the input sockets */

static void msp_input
(
int waitflag,         /* wait flag (given) */
int *status           /* global status (given and returned) */
);

/*=  MSP_SELECT - look for a message on any of the input sockets */

static void msp_select
(
int waitflag,         /* wait flag (given) */
int *nready,          /* no. of sockets with data (returned) */
int *q_number,        /* no. of first queue with data (returned) */
int *status           /* global status (given and returned) */
);

