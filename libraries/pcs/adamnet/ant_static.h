/*=  ANT_ACCEPT - accept a network connection request */

void ant_accept
(
int *status    /* global status (given and returned) */
);

/*=  ANT_ACCEPT_NETACK - accept a netack message */

void ant_accept_netack
(
int *status                         /* global status (give and returned) */
);

/*=  ANT_ACCEPT_NETDEINIT - accept a netdeinit message */

void ant_accept_netdeinit
(
int *status                            /* global status (give and returned) */
);

/*=  ANT_ACCEPT_NETINIT - accept a netinit message */

void ant_accept_netinit
(
int machnum,                         /* index to other machine details
                                        (given) */
int *status                          /* global status (give and returned) */
);

/*=  ANT_CALL_OUT - handle request to make network call */

void ant_call_out
(
struct a_rem_call_out rem_call_out, /* the received message (given) */
sendq_type reply_q,                 /* task reply queue (given) */
int *status                         /* global status (give and returned) */
);

/*=  ANT_COMMQ - handle messages received on the command queue */

void ant_commq
(
struct a_mess_out rxbuf,   /* the received message (given) */
sendq_type reply_q,        /* reply queue for the message (given) */
int *status                /* global status (given and returned) */
);

/*=  ANT_CONNECT - Connect to an adamnet on another machine */

void ant_connect
(
char *rmach,               /* name of remote machine (given) */
int *channel,              /* i/o channel for communications,
                              bound to a socket (returned) */
int *status                /* global status (give and returned) */
);

/*=  ANT_DISCON - disconnect a network call */

void ant_discon
(
int machnum,       /* machine number associated with the call to
                      be disconnected (given) */
int *status        /* global status (give and returned) */
);

/*=  ANT_EXIT - the ADAMNET exit handler */

void ant_exit
(
/* int *status  */                       /* global status (give and returned) */
void
);

/*=  ANT_FORWARD_ACK_IN - forward a network message to the target task */

void ant_forward_ack_in
(
int *status                      /* global status (given and returned) */
);

/*=  ANT_FORWARD_END_IN - forward a net END_IN message */

void ant_forward_end_in
(
int *status                       /* global status (give and returned) */
);

/*=  ANT_FORWARD_START_IN - forward a net GSOC start message */

void ant_forward_start_in
(
int *status                         /* global status (give and returned) */
);

/*=  ANT_LISTEN - start an asynchronous connection acceptance */

void ant_listen
(
int *status                         /* global status (give and returned) */
);

/*=  ANT_NETMSG - handle messages received across the network */

void ant_netmsg
(
int machnum,         /* index to the received message (given) */
int *status          /* global status (give and returned) */
);

/*=  ANT_NETWORK - handle messages received across the network */

void ant_network
(
int *status          /* global status (give and returned) */
);

/*=  ANT_OBEY - obey a command to dump diagnostics */

void ant_obey
(
struct a_loc_gsoc_start_out loc_gsoc_start_out, /* the incoming message
                                                 (given) */
int *status                         /* global status (give and returned) */
);

/*=  ANT_OPENNET - Initiate communication with a task across a network */

void ant_opennet
(
char *remote_machine_name,  /* name of other machine (given) */
sendq_type accept_q,        /* queue for sending acknowledgement to task
                               on this machine (given) */
int *status                 /* global status (give and returned) */
);

/*=  ANT_RETARGET - retarget a signal to the ant handling routine */

static void ant_retarget
(
int signo,               /* no. of signal to be retargetted (given) */
struct sigaction oact    /* action structure with flag initialised
                            (given) */
);

/*=  ANT_SEND_ACK_OUT - send a net ACK_OUT message */

void ant_send_ack_out
(
struct a_rem_msg_out rem_msg_out, /* the received message (given) */
sendq_type reply_q,               /* task reply queue (given) */
int *status                       /* global status (give and returned) */
);

/*=  ANT_SEND_END_OUT - send a net END_OUT message */

void ant_send_end_out
(
struct a_rem_msg_out rem_msg_out,  /* the received message, this is a MSG
                                      of type END (given) */
int *status                         /* global status (give and returned) */
);

/*=  ANT_SEND_NETACK - send a netack message */

void ant_send_netack
(
struct a_rem_ack_out rem_ack_out,  /* the received message (given) */
sendq_type reply_q,                 /* task reply queue (given) */
int *status                        /* global status (give and returned) */
);

/*=  ANT_SEND_NETDEINIT - send a NETDEINIT message */

void ant_send_netdeinit
(
struct a_rem_deinit_out rem_deinit_out,  /* the received message (given) */
int *status                         /* global status (give and returned) */
);

/*=  ANT_SEND_NETINIT - send a netinit message */

void ant_send_netinit
(
struct a_rem_init_out rem_init_out,  /* the received message (given) */
sendq_type reply_q,                 /* task reply queue (given) */
int *status                         /* global status (give and returned) */
);

/*=  ANT_SEND_START_OUT - send a net GSOC start message */

void ant_send_start_out
(
struct a_rem_gsoc_start_out rem_gsoc_start_out, /* the received message
                                                   (given) */
sendq_type reply_q,                 /* task reply queue (given) */
int *status                         /* global status (give and returned) */
);

/*= ANT_SETSIG - set up signal handlers */

static void ant_setsig
(
void
);

/*= ANT_EXHDLR - ADAM task signal handler for all process signals */

static void ant_exhdlr
(
int isig,
siginfo_t *info,
void *dummy
);

/*=  ANT_SIGHDLR - signal handler network messages */

void ant_sighdlr
(
int astparam,      /* the signal parameter (given) */
siginfo_t *infop,
void *ucp
);

/*=  ANT_VERIFY - verify an incoming connection */

void ant_verify
(
int channel,         /* i/o channel for communications, bound to a socket
                       (given) */
char *machine_name,  /* name of other machine (returned) */
int *status          /* global status (give and returned) */
);

