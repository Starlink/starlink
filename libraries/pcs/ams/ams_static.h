/*=  AMS_ACCEPT - accept a request to open a path */

static void ams_accept
(
const struct a_loc_init_in * loc_init_in,  /* the message requesting
                                              initialisation (given) */
sendq_type ackq,                     /* queue for returning
                                        acknowledgement (given) */
int *status                          /* global status (given and
                                        returned) */
);

/*=  AMS_ADDREST */

static void ams_addrest
(
const struct a_loc_ack_in * loc_ack_in,  /* received ack_in structure (given) */
int path,                          /* path of init transaction (given) */
int messid,                        /* messid of init transaction (given) */
sendq_type reply_q,                /* queue for sending rejection (given)
                                      */
int *status                        /* global status (given and returned) */
);

/*=  AMS_ANALYSENAME */

static void ams_analysename
(
const char *name,  /* full taskname (given) */
int *netind,      /* index to network type (returned) */
char *task,       /* task name (returned) */
char *mach,       /* machine name (returned) */
int *remote,      /* flag for whether task is remote (returned) */
int *status       /* global status (given and returned) */
);

/*=  AMS_CALL_OUT */

static void ams_call_out
(
const char *machname,    /* name of remote machine (given) */
int netind,        /* index to network parameters (given) */
int *machnum,      /* number allocated to machine in MESSYS common blocks
                      (returned) */
int *status        /* global status (given and returned) */
);

/*=  AMS_ENDTRANS */

static void ams_endtrans
(
const struct a_loc_msg_in * loc_msg_in,  /* the end-transaction message in
                                            internal format (given) */
int *path,                         /* the communications path to the
                                      other task (returned) */
int *messid,                       /* the transaction end index for this
                                      transaction (returned) */
int message_name_s,                /* space for name (given) */
int message_value_s,               /* space for value (given) */
int *message_status,               /* message status (returned) */
int *message_context,              /* message context (returned) */
char *message_name,                /* message name (returned) */
int *message_length,               /* length of value (returned) */
char *message_value,               /* message value (returned) */
int *status                        /* global status (given and returned) */
);

/*=  AMS_EXIT1 - SunOS on_exit handler */

#if USE_ON_EXIT
static void ams_exit1
(
 int iarg,		/* SunOS on_exit argument */
 void * arg
);
#endif

/*=  AMS_FREEPATH */

static void ams_freepath
(
int j              /* path number (given) */
);

/*=  AMS_FREETRANS */

static void ams_freetrans
(
int messid,       /* identifier of transaction to be freed (given) */
int *status       /* global status (given and received) */
);

/*=  AMS_GETFREEPATH */

static void ams_getfreepath
(
int *path,         /* path number (returned) */
int *status        /* global status (given and returned) */
);

/*=  AMS_GETFREETRANS */

static void ams_getfreetrans
(
int getq,          /* flag for whether reply queue required (given) */
int path,          /* associated path (given) */
sendq_type otaq,   /* other task's reply queue (given) */
int ottn,          /* other task;s transaction number (given) */
int *messid,       /* identifier for the transaction (returned) */
int *status        /* global status (given and returned) */
);

/*=  AMS_GETMACHNUM - return the index to the named machine */

static void ams_getmachnum
(
const char *machinename,/* name of machine (given) */
int *machinenumber,     /* index to machine (returned) */
int *added,             /* flag for if new entry added (returned) */
int *status             /* global status (given and returned) */
);

/*=  AMS_NALOOKUP */

static void ams_nalookup
(
const char *name,  /* full machine/task name (given) */
int *path,         /* path number for communication to task (returned) */
int *netind,       /* index to network type (returned) */
char *task,        /* task part of name (returned) */
char *mach,        /* machine part of name (returned) */
int *remote,       /* flag for whether task is across network (returned) */
int *status        /* global status (given and returned) */
);

/*=  AMS_NEWTRANS */

static void ams_newtrans
(
const struct a_loc_gsoc_start_in *loc_gsoc_start_in,
                                   /* start-transaction message in internal
                                      format (given) */
sendq_type reply_q,                /* queue for sending rejection (given) */
int *path,                         /* the communications path to the other
                                      task (returned) */
int *messid,                       /* the message identifier for this
                                      transaction (returned) */
int message_name_s,                /* space for name (given) */
int message_value_s,               /* space for value (given) */
int *message_status,               /* message status (returned) */
int *message_context,              /* message context (returned) */
char *message_name,                /* message name (returned) */
int *message_length,               /* length of value (returned) */
char *message_value,               /* message value (returned) */
int *status                        /* global status (given and returned) */
);

/*=  AMS_NLOOKUP */

static void ams_nlookup
(
const char *name, /* task name (given) */
int *path,        /* path number for communication to task (returned) */
int *status       /* global status (given and returned) */
);

/*=  AMS_RACCEPT */

static void ams_raccept
(
const struct a_rem_init_in *rem_init_in, /* init request (given) */
sendq_type ackq,                   /* acknowledgement queue (given) */
int *status                        /* global status (given and returned) */
);

/*=  AMS_RADDREST */

static void ams_raddrest
(
const struct a_rem_ack_in *rem_ack_in,  /* received ack_in structure (given) */
int path,                         /* path of init transaction (given) */
int messid,                       /* messid of init transaction (given) */
sendq_type replyq,                /* queue for sending rejections (given)
                                     */
int *status                       /* global status (given and returned) */
);

/*=  AMS_REJECT */

static void ams_reject
(
const struct a_loc_ack_in *loc_ack_in,   /* received ack_in structure (given) */
sendq_type reply_q,                /* queue for sending reply (given) */
int *status                        /* global status (given and returned) */
);

/*=  AMS_REMOVE */

static void ams_remove
(
int pathnum              /* path number (given) */
);

/*=  AMS_RENDTRANS */

static void ams_rendtrans
(
const struct a_rem_msg_in *rem_msg_in,  /* the end-transaction message in
                                           internal format (given) */
int *path,                        /* the communications path to the other
                                     task (returned) */
int *messid,                      /* the message identifier for this
                                     transaction (returned) */
int message_name_s,               /* space for name (given) */
int message_value_s,              /* space for value (given) */
int *message_status,              /* message status (returned) */
int *message_context,             /* message context (returned) */
char *message_name,               /* message name (returned) */
int *message_length,              /* length of value (returned) */
char *message_value,              /* message value (returned) */
int *status                       /* global status (given and returned) */
);

/*=  AMS_RNEWTRANS */

static void ams_rnewtrans
(
const struct a_rem_gsoc_start_in *rem_gsoc_start_in,
                                   /* the initialize-transaction message in
                                      internal format (given) */
sendq_type reply_q,                /* queue for sending rejection (given) */
int *path,                         /* the communications path to the other
                                      task (returned) */
int *messid,                       /* the message identifier for this
                                      transaction (returned) */
int message_name_s,                /* space for name (given) */
int message_value_s,               /* space for value (given) */
int *message_status,               /* message status (returned) */
int *message_context,              /* message context (returned) */
char *message_name,                /* message name (returned) */
int *message_length,               /* length of value (returned) */
char *message_value,               /* message value (returned) */
int *status                        /* global status (given and returned) */
);

/*=  AMS_RREJECT */

static void ams_rreject
(
const struct a_rem_ack_in *rem_ack_in,   /* received ack_in structure (given) */
sendq_type reply_q,                /* queue for sending rejection (given) */
int *status                        /* global status (given and returned) */
);

/*=  AMS_RTRANSLATE */

static void ams_rtranslate
(
const struct a_rem_msg_in *rem_msg_in,  /* the message in internal format
                                           (given) */
sendq_type reply_q,               /* reply queue (given) */
int *path,                        /* the communications path to the other
                                     task (returned) */
int *messid,                      /* the message identifier for this
                                     transaction (returned) */
int message_name_s,               /* space for name (given) */
int message_value_s,              /* space for value (given) */
int *message_status,              /* message status (returned) */
int *message_context,             /* message context (returned) */
char *message_name,               /* message name (returned) */
int *message_length,              /* length of value (returned) */
char *message_value,              /* message value (returned) */
int *status                       /* global status (given and returned) */
);

/*=  AMS_SENDDEINIT */

static void ams_senddeinit
(
int local,             /* flag for local or remote message (given) */
int path,              /* path number (given) */
sendq_type targetq,    /* queue for sending message (given) */
int otherpathno,       /* path number in other task (given) */
int *status            /* global status (given and returned) */
);

/*=  AMS_SENDGSOCEND */

static void ams_sendgsocend
(
int local,              /* flag for local or remote message (given) */
sendq_type targetq,     /* queue for sending message (given) */
int tttn,               /* this task transaction number (given) */
int ottn,               /* other task transaction number (given) */
int gflag,              /* gsoc flag (given) */
const char *gname,      /* message name field (given) */
int glen,               /* length of message value (given) */
int gstatus,            /* message status (given) */
const char *gvalue,     /* message value field (given) */
int *status             /* global status (given and returned) */
);

/*=  AMS_SENDGSOCSTART */

static void ams_sendgsocstart
(
int path,               /* path number (given) */
int messid,             /* transaction number (given) */
int message_status,     /* message status (given) */
int message_context,    /* message context (given) */
const char *message_name,     /* message name (given) */
int message_length,     /* length of value (given) */
const char *message_value,    /* message value (given) */
int *status             /* global status (given and returned) */
);

/*=  AMS_SENDINIT */

static void ams_sendinit
(
int path,           /* path for init (given) */
int messid,         /* temporary transaction (given) */
int *status         /* global status (given and returned) */
);

/*=  AMS_SENDINITACK */

static void ams_sendinitack
(
int local,            /* flag for local or remote (given) */
sendq_type ackq,      /* the other task's init acknowldege queue (given) */
int otherpathno,      /* other task's path number (given) */
int thispathno,       /* this task's path number (given) */
int *status           /* global status (give and returned) */
);

/*=  AMS_SENDMESSAGE */

static void ams_sendmessage
(
int path,                /* path number (given) */
int messid,              /* transaction number (given) */
int message_status,      /* message status (given) */
int message_context,     /* message context (given) */
const char *message_name,      /* message name (given) */
int message_length,      /* length of value (given) */
const char *message_value,     /* message value (given) */
int *status              /* global status (given and returned) */
);

/*=  AMS_SENDOBEY */

static void ams_sendobey
(
sendq_type targetq,    /* target queue identifier (given) */
const char *name,      /* qualifier of message (given) */
int length,            /* number of significant bytes in value (given) */
const char *value,     /* message to be passed to main-line code (given) */
int *status            /* global status (given and returned) */
);

/*=  AMS_SETTIMEOUT */

static void ams_settimeout
(
int time,        /* timeout in milliseconds (given) */
int *status      /* global status (given and returned) */
);

/*=  AMS_TIMEOUT - cause AMS_RECEIVE or GETREPLY to timeout */

static void ams_timeout
(
int parm                          /* the timeout parameter (given) */
);

/*=  AMS_TRANSLATE */

static void ams_translate
(
const struct a_loc_msg_in *loc_msg_in,  /* the message in internal format
                                           (given) */
sendq_type reply_q,               /* reply queue (given) */
int *path,                        /* the communications path to the other
                                     task (returned) */
int *messid,                      /* the message identifier for this
                                     transaction (returned) */
int message_name_s,               /* space for name (given) */
int message_value_s,              /* space for value (given) */
int *message_status,              /* message status (returned) */
int *message_context,             /* message context (returned) */
char *message_name,               /* message name (returned) */
int *message_length,              /* length of value (returned) */
char *message_value,              /* message value (returned) */
int *status                       /* global status (given and returned) */
);

/*=  AMS_UNPACKLOCGSOC */

static void ams_unpacklocgsoc
(
const struct a_loc_gsoc_start_in *localmess, /* internal format (given) */
int message_name_s,                    /* space for name (given) */
int message_value_s,                   /* space for value (given) */
int *message_status,                   /* message status (returned) */
int *message_context,                  /* message context (returned) */
char *message_name,                    /* message name (returned) */
int *message_length,                   /* length of value (returned) */
char *message_value,                   /* message value (returned) */
int *status                            /* global status (given and returned) */
);

/*=  AMS_UNPACKLOCMSG */

static void ams_unpacklocmsg
(
const struct a_loc_msg_in *localmess,  /* internal format (given) */
int message_name_s,                    /* space for name (given) */
int message_value_s,                   /* space for value (given) */
int *message_status,                   /* message status (returned) */
int *message_context,                  /* message context (returned) */
char *message_name,                    /* message name (returned) */
int *message_length,                   /* length of value (returned) */
char *message_value,                   /* message value (returned) */
int *status                            /* global status (given and returned) */
);

/*=  AMS_UNPACKREMGSOC */

static void ams_unpackremgsoc
(
const struct a_rem_gsoc_start_in *remotemess, /* internal format (given) */
int message_name_s,                    /* space for name (given) */
int message_value_s,                   /* space for value (given) */
int *message_status,                   /* message status (returned) */
int *message_context,                  /* message context (returned) */
char *message_name,                    /* message name (returned) */
int *message_length,                   /* length of value (returned) */
char *message_value,                   /* message value (returned) */
int *status                            /* global status (given and returned) */
);

/*=  AMS_UNPACKREMMSG */

static void ams_unpackremmsg
(
const struct a_rem_msg_in *remotemess, /* internal format (given) */
int message_name_s,                    /* space for name (given) */
int message_value_s,                   /* space for value (given) */
int *message_status,                   /* message status (returned) */
int *message_context,                  /* message context (returned) */
char *message_name,                    /* message name (returned) */
int *message_length,                   /* length of value (returned) */
char *message_value,                   /* message value (returned) */
int *status                            /* global status (given and returned) */
);

