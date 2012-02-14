/*+  SOCK_ACCEPT - accept a network call */

void sock_accept
(
int listen_socket,     /* Socket for connections (given) */
int *new_socket,       /* Socket for new connection (returned) */
int *status            /* global status (given and returned) */
);

/*+  SOCK_CONNECT - Make a connection */

void sock_connect
(
int sock,                             /* Socket for connection (given) */
const struct sockaddr *connect_addr,  /* socket info */
int *status                           /* global status (given and returned) */
);

/*+  SOCK_GHBA - get host detail by address */

void sock_ghba
(
struct sockaddr_in peer,   /* structure for peer details (given) */
struct hostent *peeraddr,  /* structure for peer entry (returned) */
int *status                /* global status (given and returned) */
);

/*+  SOCK_GHBN - get host details by name */

void sock_ghbn
(
char *rmach,                /* name of remote machine (given) */
struct hostent *retentptr,  /* network data structure for other
                               machine (returned) */
int *status                 /* global status (given and returned) */
);

/*+  SOCK_GPN - get peer details */

void sock_gpn
(
int channel,           /* i/o channel for communications, bound to a socket
                          (given) */
struct sockaddr_in *peer, /* structure for peer details (returned) */
int *status            /* global status (given and returned) */
);

/*+  SOCK_READ - Read a complete message from a socket */

void sock_read
(
int read_socket,       /* Socket on which to read the data (given) */
int length_required,   /* Length of the message required (given) */
char *buffer,          /* Buffer to receive the data (returned) */
int *status            /* global status (given and returned) */
);

/*+  SOCK_WRITE - Write a complete message to a socket */

void sock_write
(
int write_socket,     /* Socket on which to write the data (given) */
int length_to_send,   /* Length of the message to be sent (given) */
const char *buffer,   /* Buffer containing the data (given) */
int *status           /* global status (given and returned) */
);

