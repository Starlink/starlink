/*	@(#)selection_svc.h 10.6 88/02/08	*/

#ifndef	suntool_selection_svc_DEFINED
#define	suntool_selection_svc_DEFINED

/*
 *	Copyright (c) 1985 by Sun Microsystems, Inc.
 */

#include <sys/socket.h>
#undef	TRUE
#undef	FALSE
#include <rpc/rpc.h>
#undef	TRUE
#define	TRUE	1
#undef	FALSE
#define	FALSE	0

/*	Seln_client:	opaque handle returned to client from create    */

typedef char *Seln_client;


/*	Seln_result:	Standard return codes	*/

typedef enum	{
    SELN_FAILED, SELN_SUCCESS,		/*     the basic all-around uses  */
    SELN_NON_EXIST, SELN_DIDNT_HAVE, SELN_WRONG_RANK,	/* special cases  */
    SELN_CONTINUED, SELN_CANCEL, SELN_UNRECOGNIZED
}	Seln_result;


/*	Seln_rank:	Selection identifiers	*/

typedef enum	{
    SELN_UNKNOWN, SELN_CARET, SELN_PRIMARY,
    SELN_SECONDARY, SELN_SHELF, SELN_UNSPECIFIED
}	Seln_rank;


/*
 *	Seln_function:	Modes which affect rank of selection,
 *	controlled by function-keys or their programmatic equivalents
 */

typedef enum	{
	    SELN_FN_ERROR,
    SELN_FN_STOP,  SELN_FN_AGAIN,
    SELN_FN_PROPS, SELN_FN_UNDO,
    SELN_FN_FRONT, SELN_FN_PUT,
    SELN_FN_OPEN,  SELN_FN_GET,
    SELN_FN_FIND,  SELN_FN_DELETE
}	Seln_function;


/*
 *	Seln_state:	States a selection (or its holder) may be in
 */

typedef enum	{
    SELN_NONE, SELN_EXISTS, SELN_FILE
}	Seln_state;


/*
 *	Seln_response:	possible appropriate responses to a Seln_function_buffer
 */

typedef enum	{
    SELN_IGNORE, SELN_REQUEST, SELN_FIND, SELN_SHELVE, SELN_DELETE
}	Seln_response;

typedef struct {
    Seln_rank           rank;
    char               *pathname;
}	Seln_file_info;

typedef struct {
    int                 pid;
    int                 program;
    struct sockaddr_in  tcp_address;
    struct sockaddr_in  udp_address;
    char               *client;
}	Seln_access;

typedef struct	{
    Seln_rank           rank;
    Seln_state          state;
    Seln_access         access;
}	Seln_holder;


typedef struct	{
    Seln_holder         caret;
    Seln_holder         primary;
    Seln_holder         secondary;
    Seln_holder         shelf;
}	Seln_holders_all;

typedef struct	{
    Seln_holder         holder;
    Seln_function       function;
    int                 down;
}	Seln_inform_args;

typedef struct	{
    Seln_function       function;
    Seln_rank           addressee_rank;
    Seln_holder         caret;
    Seln_holder         primary;
    Seln_holder         secondary;
    Seln_holder         shelf;
}	Seln_function_buffer;

#define SELN_FUNCTION_WORD_COUNT 8	/*  256 bits should last a while  */

typedef struct    {
    unsigned	data[SELN_FUNCTION_WORD_COUNT];
}	Seln_functions_state;

typedef struct	{
    char		*client_data;
    Seln_rank            rank;
    char		*context;
    char	       **request_pointer;
    char	       **response_pointer;

}	Seln_replier_data;

typedef struct	{
    Seln_result	       (*consume)();
    char		*context;
}	Seln_requester;

#define SELN_RPC_BUFSIZE	2048
#define SELN_BUFSIZE  (SELN_RPC_BUFSIZE				\
			    -	128				\
			    - sizeof(Seln_replier_data *)	\
			    - sizeof(Seln_requester)		\
			    - sizeof(char *)			\
			    - sizeof(Seln_rank)			\
			    - sizeof(Seln_result)		\
			    - sizeof(unsigned))

typedef struct {
    Seln_replier_data	*replier;
    Seln_requester	 requester;
    char		*addressee;
    Seln_rank            rank;
    Seln_result	         status;
    unsigned             buf_size;
    char                 data[SELN_BUFSIZE];
}	Seln_request;


#define SELN_REPORT(event)	seln_report_event(0, event)


Seln_client     seln_create();
void            seln_destroy();

Seln_function_buffer
		seln_inform();


Seln_holder     seln_inquire();

Seln_holders_all
		seln_inquire_all();

Seln_rank       seln_acquire();

Seln_request   *seln_ask();

Seln_response	seln_figure_response();

Seln_result     seln_done(),
		seln_functions_state(),
		seln_hold_file(),
		seln_query(),
		seln_request(),
		seln_stop();

void            seln_report_event(),
		seln_yield_all();

/*	possibly useful predicates	*/

int		seln_get_function_state();

#define		seln_holder_is_me(client, holder)	\
			(seln_holder_same_client(client, holder))
int		seln_holder_same_client(),
	 	seln_holder_same_process(),
		seln_secondary_made(),
		seln_secondary_exists(),
		seln_rank_is_me();

void		seln_init_request(),
		seln_clear_functions();

/*	null structs for initialization & easy reference	*/

extern Seln_function_buffer
			seln_null_function;
extern Seln_holder	seln_null_holder;
extern Seln_request     seln_null_request;
#endif
