/*+
************************************************************************
****  C A N A D I A N   A S T R O N O M Y   D A T A   C E N T R E  *****
*
*   Module Name:	gen/src/msg.c
*
*   Purpose:
*	Message handling routines.  When more than one message is
*	found in the string, the delimiting character is a newline
*	character.
*
*   Routines:
*	static int	compare_status	: Compare function for bsearch.
*	void		msg_append	: Appends a message to another.
*	void		msg_clear	: Clears the message string.
*	static char	*msg_find	: Returns the format string.
*	void		msg_format	: Formats and appends the message.
*
*   Date		: Aug 23, 1991
*
*   SCCS data		: @(#)
*	Module Name	: msg.c
*	Version Number	: 1.10
*	Release Number	: 1
*	Last Updated	: 2/12/93
*
*   Programmer		: Severin Gaudet
*                       : Peter W. Draper
*
*   Modification History:
*		92/08/11 nrh -	Added check for vms specific errors to
*				msg_format.
*               98/02/09 pwd - modifed to use strerror on UNIX calls.
*
****  C A N A D I A N   A S T R O N O M Y   D A T A   C E N T R E  *****
************************************************************************
-*/

#include <errno.h>
#include <string.h>
#ifdef VMS
#include <perror.h>
#endif
#include <stdarg.h>
#include "gen_types.h"
#include "gen_str.h"
#include "gen_msg.h"

int errno;

#define	MSG_NOT_FOUND	"Message not found."


/*+
************************************************************************
*
*   Synopsis:
*	static int	compare_status( p, q )
*
*   Purpose:
*	Compares the status values in 2 msg structures.  This function
*	is used be bsearch().
*
*   Parameters:
*	char	*p		: (in)	Item to be compared.
*	char	*q		: (in)	Item to be compared.
*
*   Values Returned:
*	int	0		: Same status value.
*	int	< 0		: p is greater than q.
*	int	> 0		: q is greater than p.
*
************************************************************************
-*/

static int	compare_status( p, q )
const void		*p;
const void		*q;
{
    MSG		*m1;
    MSG		*m2;

    m1 = (MSG *) p;
    m2 = (MSG *) q;
    return( m2->m_status - m1->m_status );
}

/*+
************************************************************************
*
*   Synopsis:
*	void	msg_append( msg1, msg2 )
*
*   Purpose:
*	Appends msg2 to msg1.  This is useful for concatenating messages
*	from different sources.  Make sure that the total length will
*	not exceed the maximum message length.  A newline character is
*	used as the delimiter between messages.
*
*   Parameters:
*	char	*msg1		: (mod)	Message to be appended to.
*	char	*msg2		: (in)	Message to be added.
*
*   Values Returned:
*	None.
*
************************************************************************
-*/

void	msg_append( msg1, msg2 )
char	*msg1;
char	*msg2;
{
    int		len1;
    int		len2;

    len1 = strlen( msg1 ) + 1;
    len2 = strlen( msg2 );

    len2 = MIN( len2, ( MSG_MAX_LEN - len1 ) );
    if ( len2 > 0 )
    {
	if ( len1 == 1 )
	{
	    (void) strcpy( msg1, msg2 );
	}
	else
	{
	    (void) strcat( msg1, "\n" );
	    (void) strncat( msg1, msg2, len2 );
	}
    }
}

/*+
************************************************************************
*
*   Synopsis:
*	void	msg_clear( msg )
*
*   Purpose:
*	Clears the message string by setting the first character to
*	the end-of-string character.
*
*   Parameters:
*	char	*msg		: (mod)	String to be cleared.
*
*   Values Returned:
*	None.
*
************************************************************************
-*/

void	msg_clear( msg )
char	*msg;
{
    msg[0] = '\0';
}

/*+
************************************************************************
*
*   Synopsis:
*	static char	*msg_find( status, msgs, num_msgs )
*
*   Purpose:
*	Returns the format string of the message corresponding to the
*	given status.
*
*   Parameters:
*	int	status		: (in)	Status searching for.
*	MSG	*msgs		: (in)	Array of message structures.
*	int	num_msgs	: (in)	Number of messages in array.
*
*   Values Returned:
*	char	*format		: Format string.
*
************************************************************************
-*/

static char	*msg_find( status, msgs, num_msgs )
int		status;
MSG		*msgs;
int		num_msgs;
{
    MSG		*m;
    MSG		temp_m;


    temp_m.m_status = status;
    m = (MSG *) bsearch( (char *)(&temp_m), (char *) msgs,
	    (unsigned) num_msgs, sizeof( MSG ), compare_status );
    if ( m == NULL )
    {
	return( MSG_NOT_FOUND );
    }
    else
    {
	return( m->m_format );
    }
}

/*+
************************************************************************
*
*   Synopsis:
*	void	msg_format( buffer, prefix, num_msgs, msgs, status, args )
*
*   Purpose:
*	Formats the buffer according to the status using the message
*	array and arguments provided.
*
*   Parameters:
*	va_list			: (in)  Variable length arguments.
*
*   Values Returned:
*	None.
*
************************************************************************
-*/

void	msg_format( buffer, prefix, num_msgs, msgs, status, args )
char	*buffer;
char	*prefix;
int	num_msgs;
MSG	*msgs;
int	status;
va_list	args;
{
    char	*errno_arg;
    char	*format;
    int		i;
    int		indent_size;
    char	*t1;
    char	*t2;
    char	temp_buffer1[MSG_MAX_LEN];
    char	temp_buffer2[MSG_MAX_LEN];

    if ( status == MSG_ERRNO )
    {
	/*
	 *  Errno message will be used and then reset to 0.
	 *  The '+' character is used to separate the prefix from
	 *  the error number.
	 */

	errno_arg = va_arg( args, char * );

#ifdef	VMS

	if ( vaxc$errno != 0 )
	{
	    (void) sprintf( temp_buffer2, "(%s+%d)  %s: %s", prefix,
		    errno, errno_arg, strerror( EVMSERR, vaxc$errno ) );
	}
	else if ( errno != 0 )
	{
	    (void) sprintf( temp_buffer2, "(%s+%d)  %s: %s", prefix,
		    errno, errno_arg, strerror( errno ) );
	}

	vaxc$errno = 0;
	errno = 0;

#else	/* VMS */

	if ( errno != 0 )
	{
	    (void) sprintf( temp_buffer2, "(%s+%d)  %s: %s",
		    prefix, errno, errno_arg, strerror( errno ) );
	    errno = 0;
	}

#endif VMS

    }
    else
    {
	/*
	 *  Message array will be used and the prefix-number
	 *  separator will be a '-' (implicit since the number
	 *  is negative).  Since this is a programmer generated
	 *  message, parse it for newline characters and indent
	 *  accordingly.
	 */

	format = msg_find( status, msgs, num_msgs );
	(void) vsprintf( temp_buffer1, format, args );

	if ( strchr( temp_buffer1, '\n' ) == NULL )
	{
	    (void) sprintf( temp_buffer2, "(%s%d)  %s",
		    prefix, status, temp_buffer1 );
	}
	else
	{
	    (void) sprintf( temp_buffer2, "(%s%d)  ", prefix, status );
	    indent_size = strlen( temp_buffer2 );
	    t1 = temp_buffer1;
	    t2 = temp_buffer2 + indent_size;
	    for ( ; *t1; t1++ )
	    {
		*t2++ = *t1;
		if ( *t1 == '\n' )
		{
		    for ( i = 0; i < indent_size; i++ )
		    {
			*t2++ = ' ';
		    }
		}
	    }
	    *t2 = '\0';
	}
    }

    msg_append( buffer, temp_buffer2 );
}
