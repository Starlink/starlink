/*
**++
**  FACILITY:  GWM
**
**  MODULE DESCRIPTION:
**
**      GWM_Error
**
**  AUTHORS:
**
**      D L Terrett
**
**  CREATION DATE:  14-MAR-1991
**--
*/


/*
**
**  INCLUDE FILES
**
*/

#include <stdio.h>
#include "gwm_err.h"
#include "gwm_sys.h"
/*
**
**  MACRO DEFINITIONS
**
*/
#define BUFLEN 64


/*
**++
**  FUNCTIONAL DESCRIPTION:
**
**	Prints an error message on stderr. The message includes the
**      error number so that an error reported by a child process can be piped
**      to the parent process and re-reported in the normal way.
**
**  FORMAL PARAMETERS:
**
**      status:
**          Error number
**
**  RETURN VALUE:
**
**      None
**
**  DESIGN:
**
**
**--
*/
void GWM_Error( int status )
{
    char msgbuf[BUFLEN];

    if (status!=GWM_SUCCESS)
    {
	    GWM_ErrorMessage(status, msgbuf, BUFLEN);
	    fprintf(stderr, "GWM %d %% %s\n",status,msgbuf);
    }
    return;
}
