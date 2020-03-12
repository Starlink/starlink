#include "ems.h"

void pdaXermsg( const char *librar, const char *subrou, const char *messg,
                int nerr, int level, int *status ){
/*
*+
*  Name:
*     pdaXermsg

*  Purpose:
*     Process error messages for SLATEC and other libraries.

*  Synopsis:
*     void pdaXermsg( const char *librar, const char *subrou,
*                     const char *messg, int nerr, int level, int *status )

*  Description:
*     "pdaXermsg" processes a diagnostic message in a manner determined by
*     the value of "level". In the orignal, things also depended on an
*     error report control flag KONTRL. This was by default 2.
*
*     If KONTRL was zero or negative, no information other than the message
*     itself (including numeric values, if any) would have been printed. If
*     KONTRL was positive, introductory messages, trace-backs, etc., would
*     have been printed in addition to the message.
*
*     Depending on KONTRL and "level" the error handling mechanism might
*     also have included aborting the programme via a STOP statement.
*
*              ABS(KONTRL)
*        "level"        0              1              2
*
*          2        fatal          fatal          fatal
*
*          1     not printed      printed         fatal
*
*          0     not printed      printed        printed
*
*         -1     not printed      printed        printed
*                                only once      only once
*
*     In the current version, this function will always issue a message via
*     "errRep". Under no circumstances is the programme aborted. Instead
*     this function always returns control to the caller after setting the
*     "status" parameter (which is new in this version) to 1.

*  Parameters:
*     librar
*        Pointer to a null terminated string holding the name of the
*        library such as "SLATEC". This will form part of the message put
*        out.
*     subrou
*        Pointer to a null terminated string holding the name of the
*        subfunction calling "pdaXermsg". This will form part of the
*        message put out.
*     messg
*        Pointer to a null terminated string holding the principal error or
*        warning message.
*     nerr
*        Ignored.
*     level
*        Ignored.
*     *status
*        The global status. Always returned as 1.

*  Implementation Status:
*     The newline sentinel $$ is not interpreted by this function.

*  References:
*     -  R. E. Jones and D. K. Kahaner, XERROR, the SLATEC Error-handling
*        Package, SAND82-0800, Sandia Laboratories, 1982.
*     -  P. C. T. Rees and A. J. Chipperfield, MSG and ERR, Message and
*        Error Reporting Systems, Version 1.4, Programmer's Manual,
*        SUN/104.7, DRAL, 1994.

*  Authors:
*     DSB: David S. Berry (EAO)

*  History:
*     12-MAR-2020 (DSB):
*        Original version, based on equivalent Fortran function by DSB et
*        al.

*-
*/

/* Set the returned status. */
   *status = 1;

/* Report the error. */
   emsSetc( "pdaXermsg", librar );
   emsSetc( "pdaXermsg", "/" );
   emsSetc( "pdaXermsg", subrou );
   emsSetc( "pdaXermsg", ": " );
   emsSetc( "pdaXermsg", messg );
   emsRep( " ", "^pdaXermsg", status );

/* Return. */
}

