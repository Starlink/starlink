#include "f77.h"
#include "ems.h"

#define BUFLEN 500

F77_SUBROUTINE(pda_xermsg)( CHARACTER(LIBRAR), CHARACTER(SUBROU),
                            CHARACTER(MESSG), INTEGER(NERR), INTEGER(LEVEL),
                            INTEGER(STATUS) TRAIL(LIBRAR) TRAIL(SUBROU)
                            TRAIL(MESSG) ) {
/*
*+
*  Name:
*     PDA_XERMSG

*  Purpose:
*     Process error messages for SLATEC and other libraries.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PDA_XERMSG( LIBRAR, SUBROU, MESSG, NERR, LEVEL, STATUS )

*  Description:
*     PDA_XERMSG processes a diagnostic message in a manner determined by
*     the value of LEVEL. In the orignal, things also depended on an
*     error report control flag KONTRL. This was by default 2.
*
*     If KONTRL was zero or negative, no information other than the
*     message itself (including numeric values, if any) would have been
*     printed.  If KONTRL was positive, introductory messages,
*     trace-backs, etc., would have been printed in addition to the
*     message.
*
*     Depending on KONTRL and LEVEL the error handling mechanism might
*     also have included aborting the programme via a STOP statement.
*
*              ABS(KONTRL)
*        LEVEL        0              1              2
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
*     In the current version, this routine will always issue a message
*     via ERR_REP. Under no circumstances is the programme
*     aborted. Instead this routine always returns control to the caller
*     after setting the STATUS argument (which is new in this version)
*     to 1.

*  Arguments:
*     LIBRAR = CHARACTER * ( * ) (Given)
*        The name of the library such as 'SLATEC'. This will form part
*        of the message put out.
*     SUBROU = CHARACTER * ( * ) (Given)
*        The name of the subroutine calling PDA_XERMSG. This will form part
*        of the message put out.
*     MESSG = CHARACTER * ( * ) (Given)
*        The principal error or warning message.
*     NERR = INTEGER (Given)
*        Ignored.
*     LEVEL = INTEGER (Given)
*        Ignored.
*     STATUS = INTEGER (Returned)
*        The global status. Always returned as 1.

*  Implementation Status:
*     The newline sentinel $$ is not interpreted by this routine.

*  References:
*     -  R. E. Jones and D. K. Kahaner, XERROR, the SLATEC Error-handling
*        Package, SAND82-0800, Sandia Laboratories, 1982.
*     -  P. C. T. Rees and A. J. Chipperfield, MSG and ERR, Message and
*        Error Reporting Systems, Version 1.4, Programmer's Manual,
*        SUN/104.7, DRAL, 1994.

*  Authors:
*     Kirby Fong (NMFECC at LLNL)
*     wrb
*     rwc
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     01 Jan 1988:
*        Date written.
*     21 Jun 1988:
*        Revised as directed at SLATEC CML meeting of February 1988.
*        There are two basic changes.
*        1.  A new routine, XERPRN, is used instead of XERPRT to
*            print messages.  This routine will break long messages
*            into pieces for printing on multiple lines.  '$$' is
*            accepted as a new line sentinel.  A prefix can be
*            added to each line to be printed.  PDA_XERMSG uses either
*            ' ***' or ' *  ' and long messages are broken every
*            72 characters (at most) so that the maximum line
*            length output can now be as great as 76.
*        2.  The text of all messages is now in upper case since the
*            Fortran standard document does not admit the existence
*            of lower case.
*     08 Jul 1988:
*        Revised after the SLATEC CML meeting of June 29 and 30.
*        The principal changes are
*        1.  clarify comments in the prologues
*        2.  rename XRPRNT to XERPRN
*        3.  rework handling of '$$' in XERPRN to handle blank lines
*            similar to the way format statements handle the /
*            character for new records.
*     06 Jul 1989:
*        Revised with the help of Fred Fritsch and Reg Clemens to
*        clean up the coding.
*     21 Jul 1989:
*        Revised to use new feature in XERPRN to count characters in
*        prefix.
*     13 Oct 1989:
*        Revised to correct comments.
*     14 Dec 1989 (wrb):
*        Prologue converted to Version 4.0 format.
*     10 May 1990 (rwc):
*        Changed test on NERR to be -9999999 < NERR < 99999999, but
*        NERR .ne. 0, and on LEVEL to be -2 < LEVEL < 3.  Added
*        LEVEL=-1 logic, changed calls to XERSAV to XERSVE, and
*        XERCTL to XERCNT.
*     01 May 1992 (wrb):
*        Reformatted the REFERENCES section.
*     04 Apr 1995 (hme):
*        Re-write to use Starlink error reporting.
*     05 Apr 1995 (hme):
*        Do not report an error, but just issue a message. The returned
*        status then need not be treated with Starlink ERR_ routines at
*        the application level.
*     12 Jul 1995 (hme):
*        On advice from Starlink, use ERR_REP instead of MSG_OUT.
*     14 Feb 1997 (dsb):
*        On advice from Starlink, use EMS_REP instead of ERR_REP.
*     3 Jul 2020 (dsb):
*        Speed it up by re-writing it in C and using a local buffer to
*        format the message.
*-
*/

   GENPTR_CHARACTER(LIBRAR)
   GENPTR_CHARACTER(SUBROU)
   GENPTR_CHARACTER(MESSG)
   GENPTR_INTEGER(NERR)
   GENPTR_INTEGER(LEVEL)
   GENPTR_INTEGER(STATUS)

/* Local Variables: */
   char buf[ BUFLEN + 1 ];
   char *p;
   int nleft;
   int nc;

/* Set the returned status. */
   *STATUS = 1;

/* For speed, format the error message in a local buffer. */
   p = buf;
   nleft = BUFLEN;

   nc = ( LIBRAR_length < nleft ) ? LIBRAR_length : nleft;
   strncpy( p, LIBRAR, nc );
   p += nc;
   nleft -= nc;

   if( nleft > 0 ) {
      *(p++) = '/';
      nleft--;
   }

   nc = ( SUBROU_length < nleft ) ? SUBROU_length : nleft;
   if( nc ) {
      strncpy( p, SUBROU, nc );
      p += nc;
      nleft -= nc;
   }

   if( nleft > 1 ) {
      *(p++) = ':';
      *(p++) = ' ';
      nleft -= 2;
   }

   nc = ( MESSG_length < nleft ) ? MESSG_length : nleft;
   if( nc ) {
      strncpy( p, MESSG, nc );
      p += nc;
      nleft -= nc;
   }

   *p = 0;

/* Report the error. */
   emsRep( " ", buf, STATUS );

}

