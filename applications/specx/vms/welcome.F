#if HAVE_CONFIG_H
# include "config.h"
#endif
      SUBROUTINE WELCOME
*+
*  Name:
*     WELCOME

*  Purpose:
*     Print $SYS_SPECX/specx_welcome.txt to terminal.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL WELLCOME

*  Description:
*     This routine displays the welcome text of Specx 6.4.

*  Arguments:
*     None.

*  Pre-processor Symbols:
*     USESH
*        Use SH() rather than SYSTEM() for system calls (UNIX only)
*     VMS
*        We are on a VMS system

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     timj: Tim Jenness (JACH, Hawaii)
*     {enter_new_authors_here}

*  History:
*     07 Feb 1994 (hme):
*        Original version.
*     01 Sep 1996 (timj)
*        Replaced CALL SYSTEM with STATUS = SH (much faster)
*     14 December 1999 (timj)
*        Merge all versions (VMS, Alpha, Sun) using the C pre processor
*     26 July 2004 (timj)
*        Autoconf version
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Local Variables:
      INTEGER STATUS
#ifndef VMS
      CHARACTER * ( 256 ) SYS_SPECX
      INTEGER GEN_ILEN
#endif

*  External functions:
#ifdef VMS
      INTEGER LIB$SPAWN
#else

#  if HAVE_INTRINSIC_SH || HAVE_SH
      INTEGER SH
#  elif HAVE_INTRINSIC_SYSTEM || HAVE_SYSTEM
      INTEGER SYSTEM
#  else
   error "Do not know how to fork a shell"
#  endif

#endif

*.


C     For VMS Systems we have to spawn else we do the rest of the routine
C     Sorry for the nested defines

#ifdef VMS
      STATUS = LIB$SPAWN(
     :   'type/page SYS_SPECX:specx_welcome.txt',,,,,,,,, )
#else

      STATUS = 0

C     Translate the environment variable
      CALL UTRNLOG( 'SYS_SPECX', SYS_SPECX, STATUS )

C  SH is much faster but is not available on all unix systems

#if HAVE_INTRINSIC_SH || HAVE_SH
      STATUS = SH('more ' // SYS_SPECX(:GEN_ILEN(SYS_SPECX)) //
     :   '/specx_welcome.txt' )

#else

      STATUS = SYSTEM('more ' // SYS_SPECX(:GEN_ILEN(SYS_SPECX)) //
     :   '/specx_welcome.txt' )

#endif

#endif

      END
