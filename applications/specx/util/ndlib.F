*  History:
*     19 Nov 1993 (hme):
*        Realise COMND as a call to SYSTEM.
*     24 Jul 2000 (ajc)::
*        WFILE,RFILE use REC= not 'rec
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C                    [PADMAN.SPECX]NDLIB.FOR
C                    -----------------------
C
C   This library contains subroutines to replace those found in the
C   system library of the Cambridge radioastronomy NORD 100/500.
C   In the first instance the routines are those required by the
C   spectral line reduction program SPECX and its derivatives.
C
C   Rachael Padman, 25th March 1983
C    amended 16th April, ERMSG moved to FORLIB, retyped.
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#if HAVE_CONFIG_H
# include "config.h"
#endif
      SUBROUTINE COMND(COM)

C   This routine submits a request to the VAX/VMX operating system.
C   In the VAX this spawns a sub-process and puts the master process
C   or calling program into a wait state until the subprocess has been
C   completed. LIB$SPAWN is described in the RUN-TIME library users
C   reference manual

C   Rachael Padman
C   25th March, 1983
C   Berkeley astronomy VAX

C Converted to unix by Tim Jenness
C   5th March 1997(timj)
C       Use SH for Sun systems (SYSTEM for Alpha) - 10 times faster
C   14 December 1999 (timj)
C       Use compiler pre-processor to choose SH or SYSTEM
C       -DUSESH is the required symbol
C   26 Jul 2004 (timj)
C       Autoconf symbols

      IMPLICIT NONE
      CHARACTER COM*(*)
      INTEGER ISTATUS
#if HAVE_INTRINSIC_SH || HAVE_SH
      INTEGER SH
#elif HAVE_INTRINSIC_SYSTEM || HAVE_SYSTEM
      INTEGER SYSTEM
#else
  error "Unable to run system commands"
#endif

*     Execute the system command

#if HAVE_INTRINSIC_SH || HAVE_SH
      ISTATUS = SH(COM)    ! SH is far faster than SYSTEM on many systems
#else
      ISTATUS = SYSTEM(COM)
#endif


      IF (ISTATUS.NE.0) PRINT *,'Error executing system command'

      RETURN
      END

C************************************************************************

      SUBROUTINE RFILE(LUN,ISHARE,IADD,IREC,IRECL)

C   Random access read on unit LUN. Shared access OK if ISHARE.ne.0
C   IRECL I*2 words of data read from record IREC (in NORD convention, first
C   scan in file labelled 0) and returned into array IADD.

C   Shared access not implemented in this version, ISHARE ignored.

      INTEGER*2 IADD(1)

      IPOS=IREC+1
      READ(LUN, REC=IPOS) (IADD(I),I=1,IRECL)

      RETURN
      END

C***********************************************************************

      SUBROUTINE WFILE(LUN,ISHARE,IADD,IREC,IRECL)

C   Random access write on unit LUN. Shared access OK if ISHARE.ne.0
C   IRECL words of data written to record IREC (in NORD convention, first
C   scan in file labelled 0) from array IADD.

C   Shared access not implemented in this version, ISHARE ignored.

      INTEGER*2 IADD(1)

      IPOS=IREC+1
      WRITE(LUN,REC=IPOS) (IADD(I),I=1,IRECL)

      RETURN
      END

C***********************************************************************
