*-----------------------------------------------------------------------
#if HAVE_CONFIG_H
# include "config.h"
#endif

      SUBROUTINE UOPENUF (UNIT, FILENAME, STATUS, RECLW, READONLY, IERR)

*     Routine to open an unformatted direct access file. Need to
*     wrap up the OPEN statement this way because Suns do not obey
*     the FORTRAN 77 standard for the OPEN statement.

*  Authors:
*     Tim Jenness (JACH)

*  History:
*     Prehistory:
*       Someone wrote this orginally
*     14 Dec 1999 (TIMJ):
*       Uses a C pre-processor directive to determine how many bytes
*       per record there are. Could either specify the number explcitly
*       in a define or determine it from a machine specific define.
*       Currently use the latter. If SUN4 is defined use BYTEPRU=1 else
*       use BYTEPRU=4.
*     1 Aug 2000 (AJC):
*       Linux uses BYTEPRU=1
*       Replace TYPE with PRINT
*       Remove READONLY open for linux
*     26 Jul 2004 (timj):
*       Autoconf version

      IMPLICIT   NONE

*     Formal parameters:

      INTEGER    UNIT              ! Logical unit number
      CHARACTER  FILENAME*(*)      ! Filename
      CHARACTER  STATUS*(*)        ! Status (Old/New/Scratch/Unknown)
      INTEGER    RECLW             ! Record length in 4-byte words
      LOGICAL    READONLY          ! OPEN with READONLY flag if true
      INTEGER    IERR              ! Status return

*     Local variables:

      INTEGER    RECL              ! Record length in compiler units

*     Local Constants:
      INTEGER    BYTEPRU           ! # of bytes per record unit


#if FC_RECL_UNIT == 1
      PARAMETER ( BYTEPRU = 1 )
#elif FC_RECL_UNIT == 2
      PARAMETER ( BYTEPRU = 2 )
#elif FC_RECL_UNIT == 4
      PARAMETER ( BYTEPRU = 4 )
#else
   error "Unrecognized FC_RECL_UNIT"
#endif

*  Ok, go...

      IERR = 0

      RECL = RECLW*4 / BYTEPRU

*     PRINT *, ' --- openuf ---'
*     PRINT *, '     attempting to open file ', FILENAME
*     PRINT *, '     requested status = ', STATUS

      IF (READONLY) THEN
        OPEN (UNIT    =  UNIT,
     &        FILE    =  FILENAME,
     &        STATUS  =  STATUS,
     &        RECL    =  RECL,
     &        FORM    = 'UNFORMATTED',
     &        ACCESS  = 'DIRECT',
#if HAVE_FC_OPEN_ACTIONREAD
     &        ACTION='READ',
#elif HAVE_FC_OPEN_READONLY
     &        READONLY,
#endif
     &        IOSTAT  =  IERR)

      ELSE
        OPEN (UNIT    =  UNIT,
     &        FILE    =  FILENAME,
     &        STATUS  =  STATUS,
     &        RECL    =  RECL,
     &        FORM    = 'UNFORMATTED',
     &        ACCESS  = 'DIRECT',
     &        IOSTAT  =  IERR)

      END IF

      IF (IERR.GT.1) THEN
        PRINT *, ' --- openuf ---'
        PRINT *, '     error in OPEN: FORTRAN i/o error # ', IERR
      END IF

      RETURN
      END

*-----------------------------------------------------------------------





