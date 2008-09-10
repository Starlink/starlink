#include "config.h"

      SUBROUTINE GKTIME(IHOURS,IMINS,ISECS)
*
* Copyright (C) SERC 1986
*
*-----------------------------------------------------------------------
*
* Type of routine:  SYSTEM INTERFACE
* Author:           PJWR
*
      INCLUDE '../include/check.inc'
*
* PURPOSE OF THE ROUTINE
* ----------------------
*
*     Return the current time as three integers.
*
* MAINTENANCE LOG
* ---------------
*
*     30/07/86  PJWR  UNIX version stabilised.
*     18/06/04  TIMJ  Autoconf version
*
* ARGUMENTS
* ---------
*
*     IHOURS  OUT  Hours (24 hour clock).
*     IMINS   OUT  Minutes past the hour.
*     ISECS   OUT  Seconds past the minute.
*
      INTEGER IHOURS, IMINS, ISECS
*
* LOCALS
* ------
*
*     IARRAY  Array for information from ITIME.
*
      INTEGER IARRAY(3)         ! ITIME implementation
      CHARACTER *8 CTIME        ! CLOCK implementation

*     These are for DATE_AND_TIME (f90)
      INTEGER DATARR( 8 )
      CHARACTER * 8 DATDAT
      CHARACTER * 10 DATTIM
      CHARACTER * 5 DATZON
*
* -------------------------------------------------------------

#if HAVE_INTRINSIC_ITIME || HAVE_ITIME
      CALL ITIME(IARRAY)
      IHOURS = IARRAY(1)
      IMINS = IARRAY(2)
      ISECS = IARRAY(3)
#elif HAVE_INTRINSIC_CLOCK || HAVE_CLOCK
*     Cray version. Untested
      WRITE (CTIME,100) CLOCK()
 100  FORMAT (A8)
      READ (CTIME,101) IHOURS, IMINS, ISECS
 101  FORMAT (I2,1X,I2,1X,I2)

#elif HAVE_INTRINSIC_DATE_AND_TIME || HAVE_DATE_AND_TIME

* Use array of size 8 to get results.
      CALL DATE_AND_TIME( DATDAT, DATTIM, DATZON, DATARR )
      IHOURS = DATARR( 5 )
      IMINS = DATARR( 6 )
      ISECS = DATARR( 7 )

#else
 error 'Do not know how to tell the time'
#endif

      RETURN

      END
