#if HAVE_CONFIG_H
# include "config.h"
#endif
      REAL FUNCTION SCULIB_SECNDS( OFFSET, STATUS )
*+
*  Name:
*     SCULIB_SECNDS

*  Purpose:
*     Calculate seconds since start of local day minus supplied argument

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = SCULIB_SECNDS( OFFSET, STATUS )

*  Description:
*     Calculate the number of elapsed seconds since the start of local
*     day minus the supplied offset. Useful for timers.

*  Arguments:
*     OFFSET = REAL (Given)
*        Value to be subtracted from the current number of seconds
*        since local day began.
*     STATUS = INTEGER (Given & Returned)
*        Global inherited status

*  Returned Value:
*     SCULIB_SECNDS = REAL
*        Current local day seconds minus offset.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  Implementation Status:
*     - If the INTRINSIC SECNDS is available it will be used, else
*       PSX_TIME is used.

*  Copyright:
*     Copyright (C) 2005 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     15-AUG-2005 (TIMJ):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Global status

*  Arguments Given:
      REAL OFFSET
      INTEGER STATUS

*  Local Constants:

*  Local Variables:
#if HAVE_INTRINSIC_SECNDS
      INTRINSIC SECNDS
#else
      INTEGER    NTICKS         ! Current time
      INTEGER    SECS
      INTEGER    MINS
      INTEGER    HOURS
      INTEGER    DAY
      INTEGER    MONTH
      INTEGER    YEAR
      INTEGER    WDAY
      INTEGER    YDAY
      INTEGER    ISDST
      INTEGER    TSTRCT
#endif

*.

*  Initialise
      SCULIB_SECNDS = 0.0

*  Check global status
      IF ( STATUS .NE. SAI__OK ) RETURN

#if HAVE_INTRINSIC_SECNDS
      SCULIB_SECNDS = SECNDS( OFFSET )
#else
      CALL PSX_TIME( NTICKS, STATUS )
      CALL PSX_LOCALTIME( NTICKS, SECS, MINS, HOURS, DAY, MONTH, YEAR,
     :     WDAY, YDAY, ISDST, TSTRCT, STATUS )

      IF (STATUS .EQ. SAI__OK) THEN
         SCULIB_SECNDS = ( HOURS * 3600.0 ) + ( MINS * 60.0 ) + SECS
     :        - OFFSET
      END IF

#endif

      END

