      SUBROUTINE SCULIB_SUB_TAUZ (N_SUBS, SUB_FILTER, N_SKY, SKY_FILTER,
     :  SKY_TAUZ, SKY_DAY, SUB_TAUZ, SUB_DAY, STATUS)
*+
*  Name:
*     SCULIB_SUB_TAUZ

*  Purpose:
*     get tauz appropriate for each sub-instrument

*  Description:
*     This routine finds the zenith sky optical depth appropriate for the
*     filter in front of each sub-instrument being used. If it cannot find
*     an optical depth for the required filter the routine will output a
*     warning message and return an optical depth of 0.0.
*
*     The times and dates at which the optical depths were measured are also
*     returned. If the optical depth for the required filter cannot be found
*     then the date will be that at which the routine is run.

*  Invocation:
*     CALL SCULIB_GET_TAUZ (N_SUBS, SUB_FILTER, N_SKY, SKY_FILTER,
*    :  SKY_TAUZ, SKY_DAY, SUB_TAUZ, SUB_DAY, STATUS)

*  Arguments:
*     N_SUBS                         = INTEGER (Given)
*           number of sub-instruments
*     SUB_FILTER (N_SUBS)            = CHARACTER*(*) (Given)
*           names of filters in front of sub-instruments
*     N_SKY                          = INTEGER (Given)
*           number of filters for which data is available
*     SKY_FILTER (N_SKY)             = CHARACTER*(*) (Given)
*           filters for which data is available
*     SKY_TAUZ (N_SKY)               = REAL (Given)
*           tauz for each possible filter
*     SKY_DAY (N_SKY)                = DOUBLE PRECISION (Given)
*           the date and time of the tauz measurement specified as day
*           number since 1st Jan.
*     SUB_TAUZ (N_SUBS)              = REAL (Returned)
*           tauz appropriate to sub-instruments
*     SUB_DAY (N_SUBS)               = DOUBLE PRECISION (Returned)
*           the date and time of the tauz measurement
*     STATUS                         = INTEGER (Given and returned)
*           global status


*  Authors:
*     J.Lightfoot (REVAD::JFL)

*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  Method:

*  Deficiencies:

*  Bugs:


*  History:
*     $Id$
*     12-OCT-1994: Original version.
*    endhistory

*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'                      ! for VAL__BADR

*  Arguments Given:
      INTEGER          N_SUBS
      CHARACTER*(*)    SUB_FILTER (N_SUBS)
      INTEGER          N_SKY
      CHARACTER*(*)    SKY_FILTER (N_SKY)
      REAL             SKY_TAUZ (N_SKY)
      DOUBLE PRECISION SKY_DAY (N_SKY)

*  Arguments Given & Returned:

*  Arguments Returned:
      REAL             SUB_TAUZ (N_SUBS)
      DOUBLE PRECISION SUB_DAY (N_SUBS)

*  Status:
      INTEGER STATUS

*  External references:
      DOUBLE PRECISION SCULIB_DAY             ! time and date as day number
                                              ! after 1st Jan

*  Global variables:

*  Local Constants:

*  Local variables:
      INTEGER I
      INTEGER SUB

*  Internal References:

*  Local data:

*.

      IF (STATUS .NE. SAI__OK) RETURN

      IF (N_SUBS .GT. 0) THEN

*  loop through sub-instruments

         DO SUB = 1, N_SUBS

            SUB_TAUZ (SUB) = VAL__BADR

*  loop through filters

            IF (N_SKY .GT. 0) THEN

               DO I = 1, N_SKY

                  IF (SUB_FILTER(SUB) .EQ. SKY_FILTER(I)) THEN
                     SUB_TAUZ (SUB) = SKY_TAUZ (I)
                     SUB_DAY (SUB) = SKY_DAY (I)
                  END IF

               END DO

            END IF

*  see if tauz was found

            IF (SUB_TAUZ(SUB) .EQ. VAL__BADR) THEN

               CALL MSG_SETC ('FILTER', SUB_FILTER(SUB))
               STATUS = SAI__WARN
               CALL ERR_OUT (' ', 'SCULIB_SUB_TAUZ: warning - '//
     :           'failed to find tauz for filter ^FILTER, 0 has '//
     :           'been assumed', STATUS)
               SUB_TAUZ (SUB) = 0.0
               SUB_DAY (SUB) = SCULIB_DAY ()

            END IF

         END DO

      END IF


      END
