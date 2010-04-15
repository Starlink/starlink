      SUBROUTINE SCULIB_FIND_SWITCH (DEMOD_POINTER, N_SWITCHES,
     :  N_EXPOSURES, N_INTEGRATIONS, N_MEASUREMENTS, N_POS, SWITCH, EXP,
     :  INT, MEAS, S_START, S_END, STATUS)
*+
*  Name:
*     SCULIB_FIND_SWITCH

*  Purpose:
*     find the start and end indices of a switch in the
*     demodulated data array

*  Description:
*     This routine finds the start and end indices of the specified switch
*     in a SCUBA observation. The array DEMOD_POINTER contains the start
*     indices, so this part is easy. However, the end index will be 1 less
*     than the start index of the next, or the index of the last position
*     measured if the specified switch is the last in the observation. This
*     makes things a little complicated because consecutive switches within
*     an exposure need not have been taken and stored consecutively,
*     though exposures, integrations and measurements are ordered as one
*     would expect.

*  Invocation:
*     CALL SCULIB_FIND_SWITCH (DEMOD_POINTER, N_SWITCHES, N_EXPOSURES,
*    :  N_INTEGRATIONS, N_MEASUREMENTS, N_POS, SWITCH, EXP, INT, MEAS,
*    :  S_START, S_END, STATUS)

*  Arguments:
*     DEMOD_POINTER (N_SWITCHES, N_EXPOSURES, N_INTEGRATIONS, N_MEASUREMENTS) = INTEGER (Given)
*           an array of pointers to the start of each switch
*     N_SWITCHES            = INTEGER (Given)
*           the number of switches per exposure
*     N_EXPOSURES           = INTEGER (Given)
*           the number of exposures per integration
*     N_INTEGRATIONS        = INTEGER (Given)
*           the number of integrations per measurement
*     N_MEASUREMENTS        = INTEGER (Given)
*           the number of measurements in the observation
*     N_POS                 = INTEGER (Given)
*           the total number of positions measured in the observation
*     SWITCH                = INTEGER (Given)
*           the index of the switch
*     EXP                   = INTEGER (Given)
*           the exposure number of the switch
*     INT                   = INTEGER (Given)
*           the integration number of the switch
*     MEAS                  = INTEGER (Given)
*           the measurement number of the switch
*     S_START               = INTEGER (Returned)
*           pointer to the start of the switch
*     S_END                 = INTEGER (Returned)
*           pointer to the end of the switch
*     STATUS                = INTEGER (Given and returned)
*           global status

*  Authors:
*     J.Lightfoot (jfl@roe.ac.uk)

*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  Method:

*  Deficiencies:

*  Bugs:


*  History:
*     $Id$
*     $Log$
*     Revision 1.6  1999/08/19 03:37:08  timj
*     Header tweaks to ease production of SSN72 documentation.
*
*     25-JUL-1995: original version.
*    endhistory

*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      INTEGER N_SWITCHES
      INTEGER N_EXPOSURES
      INTEGER N_INTEGRATIONS
      INTEGER N_MEASUREMENTS
      INTEGER DEMOD_POINTER (N_SWITCHES, N_EXPOSURES, N_INTEGRATIONS,
     :  N_MEASUREMENTS)
      INTEGER N_POS
      INTEGER SWITCH
      INTEGER EXP
      INTEGER INT
      INTEGER MEAS

*  Arguments Given & Returned:

*  Arguments Returned:
      INTEGER S_START
      INTEGER S_END

*  Status:
      INTEGER STATUS

*  External references:

*  Global variables:

*  Local Constants:

*  Local variables:
      INTEGER E                        ! exposure index in DO loop
      INTEGER I                        ! integer index in DO loop
      INTEGER M                        ! measurement index in DO loop
      INTEGER S                        ! switch index in DO loop

*  Internal References:

*  Local data:

*.

      IF (STATUS .NE. SAI__OK) RETURN

*  switch start is easy

      S_START = DEMOD_POINTER (SWITCH, EXP, INT, MEAS)

*  switch end is more difficult because data for consecutive switch indices
*  may not have been taken and stored consecutively. The solution to this
*  is to set switch end to the value of the next highest pointer in the
*  pointer array, or to the end of the data array if this is the last
*  switch in the observation.

      S_END = N_POS + 1

      DO M = 1, N_MEASUREMENTS
         DO I = 1, N_INTEGRATIONS
            DO E = 1, N_EXPOSURES
               DO S = 1, N_SWITCHES

                  IF ((DEMOD_POINTER(S,E,I,M) .GT. S_START) .AND.
     :                 (DEMOD_POINTER(S,E,I,M) .LT. S_END)) THEN
                     S_END = DEMOD_POINTER (S,E,I,M)
                  END IF

               END DO
            END DO
         END DO
      END DO

*  whatever has happened S_END should now point to the array element
*  right after the last one in the specified switch

      S_END  = S_END - 1

      END
