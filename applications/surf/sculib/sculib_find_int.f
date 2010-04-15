      SUBROUTINE SCULIB_FIND_INT (DEMOD_POINTER, N_SWITCHES,
     :     N_EXPOSURES, N_INTEGRATIONS, N_MEASUREMENTS, N_POS,
     :     INTEG, MEAS, I_START, I_END, STATUS)
*+
*  Name:
*     SCULIB_FIND_INT

*  Purpose:
*     Return the indices of the start and end of the specified integration

*  Invocation:
*     CALL SCULIB_FIND_INT(DEMOD_POINTER, N_SWITCHES,
*    :     N_EXPOSURES, N_INTEGRATIONS, N_MEASUREMENTS, N_POS,
*    :     INTEG, MEAS, I_START, I_END, STATUS)

*  Description:
*     This routine find that start and end indices of the specified
*     integration. In general these can be found directly from
*     the DEMOD_POINTER array but with the warning that the last
*     integration will not necessarily point to the end of the
*     data array if multiple measurements are being taken. For this
*     reason, put this knowledge in one place.

*  Arguments:
*     DEMOD_POINTER ( N_SWITCHES, N_EXPOSURES, N_INTEGRATIONS, N_MEASUREMENTS ) = INTEGER (Given)
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
*     INTEG                 = INTEGER (Given)
*           the integration number of the switch
*     MEAS                  = INTEGER (Given)
*           the measurement number of the switch
*     I_START               = INTEGER (Returned)
*           pointer to the start of the integration
*     I_END                 = INTEGER (Returned)
*           pointer to the end of the integration
*     STATUS                = INTEGER (Given and returned)
*           global status

*  Authors:
*     Tim Jenness (t.jenness@jach.hawaii.edu)


*  Copyright:
*     Copyright (C) 1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     $Log$
*     Revision 1.3  1999/08/19 03:37:08  timj
*     Header tweaks to ease production of SSN72 documentation.
*
*     Revision 1.2  1999/08/03 19:34:56  timj
*     Add copyright message to header.
*     Convert old header style to new.
*
*     Revision 1.1  1999/05/25 23:35:46  timj
*     First version
*

*-

*    Type Definitions:
      IMPLICIT NONE

*    Global constants:
      INCLUDE 'SAE_PAR'

*    Arguments Given:
      INTEGER N_SWITCHES
      INTEGER N_EXPOSURES
      INTEGER N_INTEGRATIONS
      INTEGER N_MEASUREMENTS
      INTEGER DEMOD_POINTER (N_SWITCHES, N_EXPOSURES, N_INTEGRATIONS,
     :     N_MEASUREMENTS)
      INTEGER N_POS
      INTEGER INTEG
      INTEGER MEAS

*    Arguments Returned:
      INTEGER I_START
      INTEGER I_END

*    Status:
      INTEGER STATUS

*    External references:

*    Global variables:

*    Local Constants

*    Local variables:

*    Internal References:

*    Local data:

*.

      IF (STATUS .NE. SAI__OK) RETURN

*     integration start is easy

      I_START = DEMOD_POINTER (1, 1, INTEG, MEAS)

*     Now calculate End of integration - this is effectively the
*     start of the next integration-1

*     Calculate next integration number

      IF (INTEG .LT. N_INTEGRATIONS) THEN
*     Just go to the next integration

         I_END = DEMOD_POINTER(1, 1, INTEG + 1, MEAS) - 1

      ELSE IF (INTEG .EQ. N_INTEGRATIONS .AND.
     :        MEAS .LT. N_MEASUREMENTS) THEN
*     Have to increment the measurement counter and revert to next int

         I_END = DEMOD_POINTER(1, 1, 1, MEAS+1) - 1

      ELSE

*     This is the last measurement and integration
         I_END = N_POS

      END IF

      END
