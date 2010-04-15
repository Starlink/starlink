      SUBROUTINE SCULIB_SKYDIP_BOLS (FILTER, BOLOMETERS, STATUS)
*+
*  Name:
*     SCULIB_SKYDIP_BOLS

*  Purpose:
*     returns bolometers to be measured in a SKYDIP

*  Description:
*     This routine returns a string containing the names of the bolometers
*     to be measured in a SKYDIP observation.
*
*        Only those sub-instruments that are looking out through a suitable
*     filter will be measured. Thus, if the filter in front of the SHORT
*     array is '350' or '450' the returned string will contain the word
*     SHORT_DC. Other filter/sub/bolometer combinations are as follows:-
*       - 600 or 750 or 850 in front of the LONG array gives LONG_DC
*       - 1100 in front of the P1100 photometer gives        P1100_DC
*       - 1300 in front of the P1300 photometer gives        P1300_DC
*       - 2000 in front of the P2000 photometer gives        P2000_DC
*
*     The returned BOLOMETERS string will contain the words for each
*     sub-instrument to be measured separated by commas.
*

*  Invocation:
*     CALL SCULIB_SKYDIP_BOLS (FILTER, BOLOMETERS, STATUS)

*  Arguments:
*     FILTER                    = CHARACTER*(*) (Given)
*                the name of the filter combination in use
*     BOLOMETERS                = CHARACTER*(*) (Returned)
*                the names of the bolometers to be measured
*     STATUS                    = INTEGER (Given and returned)
*                global status


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
*     11-JAN-1995: Original version.
*    endhistory

*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      CHARACTER*(*) FILTER

*  Arguments Given & Returned:

*  Arguments Returned:
      CHARACTER*(*) BOLOMETERS

*  Status:
      INTEGER STATUS

*  External references:
      INTEGER      CHR_LEN             ! CHR string-length function

*  Global variables:

*  Local constants:
      INTEGER      MAX_SUBS            ! the number of SCUBA sub-instruments
      PARAMETER (MAX_SUBS = 5)

*  Local variables:
      INTEGER      N_SUBS              ! number of sub-instruments
      CHARACTER*15 SUB_FILTER (MAX_SUBS)
                                       ! the name of the filter in front of
                                       ! each sub-instrument
      CHARACTER*15 SUB_INSTRMNT (MAX_SUBS)
                                       ! the name of each sub-instrument
      REAL         SUB_WAVELENGTH (MAX_SUBS)
                                       ! the nominal wavelength of each
                                       ! the filter for each sub-instrument

*.

      IF (STATUS .NE. SAI__OK) RETURN

*  find the wavelength of the filter in front of each sub-instrument

      N_SUBS = MAX_SUBS
      SUB_INSTRMNT (1) = 'SHORT'
      SUB_INSTRMNT (2) = 'LONG'
      SUB_INSTRMNT (3) = 'P1100'
      SUB_INSTRMNT (4) = 'P1300'
      SUB_INSTRMNT (5) = 'P2000'

      CALL SCULIB_DECODE_FILTER (FILTER, N_SUBS, SUB_INSTRMNT,
     :  SUB_FILTER, SUB_WAVELENGTH, STATUS)

*  construct BOLOMETERS string to measure only those sub-instruments that
*  have a filter of the correct wavelength

      BOLOMETERS = ' '
      IF ((SUB_FILTER(1) .EQ. '350')  .OR.
     :    (SUB_FILTER(1) .EQ. '450')) THEN
         BOLOMETERS = 'SHORT_DC,'
      END IF
      IF ((SUB_FILTER(2) .EQ. '600')  .OR.
     :    (SUB_FILTER(2) .EQ. '750')  .OR.
     :    (SUB_FILTER(2) .EQ. '850')) THEN
         BOLOMETERS (CHR_LEN(BOLOMETERS)+1:) = 'LONG_DC,'
      END IF
      IF (SUB_FILTER(3) .EQ. '1100') THEN
         BOLOMETERS (CHR_LEN(BOLOMETERS)+1:) = 'P1100_DC,'
      END IF
      IF (SUB_FILTER(4) .EQ. '1300') THEN
         BOLOMETERS (CHR_LEN(BOLOMETERS)+1:) = 'P1300_DC,'
      END IF
      IF (SUB_FILTER(5) .EQ. '2000') THEN
         BOLOMETERS (CHR_LEN(BOLOMETERS)+1:) = 'P2000_DC,'
      END IF

      IF (CHR_LEN(BOLOMETERS) .GT. 0) THEN
         BOLOMETERS = BOLOMETERS (:CHR_LEN(BOLOMETERS)-1)
      END IF

      END
