      SUBROUTINE SCULIB_CALC_SUB_BOLS (N_BOL_IN, IN_BOL_ADC,
     :  IN_BOL_CHAN, NUM_CHAN, NUM_ADC, BOL_TYPE, SUB_INSTRUMENT,
     :  N_BOL_OUT, OUT_BOL_ADC, OUT_BOL_CHAN, IN_POINTER, STATUS)
*+
*  Name:
*     SCULIB_CALC_SUB_BOLS

*  Purpose:
*     find the positions of bolometers belonging to a
*     specified sub-instrument in a demodulated data array

*  Description:
*     This subroutine finds the location of data from bolometers belonging to
*     a specified sub-instrument in a demodulated data array that may contain
*     data for several sub-instruments. The number of bolometers found, their
*     indices in the data array, and their ADC,channel numbers are returned
*     by the routine.

*  Invocation:
*     CALL  SCULIB_CALC_SUB_BOLS (N_BOL_IN, IN_BOL_ADC, IN_BOL_CHAN,
*    :  NUM_CHAN, NUM_ADC, BOL_TYPE, SUB_INSTRUMENT, N_BOL_OUT,
*    :  OUT_BOL_ADC, OUT_BOL_CHAN, IN_POINTER, STATUS)

*  Arguments:
*     N_BOL_IN                     = INTEGER (Given)
*           number of bolometers in data array
*     IN_BOL_ADC (N_BOL_IN)        = INTEGER (Given)
*           ADC numbers of bolometers in data array
*     IN_BOL_CHAN (N_BOL_IN)       = INTEGER (Given)
*           channel numbers of bolometers in data array
*     NUM_CHAN                     = INTEGER (Given)
*           number of channels per A/D card
*     NUM_ADC                      = INTEGER (Given)
*           number of A/D cards
*     BOL_TYPE (NUM_CHAN,NUM_ADC)  = CHARACTER*(*) (Given)
*           types of bolometers
*     SUB_INSTRUMENT               = CHARACTER*(*) (Given)
*           the name of the sub-instrument for which data are to be extracted
*     N_BOL_OUT                    = INTEGER (Returned)
*           the number of bolometers in the data array that belong to the
*           specified sub-instrument
*     OUT_BOL_ADC (N_BOL_IN)       = INTEGER (Returned)
*           ADC numbers of bolometers in specified sub-instrument
*     OUT_BOL_CHAN (N_BOL_IN)      = INTEGER (Returned)
*           channel numbers of bolometers in specified sub-instrument
*     IN_POINTER (N_BOL_IN)        = INTEGER (Returned)
*           array pointing to indices in data array that contain
*           data for bolometers in the specified sub-instrument
*     STATUS                       = INTEGER (Given and returned)
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
*     3-AUG-1995: original version
*    endhistory

*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      INTEGER       N_BOL_IN
      INTEGER       IN_BOL_ADC (N_BOL_IN)
      INTEGER       IN_BOL_CHAN (N_BOL_IN)
      INTEGER       NUM_CHAN
      INTEGER       NUM_ADC
      CHARACTER*(*) BOL_TYPE (NUM_CHAN, NUM_ADC)
      CHARACTER*(*) SUB_INSTRUMENT

*  Arguments Given & Returned:

*  Arguments Returned:
      INTEGER       N_BOL_OUT
      INTEGER       OUT_BOL_ADC (N_BOL_IN)
      INTEGER       OUT_BOL_CHAN (N_BOL_IN)
      INTEGER       IN_POINTER (N_BOL_IN)

*  Status:
      INTEGER       STATUS

*  External references:

*  Global variables:

*  Local Constants:

*  Local variables:
      INTEGER      IN_BOL                  ! bolometer index in input array
      CHARACTER*15 STEMP1                  ! copy of SUB_INSTRUMENT
      CHARACTER*15 STEMP2                  ! scratch string

*  Internal References:

*  Local data:

*.

      IF (STATUS .NE. SAI__OK) RETURN

      STEMP1 = SUB_INSTRUMENT
      CALL CHR_UCASE (STEMP1)

*  calculate the output ADC, channel and pointer arrays

      N_BOL_OUT = 0

      IF (N_BOL_IN .GT. 0) THEN

         DO IN_BOL = 1, N_BOL_IN
            STEMP2 = BOL_TYPE(IN_BOL_CHAN(IN_BOL),IN_BOL_ADC(IN_BOL))
            CALL CHR_UCASE (STEMP2)

            IF (STEMP1 .EQ. STEMP2) THEN
               N_BOL_OUT = N_BOL_OUT + 1
               OUT_BOL_CHAN(N_BOL_OUT) = IN_BOL_CHAN(IN_BOL)
               OUT_BOL_ADC(N_BOL_OUT) = IN_BOL_ADC(IN_BOL)
               IN_POINTER(N_BOL_OUT) = IN_BOL
            END IF

         END DO

      END IF

      END
