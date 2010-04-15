      SUBROUTINE SCULIB_FLATFIELD_DATA (N_BOL, N_POS, N_BEAM,
     :  BDATA, VARIANCE, QUALITY, BOL_CHAN, BOL_ADC, NUM_CHAN,
     :  NUM_ADC, BOL_FLAT, BOL_QUALITY, STATUS)
*+
*  Name:
*     SCULIB_FLATFIELD_DATA

*  Purpose:
*     flatfield the data in an array

*  Description:
*     This routine multiplies the data in the input array by the flatfield
*     values of the bolometers that took it. Output quality will be set
*     bad if input quality was bad or the quality of the flatfield for that
*     bolometer was bad.

*  Invocation:
*     CALL SCULIB_FLATFIELD_DATA (N_BOL, N_POS, N_BEAM, BDATA, VARIANCE,
*    :  QUALITY, BOL_CHAN, BOL_ADC, NUM_CHAN, NUM_ADC, BOL_FLAT,
*    :  BOL_QUALITY, STATUS)

*  Arguments:
*     N_BOL                  = INTEGER (Given)
*           number of bolometers measured
*     N_POS                  = INTEGER (Given)
*           number of positions at which the bolometers were measured
*     N_BEAM                 = INTEGER (Given)
*           number of beams data have been reduced into
*     BDATA (N_BOL, N_POS, N_BEAM)
*                            = REAL (Given and returned)
*           measured data
*     VARIANCE (N_BOL, N_POS, N_BEAM)
*                            = REAL (Given and returned)
*           variance on BDATA
*     QUALITY (N_BOL, N_POS, N_BEAM)
*                            = BYTE (Given and returned)
*           quality on BDATA
*     BOL_CHAN (N_BOL)       = INTEGER (Given)
*           the channel numbers of the measured bolometers
*     BOL_ADC (N_BOL)        = INTEGER (Given)
*           the A/D numbers of the measured bolometers
*     NUM_CHAN               = INTEGER (Given)
*           number of channels per A/D
*     NUM_ADC                = INTEGER (Given)
*           number of A/D cards
*     BOL_FLAT (NUM_CHAN, NUM_ADC)
*                            = REAL (Given)
*           the flatfield values
*     BOL_QUALITY (NUM_CHAN, NUM_ADC)
*                            = INTEGER (Given)
*           quality on BOL_FLAT
*     STATUS                 = INTEGER (Given and returned)
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
*     18-AUG-1995: original version
*    endhistory

*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'

*  Arguments Given:
      INTEGER N_BOL
      INTEGER N_POS
      INTEGER N_BEAM
      INTEGER BOL_CHAN (N_BOL)
      INTEGER BOL_ADC (N_BOL)
      INTEGER NUM_CHAN
      INTEGER NUM_ADC
      REAL    BOL_FLAT (NUM_CHAN,NUM_ADC)
      INTEGER BOL_QUALITY (NUM_CHAN,NUM_ADC)

*  Arguments Given & Returned:
      REAL    BDATA (N_BOL,N_POS,N_BEAM)
      REAL    VARIANCE (N_BOL,N_POS,N_BEAM)
      BYTE QUALITY (N_BOL,N_POS,N_BEAM)

*  Arguments Returned:

*  Status:
      INTEGER STATUS

*  External references:
      BYTE SCULIB_BITON

*  Global variables:

*  Local Constants:

*  Local variables:
      INTEGER ADC                          ! A/D number of bolometer
      INTEGER BEAM                         ! beam index in DO loop
      INTEGER BOL                          ! bolometer index in DO loop
      INTEGER CHAN                         ! channel number of bolometer
      INTEGER POS                          ! measurement index in DO loop

*  Internal References:

*  Local data:

*.

      IF (STATUS .NE. SAI__OK) RETURN

      DO BEAM = 1, N_BEAM
         DO POS = 1, N_POS
            DO BOL = 1, N_BOL

*  multiply by the flatfield if the quality of the flatfield is
*  good, otherwise set output datum quality flatfield bit to bad

               CHAN = BOL_CHAN (BOL)
               ADC = BOL_ADC (BOL)

               IF (BOL_QUALITY(CHAN,ADC) .EQ. 0) THEN

                  IF (BDATA(BOL,POS,BEAM) .NE. VAL__BADR) THEN
                     BDATA (BOL,POS,BEAM) = BDATA (BOL,POS,BEAM) *
     :                    BOL_FLAT (CHAN,ADC)

                     IF (VARIANCE(BOL,POS,BEAM) .NE. VAL__BADR) THEN
                        VARIANCE (BOL,POS,BEAM) =
     :                       VARIANCE (BOL,POS,BEAM) *
     :                       BOL_FLAT (CHAN,ADC)**2
                     END IF
                  ELSE
*     No point keeping a variance without a data point
                     VARIANCE(BOL,POS,BEAM) = VAL__BADR
                  END IF
               ELSE
                  QUALITY (BOL,POS,BEAM) =
     :                 SCULIB_BITON(QUALITY(BOL,POS,BEAM),1)
               END IF

	    END DO
         END DO
      END DO

      END
