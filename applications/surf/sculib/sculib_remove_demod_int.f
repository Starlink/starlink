      SUBROUTINE SCULIB_REMOVE_DEMOD_INT (REMOVE_TYPE, NUM_CHAN,
     :  NUM_ADC, BOL_TYPE, N_BOLS, BOL_CHAN, BOL_ADC, J_COUNT,
     :  INT_DATA, INT_QUALITY, COADD_DATA, COADD_VARIANCE,
     :  COADD_QUALITY, COADD_NUMBER, STATUS)
*+
*  Name:
*     SCULIB_REMOVE_DEMOD_INT

*  Purpose:
*     remove demodulated data for one sub-instrument
*     in an integration from a coadded result

*  Description:
*     This routine removes the demodulated data for a specified sub-instrument
*     and integration from the coadded measurement.
*
*       After checking status on entry the routine cycles through the
*     bolometers for which data has been taken. If the bolometer belongs
*     to the sub-instrument whose data is to be removed, the routine will
*     then cycle through the jiggle pattern removing the integration data
*     from the coadd. If this reduces the number of coadded integrations
*     to 0 then all the coadd numbers are set to 0 apart from quality,
*     which is set to 1. If the number of coadded integrations is reduced
*     to 1 then the coadd variance can no longer be estimated from the
*     spread of data about the mean and will be set to 0.

*  Invocation:
*     CALL SCULIB_REMOVE_DEMOD_INT (REMOVE_TYPE, NUM_CHAN,
*    :  NUM_ADC, BOL_TYPE, N_BOLS, BOL_CHAN, BOL_ADC, J_COUNT,
*    :  INT_DATA, INT_QUALITY, COADD_DATA, COADD_VARIANCE,
*    :  COADD_QUALITY, COADD_NUMBER, STATUS)

*  Arguments:
*     REMOVE_TYPE                   = CHARACTER*(*) (Given)
*           the type of the sub-instrument whose data is to be removed
*     NUM_CHAN                      = INTEGER (Given)
*           the number of channels per ADC in SCUBA
*     NUM_ADC                       = INTEGER (Given)
*           the number of ADCs in SCUBA
*     BOL_TYPE (NUM_CHAN, NUM_ADC)  = CHARACTER*(*) (Given)
*           the types of the bolometers in SCUBA
*     N_BOLS                        = INTEGER (Given)
*           the number of bolometers being measured
*     BOL_CHAN (N_BOLS)             = INTEGER (Given)
*           the channel numbers of the bolometers being measured
*     BOL_ADC (N_BOLS)              = INTEGER (Given)
*           the ADC numbers of the bolometers being measured
*     J_COUNT                       = INTEGER (Given)
*           the number of jiggle positions in the pattern
*     INT_DATA (N_BOLS, J_COUNT)    = REAL (Given)
*           the integration data
*     INT_QUALITY (N_BOLS, J_COUNT) = INTEGER (Given)
*           quality on the integration data
*     COADD_DATA (N_BOLS, J_COUNT)  = REAL (Given and returned)
*           the mean of the coadded data
*     COADD_VARIANCE (N_BOLS, J_COUNT)
*                                   = REAL (Given and returned)
*           the variance on the mean of the coadded data
*     COADD_QUALITY (N_BOLS, J_COUNT)
*                                   = INTEGER (Given and returned)
*           the quality on the mean of the coadded data
*     COADD_NUMBER (N_BOLS, J_COUNT)= INTEGER (Given and returned)
*           the number of data coadded
*     STATUS                        = INTEGER (Given and returned)
*           the global status


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
*     19-AUG-1993: Original version.
*    endhistory

*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      CHARACTER*(*) REMOVE_TYPE
      INTEGER NUM_CHAN, NUM_ADC
      CHARACTER*(*) BOL_TYPE (NUM_CHAN, NUM_ADC)
      INTEGER N_BOLS
      INTEGER BOL_CHAN (N_BOLS), BOL_ADC (N_BOLS)
      INTEGER J_COUNT
      REAL INT_DATA (N_BOLS, J_COUNT)
      INTEGER INT_QUALITY (N_BOLS, J_COUNT)

*  Arguments Given & Returned:
      REAL COADD_DATA (N_BOLS, J_COUNT)
      REAL COADD_VARIANCE (N_BOLS, J_COUNT)
      INTEGER COADD_QUALITY (N_BOLS, J_COUNT)
      INTEGER COADD_NUMBER (N_BOLS, J_COUNT)

*  Arguments Returned:

*  Status:
      INTEGER STATUS

*  External references:
      INTEGER CHR_LEN                  ! CHR string-length function

*  Global variables:

*  Local Constants:

*  Local variables:
      INTEGER BOL, J                   ! DO loop variables
      REAL SUM                         ! sum of coadded data
      REAL SUMSQ                       ! sum of coadded data squared
      CHARACTER*20 TYPE                ! type of bolometer

*  Internal References:

*  Local data:

*.

      IF (STATUS .NE. SAI__OK) RETURN

      CALL CHR_UCASE (REMOVE_TYPE)

*  cycle through bolometers looking for those of desired type

      DO BOL = 1, N_BOLS

         TYPE = BOL_TYPE (BOL_CHAN(BOL), BOL_ADC(BOL))
         CALL CHR_UCASE (TYPE)

         IF (INDEX(TYPE, REMOVE_TYPE(:CHR_LEN(REMOVE_TYPE))) .NE. 0)
     :     THEN

*  remove the integration from the coadd

            DO J = 1, J_COUNT

               IF (INT_QUALITY(BOL,J) .EQ. 0) THEN

*  recover the sum of the data points and the sum of them squared

                  IF (COADD_NUMBER (BOL,J) .EQ. 1) THEN
                     SUM = COADD_DATA (BOL,J)
                     SUMSQ = COADD_DATA (BOL,J) **2
                  ELSE IF (COADD_NUMBER (BOL,J) .GT. 1) THEN
                     SUM = COADD_DATA (BOL,J) *
     :                 COADD_NUMBER (BOL,J)
                     SUMSQ = COADD_NUMBER (BOL,J) *
     :                 COADD_DATA (BOL,J)**2 +
     :                 COADD_NUMBER (BOL,J) *
     :                 (COADD_NUMBER (BOL,J) - 1) *
     :                 COADD_VARIANCE (BOL,J)
                  ELSE
                     COADD_NUMBER (BOL,J) = 0
                     SUM = 0.0
                     SUMSQ = 0.0
                  END IF

*  subtract the integration

                  SUM = SUM - INT_DATA (BOL,J)
                  SUMSQ = SUMSQ - INT_DATA (BOL,J) **2
                  COADD_NUMBER (BOL,J) = COADD_NUMBER (BOL,J) - 1

*  calculate new average and variance

                  IF (COADD_NUMBER (BOL,J) .LE. 0) THEN
                     COADD_NUMBER (BOL,J) = 0
                     COADD_QUALITY (BOL,J) = 1
                     COADD_DATA (BOL,J) = 0.0
                     COADD_VARIANCE (BOL,J) = 0.0
                  ELSE IF (COADD_NUMBER (BOL,J) .EQ. 1) THEN
                     COADD_DATA (BOL,J) = SUM
                     COADD_QUALITY (BOL,J) = 0
                     COADD_VARIANCE (BOL,J) = 0.0
                  ELSE
                     COADD_DATA (BOL,J) = SUM / COADD_NUMBER (BOL,J)
                     COADD_VARIANCE (BOL,J) =
     :                 (SUMSQ - COADD_NUMBER(BOL,J) *
     :                 COADD_DATA(BOL,J) **2) /
     :                 (COADD_NUMBER (BOL,J) *
     :                 (COADD_NUMBER (BOL,J) - 1))
                     COADD_QUALITY (BOL,J) = 0
                  END IF

               END IF

            END DO

         END IF

      END DO

      END
