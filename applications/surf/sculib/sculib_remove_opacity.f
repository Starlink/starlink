      SUBROUTINE SCULIB_REMOVE_OPACITY (N_BOLS, N_POS,
     :  BOL_SELECT_CHAN, BOL_SELECT_ADC, NUM_CHAN, NUM_ADC, BOL_TYPE,
     :  N_SUB, SUB_INSTRUMENT, TAUZ, AIRMASS, EXP_DATA, EXP_VARIANCE,
     :  EXP_QUALITY, STATUS)
*+
*  Name:
*     SCULIB_REMOVE_OPACITY

*  Purpose:
*     remove sky opacity from demodulated data

*  Description:
*     This routine corrects demodulated data for the effect of sky opacity.
*     All data is corrected assuming the same airmass of observation, but
*     the zenith sky opacity used will depend which SCUBA sub-instrument each
*     bolometer belongs to.
*
*       After checking status on entry, the routine will loop through the
*     bolometers measured. For each bolometer it will then ascertain the
*     appropriate zenith sky opacity from the parent sub-instrument and
*     wavelength of observation. The total sky optical depth will then
*     be calculated and, if this is in the range 0 to 20, the flux correction
*     factor derived. The routine will then loop through the positions
*     measured by this bolometer, correcting the fluxes and variances.
*     If the bolometer data quality was bad, or if sky optical depth lay
*     outside the above range, the data quality will be set bad and no
*     correction applied.

*  Invocation:
*     CALL SCULIB_REMOVE_OPACITY (N_BOLS, N_POS,
*    :  BOL_SELECT_CHAN, BOL_SELECT_ADC, NUM_CHAN, NUM_ADC, BOL_TYPE,
*    :  N_SUB, SUB_INSTRUMENT, TAUZ, AIRMASS, EXP_DATA, EXP_VARIANCE,
*    :  EXP_QUALITY, STATUS)

*  Arguments:
*     N_BOLS                        = INTEGER (Given)
*           the number of bolometers measured
*     N_POS                         = INTEGER (Given)
*           the number of positions they were measured at
*     BOL_SELECT_CHAN (N_BOLS)      = INTEGER (Given)
*           the channel numbers of the measured bolometers
*     BOL_SELECT_ADC (N_BOLS)       = INTEGER (Given)
*           the ADC numbers of the measured bolometers
*     NUM_CHAN                      = INTEGER (Given)
*           the number of channels per ADC
*     NUM_ADC                       = INTEGER (Given)
*           the number of ADCs
*     BOL_TYPE (NUM_CHAN, NUM_ADC)  = CHARACTER*(*) (Given)
*           the type of the bolometer on each channel
*     N_SUB                         = INTEGER (Given)
*           the number of SCUBA sub-instruments being measured
*     SUB_INSTRUMENT (N_SUB)        = CHARACTER*(*)
*           the names of the sub-instruments being measured
*     TAUZ (NSUB)                   = REAL (Given)
*           the zenith sky opacity for each sub-instrument
*     AIRMASS                       = REAL (Given)
*           the airmass at which the observations were made
*     EXP_DATA (N_BOLS, N_POS)      = REAL (Given and returned)
*           the bolometer data
*     EXP_VARIANCE (N_BOLS, N_POS)  = REAL (Given and returned)
*           the variance on the data
*     EXP_QUALITY (N_BOLS, N_POS)   = REAL (Given and returned)
*           the quality on the data
*     STATUS                        = INTEGER (Given and returned)
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
*     6-AUG-1993: Original version.
*    endhistory

*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'                  ! for VAL__BADR

*  Arguments Given:
      INTEGER N_BOLS
      INTEGER N_POS
      INTEGER BOL_SELECT_CHAN (N_BOLS)
      INTEGER BOL_SELECT_ADC (N_BOLS)
      INTEGER NUM_CHAN
      INTEGER NUM_ADC
      CHARACTER*(*) BOL_TYPE (NUM_CHAN, NUM_ADC)
      INTEGER N_SUB
      CHARACTER*(*) SUB_INSTRUMENT (N_SUB)
      REAL TAUZ (N_SUB)
      REAL AIRMASS

*  Arguments Given & Returned:
      REAL EXP_DATA (N_BOLS, N_POS)
      REAL EXP_VARIANCE (N_BOLS, N_POS)
      INTEGER EXP_QUALITY (N_BOLS, N_POS)

*  Arguments Returned:

*  Status:
      INTEGER STATUS

*  External references:
      INTEGER CHR_LEN                    ! CHR string-length function

*  Global variables:

*  Local Constants:

*  Local variables:
      INTEGER      BOL                   ! bolometer number in DO loop
      REAL         CORRECTION            ! correction to be applied
      INTEGER      POS                   ! measurement index in DO loop
      REAL         TAU                   ! optical depth
      INTEGER      SUB                   ! sub-instrument in DO loop
      CHARACTER*80 TYPE                  ! bolometer type

*  Internal References:

*  Local data:

*.

      IF (STATUS .NE. SAI__OK) RETURN

      DO BOL = 1, N_BOLS

*  get zenith opacity appropriate to type of bolometer, 0 if not
*  one of recognised types

         TYPE = BOL_TYPE (BOL_SELECT_CHAN(BOL), BOL_SELECT_ADC(BOL))
         TAU = 0.0
         DO SUB = 1, N_SUB
            IF (INDEX(TYPE,
     :        SUB_INSTRUMENT(SUB)(:CHR_LEN(SUB_INSTRUMENT(SUB))))
     :        .NE. 0) THEN
               TAU = TAUZ (SUB)
            END IF
         END DO

*  calculate correction

         TAU = TAU * AIRMASS
         IF ((TAU .GE. 0) .AND. (TAU .LT. 20.0)) THEN
            CORRECTION = EXP (TAU)
         ELSE
            CORRECTION = VAL__BADR
         END IF

*  and apply it

         DO POS = 1, N_POS

            IF (EXP_QUALITY (BOL,POS) .EQ. 0) THEN

               IF (CORRECTION .NE. VAL__BADR) THEN
                  EXP_DATA (BOL, POS) = EXP_DATA (BOL, POS) *
     :              CORRECTION
                  EXP_VARIANCE (BOL, POS) = EXP_VARIANCE (BOL, POS) *
     :              CORRECTION**2
               ELSE
                  EXP_QUALITY (BOL, POS) = 1
               END IF

            END IF

         END DO
      END DO

      END
