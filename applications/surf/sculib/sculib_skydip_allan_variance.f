      SUBROUTINE SCULIB_SKYDIP_ALLAN_VARIANCE (SUB_INSTRUMENT,
     :  NUM_CHAN, NUM_ADC, BOL_TYPE, N_BOLS, BOL_CHAN, BOL_ADC,
     :  N_SAMPLES, SAMPLE, SAMPLE_QUALITY, KMAX, SUM, N_SUM,
     :  ARTIFICIAL, N_ARTIFICIAL, ALLAN_VARIANCE, ALLAN_QUALITY,
     :  STATUS)
*+
*  Name:
*     SCULIB_SKYDIP_ALLAN_VARIANCE

*  Purpose:
*     incorporate latest set of SKYDIP data
*     samples into Allan variance

*  Description:
*     This routine incorporates a data slice into a run of data and updates
*     the Allan variance of the run.
*
*        The Allan variance is calculated for a range of simulated integration
*     times for which artificial samples are calculated from the input data.
*     The Allan variance for a particular integration time is calculated from:-
*
*        variance =     sum      ( (sample (i) - sample (i+1)) ** 2)
*                   i = 1 to n-1 ( ---------------------------------
*                                (            2 * n
*
*     where n is the number of artificial samples available.
*


*  Invocation:
*     CALL SCULIB_SKYDIP_ALLAN_VARIANCE (SUB_INSTRUMENT, NUM_CHAN,
*    :  NUM_ADC, BOL_TYPE, N_BOLS, BOL_CHAN, BOL_ADC, N_SAMPLES,
*    :  SAMPLE, SAMPLE_QUALITY, KMAX, SUM, N_SUM, ARTIFICIAL,
*    :  N_ARTIFICIAL, ALLAN_VARIANCE, ALLAN_QUALITY, STATUS)

*  Arguments:
*     SUB_INSTRUMENT                  = CHARACTER*(*) (Given)
*           the name of the sub-instrument for which the Allan variance
*           is to be calculated
*     NUM_CHAN                        = INTEGER (Given)
*           number of channels per A/D card
*     NUM_ADC                         = INTEGER (Given)
*           number of A/D cards
*     BOL_TYPE (NUM_CHAN, NUM_ADC)    = CHARACTER*(*) (Given)
*           the types of the bolometers
*     N_BOLS                          = INTEGER (Given)
*           the number of bolometers making measurements
*     BOL_CHAN (N_BOLS)               = INTEGER (Given)
*           channel numbers of selected bolometers
*     BOL_ADC (N_BOLS)                = INTEGER (Given)
*           ADC numbers of selected bolometers
*     N_SAMPLES                       = INTEGER (Given)
*           the number of samples
*     SAMPLE (N_BOLS, N_SAMPLES)      = REAL (Given)
*           the samples
*     SAMPLE_QUALITY (N_BOLS, N_SAMPLES) = INTEGER (Given)
*           quality on the samples
*     KMAX                            = INTEGER (Given)
*           the size of the Allan variance array
*     SUM (KMAX)                      = REAL (Given and returned)
*           the accumulator for current artificial sample of length K real
*           samples
*     N_SUM (KMAX)                    = INTEGER (Given and returned)
*           the number of samples currently in the artificial sample
*           accumulator
*     ARTIFICIAL (KMAX)               = REAL (Given and returned)
*           the last artificial sample of length K samples that was
*           calculated
*     N_ARTIFICIAL (KMAX)             = INTEGER (Given and returned)
*           the number of artificial samples of length K samples that
*           have been calculated
*     ALLAN_VARIANCE (KMAX)           = REAL (Given and returned)
*           the Allan variance
*     ALLAN_QUALITY (KMAX)            = INTEGER (Returned)
*           quality on the Allan variance
*     STATUS                          = INTEGER (Given and returned)
*           global status


*  Authors:
*     J.Lightfoot (REVAD::JFL)

*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  Method:
*     If status on entry is good the routine will:-
*
*     loop through the data samples to be incorporated -
*
*        loop through the bolometers for which data is available and calculate
*        an average value for the sample from those bolometers belonging to
*        the specified sub-instrument
*
*        if the quality of the average sample is good -
*
*           loop through the Allan variances -
*
*              add the sample to the buffer used to calculate the latest
*              artificial sample at the simulated integration time of this
*              variance
*
*              if all the data has been obtained for the latest artificial
*              sample -
*
*                 calculate the artificial sample
*
*                 if there is only one artificial sample for this simulated
*                 integration time we can't calculate the Allan variance, so
*                 set its quality to bad
*
*                 if there are 2 artifical samples then the Allan variance
*                 can be calculated from -
*
*                    variance = (artifical_2 - artifical_1) ** 2
*            .                  --------------------------------
*                                       2 * 2
*
*                 if there are more than 2 artificial samples
*
*                    - the sum of the squares of the differences between the
*                    previous samples is recovered from the current value of the
*                    Allan variance
*                    - the square of the difference between the current artificial
*                    sample and the previous one is added to the sum
*                    - the Allan variance is re-calculated
*
*                 end if
*
*                 the current artifical sample is stored to be used as the
*                 `previous' sample next time round
*
*                 the variables used to calculate the artificial samples at
*                 this simulated integration time are reset
*
*              end if
*
*           end of loop through Allan variances
*
*        end if
*
*     end of loop through samples in this dataslice
*

*  Bugs:



*  History:
*     $Id$
*     10-SEP-1993: Original version.
*      4-JAN-1995: Modified to handle more than 1 bolometer per sub-instrument,
*                  name changed to SCULIB_SKYDIP_ALLAN_VARIANCE
*    endhistory

*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      CHARACTER*(*) SUB_INSTRUMENT
      INTEGER NUM_CHAN
      INTEGER NUM_ADC
      CHARACTER*(*) BOL_TYPE (NUM_CHAN,NUM_ADC)
      INTEGER N_BOLS
      INTEGER BOL_CHAN (N_BOLS)
      INTEGER BOL_ADC (N_BOLS)
      INTEGER N_SAMPLES
      REAL SAMPLE (N_BOLS, N_SAMPLES)
      INTEGER SAMPLE_QUALITY (N_BOLS, N_SAMPLES)
      INTEGER KMAX

*  Arguments Given & Returned:
      REAL SUM (KMAX)
      INTEGER N_SUM (KMAX)
      REAL ARTIFICIAL (KMAX)
      INTEGER N_ARTIFICIAL (KMAX)
      REAL ALLAN_VARIANCE (KMAX)

*  Arguments Returned:
      INTEGER ALLAN_QUALITY (KMAX)

*  Status:
      INTEGER STATUS

*  External references:
      INTEGER CHR_LEN                     ! CHR used stringlength function

*  Global variables:

*  Local Constants:

*  Local variables:
      REAL         ALLAN_SUM              ! sum of artificial samples
      INTEGER      BOL                    ! bolometer number in DO loop
      INTEGER      BOL_BELONG (128)       ! the bolometer numbers of those that
                                          ! belong to the specified sub-
                                          ! instrument
      INTEGER      I                      ! DO loop index
      INTEGER      K                      ! Allan variance index in DO loop
      INTEGER      N_BOL_BELONG           ! the number of bolometers that belong
                                          ! to the specified sub-instrument
      INTEGER      N_SAMPLE_AVE           ! number of bolometers contributing
                                          ! to SAMPLE_AVE
      REAL         NEW_ARTIFICIAL         ! the value of an artificial sample
      INTEGER      SAMP                   ! sample number in DO loop
      REAL         SAMPLE_AVE             ! average sample for a sub-instrument,
                                          ! calculated from the bolometers that
                                          ! belong to it

*  Internal References:

*  Local data:

*.

      IF (STATUS .NE. SAI__OK) RETURN

*  find which bolometers belong to this sub-instrument

      N_BOL_BELONG = 0

      DO BOL = 1, N_BOLS
         IF (INDEX(BOL_TYPE(BOL_CHAN(BOL),BOL_ADC(BOL)),
     :     SUB_INSTRUMENT(:CHR_LEN(SUB_INSTRUMENT))) .NE. 0) THEN
            N_BOL_BELONG = N_BOL_BELONG + 1
            BOL_BELONG (N_BOL_BELONG) = BOL
         END IF
      END DO

*  loop through new samples

      DO SAMP = 1, N_SAMPLES

*  loop through bolometers belong to the required sub-instrument
*  and coadd these samples

         SAMPLE_AVE = 0.0
         N_SAMPLE_AVE = 0

         IF (N_BOL_BELONG .GT. 0) THEN
            DO I = 1, N_BOL_BELONG
               BOL = BOL_BELONG (I)

               IF (SAMPLE_QUALITY (BOL,SAMP) .EQ. 0) THEN
                  SAMPLE_AVE = SAMPLE_AVE + SAMPLE (BOL,SAMP)
                  N_SAMPLE_AVE = N_SAMPLE_AVE + 1
               END IF
            END DO
         END IF

         IF (N_SAMPLE_AVE .GT. 0) THEN
            SAMPLE_AVE = SAMPLE_AVE / REAL (N_SAMPLE_AVE)

*  loop through Allan variances, adding sample to current sum for artificial
*  sample at this K

            DO K = 1, KMAX
               SUM (K) = SUM (K) + SAMPLE_AVE
               N_SUM (K) = N_SUM (K) + 1

               IF (N_SUM(K) .GE. K) THEN

*  the artificial sample is complete, calculate it

                  NEW_ARTIFICIAL = SUM (K) / REAL (N_SUM(K))
                  N_ARTIFICIAL (K) = N_ARTIFICIAL (K) + 1

                  IF (N_ARTIFICIAL(K) .LT. 2) THEN

*  not enough artificial samples to calculate variance

                     ALLAN_VARIANCE (K) = 0.0
                     ALLAN_QUALITY (K) = 1

                  ELSE IF (N_ARTIFICIAL(K) .EQ. 2) THEN

*  2 artificial samples of this size have been contructed, so we can calculate
*  the Allan variance directly

                     ALLAN_VARIANCE (K) =
     :                 (NEW_ARTIFICIAL - ARTIFICIAL(K)) **2 /
     :                 (2.0 * REAL (N_ARTIFICIAL(K)))
                     ALLAN_QUALITY (K) = 0

                  ELSE IF (N_ARTIFICIAL(K) .GT. 2) THEN

*  more than 2 artificial samples are available, we must first recover the
*  Allan variance sum

                     ALLAN_SUM = 2.0 * REAL (N_ARTIFICIAL(K) - 1) *
     :                 ALLAN_VARIANCE (K)

*  then add in the latest contribution

                     ALLAN_SUM = ALLAN_SUM +
     :                 (NEW_ARTIFICIAL - ARTIFICIAL(K)) **2

*  and calculate the updated version

                     ALLAN_VARIANCE (K) = ALLAN_SUM /
     :                 (2.0 * REAL (N_ARTIFICIAL(K)))
                  END IF

*  store the current artificial sample

                  ARTIFICIAL (K) = NEW_ARTIFICIAL

*  and reset for calculating next artificial sample

                  SUM (K) = 0.0
                  N_SUM (K) = 0
               END IF

            END DO

         END IF

      END DO

      END
