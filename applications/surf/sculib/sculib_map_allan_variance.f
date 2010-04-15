      SUBROUTINE SCULIB_MAP_ALLAN_VARIANCE (DEMOD, N_BOLS, N_SAMPLES,
     :  N_ALLAN_BOL, ALLAN_BOL, KMAX, SUM, N_SUM, ARTIFICIAL,
     :  N_ARTIFICIAL, ALLAN_VARIANCE, ALLAN_QUALITY, STATUS)
*+
*  Name:
*     SCULIB_MAP_ALLAN_VARIANCE

*  Purpose:
*     incorporate latest set of MAP demodulated data
*     into Allan variance

*  Description:
*     This routine incorporates a data slice into a run of data and
*     updates the Allan variance of the run.
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
*     CALL SCULIB_MAP_ALLAN_VARIANCE (DEMOD, N_BOLS, N_SAMPLES,
*    :  N_ALLAN_BOL, ALLAN_BOL, KMAX, SUM, N_SUM, ARTIFICIAL,
*    :  N_ARTIFICIAL, ALLAN_VARIANCE, ALLAN_QUALITY, STATUS)

*  Arguments:
*     DEMOD (4, N_BOLS, N_SAMPLES)    = REAL (Given)
*           the demodulated data (data,-,-,quality)
*     N_BOLS                          = INTEGER (Given)
*           the number of bolometers taking data
*     N_SAMPLES                       = INTEGER (Given)
*           the number of samples taken for each bolometer
*     N_ALLAN_BOL                     = INTEGER (Given)
*           the number of bolometers whose data is to be averaged together
*           at each sample position
*     ALLAN_BOL (N_ALLAN_BOL)         = INTEGER (Given)
*           the positions in the demodulated data array of the bolometers
*           whose measurements are to be used for the calculation of Allan
*           variance
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

*  Method:
*     If status on entry is good the routine will:-
*
*     loop through the data samples to be incorporated -
*
*        loop through the bolometers whose data is to be averaged into
*        this sample -
*
*           if the bolometer was measured -
*              if the measurement had good quality -
*
*                 add the measurement to the average for this sample
*
*              end if
*           end if
*
*        end of loop
*
*        if any bolometers had good data for this sample -
*
*           calculate the sample average
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
*                              .--------------------------------
*                                       2 * 2
*
*                 if there are more than 2 artificial samples
*
*                    the sum of the squares of the differences between the
*                    previous samples is recovered from the current value of the
*                    Allan variance
*
*                    the square of the difference between the current artificial
*                    sample and the previous one is added to the sum
*
*                    the Allan variance is re-calculated
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


*  Bugs:

*  Authors:
*     J.Lightfoot (REVAD::JFL)

*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.


*  History:
*     $Id$
*     21-JUL-1994: Original version.
*    endhistory

*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      INTEGER N_BOLS
      INTEGER N_SAMPLES
      REAL DEMOD (4, N_BOLS, N_SAMPLES)
      INTEGER N_ALLAN_BOL
      INTEGER ALLAN_BOL (N_ALLAN_BOL)
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

*  Global variables:

*  Local Constants:

*  Local variables:
      REAL    ALLAN_SUM
      INTEGER A_BOL
      INTEGER K
      REAL    NEW_ARTIFICIAL
      INTEGER SAMP
      REAL    SAMPLE
      INTEGER SAMPLE_N

*  Internal References:

*  Local data:

*.

      IF (STATUS .NE. SAI__OK) RETURN

*  loop through new samples

      DO SAMP = 1, N_SAMPLES

*  loop through bolometers contributing to the Allan variance, adding up
*  their values to give an average `sample'

         SAMPLE = 0.0
         SAMPLE_N = 0

         IF (N_ALLAN_BOL .GT. 0) THEN

            DO A_BOL = 1, N_ALLAN_BOL

*  if that bolometer was measured and had good quality

               IF ((ALLAN_BOL(A_BOL) .GE. 0)      .AND.
     :             (ALLAN_BOL(A_BOL) .LE. N_BOLS)) THEN

                  IF (NINT(DEMOD(4,ALLAN_BOL(A_BOL),SAMP)) .EQ. 0) THEN

*  add into sample average

                     SAMPLE = SAMPLE + DEMOD (1,ALLAN_BOL(A_BOL),SAMP)
                     SAMPLE_N = SAMPLE_N + 1

                  END IF
               END IF

            END DO

         END IF

*  if we have a valid sample

         IF (SAMPLE_N .NE. 0) THEN

            SAMPLE = SAMPLE / REAL (SAMPLE_N)

*  loop through Allan variances

            DO K = 1, KMAX

*  add sample to current sum for artificial sample at this K

               SUM (K) = SUM (K) + SAMPLE
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
*  Allan variance sum, then add in the latest contribution and calculate
*  the updated version

                     ALLAN_SUM = 2.0 * REAL (N_ARTIFICIAL(K) - 1) *
     :                 ALLAN_VARIANCE (K)
                     ALLAN_SUM = ALLAN_SUM +
     :                 (NEW_ARTIFICIAL - ARTIFICIAL(K)) **2
                     ALLAN_VARIANCE (K) = ALLAN_SUM /
     :                 (2.0 * REAL (N_ARTIFICIAL(K)))

                  END IF

*  store the current artificial sample and rest variables

                  ARTIFICIAL (K) = NEW_ARTIFICIAL
                  SUM (K) = 0.0
                  N_SUM (K) = 0

               END IF

            END DO

         END IF

      END DO

      END
