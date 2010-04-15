      SUBROUTINE SCULIB_NOISE_MEAN (N_INTEGRATIONS, N_BOLS, DEMOD,
     :     CHOP_DATA, CHOP_VARIANCE, CAL_DATA, CAL_VARIANCE,
     :     QUALITY, WORKSPACE, STATUS)
*+
*  Name:
*     SCULIB_NOISE_MEAN

*  Purpose:
*     Calculate statistics of NOISE data from raw demodulated data

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     SCULIB_NOISE_MEAN ( N_INTEGRATIONS, N_BOLS, DEMOD,
*    :     CHOP_DATA, CHOP_VARIANCE, CAL_DATA, CAL_VARIANCE,
*    :     QUALITY, WORKSPACE, STATUS)

*  Description:
*     This routine calculates the mean and variance of a set of integrations
*     in a NOISE measurement.
*        If, only one integration was taken then the demodulated chop signal,
*     chop variance, calibrator signal, calibrator variance and quality will
*     be returned in CHOP_DATA, CHOP_VARIANCE, CAL_DATA, CAL_VARIANCE and
*     QUALITY as they were obtained from the transputers.
*        If more than 1 integration was obtained then:-
*
*          If, for a given bolometer, no valid data was obtained for any of the
*          integrations then the QUALITY for that bolometer will be returned
*          as 1.
*
*          If, for that bolometer, one valid integration was obtained then the
*          demodulated chop signal and variance will be returned in CHOP_DATA
*          and CHOP_VARIANCE, the calibrator signal and variance will be
*          returned in CAL_DATA and CAL_VARIANCE, and QUALITY will be set
*          to 0.
*
*          If more than one valid integration was obtained then the means
*          of the chop and calibrator signals will be returned in CHOP_DATA
*          and CAL_DATA, and the variances of the individual samples in
*          CHOP_VARIANCE and  CAL_VARIANCE, and QUALITY will be set to 0.

*  Arguments:
*     N_INTEGRATIONS                    = INTEGER (Given)
*           number of integrations taken in measurement
*     N_BOLS                            = INTEGER (Given)
*           number of bolometers measured
*     DEMOD (5, N_BOLS, N_INTEGRATIONS) = REAL (Given)
*           the demodulated data; chop, chop variance, calibrator, cal
*           variance, quality
*     CHOP_DATA (N_BOLS)                = REAL (Returned)
*           the mean of the demodulated chop signal over the integrations
*     CHOP_VARIANCE (N_BOLS)            = REAL (Returned)
*           the variance on CHOP_DATA, calculated from the dispersion
*           about the mean, or the demodulated variance if there was only
*           one integration
*     CAL_DATA (N_BOLS)                 = REAL (Returned)
*           the mean of the demodulated calibrator signal over the integrations
*     CAL_VARIANCE (N_BOLS)             = REAL (Returned)
*           the variance on CAL_DATA, calculated from the dispersion about the
*           mean, or the demodulated variance if there was only one integration
*     QUALITY (N_BOLS)                  = BYTE (Returned)
*           the quality on the returned data
*     WORKSPACE (N_BOLS)                = INTEGER (Scratch)
*           some scratch space for keeping track of the number of
*           valid observations to be included in the averaging
*           for each bolometer
*     STATUS = INTEGER (Given & Returned)
*           inherited status

*  Authors :
*     J.Lightfoot (REVAD::JFL)
*     TIMJ: Tim Jenness (JACH)
*     {enter_new_authors_here}


*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     26-MAY-1995 (JFL)
*        Original version.
*      7-JUN-1996 (JFL)
*        Modified to calculate error on sample distribution
*        rather than on mean.
*     26-JUN-1996 (JFL)
*        Modified to handle v220 data with cal_variance (JFL).
*     17-Nov-1998 (TIMJ)
*        Add inherited status and BYTE quality
*     $Log$
*     Revision 1.2  1999/08/03 19:35:16  timj
*     Add copyright message to header.
*     Convert old header style to new.
*
*     Revision 1.1  1999/01/06 19:44:22  timj
*     Initial revision
*
*     {enter_further_changes_here}


*  Bugs:
*     {note_any_bugs_here}
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      INTEGER       N_INTEGRATIONS
      INTEGER       N_BOLS
      REAL          DEMOD (5, N_BOLS, N_INTEGRATIONS)

*  Arguments Returned:
      REAL          CHOP_DATA (N_BOLS)
      REAL          CHOP_VARIANCE (N_BOLS)
      REAL          CAL_DATA (N_BOLS)
      REAL          CAL_VARIANCE (N_BOLS)
      BYTE          QUALITY (N_BOLS)

*  Scratch:
      INTEGER       WORKSPACE ( N_BOLS )

*  Status:
      INTEGER STATUS

*  External references:
*  Global variables:
*  Local Constants:
*  Local variables:
      INTEGER      BOL                 ! DO loop index
      INTEGER      I                   ! DO loop index
      INTEGER      USED                ! index of last integration coadded

*  Internal References:
*  Local data:

*.

      IF (STATUS .NE. SAI__OK) RETURN

*  initialise

      DO BOL = 1, N_BOLS
         CHOP_DATA (BOL) = 0.0
         CHOP_VARIANCE (BOL) = 0.0
         CAL_DATA (BOL) = 0.0
         CAL_VARIANCE (BOL) = 0.0
         QUALITY (BOL) = 0
         WORKSPACE ( BOL ) = 0
      END DO

*  deal with 1 integration case first

      IF (N_INTEGRATIONS .EQ. 1) THEN

         DO BOL = 1, N_BOLS
            CHOP_DATA (BOL) = DEMOD (1, BOL, 1)
            CHOP_VARIANCE (BOL) = DEMOD (2, BOL, 1)
            CAL_DATA (BOL) = DEMOD (3, BOL, 1)
            CAL_VARIANCE (BOL) = DEMOD (4, BOL, 1)

*     A Non-zero 'quality' should set QUALITY to 1
            IF (NINT(DEMOD(5, BOL, 1)) .GT. 0) QUALITY(BOL) = 1
         END DO

*  otherwise have more than one integration coming

      ELSE IF (N_INTEGRATIONS .GT. 1) THEN

*  calculate sums

         DO I = 1, N_INTEGRATIONS
            DO BOL = 1, N_BOLS

               IF (NINT(DEMOD(5,BOL,I)) .EQ. 0) THEN
                  CHOP_DATA (BOL) = CHOP_DATA (BOL) + DEMOD (1,BOL,I)
                  CHOP_VARIANCE (BOL) = CHOP_VARIANCE (BOL) +
     :              DEMOD (1,BOL,I)**2
                  CAL_DATA (BOL) = CAL_DATA (BOL) + DEMOD (3,BOL,I)
                  CAL_VARIANCE (BOL) = CAL_VARIANCE (BOL) +
     :              DEMOD (3,BOL,I)**2
                  WORKSPACE (BOL) = WORKSPACE (BOL) + 1
                  USED = I
               END IF

            END DO

         END DO


*  calculate results

         DO BOL = 1, N_BOLS

            IF (WORKSPACE(BOL) .EQ. 0) THEN
*     If no observations selected for this bolometer the quality is bad
               QUALITY (BOL) = 1
            ELSE IF (WORKSPACE(BOL) .EQ. 1) THEN
*     If there is one good observation the quality is good
               CHOP_VARIANCE (BOL) = DEMOD(2,BOL,USED)
               CAL_VARIANCE (BOL) = DEMOD(4,BOL,USED)
               QUALITY (BOL) = 0
            ELSE
*     Since there is more than one good observation we can calculate
*     statistics
               CHOP_DATA (BOL) = CHOP_DATA (BOL) / FLOAT(WORKSPACE(BOL))
               CHOP_VARIANCE (BOL) =
     :           (CHOP_VARIANCE(BOL) - FLOAT(WORKSPACE(BOL)) *
     :           CHOP_DATA(BOL) * CHOP_DATA(BOL)) /
     :           FLOAT (WORKSPACE (BOL) - 1)
               CHOP_VARIANCE (BOL) = MAX (CHOP_VARIANCE(BOL),0.0)

               CAL_DATA (BOL) = CAL_DATA (BOL) / FLOAT(WORKSPACE(BOL))
               CAL_VARIANCE (BOL) =
     :           (CAL_VARIANCE(BOL) - FLOAT(WORKSPACE(BOL)) *
     :           CAL_DATA(BOL) * CAL_DATA(BOL)) /
     :           FLOAT (WORKSPACE (BOL) - 1)
               CAL_VARIANCE (BOL) = MAX (CAL_VARIANCE(BOL),0.0)

               QUALITY (BOL) = 0
            END IF


         END DO

      END IF

      END
