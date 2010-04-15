      SUBROUTINE SCULIB_UNPACK_JIGGLE (N_JIGGLES, N_BOLS, DEMOD,
     :  J_START, JIGGLE_COUNT, I_JIGGLE, J_JIGGLE, IDIM, JDIM,
     :  MAP_DATA, MAP_VARIANCE, MAP_QUALITY, J_END, STATUS)
*+
*  Name:
*     SCULIB_UNPACK_JIGGLE

*  Purpose:
*     unpack demodulated data onto 2-d map

*  Description:
*     This routine unpacks the demodulated data from a switch onto a
*     rectangular 2-d map.
*
*        If status is good on entry the data and variance of the output map
*     will be initialised to `bad' values, the quality to 1. If there are
*     any data to unpack the routine will then attempt to do so. The method
*     used depends on whether the switch covered part/all of the entire jiggle
*     pattern or contains data for several repeats of the jiggle pattern.
*
*        In the first case, the routine will check that the indices of the
*     section of jiggle pattern covered by this switch lie within the bounds
*     of the full jiggle pattern. If not, an error will be reported and bad
*     status returned. Otherwise, the datablock will be unpacked into the
*     map as specified by the I_JIGGLE, J_JIGGLE arrays. Each map point
*     just has its data, variance and quality copied from the demodulated
*     data.
*
*        When the data covers several repeats of the jiggle pattern, the
*     routine will check that the jiggle index of the first point in the
*     datablock is 1 and that the number of data points is an integer multiple
*     of the size of the jiggle. If not, an error will be reported and bad
*     status returned. Otherwise, the datablock will be unpacked into the
*     map as specified by I_JIGGLE, J_JIGGLE. The unpacking differs from
*     the first case in that demodulated data with bad quality will be
*     ignored. Points in the unpacked map will be set to the average of the
*     values contributed by the separate jiggles, variances will be calculated
*     from the dispersion of the values about the mean if there was more than
*     1, or set to the demodulated variance otherwise.

*  Invocation:
*     CALL SCULIB_UNPACK_JIGGLE (N_JIGGLES, N_BOLS, DEMOD,
*    :  J_START, JIGGLE_COUNT, I_JIGGLE, J_JIGGLE, IDIM, JDIM,
*    :  MAP_DATA, MAP_VARIANCE, MAP_QUALITY, J_END, STATUS)

*  Arguments:
*     N_JIGGLES                         = INTEGER (Given)
*           number of jiggles in datablock
*     N_BOLS                            = INTEGER (Given)
*           number of bolometers measured
*     DEMOD (4, N_BOLS, N_JIGGLES)      = REAL (Given)
*           demodulated data
*     J_START                           = INTEGER (Given)
*           index of first jiggle in datablock in the overall jiggle pattern
*     JIGGLE_COUNT                      = INTEGER (Given)
*           number of jiggles in jiggle pattern
*     I_JIGGLE (JIGGLE_COUNT)           = INTEGER (Given)
*           i index in output map of each jiggle position in pattern
*     J_JIGGLE (JIGGLE_COUNT)           = INTEGER (Given)
*           j index in output map of each jiggle position in pattern
*     IDIM                              = INTEGER (Given)
*           i dimension of output map
*     JDIM                              = INTEGER (Given)
*           j dimension of output map
*     MAP_DATA (IDIM, JDIM, N_BOLS)     = REAL (Returned)
*           output map data
*     MAP_VARIANCE (IDIM, JDIM, N_BOLS) = REAL (Returned)
*           output map variance
*     MAP_QUALITY (IDIM, JDIM, N_BOLS)  = INTEGER (Returned)
*           output map quality
*     J_END                             = INTEGER (Returned)
*           index of last jiggle in datablock in the overall jiggle pattern
*     STATUS                            = INTEGER (Given and returned)
*           global status

*  Authors:
*     J.Lightfoot (REVAD::JFL)

*  Copyright:
*     Copyright (C) 1993-1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  Method:

*  Deficiencies:

*  Bugs:


*  History:
*     $Id$
*     21-MAY-1993: Original version.
*     18-NOV-1994: Checked (JFL).
*    endhistory

*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'                 ! for VAL__BADR

*  Arguments Given:
      INTEGER N_JIGGLES
      INTEGER N_BOLS
      REAL DEMOD (4, N_BOLS, N_JIGGLES)
      INTEGER J_START
      INTEGER JIGGLE_COUNT
      INTEGER I_JIGGLE (JIGGLE_COUNT)
      INTEGER J_JIGGLE (JIGGLE_COUNT)
      INTEGER IDIM
      INTEGER JDIM

*  Arguments Given & Returned:

*  Arguments Returned:
      REAL MAP_DATA (IDIM, JDIM, N_BOLS)
      REAL MAP_VARIANCE (IDIM, JDIM, N_BOLS)
      INTEGER MAP_QUALITY (IDIM, JDIM, N_BOLS)
      INTEGER J_END

*  Status:
      INTEGER STATUS

*  External references:

*  Global variables:

*  Local Constants:

*  Local variables:
      INTEGER BOL                       ! DO loop variable
      INTEGER I                         ! DO loop variable
      INTEGER IM                        ! array index
      INTEGER J                         ! DO loop variable
      INTEGER JIGGLE                    ! jiggle index
      INTEGER JM                        ! array index

*  Internal References:

*  Local data:

*.

      IF (STATUS .NE. SAI__OK) RETURN

*  initialise output array

      DO BOL = 1, N_BOLS
         DO J = 1, JDIM
            DO I = 1, IDIM
               MAP_DATA (I,J,BOL) = VAL__BADR
               MAP_VARIANCE (I,J,BOL) = VAL__BADR
               MAP_QUALITY (I,J,BOL) = 1
            END DO
         END DO
      END DO

      IF (N_JIGGLES .GT. 0) THEN
         IF (N_JIGGLES .LE. JIGGLE_COUNT) THEN

*  the datablock does not contain numbers for several repeats of the jiggle
*  pattern. Check jiggle indices are OK, if so unpack data

            IF ((J_START .LT. 1) .OR.
     :          (J_START+N_JIGGLES-1 .GT. JIGGLE_COUNT)) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP (' ', 'SCULIB_UNPACK_JIGGLE: bad '//
     :           'jiggle number', STATUS)
            ELSE

               DO I = 1, N_JIGGLES
                  JIGGLE = J_START + I - 1
                  IM = I_JIGGLE (JIGGLE)
                  JM = J_JIGGLE (JIGGLE)

                  DO BOL = 1, N_BOLS
                     MAP_DATA (IM,JM,BOL) = DEMOD (1,BOL,I)
                     MAP_VARIANCE (IM,JM,BOL) = DEMOD (2,BOL,I)
                     MAP_QUALITY (IM,JM,BOL) = NINT (DEMOD(4,BOL,I))
                  END DO
               END DO

*  index of last datapoint in overall pattern

               J_END = J_START + N_JIGGLES - 1

            END IF

         ELSE

*  datablock does contain several repeats of jiggle pattern. J_START should
*  be 1 and there should be an integer number of repeats

            IF (J_START .NE. 1) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP (' ', 'SCULIB_UNPACK_JIGGLE: J_START '//
     :           'not 1 for data from repeated jiggle pattern', STATUS)
            END IF

            IF (MOD(N_JIGGLES,JIGGLE_COUNT) .NE. 0) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP (' ', 'SCULIB_UNPACK_JIGGLE: data '//
     :           'for non-integral number of repeats of jiggle pattern',
     :           STATUS)
            END IF

            IF (STATUS .EQ. SAI__OK) THEN
               DO I = 1, N_JIGGLES
                  JIGGLE = MOD(I,JIGGLE_COUNT)
                  IF (JIGGLE .EQ. 0) THEN
                     JIGGLE = JIGGLE_COUNT
                  END IF

                  IM = I_JIGGLE (JIGGLE)
                  JM = J_JIGGLE (JIGGLE)

                  DO BOL = 1, N_BOLS
                     IF (NINT(DEMOD(4,BOL,I)) .EQ. 0) THEN

                        IF (MAP_DATA(IM,JM,BOL) .EQ. VAL__BADR) THEN

*  an unset map point, just set it

                           MAP_DATA (IM,JM,BOL) = DEMOD (1,BOL,I)
                           MAP_VARIANCE (IM,JM,BOL) = DEMOD (2,BOL,I)
                           MAP_QUALITY (IM,JM,BOL) = 1

                        ELSE

*  map point already set, coadd subsequent jiggles using quality to
*  keep count of number of coadds. For first number coadded copy
*  the demodulated data variance into the map variance, for subsequent
*  numbers store the sum of the squares of the data values in the variance
*  array so that the variance can be calculated from the distribution
*  about the mean at the end of the routine

                           IF (MAP_QUALITY(IM,JM,BOL) .EQ. 1) THEN
                              MAP_VARIANCE (IM,JM,BOL) =
     :                          MAP_DATA (IM,JM,BOL) **2 +
     :                          DEMOD (1,BOL,I) **2
                           ELSE
                              MAP_VARIANCE (IM,JM,BOL) =
     :                          MAP_VARIANCE (IM,JM,BOL) +
     :                          DEMOD (1,BOL,I) **2
                           END IF
                           MAP_DATA (IM,JM,BOL) = MAP_DATA (IM,JM,BOL) +
     :                       DEMOD (1,BOL,I)
                           MAP_QUALITY (IM,JM,BOL) =
     :                       MAP_QUALITY (IM,JM,BOL) + 1

                        END IF
                     END IF

                  END DO
               END DO

*  now go through map and calculate mean and variance on mean

               DO BOL = 1, N_BOLS
                  DO J = 1, JDIM
                     DO I = 1, IDIM
                        IF (MAP_DATA(I,J,BOL) .NE. VAL__BADR) THEN
                           MAP_DATA (I,J,BOL) = MAP_DATA (I,J,BOL) /
     :                       MAP_QUALITY (I,J,BOL)

                           IF (MAP_QUALITY(I,J,BOL) .GT. 1) THEN
                              MAP_VARIANCE (I,J,BOL) =
     :                          (MAP_VARIANCE(I,J,BOL) -
     :                          MAP_QUALITY(I,J,BOL) *
     :                          MAP_DATA(I,J,BOL)**2) /
     :                          (MAP_QUALITY(I,J,BOL) *
     :                          (MAP_QUALITY(I,J,BOL)-1))
                           END IF

                           MAP_QUALITY (I,J,BOL) = 0
                        END IF
                     END DO
                  END DO
               END DO

*  index of last datapoint in overall pattern

               J_END = JIGGLE_COUNT

            END IF
         END IF
      END IF

      END
