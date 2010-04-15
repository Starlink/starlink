      SUBROUTINE SCULIB_UNPACK_JIGGLE_SEPARATES (N_JIGGLES, N_BOLS,
     :  J_DATA, J_VARIANCE, J_QUALITY, J_START, JIGGLE_COUNT,
     :  I_JIGGLE, J_JIGGLE, IDIM, JDIM, MAP_DATA, MAP_VARIANCE,
     :  MAP_QUALITY, MAP_NUMPTS, J_END, BADBIT, STATUS)
*+
*  Name:
*     SCULIB_UNPACK_JIGGLE_SEPARATES

*  Purpose:
*     unpack jiggle data arrays onto 2-d map

*  Description:
*     This routine unpacks data from a jiggle observation into a
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
*     status returned. Otherwise, the data will be unpacked into the
*     map as specified by the I_JIGGLE, J_JIGGLE arrays.
*
*        When the data cover several repeats of the jiggle pattern, the
*     routine will check that the jiggle index of the first point in the
*     datablock is 1 and that the number of data points is an integer multiple
*     of the size of the jiggle. If not, an error will be reported and bad
*     status returned. Otherwise, the data will be unpacked into the
*     map as specified by I_JIGGLE, J_JIGGLE. The unpacking differs from
*     the first case in that points in the unpacked map will be set to the
*     average of the good quality values contributed by the separate jiggles,
*     variances will be calculated from the dispersion of the values about
*     the mean if there was more than 1, or set to the input variance
*     otherwise.

*  Invocation:
*     CALL SCULIB_UNPACK_JIGGLE_SEPARATES (N_JIGGLES, N_BOLS, J_DATA,
*    :  J_VARIANCE, J_QUALITY, J_START, JIGGLE_COUNT, I_JIGGLE,
*    :  J_JIGGLE, IDIM, JDIM, MAP_DATA, MAP_VARIANCE, MAP_QUALITY, MAP_NUMPTS,
*    :  J_END, BADBIT, STATUS)

*  Arguments:
*     N_JIGGLES                         = INTEGER (Given)
*           number of jiggles in datablock
*     N_BOLS                            = INTEGER (Given)
*           number of bolometers measured
*     J_DATA (N_BOLS, N_JIGGLES)        = REAL (Given)
*           data for each jiggle measured
*     J_VARIANCE (N_BOLS, N_JIGGLES)    = REAL (Given)
*           variance on J_DATA
*     J_QUALITY (N_BOLS, N_JIGGLES)     = BYTE (Given)
*           quality on J_DATA
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
*     MAP_NUMPTS (IDIM, JDIM, N_BOLS)   = INTEGER (Returned)
*           output map quality
*     MAP_QUALITY (IDIM, JDIM, N_BOLS)  = BYTE (Returned)
*           output map quality
*     J_END                             = INTEGER (Returned)
*           index of last jiggle in datablock in the overall jiggle pattern
*     BADBIT                            = BYTE  (Given)
*           bad bit mask
*     STATUS                            = INTEGER (Given and returned)
*           global status


*  Authors:
*     J.Lightfoot (REVAD::JFL)

*  Copyright:
*     Copyright (C) 1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  Method:

*  Deficiencies:

*  Bugs:


*  History:
*     $Id$
*     $Log$
*     Revision 1.4  1999/08/19 03:37:32  timj
*     Header tweaks to ease production of SSN72 documentation.
*
*     21-MAY-1996: Original version, adapted from SCULIB_UNPACK_JIGGLE.


*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'                 ! for VAL__BADR

*  Arguments Given:
      INTEGER N_JIGGLES
      INTEGER N_BOLS
      REAL    J_DATA (N_BOLS, N_JIGGLES)
      REAL    J_VARIANCE (N_BOLS, N_JIGGLES)
      BYTE    J_QUALITY (N_BOLS, N_JIGGLES)
      INTEGER J_START
      INTEGER JIGGLE_COUNT
      INTEGER I_JIGGLE (JIGGLE_COUNT)
      INTEGER J_JIGGLE (JIGGLE_COUNT)
      INTEGER IDIM
      INTEGER JDIM
      BYTE    BADBIT

*  Arguments Given & Returned:

*  Arguments Returned:
      REAL    MAP_DATA (IDIM, JDIM, N_BOLS)
      REAL    MAP_VARIANCE (IDIM, JDIM, N_BOLS)
      BYTE    MAP_QUALITY (IDIM, JDIM, N_BOLS)
      INTEGER MAP_NUMPTS (IDIM, JDIM, N_BOLS)
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

*  External functions:
      INCLUDE 'NDF_FUNC'

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

*  the input data does not contain numbers for several repeats of the jiggle
*  pattern. Check jiggle indices are OK, if so unpack data

            IF ((J_START .LT. 1) .OR.
     :          (J_START+N_JIGGLES-1 .GT. JIGGLE_COUNT)) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP (' ', 'SCULIB_UNPACK_JIGGLE_'//
     :           'SEPARATES: bad jiggle number', STATUS)
            ELSE

               DO I = 1, N_JIGGLES
                  JIGGLE = J_START + I - 1
                  IM = I_JIGGLE (JIGGLE)
                  JM = J_JIGGLE (JIGGLE)

                  DO BOL = 1, N_BOLS
                     MAP_DATA (IM,JM,BOL) = J_DATA (BOL,I)
                     MAP_VARIANCE (IM,JM,BOL) = J_VARIANCE (BOL,I)
                     IF (NDF_QMASK(J_QUALITY(BOL,I), BADBIT)) THEN
                        MAP_QUALITY(IM,JM,BOL) = 0
                        MAP_NUMPTS (IM,JM,BOL) = 0
                     ELSE
                        MAP_NUMPTS (IM,JM,BOL) = 1
                     END IF
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
               CALL ERR_REP (' ', 'SCULIB_UNPACK_JIGGLE'//
     :           'SEPARATES: J_START not 1 for data from '//
     :           'repeated jiggle pattern', STATUS)
            END IF

            IF (MOD(N_JIGGLES,JIGGLE_COUNT) .NE. 0) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP (' ', 'SCULIB_UNPACK_JIGGLE'//
     :           'SEPARATES: data for non-integral number of '//
     :           'repeats of jiggle pattern', STATUS)
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
                     IF (NDF_QMASK(J_QUALITY(BOL,I), BADBIT)) THEN

                        IF (MAP_DATA(IM,JM,BOL) .EQ. VAL__BADR) THEN

*  an unset map point, just set it

                           MAP_DATA (IM,JM,BOL) = J_DATA (BOL,I)
                           MAP_VARIANCE (IM,JM,BOL) = J_VARIANCE (BOL,I)
                           MAP_NUMPTS (IM,JM,BOL) = 1
                           MAP_QUALITY(IM,JM,BOL) = 1
                        ELSE

*  map point already set, coadd subsequent jiggles using quality to
*  keep count of number of coadds. For first number coadded copy
*  the demodulated data variance into the map variance, for subsequent
*  numbers store the sum of the squares of the data values in the variance
*  array so that the variance can be calculated from the distribution
*  about the mean at the end of the routine

                           IF (MAP_NUMPTS(IM,JM,BOL) .EQ. 1) THEN
                              MAP_VARIANCE (IM,JM,BOL) =
     :                          MAP_DATA (IM,JM,BOL) **2 +
     :                          J_DATA (BOL,I) **2
                           ELSE
                              MAP_VARIANCE (IM,JM,BOL) =
     :                          MAP_VARIANCE (IM,JM,BOL) +
     :                          J_DATA (BOL,I) **2
                           END IF
                           MAP_DATA (IM,JM,BOL) = MAP_DATA (IM,JM,BOL) +
     :                       J_DATA (BOL,I)
                           MAP_NUMPTS (IM,JM,BOL) =
     :                       MAP_NUMPTS (IM,JM,BOL) + 1
                           MAP_QUALITY(IM,JM,BOL) = 0
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
     :                       REAL(MAP_NUMPTS (I,J,BOL))

                           IF (MAP_NUMPTS(I,J,BOL) .GT. 1) THEN
                              MAP_VARIANCE (I,J,BOL) =
     :                          (MAP_VARIANCE(I,J,BOL) -
     :                          MAP_NUMPTS(I,J,BOL) *
     :                          MAP_DATA(I,J,BOL)**2) /
     :                          REAL(MAP_NUMPTS(I,J,BOL) *
     :                          (MAP_NUMPTS(I,J,BOL)-1))
                           END IF

                           MAP_DATA(I,J,BOL) = 0
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
