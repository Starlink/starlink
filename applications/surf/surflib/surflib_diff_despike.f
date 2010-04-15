      SUBROUTINE SURFLIB_DIFF_DESPIKE (N_EXPOSURES, N_INTEGRATIONS,
     :     N_MEASUREMENTS, DEMOD_POINTER, N_BOL, N_POS, IN_DATA,
     :     IN_VARIANCE, IN_QUALITY, BADBIT, NSIGMA, OUT_DATA,
     :     OUT_VARIANCE, OUT_QUALITY, NUM_SPIKES, STATUS)
*+
*  Name:
*    SURFLIB_DIFF_DESPIKE

*  Purpose:
*    Despike data scan by detecting points
*    more than nsigma from running 2-point mean

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SURFLIB_DIFF_DESPIKE (N_EXPOSURES, N_INTEGRATIONS,
*    :  N_MEASUREMENTS, DEMOD_POINTER, N_BOL, N_POS, IN_DATA,
*    :  IN_VARIANCE, IN_QUALITY, BADBIT, NSIGMA, OUT_DATA,
*    :  OUT_VARIANCE, OUT_QUALITY, NUM_SPIKES, STATUS)

*  Description:
*     This routine removes spikes from SCAN/MAP observations.
*     The scan map differential despiking algorithm uses 2 criteria
*     to decide which points are spikes.
*
*     First, for each bolometer used a pass is made through each
*     scan calculating for each point:-
*
*       diff(i) = point(i) - (point(i-1) + point(i+1))
*                            -------------------------
*                                       2.0
*
*
*     Values of 'diff' for the first and last points in the scan are
*     calculated in a similar way but subtracting the mean of points
*     2 and 3 and points n-1 and n-2 respectively.
*
*     The mean and standard deviation of 'diff' are calculated by
*     coadding the 10 points at each end of the scan where,
*     hopefully, there is no source emission. Spikes in these
*     regions are handled by removing points from the coadd that lie
*     further than 3 sigma from the mean, then redoing the
*     calculation recursively until no further points need be
*     removed.
*
*     The first criterion for a spike is that it's 'diff' value
*     should be further from the mean of 'diff' by NSIGMA times the
*     sigma derived from the endpoints.
*
*     The problem with this simple approach is that bright sources
*     in the scan themselves lead to excursions in 'diff' that can
*     be wrongly identified as spikes. To prevent this happening a
*     second criterion is used. In this the scan values are
*     convolved with a 3 sample wide box so that each 'box' point is
*     the average of the point itself and the points on either side of
*     it. 'Box' is expected to increase faster for real sources than
*     for spikes because in them the increase will be spread over
*     all 3 averaged points rather than just 1.
*
*     The second criterion for a spike is met, therefore, if a
*     point's 'diff' is further from the 'diff' mean than the value
*     of 'box' at that point.
*
*     Fixed-up values for points that have identified as spikes are
*     calculated by interpolating between the closest healthy points
*     on either side.
*
*     The second spike criterion also means unfortunately that the
*     technique is less sensitive to spikes on bright sources than
*     elsewhere. In addition, it is still possible to clip bright
*     sources if too low a value for NSIGMA is used. It is
*     recommended to run despike several times with different values
*     of NSIGMA. Begin with NSIGMA=5, look at the result to see how
*     effective despiking has been, then repeat the process with
*     NSIGMA=4.5, 4.0 etc. until you start to clip source
*     information.

*  Arguments:
*     N_EXPOSURES                 = INTEGER (Given)
*           maximum number of exposures per integration
*     N_INTEGRATIONS              = INTEGER (Given)
*           number of integrations in the observation
*     N_MEASUREMENTS              = INTEGER (Given)
*           number of measurements in the observation
*     DEMOD_POINTER (N_EXPOSURES, N_INTEGRATIONS, N_MEASUREMENTS)
*           array pointing to start and finish of scans in IN_DATA
*     N_BOL                       = INTEGER (Given)
*           the number of bolometers for which data was taken
*     N_POS                       = INTEGER (Given)
*           the number of positions measured in the scan
*     IN_DATA (N_BOL, N_POS)      = REAL (Given)
*           the measured data
*     IN_VARIANCE (N_BOL, N_POS)  = REAL (Given)
*           the variance on IN_DATA
*     IN_QUALITY (N_BOL, N_POS)   = BYTE (Given)
*           the quality on IN_DATA
*     BADBIT                      = BYTE (Given)
*           bad bit mask
*     NSIGMA                      = REAL (Given)
*           cut-off sigma for spikes
*     OUT_DATA (N_BOL, N_POS)     = REAL (Returned)
*           the deconvolved data
*     OUT_VARIANCE (N_BOL, N_POS) = REAL (Returned)
*           the variance on OUT_DATA
*     OUT_QUALITY (N_BOL, N_POS)  = BYTE (Returned)
*           the quality on OUT_DATA
*     NUM_SPIKES                  = INTEGER (Returned)
*           Total number of spikes detected
*     STATUS                      = INTEGER (Given and returned)
*           global status

*  Method:

*  Deficiencies:
*     - If there are 3 spikes in a row and the middle spike is approximately
*       the mean of the spikes either side, the middle spike will not be
*       treated as a spike (and will not be changed)

*    Bugs:

*    Authors:
*     J.Lightfoot (jfl@roe.ac.uk)
*     T.Jenness (timj@jach.hawaii.edu)


*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*    History:
*     $Log$
*     Revision 1.4  2004/09/01 01:06:58  timj
*     fix uninitialised warnings
*
*     Revision 1.3  1999/08/03 19:32:49  timj
*     Add copyright message to header.
*
*     Revision 1.2  1998/06/03 22:02:17  timj
*     Major upgrade (finished ages ago).
*
*     Revision 1.1  1997/12/18 21:37:43  timj (JFL)
*     Initial revision by JFL

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'                        ! for VAL__BADI
      INCLUDE 'MSG_PAR'                        ! for MSG__ constants

*  Arguments Given:
      INTEGER N_EXPOSURES
      INTEGER N_INTEGRATIONS
      INTEGER N_MEASUREMENTS
      INTEGER DEMOD_POINTER (N_EXPOSURES, N_INTEGRATIONS,
     :     N_MEASUREMENTS)
      INTEGER N_BOL
      INTEGER N_POS
      REAL    IN_DATA (N_BOL, N_POS)
      REAL    IN_VARIANCE (N_BOL, N_POS)
      BYTE    IN_QUALITY (N_BOL, N_POS)
      BYTE    BADBIT
      REAL    NSIGMA

*  Arguments Returned:
      REAL    OUT_DATA (N_BOL, N_POS)
      REAL    OUT_VARIANCE (N_BOL, N_POS)
      BYTE    OUT_QUALITY (N_BOL, N_POS)
      INTEGER NUM_SPIKES

*  Status:
      INTEGER STATUS

*  External references:
      BYTE SCULIB_BITOR                        ! SCULIB 'or' function
      BYTE SCULIB_BITON                        ! Turn on a bit

*  Global variables:

*  Local Constants:
      INTEGER MAX_SCAN                         ! the maximum length of a scan
      PARAMETER (MAX_SCAN = 6000)

*  Local variables:
      INTEGER BOL                              ! bolometer index
      REAL    BOX (MAX_SCAN)                   !
      REAL    CLIP_LEVEL                       ! Sigma clipping level
      LOGICAL DESPIKING                        ! .TRUE. while in despiking
                                               ! loop
      REAL    DIFF (MAX_SCAN)                  ! 'difference' array along
                                               ! scan
      INTEGER EXPOSURE                         ! exposure number of scan
      REAL    FACTOR                           ! interpolation factor
      LOGICAL GET_SIGMA                        ! .TRUE. while calculating
                                               ! sigma
      INTEGER I                                ! DO loop index
      INTEGER IHIGH                            ! index of upper
                                               ! interpolation point
      INTEGER ILOW                             ! index of low
                                               ! interpolation point
      INTEGER INTEGRATION                      ! integration number of scan
      INTEGER ITERATION                        ! number of despiking pass
      INTEGER J                                ! array index
      REAL    MEAN                             ! mean of DIFF
      INTEGER MEASUREMENT                      ! measurement number of scan
      INTEGER NSPIKES                          ! number of spikes found in
                                               ! despiking pass
      INTEGER NSUM                             ! number of valid points
                                               ! in SUM
      INTEGER N_SCAN                           ! number of points in scan
      INTEGER SCAN_END                         ! index of end of scan in
                                               ! demodulated data array
      INTEGER SCAN_START                       ! index of start of scan in
                                               ! demodulated data array
      LOGICAL SEARCHING                        ! .TRUE. when searching
                                               ! for valid data point
      REAL    SIGMA                            ! sigma of DIFF
      INTEGER DIFF_QUAL (MAX_SCAN)             ! quality on DIFF when
                                               ! calculating SIGMA
      INTEGER SPIKE (MAX_SCAN)                 ! 'spike' array - 1 for
                                               ! spike, 0 otherwise
      REAL    SUM                              ! sum of DIFF
      REAL    SUMSQ                            ! sum of DIFF**2
      REAL    VARIANCE                         ! variance of DIFF

*  Internal References :
      INCLUDE 'NDF_FUNC'                       ! for NDF_QMASK

*  Local data:

*.

      IF (STATUS .NE. SAI__OK) RETURN

*     Keep track of the total number of spikes
      NUM_SPIKES = 0

*     init
      MEAN = 0.0
      SIGMA = 0.0

*  cycle through the scans

      DO MEASUREMENT = 1, N_MEASUREMENTS
         DO INTEGRATION = 1, N_INTEGRATIONS
            DO EXPOSURE = 1, N_EXPOSURES

               CALL SCULIB_FIND_SWITCH (DEMOD_POINTER, 1,
     :              N_EXPOSURES, N_INTEGRATIONS, N_MEASUREMENTS, N_POS,
     :              1, EXPOSURE, INTEGRATION, MEASUREMENT, SCAN_START,
     :              SCAN_END, STATUS)

               IF ((SCAN_START .EQ. VAL__BADI) .OR.
     :              (SCAN_START .EQ. 0) ) THEN
                  CALL MSG_SETI ('E', EXPOSURE)
                  CALL MSG_SETI ('I', INTEGRATION)
                  CALL MSG_SETI ('M', MEASUREMENT)
                  CALL MSG_OUTIF (MSG__NORM, ' ',
     :                 'SURFLIB_DIFF_DESPIKE: no data '//
     :                 'for exp ^E in int ^I, meas ^M', STATUS)
               ELSE

*  OK, there is some data for the scan

                  CALL MSG_SETI('NI', INTEGRATION)
                  CALL MSG_SETI('NE', EXPOSURE)

                  CALL MSG_OUTIF(MSG__NORM,' ',
     :                 'SURFLIB_DIFF_DESPIKE: Processing '//
     :                 'exposure ^NE of integration ^NI', STATUS)

                  N_SCAN = SCAN_END - SCAN_START + 1

                  IF (N_SCAN .LT. 3) THEN
                     IF (STATUS .EQ. SAI__OK) THEN
                        STATUS = SAI__ERROR
                        CALL MSG_SETI ('LEN', N_SCAN)
                        CALL ERR_REP (' ',
     :                    'SURFLIB_DIFF_DESPIKE: scan too '//
     :                    'short (^LEN points) for despiking',
     :                    STATUS)
                     END IF
                  ELSE IF (N_SCAN .GT. MAX_SCAN) THEN
                     IF (STATUS .EQ. SAI__OK) THEN
                        STATUS = SAI__ERROR
                        CALL MSG_SETI ('LEN', N_SCAN)
                        CALL MSG_SETI ('MAX', MAX_SCAN)
                        CALL ERR_REP (' ',
     :                    'SURFLIB_DIFF_DESPIKE: scan longer '//
     :                    '(^LEN) than maximum allowed (^MAX)',
     :                    STATUS)
                     END IF
                  ELSE

*  now cycle through the bolometers

                     DO BOL = 1, N_BOL

*  copy the input data to output

                        DO I = SCAN_START, SCAN_END
                           OUT_DATA (BOL,I) = IN_DATA (BOL,I)
                           OUT_VARIANCE (BOL,I) = IN_VARIANCE (BOL,I)
                           OUT_QUALITY (BOL,I) = IN_QUALITY (BOL,I)
                        END DO

*  now start despiking cycle

                        DESPIKING = .TRUE.
                        ITERATION = 1

                        DO WHILE (DESPIKING)

*  move through the scan calculating the difference between point i and
*  the average of points i-1 and i+1, special case for 2 end points

*  1st point

                           IF (NDF_QMASK(OUT_QUALITY(BOL,SCAN_START),
     :                       BADBIT) .AND.
     :                       NDF_QMASK(OUT_QUALITY(BOL,SCAN_START+1),
     :                       BADBIT) .AND.
     :                       NDF_QMASK(OUT_QUALITY(BOL,SCAN_START+2),
     :                       BADBIT).AND.
     :                       (OUT_DATA(BOL,SCAN_START).NE.VAL__BADR)
     :                       .AND.
     :                       (OUT_DATA(BOL,SCAN_START+1).NE.VAL__BADR)
     :                       .AND.
     :                       (OUT_DATA(BOL,SCAN_START+2).NE.VAL__BADR))
     :                       THEN
                              DIFF (1) = OUT_DATA (BOL,SCAN_START) -
     :                          (OUT_DATA(BOL,SCAN_START+1) +
     :                          OUT_DATA(BOL,SCAN_START+2)) / 2.0
                           ELSE
                              DIFF (1) = VAL__BADR
                           END IF

*  body of scan

                           DO I = SCAN_START + 1, SCAN_END - 1
                              IF (NDF_QMASK(OUT_QUALITY(BOL,I-1),BADBIT)
     :                          .AND.
     :                          NDF_QMASK(OUT_QUALITY(BOL,I),BADBIT)
     :                          .AND.
     :                          NDF_QMASK(OUT_QUALITY(BOL,I+1),BADBIT)
     :                          .AND.
     :                          (OUT_DATA(BOL,I-1) .NE. VAL__BADR)
     :                          .AND.
     :                          (OUT_DATA(BOL,I) .NE. VAL__BADR)
     :                          .AND.
     :                          (OUT_DATA(BOL,I+1) .NE. VAL__BADR))
     :                          THEN

                                 DIFF(I-SCAN_START+1) = OUT_DATA(BOL,I)-
     :                             (OUT_DATA(BOL,I-1)+OUT_DATA(BOL,I+1))
     :                             / 2.0
                              ELSE
                                 DIFF (I-SCAN_START+1) = VAL__BADR
                              END IF
                           END DO

*  last point

                           IF (NDF_QMASK(OUT_QUALITY(BOL,SCAN_END-2),
     :                       BADBIT) .AND.
     :                       NDF_QMASK(OUT_QUALITY(BOL,SCAN_END-1),
     :                       BADBIT) .AND.
     :                       NDF_QMASK(OUT_QUALITY(BOL,SCAN_END),
     :                       BADBIT) .AND.
     :                       (OUT_DATA(BOL,SCAN_END-2).NE.VAL__BADR)
     :                       .AND.
     :                       (OUT_DATA(BOL,SCAN_END-1).NE.VAL__BADR)
     :                       .AND.
     :                       (OUT_DATA(BOL,SCAN_END) .NE. VAL__BADR))
     :                       THEN
                              DIFF(N_SCAN) = OUT_DATA(BOL,SCAN_END) -
     :                          (OUT_DATA(BOL,SCAN_END-2) +
     :                          OUT_DATA(BOL,SCAN_END-1)) / 2.0
                           ELSE
                              DIFF (N_SCAN) = VAL__BADR
                           END IF

*  calculate the mean and variance of the from the 10 points at each end
*  of the scan - where hopefully there is no source emission

                           GET_SIGMA = .TRUE.
                           DO I = 1, N_SCAN
                              DIFF_QUAL (I) = 0
                           END DO

                           DO WHILE (GET_SIGMA)
                              SUM = 0.0
                              SUMSQ = 0.0
                              NSUM = 0

                              DO I = 1, MIN(10, N_SCAN)
                                 IF (DIFF_QUAL(I) .EQ. 0) THEN
                                    SUM = SUM + DIFF (I)
                                    SUMSQ = SUMSQ + DIFF (I) **2
                                    NSUM = NSUM + 1
                                 END IF
                              END DO
                              DO I = MAX (1,N_SCAN-9), N_SCAN
                                 IF (DIFF_QUAL(I) .EQ. 0) THEN
                                    SUM = SUM + DIFF (I)
                                    SUMSQ = SUMSQ + DIFF (I) **2
                                    NSUM = NSUM + 1
                                 END IF
                              END DO

                              IF (NSUM .GT. 0) THEN
                                 MEAN = SUM / REAL (NSUM)
                                 VARIANCE =
     :                             (SUMSQ-REAL(NSUM)*MEAN**2) /
     :                             REAL (NSUM)
                                 SIGMA = SQRT (VARIANCE)
                              END IF

*  go through and throw out DIFF points more than 3 sigma from the mean,
*  exit loop when no points are rejected

                              CLIP_LEVEL = 3.0 * SIGMA
                              GET_SIGMA = .FALSE.

                              DO I = 1, MIN(10, N_SCAN)
                                 IF ((ABS(DIFF(I)-MEAN) .GT.
     :                             CLIP_LEVEL) .AND.
     :                             (DIFF_QUAL(I) .EQ. 0)) THEN
                                    DIFF_QUAL (I) = 1
                                    GET_SIGMA = .TRUE.
                                 END IF
                              END DO
                              DO I = MAX (1,N_SCAN-9), N_SCAN
                                 IF ((ABS(DIFF(I)-MEAN) .GT.
     :                             CLIP_LEVEL) .AND.
     :                             (DIFF_QUAL(I) .EQ. 0)) THEN
                                    DIFF_QUAL (I) = 1
                                    GET_SIGMA = .TRUE.
                                 END IF
                              END DO
                           END DO

*  calculate the 'box' function

                           DO I = SCAN_START + 1, SCAN_END - 1
                              IF (NDF_QMASK(OUT_QUALITY(BOL,I-1),BADBIT)
     :                          .AND.
     :                          NDF_QMASK(OUT_QUALITY(BOL,I),BADBIT)
     :                          .AND.
     :                          NDF_QMASK(OUT_QUALITY(BOL,I+1),BADBIT)
     :                          .AND.
     :                          (OUT_DATA(BOL,I-1) .NE. VAL__BADR)
     :                          .AND.
     :                          (OUT_DATA(BOL,I) .NE. VAL__BADR)
     :                          .AND.
     :                          (OUT_DATA(BOL,I+1) .NE. VAL__BADR))
     :                          THEN

                                 BOX (I-SCAN_START+1) =
     :                             (ABS (OUT_DATA (BOL,I)) +
     :                             ABS (OUT_DATA(BOL,I-1)) +
     :                             ABS (OUT_DATA(BOL,I+1))) / 3.0
                              ELSE
                                 BOX (I-SCAN_START+1) = 1.0E6
                              END IF
                           END DO

                           BOX (1) = BOX (2)
                           BOX (N_SCAN) = BOX (N_SCAN-1)

*     Initialise counters
                           NSPIKES = 0

*     mark spikes as bad

                           DO I = 1, N_SCAN
                              IF ((DIFF(I) .NE. VAL__BADR)      .AND.
     :                          (ABS(DIFF(I)-MEAN).GE.NSIGMA*SIGMA)
     :                          .AND.
     :                          (ABS(DIFF(I)-MEAN) .GE. BOX(I))) THEN

*     The current implementation of the algorithm can not sort
*     out the case where a spike is detected but is replaced by
*     a point that is also a spike. In this case a spike is detected
*     next time round but also replaced by a spike. This situation
*     is handled by using the cap on iteration numbers but does mean
*     that an incorrect number of spikes would be detected.
*     This is mainly because we do not interpolate over adjacent pixels
*     but use pixels 2 steps away from the spike.

                                 SPIKE (I) = 1
                                 NSPIKES = NSPIKES + 1

*                                 print *,'Spike at :',I,
*     :                                   I + SCAN_START - 1,
*     :                                   ' (',
*     :                                   DIFF(I)-MEAN,BOX(I),
*     :                                   NSIGMA*SIGMA,')'

*     The presence of a spike here will contaminate the BOX function.
*     Therefore recalculate BOX for the next point without using the
*     current value (not at present)


                              ELSE
                                 SPIKE (I) = 0
                              END IF
                           END DO

*  lastly, go through data and interpolate over spikes

                           DO I = SCAN_START, SCAN_END

*                                 IF (I .GT. 3770 .AND. I .LT. 3780
*     :                                .AND. BOL.EQ.1) THEN
*                                    print *,'Diff: ',I,
*     :                                   DIFF(I-SCAN_START+1),
*     :                                   DIFF(I-SCAN_START+1)-MEAN,
*     :                                   SPIKE(I-SCAN_START+1),
*     :                                   BOX(I-SCAN_START+1)
*                                 END IF


                              IF (SPIKE (I-SCAN_START+1) .EQ. 1) THEN

*  locate nearest good quality data points that are not themselves
*  spikes
*  Start the search 2 pixels away in case the neighbour pixel is also
*  a spike.

                                 ILOW = VAL__BADI
                                 SEARCHING = .TRUE.
                                 J = I - 2
                                 DO WHILE (SEARCHING)
                                    J = J - 1
                                    IF (J .LT. SCAN_START) THEN
                                       SEARCHING = .FALSE.
                                    ELSE
                                       IF ((SPIKE(J-SCAN_START+1).EQ.0)
     :                                   .AND.
     :                                   NDF_QMASK(OUT_QUALITY(BOL,J),
     :                                   BADBIT) .AND.
     :                                   (OUT_DATA(BOL,J).NE.VAL__BADR))
     :                                   THEN
                                          ILOW = J
                                          SEARCHING = .FALSE.
                                       END IF
                                    END IF
                                 END DO

                                 IHIGH = VAL__BADI
                                 SEARCHING = .TRUE.
                                 J = I + 2
                                 DO WHILE (SEARCHING)
                                    J = J + 1
                                    IF (J .GT. SCAN_END) THEN
                                       SEARCHING = .FALSE.
                                    ELSE
                                       IF ((SPIKE(J-SCAN_START+1).EQ.0)
     :                                   .AND.
     :                                   NDF_QMASK(OUT_QUALITY(BOL,J),
     :                                   BADBIT) .AND.
     :                                   (OUT_DATA(BOL,J).NE.VAL__BADR))
     :                                    THEN
                                           IHIGH = J
                                           SEARCHING = .FALSE.
                                       END IF
                                    END IF
                                 END DO

*                                 IF (I .GT. 3700 .AND. I .LT. 3800
*     :                                .AND. BOL.EQ.1) THEN
*                                    print *,'Good: ',I,IHIGH, ILOW
*                                 END IF


                                 IF (ILOW .EQ. VAL__BADI) THEN
                                    IF (IHIGH .EQ. VAL__BADI) THEN

*  bad quality, despiking error, leave data as before but set
*  despiking bit in quality

                                       OUT_QUALITY(BOL,I) =
     :                                      SCULIB_BITON (
     :                                      OUT_QUALITY (BOL,I), 4)
                                    ELSE
                                       OUT_DATA (BOL,I) =
     :                                   OUT_DATA (BOL,IHIGH)
                                       OUT_VARIANCE (BOL,I) =
     :                                   OUT_VARIANCE (BOL,IHIGH)
                                       OUT_QUALITY (BOL,I) =
     :                                   OUT_QUALITY (BOL,IHIGH)
                                    END IF
                                 ELSE
                                    IF (IHIGH .EQ. VAL__BADI) THEN
                                       OUT_DATA (BOL,I) =
     :                                   OUT_DATA (BOL,ILOW)
                                       OUT_VARIANCE (BOL,I) =
     :                                   OUT_VARIANCE (BOL,ILOW)
                                       OUT_QUALITY (BOL,I) =
     :                                   OUT_QUALITY (BOL,ILOW)
                                    ELSE
                                       FACTOR = REAL (I-ILOW) /
     :                                   REAL (IHIGH-ILOW)
                                       OUT_DATA (BOL,I) =
     :                                   OUT_DATA (BOL,ILOW) +
     :                                   FACTOR *
     :                                   (OUT_DATA(BOL,IHIGH) -
     :                                   OUT_DATA(BOL,ILOW))
                                       OUT_VARIANCE (BOL,I) =
     :                                   OUT_VARIANCE (BOL,ILOW) +
     :                                   FACTOR**2 *
     :                                   (OUT_VARIANCE(BOL,IHIGH) +
     :                                   OUT_VARIANCE(BOL,ILOW))
                                       OUT_QUALITY (BOL,I) =
     :                                   SCULIB_BITOR (
     :                                   OUT_QUALITY (BOL,IHIGH),
     :                                   OUT_QUALITY (BOL,ILOW))
                                    END IF
                                 END IF
                              END IF

                           END DO

*     Keep track of the total number of spikes found so far
                           NUM_SPIKES = NUM_SPIKES + NSPIKES

*     Report the detection of spikes
                           IF (NSPIKES .EQ. 1) THEN
                              CALL MSG_SETI ('B', BOL)
                              CALL MSG_SETI ('N', NSPIKES)
                              CALL MSG_SETI ('I', ITERATION)
                              CALL MSG_OUTIF (MSG__VERB, ' ',
     :                          'Bolometer ^B - ^N '//
     :                          'spike found on pass ^I', STATUS)

                           ELSE IF (NSPIKES .GT. 1) THEN
                              CALL MSG_SETI ('B', BOL)
                              CALL MSG_SETI ('N', NSPIKES)
                              CALL MSG_SETI ('I', ITERATION)
                              CALL MSG_OUTIF (MSG__VERB, ' ',
     :                          'Bolometer ^B - ^N '//
     :                          'spikes found on pass ^I', STATUS)

                           END IF

                           ITERATION = ITERATION + 1
                           IF ((NSPIKES .EQ. 0) .OR.
     :                       (ITERATION .GT. 10)) THEN
                              DESPIKING = .FALSE.
                           END IF

                        END DO
                     END DO

                  END IF
               END IF

            END DO
         END DO
      END DO

      END
