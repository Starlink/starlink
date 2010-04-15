      SUBROUTINE SCULIB_REMOVE_LINEAR_BASELINE(DORLB, N_EXPOSURES,
     :     N_INTEGRATIONS, N_MEASUREMENTS, DEMOD_POINTER, N_BOL, N_POS,
     :     IN_DATA, IN_VARIANCE, IN_QUALITY, NSTART, NEND,
     :     OUT_DATA, OUT_VARIANCE, OUT_QUALITY, BADBIT, STATUS)
*+
*  Name:
*     SCULIB_REMOVE_LINEAR_BASELINE

*  Purpose:
*     Remove linear baseline from each exposure.

*  Invocation:
*     CALL SCULIB_REMOVE_LINEAR_BASELINE(DORLB, N_EXPOSURES,
*    :     N_INTEGRATIONS, N_MEASUREMENTS, DEM_PNTR, N_BOL, N_POS,
*    :     IN_DATA, IN_QUALITY, SAMPLE_DX, CHOP_THROW, OUT_DATA,
*    :     OUT_QUALITY, BADBIT, STATUS)

*  Description:
*     This routine takes a data array. It then removes a linear baseline
*     from each scan. The ends of the scan are the size of the chop throw
*     (should be no source at the ends of a scan)

*  Arguments:
*     DORLB                       = LOGICAL (Given)
*           control whether we are subtracting the baseline (TRUE)
*           or storing the basline (FALSE)
*     N_EXPOSURES                 = INTEGER (Given)
*           maximum number of exposures per integration
*     N_INTEGRATIONS              = INTEGER (Given)
*           number of integrations in the observation
*     N_MEASUREMENTS              = INTEGER (Given)
*           number of measurements in the observation
*     DEMOD_POINTER (N_EXPOSURES, N_INTEGRATIONS, N_MEASUREMENTS) = INTEGER (Given)
*           array pointing to start and finish of scans in IN_DATA
*     N_BOL                       = INTEGER (Given)
*           the number of bolometers for which data was taken
*     N_POS                       = INTEGER (Given)
*           the number of positions measured in the scan
*     IN_DATA (N_BOL, N_POS)      = REAL (Given)
*           the measured data
*     IN_VARIANCE (N_BOL, N_POS)      = REAL (Given)
*           the measured variance
*     IN_QUALITY (N_BOL, N_POS)   = BYTE (Given)
*           the quality on IN_DATA
*     NSTART                      = INTEGER (Given)
*           Number of points used for fit at start of scan
*     NEND                        = INTEGER (Given)
*           Number of points from end of scan
*     OUT_DATA (N_BOL, N_POS)     = REAL (Returned)
*           the data with baseline removed
*     OUT_VARIANCE (N_BOL, N_POS)      = REAL (Given)
*           the output variance
*     OUT_QUALITY (N_BOL, N_POS)  = BYTE (Returned)
*           the quality on OUT_DATA
*     BADBIT                      = BYTE (Given)
*           bad bit mask
*     STATUS = INTEGER (Given and Returned)
*        Global Status value

*  Implementation Status:

*  Authors:
*     TIMJ: Tim Jenness (JACH)
*     JFL: John Lightfoot (RoE)
*     {enter_new_authors_here}


*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     1997 May 22 (TIMJ):
*       Original version
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! SSE global definitions
      INCLUDE 'PRM_PAR'               ! VAL__ constants
      INCLUDE 'MSG_PAR'               ! MSG__ constants
      INCLUDE 'CNF_PAR'               ! CNF_PVAL function

*  Arguments Given:
      BYTE    BADBIT
      LOGICAL DORLB
      INTEGER N_BOL
      INTEGER N_EXPOSURES
      INTEGER N_INTEGRATIONS
      INTEGER N_MEASUREMENTS
      INTEGER N_POS
      INTEGER DEMOD_POINTER(N_EXPOSURES, N_INTEGRATIONS, N_MEASUREMENTS)
      REAL    IN_DATA(N_BOL, N_POS)
      BYTE    IN_QUALITY(N_BOL, N_POS)
      REAL    IN_VARIANCE(N_BOL, N_POS)
      INTEGER NEND
      INTEGER NSTART

*  Arguments Returned:
      REAL    OUT_DATA(N_BOL, N_POS)
      BYTE    OUT_QUALITY(N_BOL, N_POS)
      REAL    OUT_VARIANCE(N_BOL, N_POS)

*  Status:
      INTEGER STATUS                 ! Global status

*  External references:

*  Local Constants:
      INTEGER MAXDEG                 ! Maximum degree of polynomial
      PARAMETER (MAXDEG = 1)         ! A straight line!
      INTEGER NDER                   ! Number of derivatives from fit
      PARAMETER (NDER = 0)
      REAL    MIN_VAR                 ! Minimum variance
      PARAMETER (MIN_VAR = 1.0E-4)

*  Local Variables:
      INTEGER A_END                  ! End of A_PTR
      INTEGER A_PTR                  ! Coefficients from fit
      INTEGER BOL                    ! Loop counter
      INTEGER CHUNK                  ! Loop counter
      INTEGER COUNT                  ! Number of good points for fit
      INTEGER ENDSC                  ! End of baseline copy
      DOUBLE PRECISION EPS           ! Fit tolerance
      INTEGER EXPOSURE               ! Exposure counter
      INTEGER FITDATA_END            ! End of FITDATA_PTR
      INTEGER FITDATA_PTR            ! Data values to be fitted
      INTEGER IERR                   ! Position of VEC_ error
      INTEGER IFAIL                  ! Error status from fit
      INTEGER INTEGRATION            ! Integration counter
      INTEGER MEASUREMENT            ! Measurement counter
      INTEGER NSCRATCH               ! Size of work arrays
      INTEGER NCHUNKS                ! Number of baseline sections
      INTEGER NDEG                   ! Number of degr of freedom used for fit
      INTEGER NERR                   ! Number of errors in VEC_
      INTEGER N_SCAN                 ! Number of samples in a scan
      INTEGER POS                    ! Loop counter
      INTEGER R_END                  ! End of R_PTR
      INTEGER R_PTR                  ! R coefficient for fit
      INTEGER SCAN_END               ! End of scan in array
      INTEGER SCAN_START             ! Start of scan in array
      INTEGER START                  ! Start of baseline copy
      INTEGER SZ_CHUNK               ! Size of first chunk
      REAL    WEIGHT                 ! Weight variable
      INTEGER WEIGHT_END             ! End of WEIGHT_PTR
      INTEGER WEIGHT_PTR             ! Weight of each point
      DOUBLE PRECISION XPOS          ! X position to calculate polynomial
      INTEGER X_END                  ! End of X_PTR
      INTEGER X_PTR                  ! Pixel X positions of each point
      DOUBLE PRECISION YFIT          ! Value of derivatives
      DOUBLE PRECISION YP            ! Value of the polynomial at X


*     Local References:
      INCLUDE 'NDF_FUNC'

*.

      IF (STATUS .NE. SAI__OK) RETURN

*     Initialise pointers
      FITDATA_PTR = 0
      FITDATA_END = 0
      X_PTR = 0
      X_END = 0
      WEIGHT_PTR = 0
      WEIGHT_END = 0
      R_PTR = 0
      R_END = 0
      A_PTR = 0
      A_END = 0

*     Calculate size of workspace
*     Assume baseline region is the chop throw at each end

      NSCRATCH = NSTART + NEND

*     Allocate work space for data and coordinates
      CALL SCULIB_MALLOC(NSCRATCH * VAL__NBD, FITDATA_PTR,FITDATA_END,
     :     STATUS)
      CALL SCULIB_MALLOC(NSCRATCH * VAL__NBD, X_PTR, X_END,
     :     STATUS)
      CALL SCULIB_MALLOC(NSCRATCH * VAL__NBD, WEIGHT_PTR, WEIGHT_END,
     :     STATUS)
      CALL SCULIB_MALLOC(NSCRATCH * VAL__NBD, R_PTR, R_END,
     :     STATUS)
      CALL SCULIB_MALLOC((3 * (NSCRATCH + MAXDEG + 1)) * VAL__NBD,
     :     A_PTR, A_END, STATUS)


*     Fill the output array with the values from the input array.
*     This allows us to have some values set even if the points are not
*     good and a basline can not be fitted
*     Its open to debate whether we should initially fill the output
*     arrays with BAD.

      DO BOL = 1, N_BOL
         DO POS = 1, N_POS
            OUT_DATA(BOL,POS) = IN_DATA(BOL,POS)
            OUT_QUALITY(BOL, POS) = IN_QUALITY(BOL, POS)
            OUT_VARIANCE(BOL, POS) = IN_VARIANCE(BOL,POS)
         END DO
      END DO

*     First need to select a scan

      DO MEASUREMENT = 1, N_MEASUREMENTS
         DO INTEGRATION = 1, N_INTEGRATIONS
            DO EXPOSURE = 1, N_EXPOSURES

               CALL SCULIB_FIND_SWITCH (DEMOD_POINTER, 1,
     :           N_EXPOSURES, N_INTEGRATIONS, N_MEASUREMENTS, N_POS,
     :           1, EXPOSURE, INTEGRATION, MEASUREMENT, SCAN_START,
     :           SCAN_END, STATUS)

               IF ((SCAN_START .EQ. VAL__BADI) .OR.
     :             (SCAN_START .EQ. 0) ) THEN
                  CALL MSG_SETI ('E', EXPOSURE)
                  CALL MSG_SETI ('I', INTEGRATION)
                  CALL MSG_SETI ('M', MEASUREMENT)
                  CALL MSG_OUTIF (MSG__NORM, ' ',
     :                 'REMOVE_LINEAR_BASELINE: no data '//
     :                 'for exp ^E in int ^I, meas ^M', STATUS)
               ELSE
*  OK, there is some data for the scan

                  CALL MSG_SETI('NI', INTEGRATION)
                  CALL MSG_SETI('NE', EXPOSURE)
                  CALL MSG_OUTIF(MSG__NORM,' ',
     :                 'REMOVE_LINEAR_BASELINE: Processing exposure'//
     :                 ' ^NE  of integration ^NI', STATUS)


                  N_SCAN = SCAN_END - SCAN_START + 1

                  DO BOL = 1, N_BOL

*     Copy the data into the work array
*     If the scan is shorter than NSTART+NEND, then copy all of it

                     IF (STATUS .EQ. SAI__OK) THEN

                        NCHUNKS = 2
                        SZ_CHUNK = NSTART

                        IF (N_SCAN .LT. NSTART + NEND) THEN
                           NCHUNKS = 1
                           SZ_CHUNK = N_SCAN
                        END IF

*     Copy the baseline pixels from each end into the scratch array
*     Do this in two chunks (start of scan then end). If the scan is too
*     short then do it all in one chunk

                        COUNT = 0

                        DO CHUNK = 1, NCHUNKS

                           IF (CHUNK .EQ. 1) THEN
                              START = SCAN_START
                              ENDSC = SCAN_START + SZ_CHUNK - 1
                           ELSE
                              START = SCAN_END - NEND + 1
                              ENDSC = SCAN_END
                           END IF

                           DO POS = START, ENDSC
                              IF ((NDF_QMASK(IN_QUALITY(BOL, POS),
     :                             BADBIT)) .AND.
     :                             IN_DATA(BOL, POS) .NE. VAL__BADR)
     :                             THEN

*     Can use FALSE for bad pixel checking since we already know
*     the point is good from above check.
                                 CALL VEC_RTOD(.FALSE., 1,
     :                                IN_DATA(BOL,POS),
     :                                %VAL(CNF_PVAL(FITDATA_PTR) +
     :                                COUNT * VAL__NBD),
     :                                IERR, NERR, STATUS)

*       Check that variance is okay
                                 IF (IN_VARIANCE(BOL, POS) .LT.
     :                                MIN_VAR) THEN
                                    WEIGHT = 1.0 / MIN_VAR
                                 ELSE
                                    WEIGHT = 1.0 / IN_VARIANCE(BOL, POS)
                                 END IF


                                 CALL VEC_RTOD(.FALSE., 1,
     :                                WEIGHT,
     :                                %VAL(CNF_PVAL(WEIGHT_PTR) +
     :                                COUNT * VAL__NBD),
     :                                IERR, NERR, STATUS)


                                 CALL VEC_ITOD(.FALSE., 1,
     :                                POS - SCAN_START + 1,
     :
     :   %VAL(CNF_PVAL(X_PTR) + COUNT * VAL__NBD),
     :                                IERR, NERR, STATUS)

                                 COUNT = COUNT + 1


                              END IF
                           END DO
                        END DO

*     We now have some data with COUNT pixels in it and an array
*     containing pixel distance (X pixel values relative to start of scan)
*     Simply need to fit this with a least squares polynomial fit.
*     Use first order polynomial.
*     If we have no data then skip this bolometer.
*     Cant have more than NSCRATCH points - if we do then we will have been
*     in trouble anyway as the VEC_ITOD lines (above) would cause
*     a segmentation fault.
*     Probably should make sure we have at least 2 points over which to
*     calculate the baseline.

                        IF (COUNT .GT. 1 .AND. COUNT .LE. NSCRATCH) THEN

                           EPS = 0.0D0
                           IFAIL = 0

                           CALL PDA_DPOLFT (COUNT,
     :                                      %VAL(CNF_PVAL(X_PTR)),
     :                          %VAL(CNF_PVAL(FITDATA_PTR)),
     :                          %VAL(CNF_PVAL(WEIGHT_PTR)),
     :                          MAXDEG, NDEG, EPS,
     :                          %VAL(CNF_PVAL(R_PTR)), IFAIL,
     :                          %VAL(CNF_PVAL(A_PTR)), STATUS)

                           IF (IFAIL .EQ. 1) THEN

*     Now use the coefficients to remove the baseline from the data

                              IF (STATUS .EQ. SAI__OK) THEN
                                 DO POS = SCAN_START, SCAN_END

                                    XPOS = DBLE(POS - SCAN_START + 1)

                                    CALL PDA_DP1VLU(NDEG, NDER, XPOS,
     :                                   YFIT, YP,
     :                                   %VAL(CNF_PVAL(A_PTR)), STATUS)

*     Simply remove this value (write out even if quality is bad)
*     if that is required else just store the fit

                                    IF (DORLB) THEN

                                       IF (IN_DATA(BOL,POS) .NE.
     :                                      VAL__BADR) THEN

                                          OUT_DATA(BOL,POS) =
     :                                         IN_DATA(BOL,POS) -
     :                                         REAL(YFIT)

                                       ELSE
                                          OUT_DATA(BOL,POS) =
     :                                         IN_DATA(BOL,POS)
                                       END IF

                                       OUT_QUALITY(BOL, POS) =
     :                                      IN_QUALITY(BOL, POS)
                                       OUT_VARIANCE(BOL, POS) =
     :                                      IN_VARIANCE(BOL,POS)

*     Store the fit
                                    ELSE

                                       OUT_DATA(BOL, POS) =
     :                                      REAL(YFIT)
                                       OUT_VARIANCE(BOL,POS) = 0.0
                                       OUT_QUALITY(BOL,POS) = 0

                                    END IF

                                 END DO
                              END IF

                           ELSE IF (STATUS .NE. SAI__OK) THEN

                              CALL MSG_SETI('IFAIL', IFAIL)
                              CALL MSG_SETI('BOL', BOL)
                              CALL ERR_REP(' ','REMOVE_LINEAR_BASEL'//
     :                             'INE: Error whilst removing '//
     :                             'baseline (Bol=^BOL, IFAIL=^IFAIL)',
     :                             STATUS)

                           ELSE

                              CALL MSG_SETI('IFAIL', IFAIL)
                              CALL MSG_SETI('BOL', BOL)
                              CALL MSG_OUTIF(MSG__QUIET,' ',
     :                             'REMOVE_LINEAR_BASELINE: Warning '//
     :                             'IFAIL = ^IFAIL in PDA_DPOLFT for '//
     :                             'Bolometer ^BOL',
     :                             STATUS)

                           END IF

                        END IF


                     END IF
                  END DO


               END IF

            END DO
         END DO
      END DO

*     Tidy up

      CALL SCULIB_FREE('FIT_DATA', FITDATA_PTR, FITDATA_END, STATUS)
      CALL SCULIB_FREE('X_DATA', X_PTR, X_END, STATUS)
      CALL SCULIB_FREE('X_DATA', R_PTR, R_END, STATUS)
      CALL SCULIB_FREE('X_DATA', A_PTR, A_END, STATUS)
      CALL SCULIB_FREE('WEIGHT', WEIGHT_PTR, WEIGHT_END, STATUS)

      END

