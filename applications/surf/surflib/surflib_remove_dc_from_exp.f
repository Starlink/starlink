      SUBROUTINE SURFLIB_REMOVE_DC_FROM_EXP(DORLB, N_EXPOSURES,
     :     N_INTEGRATIONS, N_MEASUREMENTS, METHOD, DEMOD_POINTER,
     :     N_BOL, N_POS, IN_DATA, IN_VARIANCE, IN_QUALITY,
     :     OUT_DATA, OUT_VARIANCE, OUT_QUALITY, BADBIT, STATUS)
*+
*  Name:
*     SURFLIB_REMOVE_DC_FROM_EXP

*  Purpose:
*     Remove linear baseline from each exposure.

*  Invocation:
*     CALL SCULIB_REMOVE_DC_FROM_EXP(DORLB, N_EXPOSURES,
*    :     N_INTEGRATIONS, N_MEASUREMENTS, METHOD, DEM_PNTR, N_BOL, N_POS,
*    :     IN_DATA, IN_QUALITY, SAMPLE_DX, CHOP_THROW, OUT_DATA,
*    :     OUT_QUALITY, BADBIT, STATUS)

*  Description:
*     This routine takes a data array. It then removes a
*     DC offset (either the mean or the median derived from the scan)
*     from each scan.

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
*     METHOD                      = CHAR (Given)
*           Removal method. MEDIAN or MEAN supported.
*     DEMOD_POINTER (N_EXPOSURES, N_INTEGRATIONS, N_MEASUREMENTS)
*                                 = INTEGER (Given)
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
*     Copyright (C) 1995-2002 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     1998 April 7 (TIMJ):
*       Original version
*     $Log$
*     Revision 1.8  2004/09/01 01:02:03  timj
*     use CNF_PVAL
*
*     Revision 1.7  2004/07/14 21:52:29  timj
*     Remove reference to MAX_BOLS
*
*     Revision 1.6  2002/09/14 03:57:41  timj
*     Fix uninitilaized variable warning
*
*     Revision 1.5  1999/08/03 19:32:53  timj
*     Add copyright message to header.
*
*     Revision 1.4  1999/07/17 02:48:48  timj
*     Check for DCVALUE == VAL__BADR before subtracting it.
*
*     Revision 1.3  1999/06/16 21:10:24  timj
*     Check for bad values
*
*     Revision 1.2  1998/06/17 07:42:56  timj
*     Correct MAX_BOLS typo
*
*     Revision 1.1  1998/04/07 16:44:19  timj
*     Initial revision
*
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
      INCLUDE 'CNF_PAR'               ! For CNF_PVAL function

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
      CHARACTER * (*) METHOD

*  Arguments Returned:
      REAL    OUT_DATA(N_BOL, N_POS)
      BYTE    OUT_QUALITY(N_BOL, N_POS)
      REAL    OUT_VARIANCE(N_BOL, N_POS)

*  Status:
      INTEGER STATUS                 ! Global status

*  External references:

*  Local Constants:

*  Local Variables:
      INTEGER BOL                    ! Loop counter
      INTEGER COUNT                  ! Loop counter
      INTEGER D_END                  ! Data for each scan
      INTEGER D_PTR                  ! End of data for each scan
      REAL    DCVALUE                ! DC offset
      INTEGER EXPOSURE               ! Exposure counter
      INTEGER IERR                   ! For VEC_
      INTEGER INTEGRATION            ! Integration counter
      INTEGER MEASUREMENT            ! Measurement counter
      DOUBLE PRECISION MEAN          ! Mean of scan
      DOUBLE PRECISION MEDIAN        ! Median of scan
      INTEGER NERR                   ! For VEC_
      INTEGER NSIGMA                 ! Number of sigma to clip mean
      INTEGER NGOOD                  ! Number of good points in scan
      INTEGER N_SCAN                 ! Number of samples in a scan
      INTEGER POS                    ! Loop counter
      INTEGER Q_END                  ! Quality for each scan
      INTEGER Q_PTR                  ! End of quality for each scan
      INTEGER QSORT_END              ! End of Scratch space for sort
      INTEGER QSORT_PTR              ! Scratch space for sort
      INTEGER SCAN_END               ! End of scan in array
      INTEGER SCAN_START             ! Start of scan in array
      DOUBLE PRECISION SUM           ! Sum of data in scan
      DOUBLE PRECISION SUMSQ         ! Sum of squares
      DOUBLE PRECISION STDEV         ! Standard deviation

*     Local References:

*.

      IF (STATUS .NE. SAI__OK) RETURN

*     Initialise pointers
      QSORT_PTR = 0
      QSORT_END = 0
      Q_PTR = 0
      Q_END = 0
      D_PTR = 0
      D_END = 0

*     Check the value of METHOD before we do anything more

      IF (METHOD .NE. 'MEDIAN' .AND. METHOD .NE. 'MEAN') THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC('MTD',METHOD)
         CALL ERR_REP(' ','REMOVE_DC_OFFSET: Method ^MTD not '//
     :        'recognised. Should be one of MEAN, MEDIAN', STATUS)
      END IF


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

*     Stop looping if STATUS is bad

               IF (STATUS .EQ. SAI__OK) THEN

                  CALL SCULIB_FIND_SWITCH (DEMOD_POINTER, 1,
     :                 N_EXPOSURES, N_INTEGRATIONS, N_MEASUREMENTS,
     :                 N_POS, 1, EXPOSURE, INTEGRATION, MEASUREMENT,
     :                 SCAN_START, SCAN_END, STATUS)

                  IF ((SCAN_START .EQ. VAL__BADI) .OR.
     :                 (SCAN_START .EQ. 0) ) THEN
                     CALL MSG_SETI ('E', EXPOSURE)
                     CALL MSG_SETI ('I', INTEGRATION)
                     CALL MSG_SETI ('M', MEASUREMENT)
                     CALL MSG_OUTIF (MSG__NORM, ' ',
     :                    'REMOVE_DC_OFFSET: no data '//
     :                    'for exp ^E in int ^I, meas ^M', STATUS)
                  ELSE
*     OK, there is some data for the scan

                     CALL MSG_SETI('NI', INTEGRATION)
                     CALL MSG_SETI('NE', EXPOSURE)
                     CALL MSG_OUTIF(MSG__NORM,' ',
     :                    'REMOVE_DC_OFFSET: Processing exposure'//
     :                    ' ^NE  of integration ^NI', STATUS)


                     N_SCAN = SCAN_END - SCAN_START + 1

*     Create workspace for the statistics calculation
                     CALL SCULIB_MALLOC(N_SCAN * VAL__NBR, QSORT_PTR,
     :                    QSORT_END, STATUS)
                     CALL SCULIB_MALLOC(N_SCAN * VAL__NBR, D_PTR,
     :                    D_END, STATUS)
                     CALL SCULIB_MALLOC(N_SCAN * VAL__NBUB, Q_PTR,
     :                    Q_END, STATUS)


                     DO BOL = 1, N_BOL

*     Copy the scan data to temporary storage
                        COUNT = 0
                        DO POS = SCAN_START, SCAN_END

                           CALL VEC_RTOR(.FALSE., 1,
     :                          IN_DATA(BOL,POS),
     :   %VAL(CNF_PVAL(D_PTR) + COUNT * VAL__NBR),
     :                          IERR, NERR, STATUS)
                           CALL VEC_UBTOUB(.FALSE., 1,
     :                          IN_QUALITY(BOL,POS),
     :   %VAL(CNF_PVAL(Q_PTR) + COUNT * VAL__NBUB),
     :                          IERR, NERR, STATUS)

                           COUNT = COUNT + 1
                        END DO


*     Calculate statistics of this exposure
*     Use 5 sigma clipping by default (make this configurable?)

                        NSIGMA = 5.0
                        CALL SCULIB_STATR(N_SCAN, NSIGMA,
     :                       %VAL(CNF_PVAL(D_PTR)),
     :                       %VAL(CNF_PVAL(Q_PTR)), BADBIT,
     :                       NGOOD, MEAN, MEDIAN, SUM, SUMSQ, STDEV,
     :                       %VAL(CNF_PVAL(QSORT_PTR)), STATUS)


*     Now remove the dc level if DORLB else store the DC level

                        IF (STATUS .EQ. SAI__OK) THEN

*     The value for the DC level depends on method
                           IF (METHOD .EQ. 'MEDIAN') THEN
                              IF (MEDIAN .NE. VAL__BADD) THEN
                                 DCVALUE = REAL(MEDIAN)
                              ELSE
                                 DCVALUE = VAL__BADR
                              END IF
                           ELSE IF (METHOD .EQ. 'MEAN') THEN
                              IF (MEAN .NE. VAL__BADD) THEN
                                 DCVALUE = REAL(MEAN)
                              ELSE
                                 DCVALUE = VAL__BADR
                              END IF
                           END IF

*     Loop over points in scan
                           DO POS = SCAN_START, SCAN_END

*     We are removing the DC level
*     Remove it even if quality is bad
                              IF (DORLB) THEN


                                 IF (IN_DATA(BOL,POS) .NE.
     :                                VAL__BADR) THEN

                                    IF (DCVALUE .NE. VAL__BADR) THEN
                                       OUT_DATA(BOL,POS) =
     :                                      IN_DATA(BOL,POS) - DCVALUE
                                    ELSE
                                       OUT_DATA(BOL,POS) = VAL__BADR
                                    END IF

                                 ELSE
                                    OUT_DATA(BOL,POS) =
     :                                   IN_DATA(BOL,POS)
                                 END IF

                                 OUT_QUALITY(BOL, POS) =
     :                                IN_QUALITY(BOL, POS)
                                 OUT_VARIANCE(BOL, POS) =
     :                                IN_VARIANCE(BOL,POS)


*     Store the fit
                              ELSE

                                 OUT_DATA(BOL, POS) = DCVALUE
                                 OUT_VARIANCE(BOL,POS) = 0.0
                                 OUT_QUALITY(BOL,POS) = 0

                              END IF

                           END DO

                        END IF

                     END DO

*     Free the stats scratch space
                     CALL SCULIB_FREE('QSORT',QSORT_PTR, QSORT_END,
     :                    STATUS)
                     CALL SCULIB_FREE('DATA', D_PTR, D_END,
     :                    STATUS)
                     CALL SCULIB_FREE('QUAL', Q_PTR, Q_END,
     :                    STATUS)

                  END IF

               END IF


            END DO
         END DO
      END DO

      END

