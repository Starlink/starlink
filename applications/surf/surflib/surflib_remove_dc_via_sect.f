      SUBROUTINE SURFLIB_REMOVE_DC_VIA_SECT(DORLB, N_SPEC, SECTION,
     :     USE_SECT, N_EXPOSURES, N_INTEGRATIONS, N_MEASUREMENTS,
     :     DEMOD_POINTER,N_BOL, N_POS, IN_DATA, IN_VARIANCE, IN_QUALITY,
     :     OUT_DATA, OUT_VARIANCE, OUT_QUALITY, BADBIT, STATUS)
*+
*  Name:
*     SURFLIB_REMOVE_DC_VIA_SECT

*  Purpose:
*     Remove median baseline from each exposure.

*  Invocation:
*     CALL SCULIB_REMOVE_DC_FROM_EXP(DORLB,N_SPEC,SECTION,USE_SECT,N_EXPOSURES,
*    :     N_INTEGRATIONS, N_MEASUREMENTS, METHOD, DEM_PNTR, N_BOL, N_POS,
*    :     IN_DATA, IN_QUALITY, SAMPLE_DX, CHOP_THROW, OUT_DATA,
*    :     OUT_QUALITY, BADBIT, STATUS)

*  Description:
*     This routine takes a data array and removes a
*     DC offset from each scan. The DC level is the median of the
*     data specified in the SCUBA section for that particular integration.
*     The same level is removed for each scan of an integration
*     with a different value for each bolometer.

*  Arguments:
*     DORLB                       = LOGICAL (Given)
*           control whether we are subtracting the baseline (TRUE)
*           or storing the basline (FALSE)
*     N_SPEC                      = INTEGER (Given)
*           Number of sections specified
*     SECTION ( N_SPEC )          = CHAR (Given)
*           Array of section specifications
*     USE_SECT                    = LOGICAL (Given)
*           Is this an inverse section
*     N_EXPOSURES                 = INTEGER (Given)
*           maximum number of exposures per integration
*     N_INTEGRATIONS              = INTEGER (Given)
*           number of integrations in the observation
*     N_MEASUREMENTS              = INTEGER (Given)
*           number of measurements in the observation
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
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     1998 April 7 (TIMJ):
*       Original version
*     $Log$
*     Revision 1.4  2004/09/01 01:02:03  timj
*     use CNF_PVAL
*
*     Revision 1.3  1999/08/03 19:32:53  timj
*     Add copyright message to header.
*
*     Revision 1.2  1999/07/17 02:48:49  timj
*     Check for DCVALUE == VAL__BADR before subtracting it.
*
*     Revision 1.1  1999/06/16 21:10:31  timj
*     First version
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
      LOGICAL USE_SECT
      INTEGER N_SPEC
      CHARACTER*(*) SECTION(N_SPEC)
      INTEGER N_BOL
      INTEGER N_EXPOSURES
      INTEGER N_INTEGRATIONS
      INTEGER N_MEASUREMENTS
      INTEGER N_POS
      INTEGER DEMOD_POINTER(N_EXPOSURES, N_INTEGRATIONS, N_MEASUREMENTS)
      REAL    IN_DATA(N_BOL, N_POS)
      BYTE    IN_QUALITY(N_BOL, N_POS)
      REAL    IN_VARIANCE(N_BOL, N_POS)

*  Arguments Returned:
      REAL    OUT_DATA(N_BOL, N_POS)
      BYTE    OUT_QUALITY(N_BOL, N_POS)
      REAL    OUT_VARIANCE(N_BOL, N_POS)

*  Status:
      INTEGER STATUS                 ! Global status

*  External references:
      LOGICAL SCULIB_BITTEST

*  Local Constants:

*  Local Variables:
      INTEGER BITNUM                 ! Bit number
      INTEGER BOL                    ! Loop counter
      BYTE    BTEMP                  ! Temporary byte
      INTEGER COUNT                  ! Loop counter
      INTEGER D_END                  ! Data for each scan
      INTEGER D_PTR                  ! End of data for each scan
      REAL    DCVALUE                ! DC offset
      INTEGER I                      ! Loop counter
      INTEGER IERR                   ! For VEC_
      INTEGER INTEGRATION            ! Integration counter
      INTEGER MASK_END               ! End of mask array
      INTEGER MASK_PTR               ! Mask byte array
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

*     Check that N_SPEC is greater than 0.
      IF (N_SPEC .LE. 0) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP(' ','REMOVE_DC_VIA_SECT: No section specified',
     :        STATUS)
      END IF

*     Fill the output array with the values from the input array.
*     This allows us to have some values set even if the points are not
*     good and a baseline can not be fitted - or no section has
*     been specified for that particular integration

      DO BOL = 1, N_BOL
         DO POS = 1, N_POS
            OUT_DATA(BOL,POS) = IN_DATA(BOL,POS)
            OUT_QUALITY(BOL, POS) = IN_QUALITY(BOL, POS)
            OUT_VARIANCE(BOL, POS) = IN_VARIANCE(BOL,POS)
         END DO
      END DO

*     Get some memory for a mask array
      MASK_PTR = 0
      MASK_END = 0

      CALL SCULIB_MALLOC( N_POS * N_BOL * VAL__NBUB, MASK_PTR,
     :     MASK_END, STATUS)

*     We are now going to merge the data quality array with
*     the supplied section. We have to make sure that we set bits
*     that are already pointing to bad quality via a BADBIT mask.
*     We do not want to merge the two arrays blindly since this
*     may lead to us switching off data that should be on.

*     First step is to look at the badbit mask and come up with
*     a bitnumber that corresponds to an exisiting bad bit.

      BITNUM = -1  ! No badbit
      DO I = 0, 7

         IF ( BITNUM .EQ. -1 .AND. SCULIB_BITTEST( BADBIT, I) ) THEN
            BITNUM = I
         END IF

      END DO

*     If BITNUM is still -1 that means there was no valid quality
*     in this case we do not want to bother copying the quality in
*     just use the raw mask information and set the local badbit
*     mask to 1.

      IF (BITNUM .LT. 0) THEN

*     Fill the mask  with 0
         BTEMP = 0
         IF (STATUS .EQ. SAI__OK) THEN
            CALL SCULIB_CFILLB(N_POS * N_BOL, BTEMP,
     :           %VAL(CNF_PVAL(MASK_PTR)))
         END IF

*     Set BITNUM to 0 and badbit to 1
         BITNUM = 0
         BADBIT = 1

      ELSE

*     Copy the QUALITY array to the mask prior to merging with the section
*     Do not need to change bitnum or badbit.
         CALL VEC_UBTOUB(.FALSE., N_POS * N_BOL, OUT_QUALITY,
     :        %VAL(CNF_PVAL(MASK_PTR)), IERR, NERR, STATUS)

      END IF

*     Now decode the section such that there is a 1 wherever no
*     section was specified and a 0 where good baseline data can
*     be found. This means we have to invert the selected section
*     We end up with a merger of the original quality array and
*     the new section mask

      BTEMP = 1  ! Not needed

      CALL SCULIB_MASK_DATA(.NOT.USE_SECT, 'BIT', N_SPEC,
     :     SECTION, DEMOD_POINTER, 1, N_EXPOSURES, N_INTEGRATIONS,
     :     N_MEASUREMENTS, N_POS, N_BOL, 1, .FALSE., 0.0, BTEMP,
     :     BITNUM, .TRUE., MASK_PTR, STATUS)


*     Cycle through each integration

      DO MEASUREMENT = 1, N_MEASUREMENTS
         DO INTEGRATION = 1, N_INTEGRATIONS

*     Stop looping if STATUS is bad

            IF (STATUS .EQ. SAI__OK) THEN

*     Get the start and end of the integration
               CALL SCULIB_FIND_INT( DEMOD_POINTER, 1,
     :              N_EXPOSURES, N_INTEGRATIONS, N_MEASUREMENTS,
     :              N_POS, INTEGRATION, MEASUREMENT, SCAN_START,
     :              SCAN_END, STATUS)

               IF ((SCAN_START .EQ. VAL__BADI) .OR.
     :              (SCAN_START .EQ. 0) ) THEN
                  CALL MSG_SETI ('I', INTEGRATION)
                  CALL MSG_SETI ('M', MEASUREMENT)
                  CALL MSG_OUTIF (MSG__NORM, ' ',
     :                 'REMOVE_DC_OFFSET_SECTION: no data '//
     :                 'for int ^I, meas ^M', STATUS)
               ELSE


*     OK, there is some data for the scan

                  CALL MSG_SETI('NI', INTEGRATION)
                  CALL MSG_OUTIF(MSG__NORM,' ',
     :                 'REMOVE_DC_OFFSET_SECTION: Processing'//
     :                 ' integration ^NI', STATUS)


*     Now we need to find out the statistics of the masked
*     data for this bolometer and integration
*     This is easy since we can use the mask array as a merged
*     quality array

                  N_SCAN = SCAN_END - SCAN_START + 1

*     Create workspace for the statistics calculation
                  CALL SCULIB_MALLOC(N_SCAN * VAL__NBR, QSORT_PTR,
     :                 QSORT_END, STATUS)
                  CALL SCULIB_MALLOC(N_SCAN * VAL__NBR, D_PTR,
     :                 D_END, STATUS)
                  CALL SCULIB_MALLOC(N_SCAN * VAL__NBUB, Q_PTR,
     :                 Q_END, STATUS)

                  DO BOL = 1, N_BOL

*     Copy the scan data to temporary storage
*     This is made more complicated since we have to copy the
*     quality data from the mask via a pointer.
                     COUNT = 0
                     DO POS = SCAN_START, SCAN_END

                        CALL VEC_RTOR(.FALSE., 1,
     :                       IN_DATA(BOL,POS),
     :                       %VAL(CNF_PVAL(D_PTR) + COUNT * VAL__NBR),
     :                       IERR, NERR, STATUS)
                        CALL VEC_UBTOUB(.FALSE., 1,
     :                       %VAL(CNF_PVAL(MASK_PTR) + ( N_BOL * (POS-1)
     :                       + BOL - 1) * VAL__NBUB),
     :                       %VAL(CNF_PVAL(Q_PTR) + COUNT * VAL__NBUB),
     :                       IERR, NERR, STATUS)

                        COUNT = COUNT + 1
                     END DO


*     Calculate statistics of this exposure
*     Use 3 sigma clipping by default (make this configurable?)


                     CALL SCULIB_STATR(N_SCAN, NSIGMA,
     :                    %VAL(CNF_PVAL(D_PTR)), %VAL(CNF_PVAL(Q_PTR)),
     :                    BADBIT,
     :                    NGOOD, MEAN, MEDIAN, SUM, SUMSQ, STDEV,
     :                    %VAL(CNF_PVAL(QSORT_PTR)), STATUS)


*     Now remove the dc level if DORLB else store the DC level

                     IF (STATUS .EQ. SAI__OK) THEN

*     The value for the DC level depends on method - use MEDIAN for now
                        IF (MEDIAN .NE. VAL__BADD) THEN
                           DCVALUE = REAL(MEDIAN)
                        ELSE
                           DCVALUE = VAL__BADR
                        END IF

*     Loop over points in scan
                        DO POS = SCAN_START, SCAN_END

*     We are removing the DC level
*     Remove it even if quality is bad
                           IF (DORLB) THEN

                              IF (IN_DATA(BOL,POS) .NE.
     :                             VAL__BADR) THEN

                                 IF (DCVALUE .NE. VAL__BADR) THEN
                                    OUT_DATA(BOL,POS) =
     :                                   IN_DATA(BOL,POS) - DCVALUE
                                 ELSE
                                    OUT_DATA(BOL,POS) = VAL__BADR
                                 END IF

                              ELSE
                                 OUT_DATA(BOL,POS) =
     :                                IN_DATA(BOL,POS)
                              END IF

                              OUT_QUALITY(BOL, POS) =
     :                             IN_QUALITY(BOL, POS)
                              OUT_VARIANCE(BOL, POS) =
     :                             IN_VARIANCE(BOL,POS)


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
     :                 STATUS)
                  CALL SCULIB_FREE('DATA', D_PTR, D_END,
     :                 STATUS)
                  CALL SCULIB_FREE('QUAL', Q_PTR, Q_END,
     :                 STATUS)

               END IF

            END IF


         END DO

      END DO

*     Free the mask
      CALL SCULIB_FREE('MASK', MASK_PTR, MASK_END, STATUS)

      END

