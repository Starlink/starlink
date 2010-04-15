      SUBROUTINE RED3_CGS3_41 (STATUS)
C+
C     R E D 3 _ C G S 3 _ 4 1
C
C     Reduces a UKIRT CGS3 spectroscopy raw data file created by the DRT
C     out of its original file to create a spectrum from it.
C
C     Command parameters -
C
C     FILE    (Character) The name of the input file
C     STARTSCAN (Integer) The name of the first scan to use.
C     ENDSCAN (Integer)   The name of the last scan to use.
C     OUTPUT  (Character) The name of the spectrum file to be created.
C
C     Command keywords - None
C
C     User variables used - None
C
C                                      AB / JAC  14th Jul 1990
C     History:
C      14-Jul-90: Add Start and End scans. (JAC::AB)
C      15-Jul-90: Turned into an ADAM A-task
C      25-Sep-90: A bug fix in the data acquisition software
C                 forced a change here. The order of the
C                 indices in the 4-dimensional data array is now
C                 (platepos, wavelength, beampos, cycles). (JAC::AB)
C       7-Jun-91: Now correctly copy the axis info from input to output
C                 (JAC::AB)
C      20-Jun-91: Use DSA_RESHAPE_DATA to make sure Y-axis does not
C                 appear in output. (JAC::AB)
C       7-Nov-91: If ENDSCAN given as 0 use Maximum instead (JAC::AB)
C       8-Jan-93: Add NSIGMA and scan rejection. (JAC::AB)
C      30-Nov-95: remove adamdefns, adamerrs for unix porting (KK)
*      27-Feb-96: rename from red4_
C
C+
      IMPLICIT NONE
C
C     Global constants

      INCLUDE 'SAE_PAR'

C
C     ADAM status
C
      INTEGER STATUS
C
C     Local variables
C
      INTEGER DIMS(4)
      INTEGER ELEMENTS
      INTEGER NDIM
      INTEGER NWAVE, NCYC, NBEAMS, IITEMS
      INTEGER IPTR, OPTR, OVPTR, AIPTR, AOPTR, SUMPTR, SUMSQPTR, SCPTR
      INTEGER SLOT
      INTEGER STARTSCAN, ENDSCAN
      REAL    NSIGMA
      CHARACTER*80 INPUT, OUTPUT
      CHARACTER*40 CITEMS(2)


      IF (STATUS .NE. SAI__OK) RETURN

C    Initialization of DSA routines
      CALL DSA_OPEN(STATUS)

C    Get the name of the input file
      CALL PAR_GET0C ('INPUT',INPUT,STATUS)
      CALL DSA_NAMED_INPUT ('INPUT',INPUT,STATUS)

C    Determine dimensions of input.
      CALL DSA_DATA_SIZE ('INPUT',4,NDIM,DIMS,ELEMENTS,STATUS)
      IF ((NDIM .NE. 4) .OR. (DIMS(1) .GT. 1) .OR. (DIMS(3) .GT. 2))
     : THEN
         CALL MSG_OUT (' ','Incorrect dimensions, probably not'//
     :    ' correct data', STATUS)
      END IF
      NWAVE = DIMS(2)
      NBEAMS = DIMS(3)
      NCYC  = DIMS(4)
!      CALL MSG_SETI ('CYC',NCYC)
!      CALL MSG_OUT (' ','Data array contains ^CYC cycles', STATUS)

C    Get start and end scans
      CALL PAR_GET0I ('STARTSCAN',STARTSCAN,STATUS)
      IF (STARTSCAN .LT. 1) THEN
         CALL MSG_OUT (' ', 'STARTSCAN must be >= 1', STATUS)
         STATUS = SAI__ERROR
      END IF
      IF (STARTSCAN .GT. NCYC) THEN
         CALL MSG_SETI ('NC', NCYC)
         CALL MSG_OUT (' ','Illegal STARTSCAN, max. is ^NC', STATUS)
         STATUS = SAI__ERROR
      END IF
      CALL PAR_GET0I ('ENDSCAN',ENDSCAN,STATUS)
      IF (ENDSCAN .EQ. 0) ENDSCAN = NCYC
      IF (ENDSCAN .LT. STARTSCAN) THEN
         CALL MSG_OUT (' ', 'Illegal ENDSCAN, '/
     :    /'must be greater than STARTSCAN', STATUS)
         STATUS = SAI__ERROR
      END IF
      IF (ENDSCAN .GT. NCYC) THEN
         CALL MSG_SETI ('NC', NCYC)
         CALL MSG_OUT (' ','Illegal ENDSCAN, max. is ^NC', STATUS)
         STATUS = SAI__ERROR
      END IF

C    Get NSIGMA
      CALL PAR_GET0R ('NSIGMA', NSIGMA, STATUS)
      IF (NSIGMA .LE. 0.0) THEN
         CALL MSG_OUT (' ', 'Illegal NSIGMA, '/
     :    /'must be greater than 0.0', STATUS)
         STATUS = SAI__ERROR
      END IF

C    Get output name
      CALL PAR_GET0C ('OUTPUT',OUTPUT,STATUS)

C    Get output and coerce the shape of the output
      CALL DSA_NAMED_OUTPUT ('OUTPUT',OUTPUT,'INPUT',0,0,STATUS)
      CALL DSA_RESHAPE_DATA('OUTPUT','INPUT',1,NWAVE,STATUS)
      CALL DSA_RESHAPE_AXIS('OUTPUT',1,'INPUT',2,1,NWAVE,STATUS)

C    Map the input file
      CALL DSA_MAP_DATA ('INPUT','READ','FLOAT',IPTR,SLOT,STATUS)
      CALL DSA_MAP_AXIS_DATA ('INPUT',2,'READ','FLOAT',AIPTR,SLOT,
     :   STATUS)

C    Get the axis2 info from input
      CALL DSA_GET_AXIS_INFO ('INPUT', 2, 2, CITEMS, 0, IITEMS, STATUS)

C    Create and Map the output file
      CALL DSA_MAP_DATA ('OUTPUT','WRITE','FLOAT',OPTR,SLOT,STATUS)
      CALL DSA_MAP_VARIANCE ('OUTPUT','WRITE','FLOAT',OVPTR,SLOT,
     :   STATUS)
      CALL DSA_MAP_AXIS_DATA ('OUTPUT',1,'WRITE','FLOAT',AOPTR,SLOT,
     :   STATUS)

C    Set the axis1 info in output
      CALL DSA_SET_AXIS_INFO ('OUTPUT', 1, 2, CITEMS, 0, IITEMS, STATUS)

C    Get workspace
      CALL DSA_GET_WORK_ARRAY (NWAVE,'FLOAT',SUMPTR,SLOT,STATUS)
      CALL DSA_GET_WORK_ARRAY (NWAVE,'FLOAT',SUMSQPTR,SLOT,STATUS)
      CALL DSA_GET_WORK_ARRAY (NCYC,'FLOAT',SCPTR,SLOT,STATUS)

C    Call routine to coadd Right-Lefts and switch round the axis array
C    to increase with array index
      IF (STATUS .EQ. SAI__OK) THEN
         CALL CGS3_2 (NWAVE,NBEAMS,NCYC,%val(IPTR),%val(AIPTR),
     :      STARTSCAN,ENDSCAN,%val(OPTR),%val(OVPTR),%val(AOPTR),
     :      %val(SUMPTR),%val(SUMSQPTR),%val(SCPTR),NSIGMA)
      ENDIF

      CALL DSA_CLOSE (STATUS)


      END

      SUBROUTINE CGS3_2 (NWAVE, NBEAMS, NCYC, IN_DATA, IN_AXIS,
     :   STARTCYC, ENDCYC, OUT_DATA, OUT_VARIANCE, OUT_AXIS, SUM,
     :   SUMSQ, SCANMN, NSIGMA)
C
C     Calculate the Right-Lefts from the input and coadd over the
C     various cycles. Also calculate the errors from the R-Ls and
C     coadds. If single beam data then just takes from the Right.
C     Second pass rejects scans which have a mean more than NSIGMA
C     S.D.'s from the mean of the scans.
C
C     Author:
C      Alan Bridger (JAC)
C
C     History:
C      13-Jul-90: Original (JAC::AB)
C      21-Jul-90: Modified to use variance (ROE::JFL)
C      25-Sep-90: Bug fix in data acquisition software forced a
C                 change here. Indices of 4-dimensional data array
C                 are now (platepos, wavelength, beampos, cycles) (JAC::AB)
C       8-Jan-93: Add NSIGMA rejection (JAC::AB)
C
C
      IMPLICIT NONE
      INTEGER  NWAVE, NCYC, NBEAMS
      INTEGER STARTCYC, ENDCYC, ISTAT
      REAL IN_DATA (1, NWAVE, NBEAMS, NCYC)
      REAL IN_AXIS (NWAVE)
      REAL OUT_DATA (NWAVE)
      REAL OUT_VARIANCE (NWAVE)
      REAL OUT_AXIS (NWAVE)
      REAL SUM (NWAVE)
      REAL SUMSQ (NWAVE)
      REAL SCANMN (NCYC)
      REAL NSIGMA, SCAN_SUM, SCAN_MEAN, SCAN_SIGMA, SCAN_SUMSQ
      INTEGER I, J, NCYCLES, N_CYC


C    Initialise the sums
      DO I = 1, NWAVE
         SUM (I) = 0.0
         SUMSQ (I) = 0.0
      END DO
      ISTAT = 0

C    Loop over the cycles, coadding the R-Ls into the SUM array,
C    And while we're at it, reverse the data, as CGS3 delivers it backwards.
      SCAN_SUM = 0.0
      DO J = STARTCYC, ENDCYC
         SCANMN(J) = 0.0
         DO I = 1, NWAVE
            IF (NBEAMS .EQ. 2) THEN
               SUM (NWAVE-I+1) = SUM (NWAVE-I+1) +
     :                   (IN_DATA(1,I,1,J) - IN_DATA(1,I,2,J))/2.0
               SCANMN(J) = SCANMN(J) +
     :                (IN_DATA(1,I,1,J) - IN_DATA(1,I,2,J))/2.0
               SUMSQ (NWAVE-I+1) = SUMSQ (NWAVE-I+1) +
     :                   ((IN_DATA(1,I,1,J) - IN_DATA(1,I,2,J))/2.0)**2
            ELSE
               SUM (NWAVE-I+1) = SUM (NWAVE-I+1) + IN_DATA(1,I,1,J)
               SCANMN(J) = SCANMN(J) + IN_DATA(1,I,1,J)
               SUMSQ (NWAVE-I+1) = SUMSQ (NWAVE-I+1) + IN_DATA(1,I,1,J)
            END IF
         END DO
         SCANMN(J) = SCANMN(J)/FLOAT(NWAVE)
         SCAN_SUM = SCAN_SUM + SCANMN(J)
      END DO
      NCYCLES = ENDCYC - STARTCYC + 1
      SCAN_MEAN = SCAN_SUM/FLOAT(NCYCLES)

C    Calculate the average and variance for each point
      DO I = 1, NWAVE
         OUT_DATA(I) = SUM(I) / NCYCLES
         IF (NCYCLES .GT. 1) OUT_VARIANCE (I) =
     :    (SUMSQ(I) - (SUM(I)**2)/NCYCLES)/(NCYCLES * (NCYCLES-1))
         IF (OUT_VARIANCE(I) .LT. 0.0) THEN
            OUT_VARIANCE (I) = 0.0
         END IF
      END DO

C    Reverse the axis arrays
      DO I = 1, NWAVE
         OUT_AXIS (NWAVE-I+1) = IN_AXIS (I)
      END DO

C    Find the sd. of the scans
      SCAN_SUMSQ = 0.0
      DO J = STARTCYC, ENDCYC
         SCAN_SUMSQ = SCAN_SUMSQ + (SCAN_MEAN - SCANMN(J))**2
      END DO
      SCAN_SIGMA = SQRT(SCAN_SUMSQ/FLOAT(NCYCLES))

C    Now loop through rejecting scans with mean values outside the scan mean,
C    also recalculating the new variances.
C    Re-Initialise the sums
      DO I = 1, NWAVE
         SUM (I) = 0.0
         SUMSQ (I) = 0.0
      END DO

      N_CYC = 0
      DO J = STARTCYC, ENDCYC
         IF (SCANMN(J) .LE. SCAN_MEAN+NSIGMA*SCAN_SIGMA .AND.
     :    SCANMN(J) .GE. SCAN_MEAN-NSIGMA*SCAN_SIGMA ) THEN
            DO I = 1, NWAVE
               IF (NBEAMS .EQ. 2) THEN
                  SUM (NWAVE-I+1) = SUM (NWAVE-I+1) +
     :             (IN_DATA(1,I,1,J) - IN_DATA(1,I,2,J))/2.0
                  SUMSQ (NWAVE-I+1) = SUMSQ (NWAVE-I+1) +
     :             ((IN_DATA(1,I,1,J) - IN_DATA(1,I,2,J))/2.0)**2
               ELSE
                  SUM (NWAVE-I+1) = SUM (NWAVE-I+1) + IN_DATA(1,I,1,J)
                  SUMSQ (NWAVE-I+1) = SUMSQ (NWAVE-I+1) +
     :             IN_DATA(1,I,1,J)
               END IF
            END DO
            N_CYC = N_CYC + 1
         ELSE
            CALL MSG_SETI ('CYC',J)
            CALL MSG_OUT (' ', 'Cycle ^CYC rejected', ISTAT)
         END IF
      END DO

      IF (N_CYC .GT. 0) THEN
         DO I = 1, NWAVE
            OUT_DATA(I) = SUM(I) / N_CYC
            IF (N_CYC .GT. 1) OUT_VARIANCE (I) =
     :       (SUMSQ(I) - (SUM(I)**2)/N_CYC)/(N_CYC * (N_CYC-1))
            IF (OUT_VARIANCE(I) .LT. 0.0) THEN
               OUT_VARIANCE (I) = 0.0
            END IF
         END DO
      ELSE
         CALL MSG_OUT (' ', 'All scans rejected!!', ISTAT)
      END IF


      END
