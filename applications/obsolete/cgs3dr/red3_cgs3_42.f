      SUBROUTINE RED3_CGS3_42 (STATUS)
C+
C     R E D 3 _ C G S 3 _ 4 2
C
C     Reduces a UKIRT CGS3 spectroscopy raw data file created by the DRT
C     out of its original file. If the data are ordinary spectroscopy
C     then an image of the individual cycle spectra vs. wavelength is
C     created, if the data are spectropolarimetry then the image is
C     of polarimeter plate position vs. wavelength.
C
C     Command parameters -
C
C     INPUT   (Character) The name of the input file
C     STARTSCAN (Integer) The name of the first scan to use (POL only)
C     ENDSCAN (Integer)   The name of the last scan to use (POL only)
C     OUTPUT  (Character) The name of the spectrum file to be created.
C
C     Command keywords - None
C
C     User variables used - None
C
C                                      AB / JAC  14th Jul 1990
C     History:
C      25-Sep-90: Corrected after bug found in data acquisition software.
C                 The order of the indices in the main data array is now
C                 (platepos, wavelength, beampos, cycles) (JAC::AB)
C       7-Jun-91: Now correctly copy the axis info from input to output (JAC::AB)
C      20-Jun-91: Use DSA_RESHAPE_DATA to get correct y-axis shapes in
C                 output (JAC::AB)
C      18-Nov-91: Modify to include polarimetry option (JAC::AB)
C      18-Nov-91: Remove use of input variance - it is never there (JAC::AB)
C       8-Feb-95: Add NSIGMA and scan rejection. (JAC::AB)
C      30-Nov-95: remove adamdefns, adamerrs, add sae_par for unix porting (KK)
*      27-Feb-96: rename from red4_
C
C+
      IMPLICIT NONE
C
C     ADAM status
C
      INTEGER STATUS
C
C     Global constants
C
      INCLUDE 'SAE_PAR'
C
C     Local variables
C
      INTEGER DIMS(4)
      INTEGER ODIMS(2)
      INTEGER ELEMENTS
      INTEGER NDIM
      INTEGER NWAVE, NCYC, NBEAMS, NPOL, IITEMS, TMPLAT
      INTEGER IPTR, OPTR, OVPTR, AIPTR, AOPTR, A2OPTR,
     :        SUMPTR, SUMSQPTR, SCPTR, SCSPTR, SCSGPTR
      INTEGER SLOT
      INTEGER STARTSCAN, ENDSCAN
      REAL    NSIGMA
      CHARACTER*80 INPUT, OUTPUT
      CHARACTER*40 CITEMS(2)


      IF (STATUS .NE. SAI__OK) RETURN

      CALL DSA_OPEN(STATUS)

C    Get the name of the input file
      CALL PAR_GET0C ('INPUT', INPUT, STATUS)
      CALL DSA_NAMED_INPUT ('INPUT',INPUT,STATUS)

C    Determine dimensions of input.
      CALL DSA_DATA_SIZE ('INPUT', 4, NDIM, DIMS, ELEMENTS,
     : STATUS)
      IF ((NDIM .NE. 4) .OR. (DIMS(3) .GT. 2)) THEN
         CALL MSG_OUT (' ','Incorrect dimensions, probably not'//
     :    ' correct data', STATUS)
         STATUS = SAI__ERROR
      END IF

      NPOL = DIMS(1)
      NWAVE = DIMS(2)
      NBEAMS = DIMS(3)
      NCYC  = DIMS(4)
      IF (NPOL .EQ. 1) THEN
         CALL MSG_OUT (' ','This is spectroscopic data: ', STATUS)
         CALL MSG_OUT (' ','Creating image of wavelength vs. '/
     :    /'cycles', STATUS)
      ELSE
         CALL MSG_OUT (' ','This is spectropolarimetry data:', STATUS)
         CALL MSG_OUT (' ','Creating image of wavelength vs. plate '/
     :    /'position', STATUS)
      END IF
!      CALL MSG_OUT (' ', ' ', STATUS)
!      CALL MSG_SETI ('CYC',NCYC)
!      CALL MSG_OUT (' ','Data array contains ^CYC cycles', STATUS)

      IF (NPOL .GT. 1) THEN

C       Get start and end scans
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
     :       /'must be greater than STARTSCAN', STATUS)
            STATUS = SAI__ERROR
         END IF
         IF (ENDSCAN .GT. NCYC) THEN
            CALL MSG_SETI ('NC', NCYC)
            CALL MSG_OUT (' ','Illegal ENDSCAN, max. is ^NC', STATUS)
            STATUS = SAI__ERROR
         END IF

C       Get NSIGMA
         CALL PAR_GET0R ('NSIGMA', NSIGMA, STATUS)
         IF (NSIGMA .LE. 0.0) THEN
            CALL MSG_OUT (' ', 'Illegal NSIGMA, '/
     :       /'must be greater than 0.0', STATUS)
            STATUS = SAI__ERROR
         END IF
      END IF

C    Create output structure
      CALL PAR_GET0C ('OUTPUT', OUTPUT, STATUS)
      CALL DSA_NAMED_OUTPUT ('OUTPUT', OUTPUT, 'INPUT', 0, 0, STATUS)

C    Coerce the shape of the output
      ODIMS(1) = DIMS(2)
      IF (NPOL .EQ. 1) THEN
         ODIMS(2) = NCYC
         TMPLAT = 4
      ELSE
         ODIMS(2) = NPOL
         TMPLAT = 1
      END IF

      CALL DSA_RESHAPE_DATA('OUTPUT','INPUT',2,ODIMS,STATUS)
      CALL DSA_RESHAPE_AXIS('OUTPUT',1,'INPUT',2,1,ODIMS(1),STATUS)
      CALL DSA_RESHAPE_AXIS('OUTPUT',2,'INPUT',TMPLAT,1,ODIMS(2),
     : STATUS)

C    Map the input file
      CALL DSA_MAP_DATA ('INPUT', 'READ', 'FLOAT', IPTR, SLOT, STATUS)
      IF (NPOL .GT. 1) THEN
         CALL DSA_MAP_VARIANCE ('OUTPUT','WRITE','FLOAT',OVPTR,SLOT,
     :    STATUS)
      END IF
      CALL DSA_MAP_AXIS_DATA ('INPUT', 2, 'READ', 'FLOAT', AIPTR,
     :   SLOT, STATUS)

C    Get the axis2 info from input
      CALL DSA_GET_AXIS_INFO ('INPUT', 2, 2, CITEMS, 0, IITEMS, STATUS)

C    Map the output file
      CALL DSA_MAP_DATA ('OUTPUT', 'WRITE', 'FLOAT', OPTR, SLOT,
     : STATUS)
      CALL DSA_MAP_AXIS_DATA ('OUTPUT', 1, 'WRITE', 'FLOAT', AOPTR,
     :   SLOT, STATUS)

C    Set the axis1 info in output
      CALL DSA_SET_AXIS_INFO ('OUTPUT', 1, 2, CITEMS, 0, IITEMS, STATUS)

C    and modify the axis2 info in output
      IF (NPOL .EQ. 1) THEN
         CITEMS(1) = ' '
         CITEMS(2) = 'Cycle No.'
         CALL DSA_SET_AXIS_INFO ('OUTPUT', 2, 2, CITEMS, 0, IITEMS,
     :    STATUS)

C       Coerce the axis 2 data
         CALL DSA_COERCE_AXIS_DATA ('OUTPUT', 2, 'INT', 1, NCYC, STATUS)
         CALL DSA_MAP_AXIS_DATA ('OUTPUT', 2, 'WRITE', 'INT', A2OPTR,
     :    SLOT, STATUS)
      ELSE
         CITEMS(1) = ' '
         CITEMS(2) = 'Plate Pos.'
         CALL DSA_SET_AXIS_INFO ('OUTPUT', 2, 2, CITEMS, 0, IITEMS,
     :    STATUS)

C       Coerce the axis 2 data
         CALL DSA_COERCE_AXIS_DATA ('OUTPUT', 2, 'INT', 1, NPOL, STATUS)
         CALL DSA_MAP_AXIS_DATA ('OUTPUT', 2, 'WRITE', 'INT', A2OPTR,
     :    SLOT, STATUS)
      END IF

C    Reduce the input data into the output file
      IF (STATUS .EQ. SAI__OK) THEN
         IF (NPOL .EQ. 1) THEN
            CALL CGS3_1 (NWAVE, NBEAMS, NCYC, %val(IPTR),
     :       %val(AIPTR), %val(OPTR), %val(AOPTR), %val(A2OPTR))
         ELSE

C          Get workspace
            CALL DSA_GET_WORK_ARRAY (NWAVE*NPOL,'FLOAT',SUMPTR,SLOT,
     :       STATUS)
            CALL DSA_GET_WORK_ARRAY (NWAVE*NPOL,'FLOAT',SUMSQPTR,SLOT,
     :       STATUS)
            CALL DSA_GET_WORK_ARRAY (NCYC*NPOL,'FLOAT',SCPTR,SLOT,
     :        STATUS)
            CALL DSA_GET_WORK_ARRAY (NPOL,'FLOAT',SCSPTR,SLOT,
     :        STATUS)
            CALL DSA_GET_WORK_ARRAY (NPOL,'FLOAT',SCSGPTR,SLOT,
     :        STATUS)
            CALL CGS3_1P (NWAVE, NBEAMS, NCYC, NPOL, STARTSCAN, ENDSCAN,
     :       %val(IPTR), %val(AIPTR), %val(OPTR),
     :       %val(OVPTR), %val(AOPTR), %val(A2OPTR),%val(SUMPTR),
     :       %val(SUMSQPTR), %val(SCPTR), %val(SCSPTR), %val(SCSGPTR),
     :       NSIGMA)
         END IF
      ENDIF

      CALL DSA_CLOSE (STATUS)


      END

      SUBROUTINE CGS3_1P (NWAVE, NBEAMS, NCYC, NPOL, STARTCYC,
     : ENDCYC, IN_DATA, IN_AXIS, OUT_DATA, OUT_VARIANCE, OUT_AXIS,
     : OUT_AXIS2, SUM, SUMSQ, SCANMN, SCANSM, SCANSG, NSIGMA)
C
C     Author:
C      Alan Bridger (JAC)
C
C     History:
C      18-Nov-91: Original (JAC::AB)
C       8-Feb-95: Add NSIGMA argument and scan rejection (JAC::AB)
C
C
      IMPLICIT NONE
      INTEGER  NWAVE, NCYC, NBEAMS, NPOL, STARTCYC, ENDCYC
      REAL IN_DATA (NPOL, NWAVE, NBEAMS, NCYC)
      REAL OUT_DATA (NWAVE,NPOL)
      REAL OUT_VARIANCE (NWAVE, NPOL)
      REAL IN_AXIS (NWAVE)
      REAL OUT_AXIS (NWAVE)
      REAL SUM(NWAVE,NPOL), SUMSQ(NWAVE,NPOL), SCANMN(NCYC,NPOL),
     :     SCANSM(NPOL), SCANSG(NPOL)
      REAL SCAN_SUMSQ
      REAL NSIGMA
      INTEGER OUT_AXIS2 (NPOL)
      INTEGER N_CYC
      INTEGER ISTAT
      INTEGER I, J, K, NCYCLES

C    Initialise the outputs
      DO J = 1, NPOL
         DO I = 1, NWAVE
            SUM (I,J) = 0.0
            SUMSQ (I,J) = 0.0
         END DO
         DO I = 1, NCYC
            SCANMN (I,J) = 0.0
         END DO
         SCANSM(J) = 0.0
      END DO
      ISTAT = 0

C    Loop over the cycles, coadding the R-Ls into the output array,
C    And while we're at it, reverse the data, as CGS3 delivers it backwards.
      DO J = STARTCYC, ENDCYC
         DO I = 1, NWAVE
            DO K = 1, NPOL
               IF (NBEAMS .EQ. 2) THEN
                  SUM (NWAVE-I+1,K) = SUM (NWAVE-I+1,K) +
     :             (IN_DATA(K,I,1,J) - IN_DATA(K,I,2,J))/2.0
                  SCANMN(J,K) = SCANMN(J,K) +
     :             (IN_DATA(K,I,1,J) - IN_DATA(K,I,2,J))/2.0
                  SUMSQ (NWAVE-I+1,K) = SUMSQ (NWAVE-I+1,K) +
     :             ((IN_DATA(K,I,1,J) - IN_DATA(K,I,2,J))/2.0)**2
               ELSE
                  SUM (NWAVE-I+1,K) = SUM (NWAVE-I+1,K) +
     :             IN_DATA(K,I,1,J)
                  SCANMN(J,K) = SCANMN(J,K) + IN_DATA(K,I,1,J)
                  SUMSQ (NWAVE-I+1,K) = SUMSQ (NWAVE-I+1,K) +
     :             IN_DATA(K,I,1,J)
               END IF
            END DO
         END DO
         DO K = 1, NPOL
            SCANMN(J,K) = SCANMN(J,K)/FLOAT(NWAVE)
            SCANSM(K) = SCANSM(K) + SCANMN(J,K)
         END DO
      END DO

      NCYCLES = ENDCYC - STARTCYC + 1
      DO I = 1, NPOL
         SCANSM(K) = SCANSM(K)/FLOAT(NCYCLES)
      END DO

C    Calculate the average and variance for each point
      DO I = 1, NWAVE
         DO J = 1, NPOL
            OUT_DATA(I,J) = SUM(I,J) / NCYCLES
            IF (NCYCLES .GT. 1) OUT_VARIANCE (I,J) =
     :       (SUMSQ(I,J) - (SUM(I,J)**2)/NCYCLES)/(NCYCLES*(NCYCLES-1))
            IF (OUT_VARIANCE(I,J) .LT. 0.0) THEN
               OUT_VARIANCE (I,J) = 0.0
            END IF
         END DO
      END DO

C    Reverse the axis array
      DO I = 1, NWAVE
         OUT_AXIS(I) = IN_AXIS(NWAVE-I+1)
      END DO

      DO I = 1, NPOL
         OUT_AXIS2(I) = I
      END DO

C    Find the sd. of the scans
      DO K = 1, NPOL
         SCAN_SUMSQ = 0.0
         DO J = STARTCYC, ENDCYC
            SCAN_SUMSQ = SCAN_SUMSQ + (SCANSM(K) - SCANMN(J,K))**2
         END DO
         SCANSG(K) = SQRT(SCAN_SUMSQ/FLOAT(NCYCLES))
      END DO

C    reinitialise the sums
      DO J = 1, NPOL
         DO I = 1, NWAVE
            SUM (I,J) = 0.0
            SUMSQ (I,J) = 0.0
         END DO
      END DO

C    Now loop through rejecting scans with mean values outside the scan mean,
C    also recalculating the new variances.
      DO K = 1, NPOL
         N_CYC = 0
         DO J = STARTCYC, ENDCYC
            IF (SCANMN(J,K) .LE. SCANSM(K)+NSIGMA*SCANSG(K) .AND.
     :       SCANMN(J,K) .GE. SCANSM(K)-NSIGMA*SCANSG(K) ) THEN
               DO I = 1, NWAVE
                  IF (NBEAMS .EQ. 2) THEN
                     SUM (NWAVE-I+1,K) = SUM (NWAVE-I+1,K) +
     :                (IN_DATA(K,I,1,J) - IN_DATA(K,I,2,J))/2.0
                     SUMSQ (NWAVE-I+1,K) = SUMSQ (NWAVE-I+1,K) +
     :                ((IN_DATA(K,I,1,J) - IN_DATA(K,I,2,J))/2.0)**2
                  ELSE
                     SUM (NWAVE-I+1,K) = SUM (NWAVE-I+1,K) +
     :                IN_DATA(K,I,1,J)
                     SUMSQ (NWAVE-I+1,K) = SUMSQ (NWAVE-I+1,K) +
     :                IN_DATA(K,I,1,J)
                  END IF
               END DO
               N_CYC = N_CYC + 1
            ELSE
               CALL MSG_SETI ('POL',K)
               CALL MSG_SETI ('CYC',J)
               CALL MSG_OUT (' ', 'Waveplate position ^POL, '/
     :          /'Cycle ^CYC rejected', ISTAT)
            END IF
         END DO

C       Calculate the average and variance for each point
         IF (N_CYC .GT. 0) THEN
            DO I = 1, NWAVE
               OUT_DATA(I,K) = SUM(I,K) / NCYCLES
               IF (NCYCLES .GT. 1) OUT_VARIANCE (I,K) =
     :          (SUMSQ(I,K) - (SUM(I,K)**2)/NCYCLES)/
     :          (NCYCLES*(NCYCLES-1))
               IF (OUT_VARIANCE(I,K) .LT. 0.0) THEN
                  OUT_VARIANCE (I,K) = 0.0
               END IF
            END DO
         ELSE
            CALL MSG_SETI ('POL',K)
            CALL MSG_OUT (' ', 'Waveplate position ^POL, all scans '/
     :       /'rejected!!', ISTAT)
         END IF
      END DO


      END

      SUBROUTINE CGS3_1 (NWAVE, NBEAMS, NCYC, IN_DATA,
     :   IN_AXIS, OUT_DATA, OUT_AXIS, OUT_AXIS2)
C
C     Author:
C      John Lightfoot (ROE)
C
C     History:
C      17-Jul-90: Original (ROE::JFL)
C      25-Sep-90: Corrected after bug found in data acquisition, order
C                 of indices in data array is now (platepos, wavelength,
C                 beampos, cycles) (JAC::AB)
C      20-Jun-91: Added OUT_AXIS2 (JAC::AB)
C
C
      IMPLICIT NONE
      INTEGER  NWAVE, NCYC, NBEAMS
      REAL IN_DATA (1, NWAVE, NBEAMS, NCYC)
      REAL OUT_DATA (NWAVE,NCYC)
      REAL IN_AXIS (NWAVE)
      REAL OUT_AXIS (NWAVE)
      INTEGER OUT_AXIS2 (NCYC)

      INTEGER I, J
C
C     Initialise the outputs
C
      DO J = 1, NCYC
         DO I = 1, NWAVE
            OUT_DATA(I,J) = 0.0
         END DO
      END DO
C
C     Loop over the cycles, coadding the R-Ls into the output array,
C     And while we're at it, reverse the data, as CGS3 delivers it backwards.
C
      DO J = 1, NCYC
         DO I = 1, NWAVE
            IF (NBEAMS .EQ. 2 ) THEN
               OUT_DATA(NWAVE-I+1,J) =
     :            (IN_DATA(1,I,1,J)-IN_DATA(1,I,2,J))/2.0
            ELSE
               OUT_DATA(NWAVE-I+1,J) = IN_DATA(1,I,1,J)
            END IF
         END DO
         OUT_AXIS2(J) = J
      END DO
C
C     Reverse the axis array
C
      DO I = 1, NWAVE
         OUT_AXIS(I) = IN_AXIS(NWAVE-I+1)
      END DO

      END
