      SUBROUTINE RED3_CGS3_DET (STATUS)
C+
C     R E D 3 _ C G S 3 _ D E T
C
C     Reduces a UKIRT CGS3 wavelength calibration data file created by the
C     CALCGS3 mode out of its original file to create a detector calibration
C     spectrum from it.
C
C     Command parameters -
C
C     FILE    (Character) The name of the input file
C     DETECTOR (Integer) The number of the detector use.
C     STARTSCAN (Integer) Start scan(cycle) to extract
C     ENDSCAN (Integer) Start End scan(cycle) to extract
C     OUTPUT  (Character) The name of the spectrum file to be created.
C
C     Command keywords - None
C
C     User variables used - None
C
C                                      AB / JAC  28th Sep 1990
C     History:
C      28-Sep-90: Original (JAC::AB)
C      30-Nov-95: remove adamdefns, adamerrs, add sae_par for unix porting (KK)
*      27-Feb-96: rename from red4_
C
C+
      IMPLICIT NONE
C
C     Global constants
C
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
      INTEGER NWAVE, NCYC, NSPEC
      INTEGER IPTR, OPTR, OVPTR, AIPTR, AOPTR, SUMPTR, SUMSQPTR
      INTEGER SLOT
      INTEGER STARTSCAN, ENDSCAN, DETECTOR
      CHARACTER*80 INPUT, OUTPUT


      IF (STATUS .NE. SAI__OK) RETURN
C
C     Initialization of DSA routines
C
      CALL DSA_OPEN(STATUS)
C
C     Get the name of the input file
C
      CALL PAR_GET0C ('INPUT',INPUT,STATUS)
      CALL DSA_NAMED_INPUT ('INPUT',INPUT,STATUS)
C
C     Determine dimensions of input.
C
      CALL DSA_DATA_SIZE ('INPUT',4,NDIM,DIMS,ELEMENTS,STATUS)
      IF ((NDIM .NE. 4) .OR. (DIMS(1) .GT. 1))
     : THEN
         CALL MSG_OUT (' ','Incorrect dimensions, probably not'//
     :    ' correct data', STATUS)
      END IF
      NWAVE = DIMS(2)
      NSPEC = DIMS(3)
      NCYC  = DIMS(4)
      CALL MSG_SETI ('CYC',NCYC)
      CALL MSG_OUT (' ','Data array contains ^CYC cycles', STATUS)
C
C     Get detector number
C
      CALL PAR_GET0I ('DETECTOR',DETECTOR,STATUS)
      IF (DETECTOR .LT. 1) THEN
         CALL MSG_OUT (' ', 'DETECTOR must be >= 1', STATUS)
         STATUS = SAI__ERROR
      END IF
C
C     Get start and end scans
C
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
C
C     Create output structure
C
      CALL PAR_GET0C ('OUTPUT',OUTPUT,STATUS)
      CALL DSA_NAMED_OUTPUT ('OUTPUT',OUTPUT,'INPUT',0,0,STATUS)
C
C     Coerce the shape of the output
C
      CALL DSA_COERCE_DATA_ARRAY ('OUTPUT','FLOAT',1,NSPEC,STATUS)
C
C     Map the input file
C
      CALL DSA_MAP_DATA ('INPUT','READ','FLOAT',IPTR,SLOT,STATUS)
      CALL DSA_MAP_AXIS_DATA ('INPUT',3,'READ','FLOAT',AIPTR,SLOT,
     :   STATUS)
C
C     Create and Map the output file
C
      CALL DSA_MAP_DATA ('OUTPUT','WRITE','FLOAT',OPTR,SLOT,STATUS)
      CALL DSA_MAP_VARIANCE ('OUTPUT','WRITE','FLOAT',OVPTR,SLOT,
     :   STATUS)
      CALL DSA_MAP_AXIS_DATA ('OUTPUT',1,'WRITE','FLOAT',AOPTR,SLOT,
     :   STATUS)
C
C     Get workspace
C
      CALL DSA_GET_WORK_ARRAY (NSPEC,'FLOAT',SUMPTR,SLOT,STATUS)
      CALL DSA_GET_WORK_ARRAY (NSPEC,'FLOAT',SUMSQPTR,SLOT,STATUS)
C
C     Call routine to coadd Right-Lefts and switch round the axis array
C     to increase with array index
C
      IF (STATUS .EQ. SAI__OK) THEN
         CALL CGS3_DET (NWAVE,NSPEC,NCYC,%val(IPTR),%val(AIPTR),
     :      DETECTOR,STARTSCAN,ENDSCAN,%val(OPTR),%val(OVPTR),
     :      %val(AOPTR),%val(SUMPTR),%val(SUMSQPTR))
      ENDIF

      CALL DSA_CLOSE (STATUS)

      END


      SUBROUTINE CGS3_DET (NWAVE, NSPEC, NCYC, IN_DATA, IN_AXIS,
     :   DET, STARTCYC, ENDCYC, OUT_DATA, OUT_VARIANCE, OUT_AXIS,
     :   SUM, SUMSQ)
C
C     Extracts a single detector scan from the array, summing over
C     the requested cycles.
C
C     Author:
C      Alan Bridger (JAC)
C
C     History:
C      28-Sep-90: Original (JAC::AB)
C
C
      IMPLICIT NONE
      INTEGER  NWAVE, NCYC, NSPEC
      INTEGER STARTCYC, ENDCYC, DET
      REAL IN_DATA (1, NWAVE, NSPEC, NCYC)
      REAL IN_AXIS (NSPEC)
      REAL OUT_DATA (NSPEC)
      REAL OUT_VARIANCE (NSPEC)
      REAL OUT_AXIS (NSPEC)
      REAL SUM (NSPEC)
      REAL SUMSQ (NSPEC)

      INTEGER I, J, NCYCLES
C
C     Initialise the outputs
C
      DO I = 1, NSPEC
         OUT_DATA(I) = 0.0
         OUT_VARIANCE(I) = 0.0
         SUM (I) = 0.0
         SUMSQ (I) = 0.0
      END DO
C
C     Loop over the cycles
C
      DO J = STARTCYC, ENDCYC
         DO I = 1, NSPEC
               SUM (I) = SUM (I) + IN_DATA(1,DET,I,J)
               SUMSQ (I) = SUMSQ (I) + IN_DATA(1,DET,I,J)
         END DO
      END DO
C
C     Calculate the average and variance for each point
C
      NCYCLES = ENDCYC - STARTCYC + 1
      DO I = 1, NSPEC
         OUT_DATA(I) = SUM(I) / NCYCLES
         IF (NCYCLES .GT. 1) OUT_VARIANCE (I) =
     :    (SUMSQ(I) - (SUM(I)**2)/NCYCLES)/(NCYCLES * (NCYCLES-1))
         IF (OUT_VARIANCE(I) .LT. 0.0) THEN
            OUT_VARIANCE (I) = 0.0
         END IF
      END DO
C
C     Copy the axis array
C
      DO I = 1, NSPEC
         OUT_AXIS (I) = IN_AXIS (I)
      END DO


      END
