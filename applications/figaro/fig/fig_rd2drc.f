C+
      SUBROUTINE FIG_RD2DRC(FILE,NX,NY,COEFFS,WMIN,WMAX,STATUS)
C
C     F I G _ R D 2 D R C
C
C     Reads a file of 2D arc coefficients, produced by IARC.
C
C     Parameters -  (">" input, "<" output)
C
C     (>) FILE    (Character) The name of the file to be read
C     (>) NX      (Integer) The number of pixels in each line of
C                 the image to be processed using the data read from
C                 the file.  If this does not match the value in
C                 the file, an error condition exists.
C     (>) NY      (Integer) The number of lines in the image to be
C                 processed.  See NX.
C     (<) COEFFS  (Double precision COEFFS(11,NY)) The coefficients
C                 read from the file.
C     (<) WMIN    (Real) The lowest value for channel 1 given from the
C                 various sets of coefficients.
C     (<) WMAX    (Real) The highest value for channel NX given from
C                 the various sets of coefficients.
C     (<) STATUS  (Integer) Return status code.  0 => OK, non-zero
C                 => some error, for which a message will have been
C                 output by this routine.
C
C     Common variables used -  None
C
C     Subroutines / function used -
C
C     ICH_LEN      (ICH_ package) Position of last non-blank char
C     GEN_EPOLYD   (GEN_    "   ) Evaluate double precision polynomial
C     DSA_GET_LU   (VMS standard) Get available logical unit number
C     DSA_FREE_LU  ( "      "   ) Free logical unit number
C
C     Input files -
C
C     2D arcfit file, as produced by IARC.  See IARC for format
C     details.
C
C                                            KS / CIT 9th July 1984
C     Modifications:
C        21 Jul 1993  HME / UoE, Starlink.  Use DSA_*_LU.
C        9  Jun 1999  TDCA / RAL, Starlink. Removed unsupported
C                     keywords from OPEN statements.
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NX, NY, STATUS
      REAL    WMAX, WMIN
      DOUBLE PRECISION COEFFS(11,NY)
      CHARACTER*(*) FILE
C
C     Real value limits
C
      REAL FMAX, FMIN
      PARAMETER (FMAX=1.7E38, FMIN=-1.7E38)
C
C     Functions
C
      INTEGER ICH_LEN
      DOUBLE PRECISION GEN_EPOLYD
C
C     Local variables
C
      INTEGER I, IGNORE, INPUT, IY, IYF, MORD, NCOEFF, NXF, NYF
      CHARACTER STRING*80
C
C     Get logical unit number, and open file
C
      IGNORE=0
      CALL DSA_GET_LU(INPUT,IGNORE)
      OPEN (UNIT=INPUT,FILE=FILE,STATUS='OLD',
     :                                     IOSTAT=STATUS)
      IF (STATUS.NE.0) THEN
         STRING='Unable to open file '//FILE(:ICH_LEN(FILE))
         CALL PAR_WRUSER(STRING,STATUS)
         GO TO 600
      END IF
C
C     Read and check data dimensions
C
      READ (INPUT,'(/17X,I5,4X,I5)',IOSTAT=STATUS) NXF,NYF
      IF (STATUS.NE.0) THEN
         CALL PAR_WRUSER(
     :           'I/O error reading dimensions from file',STATUS)
         GO TO 600
      END IF
      IF ((NX.NE.NXF).OR.(NY.NE.NYF)) THEN
         CALL PAR_WRUSER(
     :           '2D arc fit is for image with dimensions that',STATUS)
         CALL PAR_WRUSER(
     :           'do not match those of image being processed.',STATUS)
         GO TO 600
      END IF
C
C     Read order of coefficients, and then coefficients
C
      READ (INPUT,'(//33X,I3)',IOSTAT=STATUS) MORD
      IF (STATUS.NE.0) THEN
         CALL PAR_WRUSER('I/O error reading coefficient order',STATUS)
         GO TO 600
      END IF
      IF ((MORD.LT.0).OR.(MORD.GT.10)) THEN
         CALL PAR_WRUSER('File specifies invalid order for fits',STATUS)
         GO TO 600
      END IF
      WMIN=FMAX
      WMAX=FMIN
      DO IY=1,NY
         DO I=1,11
            COEFFS(I,IY)=0.
         END DO
         IF (MORD.LE.1) THEN
            READ (INPUT,'(I14,10X,2D24.16)',IOSTAT=STATUS)
     :                                IYF,(COEFFS(I,IY),I=1,MORD+1)
         ELSE
            READ (INPUT,'(I14,10X,2D24.16,3(/3D24.16))',IOSTAT=STATUS)
     :                                IYF,(COEFFS(I,IY),I=1,MORD+1)
         END IF
         IF (STATUS.NE.0) THEN
            CALL PAR_WRUSER('I/O error reading coefficients',STATUS)
            GO TO 600
         END IF
         IF (IY.NE.IYF) THEN
            CALL PAR_WRUSER(
     :        'Internal error in arc file. Row number out of sequence',
     :                                                         STATUS)
            GO TO 600
         END IF
         NCOEFF=1
         DO I=2,11
            IF (COEFFS(I,IY).NE.0.) NCOEFF=I
         END DO
         WMAX=MAX(DBLE(WMAX),GEN_EPOLYD(DBLE(NX),COEFFS(1,IY),NCOEFF))
         WMIN=MIN(DBLE(WMIN),GEN_EPOLYD(1.0D0,COEFFS(1,IY),NCOEFF))
      END DO
C
C     All read without error, return OK status and tidy up.
C
      STATUS=0
      GO TO 630
C
C     Error exit
C
  600 CONTINUE
      STATUS=1
C
C     Exit
C
  630 CONTINUE
      CLOSE (UNIT=INPUT,IOSTAT=IGNORE)
      IGNORE=0
      CALL DSA_FREE_LU(INPUT,IGNORE)
C
      END
