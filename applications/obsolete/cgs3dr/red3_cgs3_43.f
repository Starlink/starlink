      SUBROUTINE RED3_CGS3_43 (STATUS)
C+
C     R E D 3 _ C G S 3 _ 4 3
C
C     Reduces a UKIRT CGS3 spectroscopy raw data file created by the DRT
C     out of its original file to create a cube of the individual
C     cycle spectra, for polarimetry data only.
C     Produces a cube of wavelength (axis 1) vs. cycle number (2) vs
C     polarimeter plate position (3).
C
C     Command parameters -
C
C     INPUT   (Character) The name of the input file
C     OUTPUT  (Character) The name of the spectrum file to be created.
C
C     Command keywords - None
C
C     User variables used - None
C
C                                      AB / JAC  18th Nov 1991
C     History:
C      18-Nov-91: Original (JAC::AB)
C      30-Nov-95: remove adamdefns, adamerrs, add sae_par for unix porting (KK
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
      LOGICAL VARIANCE
      INTEGER DIMS(4)
      INTEGER ODIMS(3)
      INTEGER ELEMENTS
      INTEGER NDIM
      INTEGER NWAVE, NCYC, NBEAMS, NPOL, IITEMS
      INTEGER IPTR, OPTR, IVPTR, OVPTR, AIPTR, AOPTR, A2OPTR
      INTEGER SLOT
      CHARACTER*80 INPUT, OUTPUT
      CHARACTER*40 CITEMS(2)


      IF (STATUS .NE. SAI__OK) RETURN

      CALL DSA_OPEN(STATUS)
C
C     Get the name of the input file
C
      CALL PAR_GET0C ('INPUT', INPUT, STATUS)
      CALL DSA_NAMED_INPUT ('INPUT',INPUT,STATUS)
C
C     Determine dimensions of input.
C
      CALL DSA_DATA_SIZE ('INPUT', 4, NDIM, DIMS, ELEMENTS,
     : STATUS)
      IF ((NDIM .NE. 4) .OR. (DIMS(1) .NE. 4) .OR. (DIMS(3) .GT. 2))
     : THEN
         CALL MSG_OUT (' ','Incorrect dimensions, probably not'//
     :    ' correct data', STATUS)
         STATUS = SAI__ERROR
      END IF
      NPOL = DIMS(1)
      NWAVE = DIMS(2)
      NBEAMS = DIMS(3)
      NCYC  = DIMS(4)
      CALL MSG_SETI ('CYC',NCYC)
      CALL MSG_OUT (' ','Data array contains ^CYC cycles', STATUS)
C
C     Look for error array
C
      CALL DSA_SEEK_ERRORS ('INPUT', VARIANCE, STATUS)
C
C     Create output structure
C
      CALL PAR_GET0C ('OUTPUT', OUTPUT, STATUS)
      CALL DSA_NAMED_OUTPUT ('OUTPUT', OUTPUT, 'INPUT', 0, 0, STATUS)
C
C     Coerce the shape of the output
C
      ODIMS(1) = NWAVE
      ODIMS(2) = NCYC
      ODIMS(3) = NPOL
      CALL DSA_RESHAPE_DATA('OUTPUT','INPUT',3,ODIMS,STATUS)
      CALL DSA_RESHAPE_AXIS('OUTPUT',1,'INPUT',2,1,ODIMS(1),STATUS)
      CALL DSA_RESHAPE_AXIS('OUTPUT',2,'INPUT',4,1,ODIMS(2),STATUS)
      CALL DSA_RESHAPE_AXIS('OUTPUT',3,'INPUT',1,1,ODIMS(3),STATUS)
C
C     Map the input file
C
      CALL DSA_MAP_DATA ('INPUT', 'READ', 'FLOAT', IPTR, SLOT, STATUS)
      CALL DSA_MAP_AXIS_DATA ('INPUT', 2, 'READ', 'FLOAT', AIPTR,
     :   SLOT, STATUS)
      IF (VARIANCE) THEN
         CALL DSA_MAP_VARIANCE ('INPUT','READ','FLOAT',IVPTR,SLOT,
     :      STATUS)
      ENDIF
C
C     Get the axis2 info from input
C
      CALL DSA_GET_AXIS_INFO ('INPUT', 2, 2, CITEMS, 0, IITEMS, STATUS)
C
C     Map the output file
C
      CALL DSA_MAP_DATA ('OUTPUT', 'WRITE', 'FLOAT', OPTR, SLOT,
     : STATUS)
      IF (VARIANCE) THEN
         CALL DSA_MAP_VARIANCE ('OUTPUT', 'WRITE', 'FLOAT', OVPTR,
     :      SLOT, STATUS)
      ENDIF
      CALL DSA_MAP_AXIS_DATA ('OUTPUT', 1, 'WRITE', 'FLOAT', AOPTR,
     :   SLOT, STATUS)
C
C     Set the axis1 info in output
C
      CALL DSA_SET_AXIS_INFO ('OUTPUT', 1, 2, CITEMS, 0, IITEMS, STATUS)
C
C     and modify the axis2 info in output
C
      CITEMS(1) = ' '
      CITEMS(2) = 'Cycle No.'
      CALL DSA_SET_AXIS_INFO ('OUTPUT', 2, 2, CITEMS, 0, IITEMS, STATUS)
C
C     Coerce the axis 2 data
C
      CALL DSA_COERCE_AXIS_DATA ('OUTPUT', 2, 'INT', 1, NCYC, STATUS)
      CALL DSA_MAP_AXIS_DATA ('OUTPUT', 2, 'WRITE', 'INT', A2OPTR,
     :   SLOT, STATUS)
C
C     and modify the axis3 info in output
C
      CITEMS(1) = ' '
      CITEMS(2) = 'Plate Pos.'
      CALL DSA_SET_AXIS_INFO ('OUTPUT', 3, 2, CITEMS, 0, IITEMS, STATUS)
C
C     Coerce the axis 3 data
C
      CALL DSA_COERCE_AXIS_DATA ('OUTPUT', 3, 'INT', 1, NPOL, STATUS)
!      CALL DSA_MAP_AXIS_DATA ('OUTPUT', 3, 'WRITE', 'INT', A3OPTR,
!     :   SLOT, STATUS)
C
C     Reduce the input data into the output file
C
      IF (STATUS .EQ. SAI__OK) THEN
         CALL CGS3_3 (NWAVE, NBEAMS, NCYC, NPOL, VARIANCE, %val(IPTR),
     :      %val(IVPTR), %val(AIPTR), %val(OPTR), %val(OVPTR),
     :      %val(AOPTR), %val(A2OPTR))
      ENDIF

      CALL DSA_CLOSE (STATUS)


      END

      SUBROUTINE CGS3_3 (NWAVE, NBEAMS, NCYC, NPOL, VARIANCE, IN_DATA,
     : IN_VARIANCE, IN_AXIS, OUT_DATA, OUT_VARIANCE, OUT_AXIS,
     : OUT_AXIS2)
C
C     Author:
C      Alan Bridger (JAC)
C
C     History:
C      18-Nov-91: Original (JAC::AB)
C
C
      IMPLICIT NONE
      INTEGER  NWAVE, NCYC, NBEAMS, NPOL
      LOGICAL VARIANCE
      REAL IN_DATA (NPOL, NWAVE, NBEAMS, NCYC)
      REAL IN_VARIANCE (NPOL, NWAVE, NBEAMS, NCYC)
      REAL OUT_DATA (NWAVE, NCYC, NPOL)
      REAL OUT_VARIANCE (NWAVE, NCYC, NPOL)
      REAL IN_AXIS (NWAVE)
      REAL OUT_AXIS (NWAVE)
      INTEGER OUT_AXIS2 (NCYC)

      INTEGER I, J, K
C
C     Initialise the outputs
C
      DO J = 1, NCYC
         DO I = 1, NWAVE
            DO K = 1, NPOL
               OUT_DATA(I,J,K) = 0.0
            END DO
         END DO
      END DO

      IF (VARIANCE) THEN
         DO J = 1, NCYC
            DO I = 1, NWAVE
               DO K = 1, NPOL
                  OUT_VARIANCE(I,J,K) = 0.0
               END DO
            END DO
         END DO
      END IF
C
C     Loop over the cycles, coadding the R-Ls into the output array,
C     And while we're at it, reverse the data, as CGS3 delivers it backwards.
C
      DO J = 1, NCYC
         DO I = 1, NWAVE
            DO K = 1, NPOL
               IF (NBEAMS .EQ. 2 ) THEN
                  OUT_DATA(NWAVE-I+1,J,K) =
     :             (IN_DATA(K,I,1,J)-IN_DATA(K,I,2,J))/2.0
               ELSE
                  OUT_DATA(NWAVE-I+1,J,K) = IN_DATA(K,I,1,J)
               END IF
            END DO
         END DO
         OUT_AXIS2(J) = J
      END DO
C
      IF (VARIANCE) THEN
         DO J = 1, NCYC
            DO I = 1, NWAVE
               DO K = 1, NPOL
                  IF (NBEAMS .EQ. 2 ) THEN
                     OUT_VARIANCE(NWAVE-I+1,J,K) =
     :                (IN_VARIANCE(K,I,1,J)+IN_VARIANCE(K,I,2,J))/4.0
                  ELSE
                     OUT_VARIANCE(NWAVE-I+1,J,K) = IN_VARIANCE(K,I,1,J)
                  END IF
               END DO
            END DO
         END DO
      END IF
C
C     Reverse the axis array
C
      DO I = 1, NWAVE
         OUT_AXIS(I) = IN_AXIS(NWAVE-I+1)
      END DO


      END
