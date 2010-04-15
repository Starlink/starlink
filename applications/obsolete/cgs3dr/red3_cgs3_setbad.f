      SUBROUTINE RED3_CGS3_SETBAD (STATUS)
C+
C     R E D 3 _ C G S 3 _ S E T B A D
C
C     Sets bad quality on specified cycles of a CGS3 wavelength v cycle
C     image.
C
C     Command parameters -
C
C     IMAGE      (Character) The name of the input file
C     BAD_CYCLES (Character) Contains the cycles to be set bad.
C
C     Command keywords - None
C
C     User variables used - None
C
C                                      JFL / JAC  21th Jul 1990
C     History:
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
      LOGICAL VARIANCE
      LOGICAL FINISHED
      INTEGER DIMS(4)
      INTEGER ELEMENTS
      INTEGER NDIM
      INTEGER NWAVE, NCYC
      INTEGER IPTR, IVPTR, IQPTR, IAPTR, OFFSET
      INTEGER SLOT
      INTEGER BAD_CYCLE
      CHARACTER*80 INPUT


      IF (STATUS .NE. SAI__OK) RETURN

      CALL DSA_OPEN(STATUS)
C
C     Get the name of the input file
C
      CALL PAR_GET0C ('IMAGE',INPUT,STATUS)
      CALL DSA_NAMED_INPUT ('INPUT',INPUT,STATUS)
C
C     Determine dimensions of input.
C
      CALL DSA_DATA_SIZE ('INPUT',2,NDIM,DIMS,ELEMENTS,STATUS)
      IF (NDIM .NE. 2) THEN
         CALL MSG_OUT (' ','Incorrect dimensions, probably not'//
     :    ' correct data', STATUS)
         STATUS = SAI__ERROR
      END IF
      NWAVE = DIMS(1)
      NCYC  = DIMS(2)
      CALL MSG_SETI ('NCYC',NCYC)
      CALL MSG_OUT (' ','Image contains ^NCYC cycles',STATUS)
C
C     Look for error array
C
      CALL DSA_SEEK_ERRORS ('INPUT',VARIANCE,STATUS)
C
C     Map the input file
C
      CALL DSA_USE_QUALITY ('INPUT',STATUS)
      CALL DSA_MAP_DATA ('INPUT','UPDATE','FLOAT',IPTR,SLOT,STATUS)
      CALL DSA_MAP_AXIS_DATA ('INPUT',1,'READ','FLOAT',IAPTR,SLOT,
     :   STATUS)
      CALL DSA_MAP_QUALITY ('INPUT','UPDATE','BYTE',IQPTR,SLOT,STATUS)
      IF (VARIANCE) THEN
         CALL DSA_MAP_VARIANCE ('INPUT','UPDATE','FLOAT',IVPTR,SLOT,
     :      STATUS)
      ENDIF

      IF (STATUS .EQ. SAI__OK) THEN
         FINISHED = .FALSE.
         DO WHILE (.NOT.FINISHED)
C
C     Read the cycle numbers to be set bad
C
            CALL PAR_GET0I ('BAD_CYCLE',BAD_CYCLE,STATUS)
            CALL PAR_CANCL ('BAD_CYCLE',STATUS)
            IF (STATUS .EQ. SAI__OK) THEN
               IF (BAD_CYCLE .GT. 0) THEN
C
C     Set them bad, and fill the corresponding data and variance areas
C     with zeros
C
                  OFFSET = (BAD_CYCLE-1) * NWAVE
                  CALL GEN_FILL (NWAVE,1,%val(IQPTR+OFFSET))
                  CALL GEN_CFILL (OFFSET+1,OFFSET+NWAVE+1,0.0,
     :               %val(IPTR))
                  IF (VARIANCE) THEN
                     CALL GEN_CFILL (OFFSET+1,OFFSET+NWAVE+1,0.0,
     :                  %val(IVPTR))
                  END IF
               ELSE
                  FINISHED = .TRUE.
               END IF
            ELSE
               FINISHED = .TRUE.
            END IF

         END DO

      END IF

      CALL DSA_CLOSE (STATUS)

      END

