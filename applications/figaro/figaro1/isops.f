C+
      SUBROUTINE ISOPS
C
C     I S O P S
C
C     Performs a number of operations all of the general form
C
C     Image=function(image,spectrum)
C
C     Specifically, if an image is of format (IX,IY) then
C
C     ISXSUB is Result = IMAGE(I,N)-SPECTRUM(I) 1<=I<=IX 1<=N<=IY
C     ISXADD  "   "    = IMAGE(I,N)+SPECTRUM(I)    "        "
C     ISXDIV  "   "    = IMAGE(I,N)/SPECTRUM(I)    "        "
C     ISXMUL  "   "    = IMAGE(I,N)*SPECTRUM(I)    "        "
C     ISYSUB is Result = IMAGE(N,I)-SPECTRUM(I) 1<=N<=IX 1<=I<=IY
C     ISYADD  "   "    = IMAGE(N,I)+SPECTRUM(I)    "        "
C     ISYDIV  "   "    = IMAGE(N,I)/SPECTRUM(I)    "        "
C     ISYMUL  "   "    = IMAGE(N,I)*SPECTRUM(I)    "        "
C
C     So the ISX... operations require a spectrum IX elements long,
C     and the ISY... operations need one IY elements long.
C
C     Command parameters -
C
C     IMAGE    The name of the structure containing the first image.
C
C     SPECTRUM The name of the structure containing the second
C              image data.
C
C     OUTPUT   The name of the result of the operation.  This can
C              be the same as for IMAGE.  If not, a new structure
C              is created, with everything but the data a direct
C              copy of the input.
C
C     The command itself (IXSADD,IXSMUL, etc.) is used to
C     differentiate between the possible operations.
C
C                                      KS / CIT 18th Feb 1983
C
C     Modified:
C
C     28th Jul 1987  DJA / AAO. Revised DSA_ routines - some specs
C                    changed. Now uses DYN_ package for dynamic-memory
C                    handling. All WRUSERs converted to PAR_WRUSERs.
C     29th Sep 1992  HME / UoE, Starlink.  INCLUDE changed, TABs
C                    removed.
C      1st May 1997  JJL / Soton, Starlink. Variances included.
C     2005 June 8    MJC / Starlink  Use CNF_PVAL for pointers to
C                    mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Functions
C
      INTEGER ICH_FOLD
C
C     Local variables
C
      CHARACTER    COMMAND*64    ! The actual FIGARO command passed
      INTEGER      DIMS(10)      ! Sizes of dimensions of data
      INTEGER      DPTR          ! Dynamic-memory pointer to data array
      INTEGER      DSLOT         ! Map slot number of input data array
      INTEGER      IGNORE        ! Dummy status arguement for PAR_WRUSER
      INTEGER      INVOKE        ! Used to invoke function calls
      INTEGER      LSPECT        ! The length of the spectrum
      INTEGER      NDIM          ! Number of dimensions in data
      INTEGER      NELM          ! Total number of elements in data
      INTEGER      NX            ! Size of 1st dimension
      INTEGER      NY            ! Size of 2nd dimension (if present)
      INTEGER      OPTR          ! Dynamic-memory pointer to output data
                                 ! array
      INTEGER      OSLOT         ! Map slot number outputdata array
      INTEGER      SPTR          ! Dynamic-memory pointer to spectrum
                                 ! data
      INTEGER      SSLOT         ! Map slot number of spectrum data
      INTEGER      STATUS        ! Running status for DSA_ routines
      INTEGER      VDPTR         ! Dynamic-memory pointer to variance
                                 ! array
      INTEGER      VDSLOT        ! Map slot number of input variance
                                 ! array
      INTEGER      VOPTR         ! Dynamic-memory pointer to output
                                 ! variance
      LOGICAL      VEXIST        ! TRUE if variance exists in both files
      INTEGER      VOSLOT        ! Map slot number output variance array
      INTEGER      VSPTR         ! Dynamic-memory pointer to spectrum
                                 ! variance
      INTEGER      VSSLOT        ! Map slot number of spectrum variance
      LOGICAL      XCOM          ! FIGARO command was an X one?
C
C     Set variances FALSE
C
      VEXIST = .FALSE.
C
C     Initialisation of DSA_ routines
C
      STATUS=0
      CALL DSA_OPEN(STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get input name
C
      CALL DSA_INPUT('IMAGE','IMAGE',STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get command name, see if an X or Y operation
C
      CALL PAR_COMMAND(COMMAND)
      INVOKE=ICH_FOLD(COMMAND)
      XCOM=COMMAND(3:3).EQ.'X'
C
C     Get dimensions of input data
C
      CALL DSA_DATA_SIZE('IMAGE',2,NDIM,DIMS,NELM,STATUS)
      IF (NDIM.NE.2) THEN
         CALL PAR_WRUSER('Input image is NOT a 2D array',STATUS)
         GO TO 500
      END IF
      NX=DIMS(1)
      NY=DIMS(2)
C
C     Get spectrum name and open file
C
      CALL DSA_INPUT('SPECT','SPECTRUM',STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Get dimension of spectrum data and check it.
C
      CALL DSA_DATA_SIZE('SPECT',1,NDIM,DIMS,LSPECT,STATUS)
      IF (XCOM) THEN
         IF (LSPECT.NE.NX) THEN
            CALL PAR_WRUSER('Spectrum length does not match image''s'
     :                                   //' X dimension',IGNORE)
            GO TO 500
         END IF
      ELSE
         IF (LSPECT.NE.NY) THEN
            CALL PAR_WRUSER('Spectrum length does not match image''s'
     :                                   //' Y dimension',IGNORE)
            GO TO 500
         END IF
      END IF
C
C     Get output structure name
C
      CALL DSA_OUTPUT('OUTPUT','OUTPUT','IMAGE',0,0,STATUS)
C
C     Map output data (this is either the actual input data, or a
C     copy of it)
C
      CALL DSA_MAP_DATA('IMAGE','READ','FLOAT',DPTR,DSLOT,STATUS)
      CALL DSA_MAP_DATA('SPECT','READ','FLOAT',SPTR,SSLOT,STATUS)
      CALL DSA_MAP_DATA('OUTPUT','WRITE','FLOAT',OPTR,OSLOT,STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Now check to see if the variance structure exists. If so, map it.
C
      CALL DSA_SEEK_VARIANCE('IMAGE',VEXIST,STATUS)
      IF (VEXIST) CALL DSA_SEEK_VARIANCE('SPECT',VEXIST,STATUS)
      IF (VEXIST) THEN
          CALL DSA_MAP_VARIANCE('IMAGE','READ','FLOAT',VDPTR,
     :                           VDSLOT,STATUS)
          CALL DSA_MAP_VARIANCE('SPECT','READ','FLOAT',VSPTR,
     :                           VSSLOT,STATUS)
          CALL DSA_MAP_VARIANCE('OUTPUT','WRITE','FLOAT',VOPTR,
     :                           VOSLOT,STATUS)
      END IF
      IF (STATUS.NE.0) GOTO 500
C
C     Perform the operation
C
      IF (XCOM) THEN
         CALL GEN_XOP21(COMMAND(4:4), VEXIST, NX, NY,
     :                  %VAL(CNF_PVAL(DPTR)), %VAL(CNF_PVAL(VDPTR)),
     :                  %VAL(CNF_PVAL(SPTR)), %VAL(CNF_PVAL(VSPTR)),
     :                  %VAL(CNF_PVAL(OPTR)), %VAL(CNF_PVAL(VOPTR)))
      ELSE
         CALL GEN_YOP21(COMMAND(4:4), VEXIST, NX, NY,
     :                  %VAL(CNF_PVAL(DPTR)), %VAL(CNF_PVAL(VDPTR)),
     :                  %VAL(CNF_PVAL(SPTR)), %VAL(CNF_PVAL(VSPTR)),
     :                  %VAL(CNF_PVAL(OPTR)), %VAL(CNF_PVAL(VOPTR)))
      END IF
C
C     Closedown everything
C
500   CONTINUE
      CALL DSA_CLOSE(STATUS)
C
      END
