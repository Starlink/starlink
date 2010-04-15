C+
      SUBROUTINE FLAG2QUAL
C
C                              FLAG2QUAL
C
C  Description:
C     This is a Figaro program that removes any 'flagged' values from
C     the main data array in a Figaro data file. If there are in fact
C     such values in the main data array (many arrays are flagged as
C     'may contain flagged values', but in fact do not) then this
C     routine sets the equivalent elements of an associated quality
C     array (which it may have to create).
C
C  Command parameters:
C
C     INPUT  (Character) The name of the structure containing the data.
C
C     OUTPUT (Character) The name of the result of the operation.  This
C            can be the same as for INPUT.  If not, a new structure
C            is created, with everything but the data a direct
C            copy of the input.
C
C     VALUE  (Numeric) If a fixed value is to be used to replace flagged
C            data values, this supplies that value.
C
C  Command keywords:
C
C     FIXED  If specified, a fixed value (supplied in VALUE) is used
C            to replace the flagged data values. If not, the program
C            interpolates over them.
C-
C  History:
C     11th Feb 1995  KS / AAO.  Original version.
C     27th Nov 1995  KS / AAO.  Added call to DSA_QUALITY_AND_FLAGS_OK.
C     2005 June 7    MJC / Starlink  Use CNF_PVAL for pointers to
C                    mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Functions
C
      INTEGER ICH_LEN
      CHARACTER ICH_CI*16,ICH_CF*32
C
C     Local variables
C
      INTEGER   DIMS(10)    ! The sizes of the data's dimensions
      REAL      FBAD        ! Flag value for 'FLOAT' data
      LOGICAL   FIXED       ! Value of FIXED keyword
      LOGICAL   FLAGS       ! True if image has flagged data values
      INTEGER   IGNORE      ! Used for status we don't care about
      INTEGER   NDIM        ! The number of dimensions in the data
      INTEGER   NELM        ! The total number of elements in the data
      INTEGER   NFLAGGED    ! Total number of flagged values found
      CHARACTER NUMBER*16   ! Used to format an integer
      INTEGER   OUTELM      ! Dynamic memory pointer to output data
      CHARACTER QMODE*8     ! Mapping mode for quality array
      INTEGER   QPTR        ! Dynamic pointer to output quality array
      LOGICAL   QUAL        ! True if image has a quality array
      INTEGER   SLOT        ! Slot number used for mapping - ignored
      INTEGER   STATUS      ! Running status for DSA_ routines
      REAL      VALUE       ! Numeric value for parameter
      CHARACTER VSTRING*32  ! Used to format a real number
C
C     Floating point range limits - close to those of the VAX.
C
      REAL FMAX, FMIN
      PARAMETER (FMAX=1.7E38, FMIN=-1.7E38)
C
C     Initial values
C
      QUAL=.FALSE.
      FLAGS=.FALSE.
C
C     Initialisation of DSA_ routines
C
      STATUS=0
      CALL DSA_OPEN(STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Indicate that handling both quality and flagged values is OK,
C     although it's unusual for a Figaro routine.
C
      CALL DSA_QUALITY_AND_FLAGS_OK
C
C     Get input name
C
      CALL DSA_INPUT ('INPUT','INPUT',STATUS)
C
C     Get dimensions of input data
C
      CALL DSA_DATA_SIZE ('INPUT',10,NDIM,DIMS,NELM,STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Get output structure name and open it, copying input structure
C     into it if the two are not the same.
C
      CALL DSA_OUTPUT ('OUTPUT','OUTPUT','INPUT',0,0,STATUS)
C
C     If the main data array isn't flagged anyway, there's nothing for
C     us to do.
C
      CALL DSA_SEEK_FLAGGED_VALUES ('OUTPUT',FLAGS,STATUS)
      IF (STATUS.NE.0) GO TO 500  ! Error exit
      IF (.NOT.FLAGS) THEN
         CALL PAR_WRUSER ('Input data was not flagged',IGNORE)
         GO TO 500                ! Exit now, all done
      END IF
C
C     See if we are to interpolate over the bad pixels, or to set them
C     to a specified value which we get from the user.
C
      CALL PAR_RDKEY('FIXED',.FALSE.,FIXED)
      IF (FIXED) THEN
         CALL PAR_RDVAL ('VALUE',FMIN,FMAX,0.0,' ',VALUE)
      ELSE
         VALUE=0.0
      END IF
C
C     See if there is already a quality array. This affects the way
C     we map the data in it.
C
      CALL DSA_SEEK_QUALITY ('OUTPUT',QUAL,STATUS)
      IF (QUAL) THEN
         QMODE='UPDATE'
      ELSE
         QMODE='WRITE'
      END IF
C
C     We indicate our intention to use both quality and flagged values
C     in handling the data - this ensures that we get both arrays quite
C     unchanged by the DSA routines (apart from any necessary type
C     conversion).
C
      CALL DSA_USE_QUALITY ('OUTPUT',STATUS)
      CALL DSA_USE_FLAGGED_VALUES ('OUTPUT',STATUS)
      CALL DSA_GET_FLAG_VALUE ('FLOAT',FBAD,STATUS)
C
C     Map main output data array.
C
      CALL DSA_MAP_DATA ('OUTPUT','UPDATE','FLOAT',OUTELM,SLOT,STATUS)
C
C     Map the data quality array
C
      CALL DSA_MAP_QUALITY('OUTPUT',QMODE,'BYTE',QPTR,SLOT,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Process the flagged values out of the data.
C
      CALL FLAG2QUAL_DO (%VAL(CNF_PVAL(OUTELM)),NELM,
     :                   %VAL(CNF_PVAL(QPTR)),FBAD,FIXED,VALUE,NFLAGGED)
C
C     Report on what happened
C
      IF (NFLAGGED.EQ.0) THEN
         CALL PAR_WRUSER(
     :      'File did not in fact contain any flagged values',IGNORE)
      ELSE
         NUMBER=ICH_CI(NFLAGGED)
         IF (FIXED) THEN
            VSTRING=ICH_CF(VALUE)
            CALL PAR_WRUSER(NUMBER(:ICH_LEN(NUMBER))//
     :             ' flagged value(s) replaced with the value '//
     :                           VSTRING(:ICH_LEN(VSTRING)),IGNORE)
         ELSE
            CALL PAR_WRUSER(NUMBER(:ICH_LEN(NUMBER))//
     :          ' flagged value(s) replaced by interpolation',IGNORE)
         END IF
      END IF
C
C     Set the file to indicate it has no flagged values
C
      CALL DSA_SET_FLAGGED_VALUES ('OUTPUT',.FALSE.,STATUS)
C
C     Close down everything
C
  500 CONTINUE
      CALL DSA_CLOSE(STATUS)
C
      END


      SUBROUTINE FLAG2QUAL_DO (ARRAY,NELM,FLAGS,FBAD,
     :                                      FIXED,VALUE,NFLAGGED)
C
C     F L A G 2 Q U A L _ D O
C
C     Utility routine for FLAG2QUAL. This is the routine that actually
C     replaces the flagged values in the NELM elements of ARRAY. FBAD
C     is the flag value the routine looks for. If a flagged value is
C     found and replaced the corresponding element of FLAGS is set.
C     Other elements of FLAGS are left unchanged. If FIXED is set,
C     all the flagged elements of ARRAY are set to VALUE; otherwise
C     VALUE is ignored and the routine interpolates over the flagged
C     values. NFLAGGED returns the number of flagged values found.
C
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL FIXED
      INTEGER NELM,NFLAGGED
      REAL ARRAY(NELM),FBAD,VALUE
      BYTE FLAGS(NELM)
C
C     Local variables
C
      REAL    DELTA             ! Increment for interpolation
      INTEGER I                 ! Loop index through all elements of array
      INTEGER IBAD              ! Loop index through flagged elements
      INTEGER LAST_GOOD         ! Index of last good element
      REAL    LAST_VALUE        ! Value of last good element
C
      NFLAGGED=0
      IF (FIXED) THEN
         DO I=1,NELM
            IF (ARRAY(I).EQ.FBAD) THEN
               FLAGS(I)=1
               NFLAGGED=NFLAGGED+1
               ARRAY(I)=VALUE
            END IF
         END DO
      ELSE
         LAST_GOOD=0
         LAST_VALUE=0.0
         DO I=1,NELM
            IF ((ARRAY(I).NE.FBAD).AND.(FLAGS(I).EQ.0)) THEN
               IF (LAST_GOOD.NE.(I-1)) THEN
                  IF (LAST_GOOD.EQ.0) LAST_VALUE=ARRAY(I)
                  DELTA=(ARRAY(I)-LAST_VALUE)/(I-LAST_GOOD)
                  DO IBAD=LAST_GOOD+1,I-1
                     LAST_VALUE=LAST_VALUE+DELTA
                     IF (ARRAY(IBAD).EQ.FBAD) THEN
                        ARRAY(IBAD)=LAST_VALUE
                        NFLAGGED=NFLAGGED+1
                     END IF
                     FLAGS(IBAD)=1
                  END DO
               END IF
               LAST_GOOD=I
               LAST_VALUE=ARRAY(I)
            END IF
         END DO
         IF (LAST_GOOD.NE.NELM) THEN
            DO IBAD=LAST_GOOD+1,NELM
               FLAGS(IBAD)=1
               IF (ARRAY(IBAD).EQ.FBAD) THEN
                  ARRAY(IBAD)=LAST_VALUE
                  NFLAGGED=NFLAGGED+1
               END IF
            END DO
         END IF
      END IF
C
      END
