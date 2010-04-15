C+
      SUBROUTINE ICONST
C
C     I C O N S T
C
C     This routine is the main body of ICMULT,ICDIV,ICADD and ICSUB,
C     and of XCMULT, XCDIV, XCADD and XCSUB.  The Ixxxx routines
C     operate on the data in an image, the Xxxx routines operate on
C     the data in the X array of the input file.
C     ICMULT multiplies an image by a constant.  Since the constant
C     can be less than 1., this function will also divide an
C     image by a constant, but ICDIV saves the caller from having
C     to calculate a reciprocal. ICADD adds a constant to an image and
C     ICSUB subtracts a constant from an image.
C
C     Command parameters -
C
C     IMAGE  (Character) The name of the structure containing the image.
C            Uses main data array, or the x-axis data for the XCxxx routines.
C
C     FACTOR (Numeric) The value of the constant factor.
C
C     OUTPUT (Character) The name of the result of the operation.  This
C            can be the same as for IMAGE.  If not, a new structure
C            is created, with everything but the data a direct
C            copy of the input.
C
C     The command name is used to distinguish between the
C     possible operations.
C                                      KS / CIT 12th June 1984
C
C     Note that this subroutine ignores any quality array associated
C     with the input image and creates no quality array for the
C     output image.
C                                      ACD / Starlink 26 Feb 2001
C
C     Modified:
C
C     30 Dec 1985  KS / AAO.  ICSUB added.
C     10 Jul 1986  KS / AAO.  XCxxx routines added.
C     24 Jul 1987  DJA/ AAO.  Revised DSA_ routines - some specs
C                  changed.  Modified dynamic memory handling - now uses
C                  DYN_ package
C     30 Oct 1987  KS / AAO.  Added calls to check match between input
C                  and output arrays.
C     20 Mar 1989  JM / RAL.  Error and quality handling added.
C     13 Dec 1989  KS / AAO.  Reorganised to remove redundant mapping of
C                  arrays copied by DSA_OUTPUT.  Quality and flagged
C                  value tests moved prior to map calls.
C     02 May 1995  PND / JAC. Added support for YCxxx routines.
C     20 Jul 1995  HME / UoE, Starlink.  INCLUDE changed.
C     20 Jul 1995  KS / AAO.  Calls to DSA_MATCH_AXES and
C                  DSA_MATCH_SIZES were in the wrong places. Switched.
C     15 Feb 1996  HME / UoE, Starlink. Convert to FDA:
C                  Bad pixel handling.
C     26 Jul 1996  MJCL / Starlink, UCL.  Handling for ABORT response
C                  to FACTOR parameter prompt.
C     24 Mar 1997  JJL / Starlink, Southampton. Replaced errors with
C                  variance.
C     26 Feb 2001  ACD / UoE, Starlink. Initialise the pointer to
C                  the quality array to zero (previously it was
C                  uninitialised).
C     2005 June 8  MJC / Starlink  Use CNF_PVAL for pointers to
C                  mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Functions
C
      LOGICAL PAR_ABORT
C
C     Local variables
C
      CHARACTER COMMAND*64  ! The actual FIGARO command requested
      INTEGER   DIMS(10)    ! The sizes of the data's dimensions
      INTEGER   ERPTR       ! Dynamic pointer to output error array
      LOGICAL   ERRORS      ! True if image has variance array
      REAL      FACTOR      ! The factor used to operate on the data
      REAL      FBAD        ! Flag value for 'FLOAT' data
      LOGICAL   FLAGS       ! True if image has flagged data values
      INTEGER   NDIM        ! The number of dimensions in the data
      INTEGER   NELM        ! The total number of elements in the data
      INTEGER   OUTELM      ! Dynamic memory pointer to output data
      INTEGER   QPTR        ! Dynamic pointer to output quality array
      INTEGER   SLOT        ! Slot number used for mapping - ignored
      INTEGER   STATUS      ! Running status for DSA_ routines
      LOGICAL   XFUNC       ! TRUE if operations are on x-axis data
      LOGICAL   YFUNC       ! TRUE if operations are on y-axis data
C
C     Numeric parameter limits - close to VAX real limits
C
      REAL FMAX, FMIN
      PARAMETER (FMAX=1.7E38, FMIN=-1.7E38)
C
C     Initial values
C
      ERRORS=.FALSE.
      FLAGS=.FALSE.
C
C     Initialisation of DSA_ routines
C
      STATUS=0
      CALL DSA_OPEN(STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get command name
C
      CALL PAR_COMMAND(COMMAND)
      XFUNC=COMMAND(1:1).EQ.'X'
      YFUNC=COMMAND(1:1).EQ.'Y'
C
C     Get input name
C
      CALL DSA_INPUT ('IMAGE','IMAGE',STATUS)
C
C     Get dimensions of input data
C
      IF (XFUNC) THEN
         CALL DSA_AXIS_SIZE ('IMAGE',1,10,NDIM,DIMS,NELM,STATUS)
      ELSE IF (YFUNC) THEN
         CALL DSA_AXIS_SIZE ('IMAGE',2,10,NDIM,DIMS,NELM,STATUS)
      ELSE
         CALL DSA_DATA_SIZE ('IMAGE',10,NDIM,DIMS,NELM,STATUS)
      END IF
      IF (STATUS.NE.0) GOTO 500
C
C     Get value of constant factor
C
      CALL PAR_RDVAL('FACTOR',FMIN,FMAX,1.,' ',FACTOR)
      IF ( PAR_ABORT() ) GO TO 500
C
C     Check for divide by zero
C
      IF ( COMMAND( 2: ) .EQ. 'CDIV' ) THEN
         IF ( FACTOR .EQ. 0.0 ) THEN
            CALL PAR_WRUSER( 'Cannot divide by zero.', STATUS )
            GO TO 500
         END IF
      END IF
C
C     Get output structure name and open it, copying input structure
C     into it if the two are not the same.
C
      CALL DSA_OUTPUT ('OUTPUT','OUTPUT','IMAGE',0,0,STATUS)
C
C     If we are operating on the main data array, see if it has
C     quality data or flagged pixels and indicate that we can handle
C     whichever it has.  If data is flagged, get the flag value.
C     Also, check for the presence of an error array.
C
      IF (.NOT.XFUNC .AND. .NOT.YFUNC) THEN
         CALL DSA_USE_FLAGGED_VALUES ('OUTPUT',STATUS)
         CALL DSA_GET_FLAG_VALUE ('FLOAT',FBAD,STATUS)
         CALL DSA_SEEK_FLAGGED_VALUES ('OUTPUT',FLAGS,STATUS)
         CALL DSA_SEEK_VARIANCE ('OUTPUT',ERRORS,STATUS)
      END IF
C
C     Map data arrays (main data array or axis data array)
C
      IF (XFUNC) THEN
         CALL DSA_MATCH_SIZES ('IMAGE','OUTPUT',STATUS)
         CALL DSA_MAP_AXIS_DATA ('OUTPUT',1,'UPDATE','FLOAT',OUTELM,
     :                           SLOT,STATUS)
      ELSE IF (YFUNC) THEN
         CALL DSA_MATCH_SIZES ('IMAGE','OUTPUT',STATUS)
         CALL DSA_MAP_AXIS_DATA ('OUTPUT',2,'UPDATE','FLOAT',OUTELM,
     :                           SLOT,STATUS)
      ELSE
         CALL DSA_MATCH_AXES ('IMAGE','OUTPUT',STATUS)
         CALL DSA_MAP_DATA ('OUTPUT','UPDATE','FLOAT',OUTELM,SLOT,
     :                      STATUS)
      END IF
      IF (STATUS.NE.0) GOTO 500
C
C     The main data array may have an error array associated
C     with it.  If so, map it.  Note that the additive operations don't
C     change the error array, which is already copied by DSA_OUTPUT (as
C     was any quality array - however, we have to map both for update,
C     just so the DSA system doesn't think they're now invalid.
C
      IF (ERRORS) THEN
         CALL DSA_MAP_VARIANCE ('OUTPUT','UPDATE','FLOAT',ERPTR,SLOT,
     :                           STATUS)
      END IF
      IF (STATUS.NE.0) GO TO 500
C
C     Operate on the data using FACTOR, depending on
C     the actual command.
C
      QPTR = 0
      IF (COMMAND(2:).EQ.'CMULT') THEN
         CALL GEN_MULCAFE(%VAL(CNF_PVAL(OUTELM)),NELM,FACTOR,
     :                    %VAL(CNF_PVAL(OUTELM)),
     :                    %VAL(CNF_PVAL(QPTR)),%VAL(CNF_PVAL(QPTR)),
     :                    %VAL(CNF_PVAL(ERPTR)),%VAL(CNF_PVAL(ERPTR)),
     :                    .FALSE.,FLAGS,FBAD,ERRORS)
      ELSE IF (COMMAND(2:).EQ.'CADD') THEN
         CALL GEN_ADDCAFE(%VAL(CNF_PVAL(OUTELM)),NELM,FACTOR,
     :                    %VAL(CNF_PVAL(OUTELM)),
     :                    %VAL(CNF_PVAL(QPTR)),
     :                   .FALSE.,FLAGS,FBAD)
      ELSE IF (COMMAND(2:).EQ.'CSUB') THEN
         CALL GEN_ADDCAFE(%VAL(CNF_PVAL(OUTELM)),NELM,-FACTOR,
     :                    %VAL(CNF_PVAL(OUTELM)),
     :                    %VAL(CNF_PVAL(QPTR)),
     :                    .FALSE.,FLAGS,FBAD)
      ELSE IF (COMMAND(2:).EQ.'CDIV') THEN
         CALL GEN_MULCAFE(%VAL(CNF_PVAL(OUTELM)),NELM,1.0/FACTOR,
     :                    %VAL(CNF_PVAL(OUTELM)),
     :                    %VAL(CNF_PVAL(QPTR)),%VAL(CNF_PVAL(QPTR)),
     :                    %VAL(CNF_PVAL(ERPTR)),%VAL(CNF_PVAL(ERPTR)),
     :                    .FALSE.,FLAGS,FBAD,ERRORS)
      END IF
C
C     Closedown everything
C
  500 CONTINUE
      CALL DSA_CLOSE(STATUS)
C
      END
