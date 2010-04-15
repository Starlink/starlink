C+
      SUBROUTINE IADSUB
C
C     I A D S U B
C
C     Adds, multiplies, divides or subtracts two images.
C
C     Command parameters -
C
C     IMAGE  The name of the structure containing the first image.
C
C     IMAGE1 The name of the structure containing the second
C            image data.
C
C     OUTPUT The name of the result of the operation.  This can
C            be the same as for IMAGE.  If not, a new structure
C            is created, with everything but the data (and any error
C            or data quality information) a direct copy of the first
C            image.
C
C     The command itself (IADD,IMULT,IDIV or ISUB) is used to
C     differentiate between the two operations.
C
C                                      KS / CIT 26th Sept 1983
C
C     Note that this subroutine ignores any quality arrays associated
C     with the input images and creates no quality array for the
C     output image.
C                                      ACD / Starlink 23 Feb 2001
C     Modified:
C
C     08 May 1986  KS / AAO.  Now tidies up after an error in
C                  the usual Figaro way, after label 500.
C     03 Jun 1987  KS / AAO.  Re-written using the new DSA_ routines.
C     24 Jul 1987  DJA/ AAO.  Modified dynamic memory handling - now
C                  uses DYN_ routines
C     20 Mar 1989  JM / RAL.  Modified to handle quality and errors.
C     12 Dec 1989  KS / AAO.  Now allows only one image to have error
C                  information, and uses variance instead of errors.
C                  Test on quality information moved before data
C                  mapping.
C     30 Jul 1991  HME / UoE. Typo in GEN_DIVAFE. That routine is
C                  appended here as DIVAFE.
C     30 Jan 1992  HME / UoE. Zero-initialise output variance and
C                  quality.
C     06 Oct 1992  HME / UoE, Starlink.  INCLUDE changed. Make the
C                  local DIVAFE official as GEN_DIVAFE and use that.
C     15 Feb 1996  HME / UoE, Starlink. Convert to FDA:
C                  Bad pixel handling.
C     23 Feb 2001  ACD / UoE, Starlink. Initialise the pointers to
C                  the quality arrays to zero (previously they were
C                  uninitialised).
C     2005 June 8  MJC / Starlink  Use CNF_PVAL for pointers to
C                  mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Local variables
C
      CHARACTER COMMAND*16 ! Figaro command being serviced.
      INTEGER   D1PTR      ! Dynamic pointer to first image data.
      INTEGER   D2PTR      ! Dynamic pointer to second image data.
      INTEGER   D3PTR      ! Dynamic pointer to output image data.
      INTEGER   DIMS(10)   ! Dimensions of first image.  Ignored.
      INTEGER   E1PTR      ! Dynamic pointer to first image variance
                           ! array
      INTEGER   E2PTR      ! Dynamic pointer to second image variance
                           ! array
      INTEGER   E3PTR      ! Dynamic pointer to output variance array
      LOGICAL   ERR        ! True if IMAGE1 has variance information
      LOGICAL   ERR1       ! True if IMAGE2 has variance information
      REAL      FBAD       ! Flag value for 'FLOAT' data
      LOGICAL   FLAGS      ! True if image has flagged data values
      INTEGER   IGNORE     ! Disregarded status return value.
      INTEGER   NDIM       ! Dimensionality of first image.  Ignored.
      INTEGER   NELM       ! Number of elements in input image.
      INTEGER   Q1PTR      ! Dynamic pointer to first quality array
      INTEGER   Q2PTR      ! Dynamic pointer to second quality array
      INTEGER   Q3PTR      ! Dynamic pointer to output quality array
      INTEGER   SLOT       ! Map slot number - ignored
      INTEGER   STATUS     ! Running status for DSA_ routines.
      LOGICAL   VARIANCE   ! True if either inputs has variance data
C
C     Initial settings
C
      VARIANCE=.FALSE.
      STATUS=0
      CALL DSA_OPEN (STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Open the two input image files
C
      CALL DSA_INPUT ('IMAGE','IMAGE',STATUS)
      CALL DSA_INPUT ('IMAGE1','IMAGE1',STATUS)
C
C     Get dimensions of input data, and check that the images
C     match in size.  Check for axis mismatches, but allow them.
C
      CALL DSA_DATA_SIZE ('IMAGE',10,NDIM,DIMS,NELM,STATUS)
      CALL DSA_MATCH_SIZES ('IMAGE','IMAGE1',STATUS)
      IGNORE=STATUS
      CALL DSA_MATCH_AXES ('IMAGE','IMAGE1',IGNORE)
C
C     Open output image
C
      CALL DSA_OUTPUT ('OUTPUT','OUTPUT','IMAGE',0,0,STATUS)
C
C     Use flagged values, but then see if there are any.
C
      CALL DSA_USE_FLAGGED_VALUES ('IMAGE',STATUS)
      CALL DSA_USE_FLAGGED_VALUES ('IMAGE1',STATUS)
      CALL DSA_USE_FLAGGED_VALUES ('OUTPUT',STATUS)
      CALL DSA_GET_FLAG_VALUE ('FLOAT',FBAD,STATUS)
      CALL DSA_SEEK_FLAGGED_VALUES ('IMAGE',FLAGS,STATUS)
      IF (.NOT.FLAGS)
     :   CALL DSA_SEEK_FLAGGED_VALUES ('IMAGE1',FLAGS,STATUS)
C
C     Map data arrays.
C
      CALL DSA_MAP_DATA ('IMAGE','READ','FLOAT',D1PTR,SLOT,STATUS)
      CALL DSA_MAP_DATA ('IMAGE1','READ','FLOAT',D2PTR,SLOT,STATUS)
      CALL DSA_MAP_DATA ('OUTPUT','WRITE','FLOAT',D3PTR,SLOT,STATUS)
C
C     If either image has variance information, map all three variance
C     arrays. Note that MAP_VARIANCE returns a zero array if there is
C     no actual error array.
C     Zero-initialise the output variance. (DSA should do that.)
C
      ERR = .FALSE.
      CALL DSA_SEEK_VARIANCE ('IMAGE',ERR,STATUS)
      ERR1 = .FALSE.
      CALL DSA_SEEK_VARIANCE ('IMAGE1',ERR1,STATUS)
      IF (ERR.OR.ERR1) THEN
         CALL DSA_MAP_VARIANCE ('IMAGE','READ','FLOAT',E1PTR,SLOT,
     :                         STATUS)
         CALL DSA_MAP_VARIANCE ('IMAGE1','READ','FLOAT',E2PTR,SLOT,
     :                         STATUS)
         CALL DSA_MAP_VARIANCE ('OUTPUT','WRITE','FLOAT',E3PTR,SLOT,
     :                         STATUS)
         VARIANCE=.TRUE.
         CALL GEN_FILL( 4*NELM, 0, %VAL(CNF_PVAL(E3PTR)) )
      END IF
      IF (STATUS.NE.0) GO TO 500
C
C     Initialise the pointers to the quality arrays to zero.
C
      Q1PTR = 0
      Q2PTR = 0
      Q3PTR = 0
C
C     Operate on the image data
C
      CALL PAR_COMMAND(COMMAND)
      IF (COMMAND.EQ.'IADD') THEN
         CALL GEN_ADDAFE(NELM,%VAL(CNF_PVAL(D1PTR)),
     :                   %VAL(CNF_PVAL(D2PTR)),%VAL(CNF_PVAL(D3PTR)),
     :                   %VAL(CNF_PVAL(Q1PTR)),%VAL(CNF_PVAL(Q2PTR)),
     :                   %VAL(CNF_PVAL(Q3PTR)),%VAL(CNF_PVAL(E1PTR)),
     :                   %VAL(CNF_PVAL(E2PTR)),%VAL(CNF_PVAL(E3PTR)),
     :                   .FALSE.,FLAGS,FBAD,VARIANCE)
C
      ELSE IF (COMMAND.EQ.'ISUB') THEN
         CALL GEN_SUBAFE(NELM,%VAL(CNF_PVAL(D1PTR)),
     :                    %VAL(CNF_PVAL(D2PTR)),%VAL(CNF_PVAL(D3PTR)),
     :                   %VAL(CNF_PVAL(Q1PTR)),%VAL(CNF_PVAL(Q2PTR)),
     :                   %VAL(CNF_PVAL(Q3PTR)),%VAL(CNF_PVAL(E1PTR)),
     :                   %VAL(CNF_PVAL(E2PTR)),%VAL(CNF_PVAL(E3PTR)),
     :                   .FALSE.,FLAGS,FBAD,VARIANCE)
C
      ELSE IF (COMMAND.EQ.'IMULT') THEN
         CALL GEN_MULTAFE(NELM,%VAL(CNF_PVAL(D1PTR)),
     :                    %VAL(CNF_PVAL(D2PTR)),%VAL(CNF_PVAL(D3PTR)),
     :                    %VAL(CNF_PVAL(Q1PTR)),%VAL(CNF_PVAL(Q2PTR)),
     :                    %VAL(CNF_PVAL(Q3PTR)),%VAL(CNF_PVAL(E1PTR)),
     :                    %VAL(CNF_PVAL(E2PTR)),%VAL(CNF_PVAL(E3PTR)),
     :                    .FALSE.,FLAGS,FBAD,VARIANCE)
C
      ELSE IF (COMMAND.EQ.'IDIV') THEN
         CALL GEN_DIVAFE(NELM,%VAL(CNF_PVAL(D1PTR)),
     :                   %VAL(CNF_PVAL(D2PTR)),%VAL(CNF_PVAL(D3PTR)),
     :                   %VAL(CNF_PVAL(Q1PTR)),%VAL(CNF_PVAL(Q2PTR)),
     :                   %VAL(CNF_PVAL(Q3PTR)),%VAL(CNF_PVAL(E1PTR)),
     :                   %VAL(CNF_PVAL(E2PTR)),%VAL(CNF_PVAL(E3PTR)),
     :                   .FALSE.,FLAGS,FBAD,VARIANCE)
      END IF
C
C     Tidy up.
C
500   CONTINUE
      CALL DSA_CLOSE (STATUS)
C
      END
