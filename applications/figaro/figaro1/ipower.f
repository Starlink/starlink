C+
      SUBROUTINE IPOWER
C
C     I P O W E R
C
C     Raises the data in an image to a power of itself.  This can be used
C     to multiply an image by itself (POWER=2.0) or to take the square
C     root of an image (POWER=0.5), or may be used with an arbitrary power.
C     Pixels whose value is such that the operation is illegal will give
C     a zero result.
C
C     Command parameters -
C
C     IMAGE  (File) The name of the structure containing the image.
C
C     POWER  (Numeric) The power to which the image is to be raised.
C
C     OUTPUT (File) The name of the result of the operation.  This can
C            be the same as for IMAGE.  If not, a new structure
C            is created, with everything but the data a direct
C            copy of the input.
C
C     Command keywords  - None
C
C     User variables used - None
C                                      KS / AAO 28th May 1987
C
C     Note that this subroutine ignores any quality array associated
C     with the input image and creates no quality array for the
C     output image.
C                                      ACD / Starlink 23 Feb 2001
C
C     27 Oct 1988  JM / RAL. Modified to use DSA_ routines
C                  Dynamic memory handling changed to use
C                  DYN_ routines
C     09 Jan 1991  JMS / AAO. Modified subroutine GEN_POWEREQ - 
C                  Now handles data arrays containing quality and
C                  flagged information. Modified main routine so that
C                  there is no need to use work arrays.
C     06 Oct 1992  HME / UoE, Starlink.  INCLUDE changed, TAB removed.
C     16 Feb 1996  HME / UoE, Starlink. Convert to FDA:
C                  Bad pixel handling.
C     26 Jul 1996  MJCL / Starlink, UCL.  Added PAR_ABORT call.
C                  Check status of DSA_OUTPUT.
C     24 Mar 1997  JJL / Starlink, Southampton. Modified so that
C                  code looks for a variance structure rather
C                  than an error structure in the NDF.
C     23 Feb 2001  ACD / UoE, Starlink. Initialise the pointer to
C                  the quality array to zero (previously it was
C                  uninitialised).
C+
      IMPLICIT NONE
C
C     Functions
C
      INTEGER DYN_ELEMENT
      INTEGER DSA_TYPESIZE
      LOGICAL DSA_SAME_DATA
      CHARACTER*11 ICH_CI          ! Returns an integer as a string
      INTEGER   ICH_LEN            ! Returns the length of a string
      LOGICAL PAR_ABORT            ! (F)PAR abort flag
C
C     Local variables
C
      INTEGER   ADDRESS            ! Virtual address for data array
      INTEGER   BYTES              ! Bytes required for an array
      INTEGER   DIMS(10)           ! Image dimensions
      INTEGER   NDIM               ! Number of image dimensions
      INTEGER   NELM               ! Number of elements in image - ignored
      INTEGER   SLOT               ! Slot number for mapped data - ignored
      INTEGER   OPTR               ! Dynamic memory element for output daa
      REAL      POWER              ! Power to which data array is raised
      INTEGER   STATUS             ! Running status for DSA routines
      INTEGER   Q2PTR              ! Dynamic pointer to second image data.
      INTEGER   E2PTR              ! Dynamic pointer to second image data.
      LOGICAL   ERRORS             ! True if image has variance data
      INTEGER   FAILURES           ! Number of times illegal values encountered
      REAL      FBAD               ! Flag value for 'FLOAT' data
      LOGICAL   FLAGS              ! True if image has flagged data values
      CHARACTER*80 STRING          ! Used to format user message
      INTEGER   IPT                ! Holds the value returned by ich_len
      INTEGER   IGNORE             ! Value of status returned by par_wruser.
C
C     Maximum power allowed (a completely arbitrary value)
C
      REAL PMAX
      PARAMETER (PMAX=65536.0)

C
C     Parameters controlling the way DSA_OUTPUT opens the spectrum file
C
      INTEGER   NEW_FILE, NO_DATA
      PARAMETER (NEW_FILE=1, NO_DATA=1)
C
C     Dynamic memory common - defines DYNAMIC_MEM
C
      INCLUDE 'DYNAMIC_MEMORY'
C     
C     Initial values
C
      STATUS=0
C
C     Open DSA routines
C
      CALL DSA_OPEN(STATUS)
C
C     Open IMAGE file
C
      CALL DSA_INPUT ('IMAGE','IMAGE',STATUS)
C
C     Get size of data in IMAGE
C
      CALL DSA_DATA_SIZE ('IMAGE',2,NDIM,DIMS,NELM,STATUS)
      IF(STATUS.NE.0)GOTO 500
C
C     Get the power value
C
      CALL PAR_RDVAL ('POWER',-PMAX,PMAX,0.0,' ',POWER)
      IF ( PAR_ABORT() ) GO TO 500
C
C     Get output structure name
C
      CALL DSA_OUTPUT('OUTPUT','OUTPUT','IMAGE',0,0,STATUS)      
      IF (STATUS.NE.0) GOTO 500
C
C     If the image had flagged data values, use them and get
C     the flag value being used.
C
      CALL DSA_USE_FLAGGED_VALUES ('OUTPUT',STATUS)
      CALL DSA_GET_FLAG_VALUE ('FLOAT',FBAD,STATUS)
      CALL DSA_SEEK_FLAGGED_VALUES ('IMAGE',FLAGS,STATUS)
C
C     Map output data
C
      CALL DSA_MAP_DATA('OUTPUT','UPDATE','FLOAT',ADDRESS,SLOT,STATUS)
      OPTR=DYN_ELEMENT(ADDRESS)
C
C     If the image had an error array, map it and the error array
C     for the spectrum.
C
      ERRORS = .FALSE.
      CALL DSA_SEEK_VARIANCE ('IMAGE',ERRORS,STATUS)
      IF (ERRORS) THEN
         CALL DSA_MAP_VARIANCE ('OUTPUT','UPDATE','FLOAT',ADDRESS,SLOT,
     :                         STATUS)
         E2PTR=DYN_ELEMENT(ADDRESS)
      END IF
C
C     Operate on the data.
C
      Q2PTR = 0
      CALL GEN_POWEREQ(DYNAMIC_MEM(OPTR),NELM,POWER,
     :                 .FALSE.,DYNAMIC_MEM(Q2PTR),FLAGS,FBAD,ERRORS,
     :                 DYNAMIC_MEM(E2PTR),DYNAMIC_MEM(E2PTR),
     :                 DYNAMIC_MEM(Q2PTR),DYNAMIC_MEM(OPTR),FAILURES)
C
C
C     Display the number of times the operation failed
C
      IF (FAILURES.EQ.0) GOTO 500
      STRING = ICH_CI(FAILURES)
      IPT = ICH_LEN(STRING)
      STRING(IPT+2:)='pixels had values that could not be raised to '//
     :               'the desired power.'
      CALL PAR_WRUSER(STRING,IGNORE)

  500 CONTINUE

C     Close down everything

      CALL DSA_CLOSE(STATUS)

      END

