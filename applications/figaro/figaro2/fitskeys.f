C+
C                         F I T S K E Y S
C
C  Name: FITSKEYS
C
C  Function:
C     List the contents of a FITS-specific structure.
C
C  Description:
C     This Figaro program lists all the items in the FITS-specific
C     substructure associated with a data structure.  It doesn't
C     give any information that can't be obtained using EXAM, but
C     it presents it in a more convenient form, especially when the
C     structure contains multiple comment keywords.
C
C  Command Parameters:
C     INPUT     (Character) Name of structure whose FITS substructure
C               is to be listed.
C
C  Command keywords: None.
C
C  Support: K. Shortridge, AAO
C
C  Version date: 8th May 1990.
C
C  History:
C     8th  May 1990.  KS/AAO. Original version (based on example taken
C                     for Figaro Programmer's Guide).
C     13th Oct 1992.  HME / UoE, Starlink.  Renamed from FITSLIST to
C                     FITSKEYS.
C     11th Apr 1996.  Malcolm J. Currie, Starlink.  Added status checks
C                     to prevent an infinite loop if a format-conversion
C                     fails.
C     18th Jul 1996.  MJCL / Starlink, UCL.  Set variables for storage of
C                     file names to 132 chars.
C+
      SUBROUTINE FITSKEYS
C
      IMPLICIT NONE
C
C     Functions
C
      INTEGER   ICH_LEN
      CHARACTER ICH_CD*22, ICH_CF*13, ICH_CI*11
C
C     Local variables
C
      CHARACTER ACCESS*1          ! Routine to use to access item
      CHARACTER COMMENT*80        ! Comment associated with item
      DOUBLE PRECISION DVALUE     ! Used to hold double prec values
      LOGICAL   EXIST             ! True if Nth item exists
      INTEGER   I                 ! Loop index through elements
      INTEGER   IGNORE            ! Dummy status argument
      INTEGER   IPTR              ! Posn in OUTSTR to start next field
      INTEGER   IVALUE            ! Used to hold integer values
      LOGICAL   LVALUE            ! Used to hold logical values
      INTEGER   N                 ! Counter through items
      CHARACTER NAME*16           ! Name of item
      INTEGER   NELM              ! Number of elements in item
      CHARACTER OUTSTR*132        ! String used for final output
      INTEGER   STATUS            ! Running status for DSA routines
      CHARACTER STRING*80         ! String to hold values of items
      INTEGER   STRLEN            ! Length of character items
      CHARACTER STRUCTURE*132     ! Full name of structure
      REAL      VALUE             ! Used to hold real values
C
C     Initialise DSA and open input structure
C
      STATUS=0
      CALL DSA_OPEN (STATUS)
      CALL DSA_INPUT ('INPUT','INPUT',STATUS)
      IF (STATUS.NE.0) GO TO 500       ! Error exit
C
C     Header
C
      CALL PAR_WRUSER (' ',IGNORE)
      CALL DSA_GET_ACTUAL_NAME ('INPUT',STRUCTURE,STATUS)
      CALL PAR_WRUSER ('FITS keywords in '//
     :                    STRUCTURE(:ICH_LEN(STRUCTURE)),IGNORE)

      CALL PAR_WRUSER (' ',IGNORE)
C
C     Set up loop through all items in FITS substructure.  We loop
C     so long as the last item existed.  Once we reach an item that
C     doesn't exist, we can stop.
C
      N = 1
      EXIST = .TRUE.
      DO WHILE (EXIST.AND.STATUS.EQ.0)
C
C        See if the Nth item exists, and get its details.
C
         CALL DSA_NTH_FITS_ITEM ('INPUT',N,EXIST,NAME,ACCESS,
     :                                          NELM,STRLEN,STATUS)
         IF (EXIST) THEN
            IF (ACCESS.EQ.' ') THEN
               CALL PAR_WRUSER ('Non-standard FITS item: '//
     :                             NAME(:ICH_LEN(NAME)),IGNORE)
            ELSE
C
C              We can handle this item.  For all its elements,
C              either read strings directly into STRING or read
C              numeric values into variables of suitable type
C              and then format them into STRING.
C
               DO I=1,NELM
C
C                 Character strings
C
                  IF (ACCESS.EQ.'C') THEN
                     CALL DSA_GET_FITS_C ('INPUT',NAME,I,STRING,
     :                                              COMMENT,STATUS)
C
C                 Logical values
C
                  ELSE IF (ACCESS.EQ.'L') THEN
                     CALL DSA_GET_FITS_L ('INPUT',NAME,I,LVALUE,
     :                                              COMMENT,STATUS)
                     IF (LVALUE) THEN
                        STRING='True'
                     ELSE
                        STRING='False'
                     END IF
C
C                 Double precision floating point
C
                  ELSE IF (ACCESS.EQ.'D') THEN
                     CALL DSA_GET_FITS_D ('INPUT',NAME,I,DVALUE,
     :                                              COMMENT,STATUS)
                     STRING=ICH_CD(DVALUE)
C
C                 Single precision floating point
C
                  ELSE IF (ACCESS.EQ.'F') THEN
                     CALL DSA_GET_FITS_F ('INPUT',NAME,I,VALUE,
     :                                              COMMENT,STATUS)
                     STRING=ICH_CF(VALUE)
C
C                 Both long and short integers
C
                  ELSE IF ((ACCESS.EQ.'I').OR.(ACCESS.EQ.'S')) THEN
                     CALL DSA_GET_FITS_I ('INPUT',NAME,I,IVALUE,
     :                                              COMMENT,STATUS)
                     STRING=ICH_CI(IVALUE)
                  END IF
C
C                 Now format the name and its value and the comment
C                 into one string and output it.
C
                  IF (STATUS.EQ.0) THEN
                     OUTSTR=NAME
                     IPTR=MAX(ICH_LEN(NAME)+1,10)
                     OUTSTR(IPTR:)=STRING
                     IPTR=MAX(ICH_LEN(OUTSTR)+1,30)
                     OUTSTR(IPTR:)=COMMENT
                     CALL PAR_WRUSER (OUTSTR(:ICH_LEN(OUTSTR)),IGNORE)
                  END IF
               END DO
            END IF
C
C           Increment N ready for the next item in the substructure
C
            N=N+1
         END IF
      END DO
C
      IF (N.EQ.1) CALL PAR_WRUSER ('No FITS keywords found.',IGNORE)
C
C     Exit
C
  500 CONTINUE
      CALL DSA_CLOSE (STATUS)
C
      END
