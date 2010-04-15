C+
      SUBROUTINE DTA_DTATYP (HDSTYPE,TYPE,NCHAR,STATUS)
C
C     D T A _ D T A T Y P
C
C     Converts an HDS type into the corresponding DTA type.
C     In most cases, there is a simple correspondence between
C     DTA and HDS types, but HDS supports character strings
C     whereas DTA only supports arrays of single characters.
C     This routine will return all HDS _CHAR*n types as the
C     DTA type CHAR, with NCHAR set to the value of n.
C
C     Parameters -   (">" input, "<" output)
C
C     (>) HDSTYPE (Character) The HDS type.  Should be in upper
C                 case.
C     (<) TYPE    (Character) The DTA type corresponding to HDSTYPE,
C                 if HDSTYPE represents a primitive data type.  If
C                 HDSTYPE represents a structure, TYPE will be
C                 returned set equal to HDSTYPE.
C     (<) NCHAR   (Integer) If HDSTYPE='_CHAR*n' this will be the
C                 string length.
C     (<) STATUS  (Integer) Returned status code.
C                 0 => OK
C                 DTA_INVPAR => HDSTYPE invalid.
C
C     Common variables used - None
C
C     Subroutines / functions used - None
C
C                                         KS / AAO 13th March 1986
C     Modified:
C
C     7th Jan 1992  KS/AAO. Include files no longer specified using a logical
C                   name, and now specified in lower case.  Routine will now
C                   compile on a SUN.
C     25th Jul 1996 MJCL/Starlink, UCL. Moved DTATYPES.
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NCHAR, STATUS
      CHARACTER*(*) TYPE, HDSTYPE
C
C     DTA system error codes
C
      INCLUDE 'DTACODES'
C
C     Data types.  Defines the following -
C
C     NHTYPES    Number of Types supported by HDS
C     HTYPES     Names of these HDS types
C     DTYPES     Names of the corresponding DTA types
C
      INCLUDE 'DTATCON'
C
C     Local variables
C
      INTEGER I
      CHARACTER STRING*12
C
C     Data types - contains DATA statements
C
      INCLUDE 'DTATYPES'
C
C     Assume input is OK
C
      STATUS=0
      NCHAR=0
C
C     Test for character type
C
      STRING=HDSTYPE
      IF (STRING(1:6).EQ.'_CHAR*') THEN
         READ (STRING(7:),*,IOSTAT=STATUS) NCHAR
         IF (STATUS.NE.0) THEN
            STATUS=DTA_INVPAR
            GO TO 600
         END IF
         TYPE='CHAR'
         IF (NCHAR.EQ.1) NCHAR=0
      ELSE
C
C        Ordinary, ie non-character string type.  Just look for it in the
C        list of types, use the corresponding DTA type if found,
C        use the original value if not.
C
         DO I=1,NHTYPES
            IF (HDSTYPE.EQ.HTYPES(I)) THEN
               TYPE=DTYPES(I)
               GO TO 600
            END IF
         END DO
         TYPE=HDSTYPE
      END IF
C
  600 CONTINUE
C
      END

