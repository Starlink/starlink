C+
      SUBROUTINE DTA_HDSTYP (TYPE,NCHAR,HDSTYPE,STATUS)
C
C     D T A _ H D S T Y P
C
C     Converts a DTA_ type into the corresponding HDS type.
C     In most cases, there is a simple correspondence between
C     DTA and HDS types, but HDS supports character strings
C     whereas DTA only supports arrays of single characters.
C     This is handled by treating a DTA character array as an
C     HDS array (of one less dimension) of strings with length
C     given by the first dimension.  Hence the NCHAR argument to
C     this routine.
C
C     Parameters -   (">" input, "<" output)
C
C     (>) TYPE    (Character) The DTA type.  Should be in upper
C                 case.
C     (>) NCHAR   (Integer) If TYPE='CHAR' this should be the
C                 string length to be used.  HDSTYPE will then
C                 be returned as '_CHAR*n' where n=NCHAR.  If
C                 TYPE is not 'CHAR', NCHAR is ignored.
C     (<) HDSTYPE (Character) The HDS type corresponding to TYPE,
C                 if TYPE represents a primitive data type.  If
C                 TYPE represents a structure, HDSTYPE will be
C                 returned set equal to TYPE.
C     (<) STATUS  (Integer) Returned status code.
C                 0 => OK
C                 DTA_INVPAR => TYPE or NCHAR invalid.
C
C     Common variables used - None
C
C     Subroutines / functions used - None
C
C                                         KS / AAO 12th March 1986
C     Modified:
C
C     8th  Jan  1992.  KS / AAO.  Syntax of include statements changed to
C                      remove VMS logical names and to use lower case, to
C                      enable compilation on a SUN.
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
C     NTYPES     Number of Types supported by DTA
C     TYPES      Names of these DTA types
C     HTYPE      Names of the corresponding HDS types
C
      INCLUDE 'DTATCON'
C
C     Local variables
C
      INTEGER I, N
      CHARACTER FORMT*16
C
C     Include file containing DATA statements
C
      INCLUDE 'DTATYPES'
C
C     Assume input is OK
C
      STATUS=0
C
C     Test for character type
C
      IF (TYPE.EQ.'CHAR') THEN
         IF ((NCHAR.LT.0).OR.(NCHAR.GT.65535)) THEN
            STATUS=DTA_INVPAR
            GO TO 600
         ELSE
            IF ((NCHAR.EQ.0).OR.(NCHAR.EQ.1)) THEN
               HDSTYPE='_CHAR'
            ELSE
C
C              This is messy, using run-time formatting to get the
C              _CHAR*n format with any leading blanks or zeros in n.
C
               N=ALOG10(FLOAT(NCHAR))+1
               FORMT='(''_CHAR*'',I'//CHAR(ICHAR('0')+N)//')'
               WRITE (HDSTYPE,FORMT,IOSTAT=STATUS) NCHAR
               IF (STATUS.NE.0) THEN
                  STATUS=DTA_INVPAR
                  GO TO 600
               END IF
            END IF
         END IF
      ELSE
C
C        Ordinary, ie non-character type.  Just look for it in the
C        list of types, use the corresponding HDS type if found,
C        use the original value if not.
C
         DO I=1,NTYPES
            IF (TYPE.EQ.TYPES(I)) THEN
               HDSTYPE=HTYPE(I)
               GO TO 600
            END IF
         END DO
         HDSTYPE=TYPE
      END IF
C
  600 CONTINUE
C
      END

