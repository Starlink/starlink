C+
      SUBROUTINE DTA_SPLITN (NAME,MAXLEV,MAXDIM,LEVELS,LASTC,
     :                                           NDIM,DIMS,STATUS)
C
C     D T A _ S P L I T N
C
C     Splits an object name into its component parts.  The name
C     is actually returned unchanged, but two arrays are filled,
C     one listing the positions within the name of the ends
C     of each of the name components, the other giving the
C     dimension information (if any) from the name.
C
C     Parameters -   (">" input, "<" output)
C
C     (>) NAME     (Character) The full object name, with the
C                  components separated by periods and the
C                  dimension information (if any) enclosed in
C                  square braces and separated by commas.  Should
C                  be terminated either by a space or the end of
C                  the string.
C     (>) MAXLEV   (Integer) The maximum number of components -
C                  ie the dimension of the array LASTC.
C     (>) MAXDIM   (Integer) The maximum number of dimensions
C                  expected - ie the dimension of the array DIMS.
C     (<) LEVELS   (Integer) Returns the number of component names
C                  in the object name.
C     (<) LASTC    (Integer LASTC(MAXLEV)) Returns the position
C                  in NAME of the last characters of each of the
C                  component names.  The first character is
C                  in position 1, not 0.
C     (<) NDIM     (Integer) Returns the number of dimensions
C                  for the final component specified in NAME.
C     (<) DIMS     (Integer DIMS(MAXDIM)) Returns the values of
C                  the final dimension specifiers in NAME.
C     (<) STATUS   (Integer) Returns a status code.
C                  0 => OK, possible error codes are
C                  DTA_INVDIM => Invalid dimension specification
C                  DTA_INVNAM => Invalid component name
C                  DTA_TOODEEP => Too many components or dimensions
C                  Lower level routines may return other error codes.
C
C     Note that the dimension specifications returned in NDIM and DIMS
C     are those of the object specified by the name - higher level
C     component names may be dimensioned, and if so are checked for
C     validity, but their values are not returned in DIMS.  The
C     dimensions of such names are treated as part of the name, but
C     the dimensions of the final component are not.
C
C     Example:  If NAME = 'OUTPUT.AXES[5].DATA[1524,62]'
C
C     DTA_SPLITN will return   LEVELS=3  LASTC(1)=6  LASTC(2)=14
C                              LASTC(3)=19   NDIM=2  DIMS(1)=1524
C                              DIMS(2)=62
C
C     Functions / subroutines used -
C
C     DTA_DECDIM   (DTA_ package) Decode dimension information.
C
C                                       KS / CIT  14th Oct 1982
C     Modified:
C
C     30th May 1988.  KS/AAO.  Now supports arrays of structures.
C                     Note that a ']' can no longer be taken as
C                     terminating the string.
C     8th  Jan 1992.  KS / AAO.  Syntax of include statements changed to
C                     remove VMS logical names and to use lower case, to
C                     enable compilation on a SUN.
C     5th  Jul 1993.  KS / AAO. Removed unreferenced 520 CONTINUE statement
C                     which was generating warnings when compiled under ULTRIX.
C+
      IMPLICIT NONE
C
C     Parameters -
C
      CHARACTER*(*) NAME
      INTEGER MAXLEV,MAXDIM,NDIM,LEVELS,STATUS
      INTEGER LASTC(MAXLEV),DIMS(MAXDIM)
C
C     Error codes -
C
      INCLUDE 'DTACODES'
C
C     Local variables -
C
      LOGICAL ARRAY, LEVOK
      INTEGER I, IEND
      CHARACTER CHR*1
C
C     Start to search through the components.  A blank is the
C     end of the name, a [ is the start of dimension info, a
C     period delimits a component.  LEVOK indicates that the
C     current component contains at least one character.  ARRAY
C     indicates that the current component has dimension info.
C
      ARRAY=.FALSE.
      LEVOK=.FALSE.
      LEVELS=1
      NDIM=0
      I=1
      DO WHILE (I.LE.LEN(NAME))
         CHR=NAME(I:I)
         IF (CHR.EQ.' ') THEN
            IF (LEVOK) THEN
                IF (.NOT.ARRAY) LASTC(LEVELS)=I-1
                GO TO 500
            ELSE
                GO TO 530
            END IF
         END IF
         IF (CHR.EQ.'[') THEN
            IF (LEVOK) THEN
               LASTC(LEVELS)=I-1
               ARRAY=.TRUE.
               CALL DTA_DECDIM (NAME,I,MAXDIM,IEND,NDIM,DIMS,STATUS)
               IF (STATUS.NE.0) GO TO 600
               I=IEND
            ELSE
               GO TO 530
            END IF
         END IF
         IF (CHR.EQ.'.') THEN
C
C           Component delimiter
C
            IF (LEVOK) THEN
               LASTC(LEVELS)=I-1
               LEVELS=LEVELS+1
               IF (LEVELS.GT.MAXLEV) GO TO 540
               LEVOK=.FALSE.
               ARRAY=.FALSE.
               NDIM=0
            ELSE
               GO TO 530
            END IF
         ELSE
C
C           An ordinary character
C
            LEVOK=.TRUE.
         END IF
         I=I+1
      END DO
C
C     If loop falls through, we've hit the end of NAME
C
      IF (LEVOK) THEN
         IF (.NOT.ARRAY) LASTC(LEVELS)=LEN(NAME)
         GO TO 500
      ELSE
         GO TO 530
      END IF
C
C     Normal return, all OK
C
  500 CONTINUE
      STATUS=0
      GO TO 600
C
C     Error conditions -
C
  530 CONTINUE
      STATUS=DTA_INVNAM
      GO TO 600
  540 CONTINUE
      STATUS=DTA_TOODEEP
C
  600 CONTINUE
      END

