C+
      SUBROUTINE DTA_DECDIM (STRING,IST,MAXDIM,IEND,NDIM,DIMS,STATUS)
C
C     D T A _ D E C D I M
C
C     Decodes a dimension specification from an object name.  Such
C     a specification is a string of numeric values separated by
C     commas and enclosed within square brackets, eg "[10,20,30]".
C     It returns the number of dimensions, an array giving
C     each dimension, and the position of the terminating "]".
C
C     Parameters -   (">" input, "<" output)
C
C     (>) STRING   (Character) An object name in which the dimension
C                  specification is embedded.   A space is treated as
C                  terminating the string.
C     (>) IST      (Integer) the start of the dimension specification
C                  in the string - ie STRING(IST:IST) is the "[" that
C                  begins the specification.
C     (>) MAXDIM   (Integer) The maximum number of dimensions
C                  expected - ie the dimension of the array DIMS.
C     (<) IEND     (Integer) the end of the dimension specification
C                  in the string - ie STRING(IEND:IEND) is the "]" that
C                  ends the specification.
C     (<) NDIM     (Integer) Returns the number of dimensions
C                  specified.
C     (<) DIMS     (Integer DIMS(MAXDIM)) Returns the values of
C                  the dimension specifiers in NAME.
C     (<) STATUS   (Integer) Returns a status code.
C                  0 => OK, possible error codes are
C                  DTA_INVDIM => Invalid dimension specification
C                  DTA_TOODEEP => Too many dimensions
C
C     Example:  If STRING = 'OUTPUT.AXES[1,3].DATA' and IST = 12
C
C     DTA_DECDIM will return   IEND=16  NDIM=2  DIMS(1)=1  DIMS(2)=3
C
C     Functions / subroutines used -  Only Fortran standard functions
C
C                                       KS / AAO 30th May 1988
C     Modified:
C
C     1st Sept 1988  Test disalowing leading zeros removed.  KS/AAO.
C     8th Jan  1992.  KS / AAO.  Syntax of include statements changed to
C                     remove VMS logical names and to use lower case, to
C                     enable compilation on a SUN. Unused variable I deleted.
C+
      IMPLICIT NONE
C
C     Parameters -
C
      CHARACTER*(*) STRING
      INTEGER IST,IEND,MAXDIM,NDIM,DIMS(MAXDIM),STATUS
C
C     Error codes -
C
      INCLUDE 'DTACODES'
C
C     Local variables -
C
      LOGICAL ENDED
      INTEGER J,N,NCHR
      CHARACTER CHR
C
C     Start to decode the dimension information.  Must end with a ],
C     and dimensions are delimited by commas.  Each dimension value in
C     turn is summed up in N.
C
C     First setup the initial pointers etc
C
      IF ((STRING(IST:IST).NE.'[').OR.(IST.GE.LEN(STRING))) THEN
         STATUS=DTA_INVDIM
         ENDED=.TRUE.
      ELSE
         NDIM=1
         N=0
         J=IST+1
         ENDED=.FALSE.
      END IF
C
C     This loop continues until 'ended' is signalled, either by the
C     end of STRING being reached, the end of the specification being
C     reached, or an error being detected.
C
      DO WHILE (.NOT.ENDED)
         CHR=STRING(J:J)
         IF (CHR.EQ.']') THEN
C
C           End of specification
C
            IF (N.EQ.0) THEN
               STATUS=DTA_INVNAM
               ENDED=.TRUE.
            END IF
            DIMS(NDIM)=N
            STATUS=0
            ENDED=.TRUE.
            IEND=J
         ELSE IF (CHR.EQ.',') THEN
C
C           Dimension delimiter
C
            IF (N.EQ.0) THEN
               STATUS=DTA_INVDIM
               ENDED=.TRUE.
            END IF
            DIMS(NDIM)=N
            N=0
            NDIM=NDIM+1
            IF (NDIM.GT.MAXDIM)  THEN
               STATUS=DTA_TOODEEP
               ENDED=.TRUE.
            END IF
         ELSE
C
C           Ordinary character - should be numeric!
C
            NCHR=ICHAR(CHR)-ICHAR('0')
            IF ((NCHR.LT.0).OR.(NCHR.GT.9)) THEN
               STATUS=DTA_INVDIM
               ENDED=.TRUE.
            END IF
            N=N*10+NCHR
         END IF
C
C        Point to the next character in the string.
C
         IF (.NOT.ENDED) THEN
            J=J+1
            IF (J.GT.LEN(STRING)) THEN
               STATUS=DTA_INVDIM
               ENDED=.TRUE.
            END IF
         END IF
      END DO
C
      END

