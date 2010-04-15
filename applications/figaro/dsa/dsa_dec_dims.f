C+
C                      D S A _ D E C O D E _ D I M S
C
C  Routine name:
C     DSA_DECODE_DIMS
C
C  Function:
C     Decodes a dimension specification in a character string.
C
C  Description:
C     Decodes a dimension specification from an object name.  Such
C     a specification is a string of numeric values separated by
C     commas and enclosed within square brackets, eg "[10,20,30]".
C     It returns the number of dimensions, an array giving
C     each dimension, and the position of the terminating "]".
C     Dimension specifications should not have leading zeros.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_DECODE_DIMS (STRING, IST, MAXDIM, IEND, NDIM,
C                                                   DIMS, STATUS)
C
C  Parameters:   (">" input, "<" output, "W" workspace, "!" modified)
C     (>) STRING   (Fixed string,descr) An object name in which the
C                  dimension specification is embedded.   A space is
C                  treated as terminating the string.
C     (>) IST      (Integer,ref) the start of the dimension specification
C                  in the string - ie STRING(IST:IST) is the "[" that
C                  begins the specification.
C     (>) MAXDIM   (Integer,ref) The maximum number of dimensions
C                  expected - ie the dimension of the array DIMS.
C     (<) IEND     (Integer,ref) the end of the dimension specification
C                  in the string - ie STRING(IEND:IEND) is the "]" that
C                  ends the specification.
C     (<) NDIM     (Integer,ref) Returns the number of dimensions
C                  specified.
C     (<) DIMS     (Integer array,ref) Returns the values of
C                  the dimension specifiers in NAME.
C     (!) STATUS   (Integer) Returns a status code. If bad status
C                  is passed, this routine returns immediately.
C
C  External variables used:  None
C
C  External routines used: None
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  History:
C    5th July 1988.  Original version - well, a reworking of DTA_DECDIM,
C                    in an DSA guise.  KS / AAO.
C    21st Aug 1992   Automatic portability modifications
C                    ("INCLUDE" syntax etc) made. KS/AAO
C    23rd Aug 1992   Remove unused variable declarations. KS/AAO
C    29th Aug 1992   "INCLUDE" filenames now upper case. KS/AAO
C+
      SUBROUTINE DSA_DECODE_DIMS (STRING,IST,MAXDIM,IEND,NDIM,
     :                                                DIMS,STATUS)
C
      IMPLICIT NONE
C
C     Parameters -
C
      CHARACTER*(*) STRING
      INTEGER IST,IEND,MAXDIM,NDIM,DIMS(MAXDIM),STATUS
C
C     Error codes -
C
      INCLUDE 'DSA_ERRORS'
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
         STATUS=DSA__INVDIM
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
               STATUS=DSA__INVDIM
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
               STATUS=DSA__INVDIM
               ENDED=.TRUE.
            END IF
            DIMS(NDIM)=N
            N=0
            NDIM=NDIM+1
            IF (NDIM.GT.MAXDIM)  THEN
               STATUS=DSA__INVDIM
               ENDED=.TRUE.
            END IF
         ELSE
C
C           Ordinary character - should be numeric!
C
            NCHR=ICHAR(CHR)-ICHAR('0')
            IF ((NCHR.LT.0).OR.(NCHR.GT.9)) THEN
               STATUS=DSA__INVDIM
               ENDED=.TRUE.
            END IF
            N=N*10+NCHR
            IF ((NCHR.EQ.0).AND.(N.EQ.0)) THEN
               STATUS=DSA__INVDIM
               ENDED=.TRUE.
            END IF
         END IF
C
C        Point to the next character in the string.
C
         IF (.NOT.ENDED) THEN
            J=J+1
            IF (J.GT.LEN(STRING)) THEN
               STATUS=DSA__INVDIM
               ENDED=.TRUE.
            END IF
         END IF
      END DO
C
      END
