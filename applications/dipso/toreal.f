      SUBROUTINE TOREAL (STRING,RVALUE,OK)

*    Description :
*     Read a real number from a character string.

*    Type definitions :
      IMPLICIT NONE

      CHARACTER STRING*(*)
      REAL   RVALUE
      LOGICAL OK
      INTEGER SLEN

*    Local variables :
      CHARACTER FORMAT*8,   COUNT*3
      INTEGER NCHAR, CODE

      NCHAR = SLEN(STRING)
      WRITE (COUNT, '(I3)', IOSTAT=CODE) NCHAR
      IF (CODE .NE. 0) THEN
         OK = .FALSE.
      ELSE
         FORMAT = '(G'//COUNT//'.0)'
         READ (STRING(1:NCHAR),FORMAT, IOSTAT=CODE) RVALUE
         IF (CODE .NE. 0) THEN
            OK=.FALSE.
         END IF
      END IF

      END
