      SUBROUTINE PROMPT (LU, STRING, NONL)
*+
*  - - - - - - -
*   P R O M P T
*  - - - - - - -
*
*  !!! Version for VAX, which (like some other platforms, including !!!
*  !!! DECstation and Sun SPARCstation) uses $ to suppress newline. !!!
*
*  Write a prompt string, with or without suppressing the newline.
*
*  Given:
*     LU     i      I/O unit number
*     STRING c*(*)  prompt string
*     NONL   l      "no newline" flag - .TRUE. = suppress newline
*
*  P T Wallace   Starlink   2 June 1992
*-

      IMPLICIT NONE

      INTEGER LU
      CHARACTER*(*) STRING
      LOGICAL NONL



      IF (NONL) THEN
         WRITE (LU,'(1X,A,$)') STRING        !!! Not ANSI !!!
      ELSE
         WRITE (LU,'(1X,A)') STRING
      END IF

      END
