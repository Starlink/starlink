      SUBROUTINE EXTRACT_HEADER (ARRAY)

      IMPLICIT NONE

*     Formal parameters:

      REAL*4   ARRAY(*)

*     Include files:

      INCLUDE 'PROTOTYPE'
      INCLUDE 'MAPHD'

*  Ok, go...

      CALL XCOPY (512, SCAN_HEADER, ARRAY)

      RETURN
      END
