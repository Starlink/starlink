      SUBROUTINE EQEP (E,J)
*+
*
*     - - - - -
*      E Q E P
*     - - - - -
*
*  Validate equinox or epoch
*
*  Given:
*     E     d     equinox or epoch
*
*  Returned:
*     J     i     status:  0 = OK
*
*+

      IMPLICIT NONE

      DOUBLE PRECISION E
      INTEGER J


      IF (E.LT.1800D0.OR.E.GT.2200D0) THEN
         J=1
      ELSE
         J=0
      END IF

      END
