*-----------------------------------------------------------------------

      REAL FUNCTION AMOM1 (NC1, NC2, BADVAL, DATA)

      IMPLICIT  NONE

*     Formal parameters

      INTEGER   NC1
      INTEGER   NC2
      REAL      BADVAL
      REAL      DATA(*)

*     Local variables

      INTEGER   N
      REAL      XBAR
      REAL      XN

*  Ok, go...

      XBAR = 0.0
      XN   = 0.0

      DO N = NC1, NC2
       IF (DATA(N).NE.BADVAL) THEN
         XN   = XN   + DATA(N)
         XBAR = XBAR + DATA(N)*N
       END IF
      END DO

      AMOM1 = XBAR/XN

      RETURN
      END

*-----------------------------------------------------------------------

