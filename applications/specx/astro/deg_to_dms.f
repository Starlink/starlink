C-----------------------------------------------------------------------

      SUBROUTINE DEG_TO_DMS (RA,IRA)

      INTEGER IRA(4)
      REAL*8  RA,XRA

      ISIGN = 1
      IF (RA.LT.0.D0) ISIGN = -1
      XRA   = DABS (RA)

      DO I = 1,4
        IRA(I) = XRA
        XRA    = (XRA-IRA(I))*60.D0
      END DO
      IRA(4) = 1.6666666*IRA(4)

      I = 1
      DO WHILE (IRA(I).EQ.0 .AND. I.LT.4)
        I = I+1
      END DO
      IRA(I) = ISIGN*IRA(I)

      RETURN
      END

C-------------------------------------------------------------------------

