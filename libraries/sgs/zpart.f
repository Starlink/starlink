      SUBROUTINE sgs_ZPART (NX,NY, IZONID, JSTAT)
*+
*   - - - - - -
*    Z P A R T
*   - - - - - -
*
*   Partition the current zone into NX by NY segments.  The current
*   zone is unchanged.
*
*   Given:
*      NX        i       number of zones in horizontal direction
*      NY        i       number of zones in vertical direction
*      JSTAT     i       inherited status (if option selected)
*
*   Returned:
*      IZONID    i()     array of zone identifiers for new zones
*      JSTAT     i       status (0=OK)
*
*   Externals:
*      sgs_1HSTAT, sgs_ICURZ, sgs_IZONE, sgs_ZONE, sgs_SELZ, sgs_RELZ
*
*   Errors:
*      Too many zones
*
*   P.T.Wallace, D.L.Terrett   Starlink   14 September 1991
*-

      IMPLICIT NONE

      INTEGER NX,NY,IZONID(NX*NY),JSTAT

      INTEGER I,J,II,IZ
      REAL X1,X2,Y1,Y2,XM,YM,XSTEP,YSTEP,X1S



*  Handle incoming status
      CALL sgs_1HSTAT(JSTAT)
      IF (JSTAT.NE.0) GO TO 9999

*  Get the limits and ID of the current zone
      CALL sgs_IZONE(X1,X2,Y1,Y2,XM,YM)
      CALL sgs_ICURZ(IZ)

*  Calculate step size and save start X
      XSTEP = (X2 - X1) / NX
      YSTEP = (Y2 - Y1) / NY
      X1S = X1

*  Create the zones
      DO 20 J = 1,NY
         X1 = X1S
         DO 10 I = 1,NX
            CALL sgs_ZONE(X1,MIN(X1+XSTEP,X2),Y1,MIN(Y1+YSTEP,Y2),
     :                                         IZONID(I+(J-1)*NX),JSTAT)
            IF (JSTAT.NE.0) GO TO 9000
            CALL sgs_SELZ(IZ,JSTAT)
            X1 = X1 + XSTEP
   10    CONTINUE
         Y1 = Y1 + YSTEP
   20 CONTINUE

*  Success
      GO TO 9999

 9000 CONTINUE

*  A zone create failed: delete all the ones created so far
      CALL sgs_SELZ(IZ,JSTAT)
      DO 30 II = 1,(J-1)*NX + I - 1
         CALL sgs_RELZ(IZONID(II))
   30 CONTINUE

 9999 CONTINUE

      END
