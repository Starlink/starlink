      SUBROUTINE sgs_SW (X1, X2, Y1, Y2, JSTAT)
*+
*   - - -
*    S W
*   - - -
*
*   Set window for current zone.
*
*   The smaller of X1 and X2, and of Y1 and Y2, are the coordinates of
*   the bottom left-hand corner of the resulting window.  Both extents
*   must be greater than zero.
*
*   Given:
*      X1        r       window limit x
*      X2        r          "     "   x
*      Y1        r          "     "   y
*      Y2        r          "     "   y
*      JSTAT     i       inherited status (if option selected)
*
*   Returned:
*      JSTAT     i       status (0=OK)
*
*   Read from COMMON:
*      ISZID     i       current zone id
*      NPOLY     i       length of current polyline
*      NTEXT     i       length of current text
*
*   Written to COMMON:
*      ZTW       r()     zone table - window
*
*   Externals:
*      sgs_1HSTAT, sgs_1BNORM, sgs_1GKERR, sgs_OPOLY, sgs_OTEXT, GSWN
*
*   P.T.Wallace, D.L.Terrett   Starlink   14 September 1991
*-

      IMPLICIT NONE

      REAL X1,X2,Y1,Y2
      INTEGER JSTAT

      REAL X1N,X2N,Y1N,Y2N
      CHARACTER RNAME*2
      PARAMETER (RNAME='SW')
 
      INCLUDE 'sgscom'

      INCLUDE 'SGS_ERR'




*  Handle incoming status
      CALL sgs_1HSTAT(JSTAT)
      IF (JSTAT.NE.0) GO TO 9999

*  Finish any plotting
      IF (NPOLY.GT.1) CALL sgs_OPOLY
      IF (NTEXT.GT.0) CALL sgs_OTEXT

*  Normalise the window
      CALL sgs_1BNORM(X1,X2,Y1,Y2,X1N,X2N,Y1N,Y2N,JSTAT)
      IF (JSTAT.NE.0) THEN
         CALL SGS_1ERR(SGS__FLSWN, RNAME,'Failed to set window',JSTAT)
         GO TO 9999
      END IF

*  Update zone table
      ZTW(1,ISZID)=X1N
      ZTW(2,ISZID)=X2N
      ZTW(3,ISZID)=Y1N
      ZTW(4,ISZID)=Y2N

*  Set window
      CALL GSWN(1,X1N,X2N,Y1N,Y2N)
      CALL sgs_1GKERR(RNAME,JSTAT)

*  Exit
 9999 CONTINUE

      END
                                                           
