      SUBROUTINE sgs_SAMCU (X,Y)
*+
*   - - - - - -
*    S A M C U
*   - - - - - -
*
*   Sample cursor position.
*
*   Returned:
*       X        r     cursor position in world coordinates (x)
*       Y        r       "       "      "   "        "      (y)
*
*   Read from COMMON:
*       ISZID    i     current zone ID
*       IZTW     i()   zone table - workstation ID
*       NPOLY    i     length of current polyline
*       NTEXT    i     length of current text string
*
*   Constants from GKS_PAR:
*       GSAMPL   i     mode - sample
*
*   Errors:
*       Errror returned by GKS inquiry
*
*   Externals:
*      sgs_SETCU, sgs_OPOLY, sgs_OTEXT, sgs_1ERR, GSMLC, GQNTN, GQNT
*
*   P.T.Wallace, D.L.Terrett   Starlink   7 September 1991
*-

      IMPLICIT NONE

      REAL X,Y

      INCLUDE 'GKS_PAR'

      INCLUDE 'sgscom'

      INCLUDE 'SGS_ERR'


      REAL WINDO(4),VIEWP(4),XNDC,YNDC
      INTEGER JSTAT,IERR,ITNR,ICNTR
      CHARACTER*5 RNAME
      PARAMETER (RNAME='SAMCU')



*  Flush any outstanding plotting
      IF (NPOLY.GT.1) CALL sgs_OPOLY
      IF (NTEXT.GT.0) CALL sgs_OTEXT

*  Sample locator
      CALL GSMLC(ABS(IZTW(ISZID)),1,ITNR,X,Y)

*  Convert position to current transformation
      CALL GQCNTN(IERR,ICNTR)
      IF (IERR.NE.0) THEN
        CALL SGS_1ERR(SGS__INQER,RNAME,'Error returned by GQCNTN',JSTAT)
        GO TO 9999
      END IF
      IF (ITNR.NE.ICNTR) THEN

*    Convert position to NDC
        CALL GQNT(ITNR,IERR,WINDO,VIEWP)
        IF (IERR.NE.0) THEN
          CALL SGS_1ERR(SGS__INQER,RNAME,'Error returned by GQNT',JSTAT)
          GO TO 9999
        END IF
        XNDC = (X - WINDO(1))/(WINDO(2) - WINDO(1)) *
     :         (VIEWP(2) - VIEWP(1)) + VIEWP(1)
        YNDC = (Y - WINDO(3))/(WINDO(4) - WINDO(3)) *
     :         (VIEWP(4) - VIEWP(3)) + VIEWP(3)

*    Convert to current norm trans
        CALL GQNT(ICNTR,IERR,WINDO,VIEWP)
        IF (IERR.NE.0) THEN
           CALL SGS_1ERR(SGS__INQER,RNAME,'Error returned by GQNT',
     :                                                            JSTAT)
           GO TO 9999
        END IF
        X = (XNDC - VIEWP(1))/(VIEWP(2) - VIEWP(1)) *
     :      (WINDO(2) - WINDO(1)) + WINDO(1)
        Y = (YNDC - VIEWP(3))/(VIEWP(4) - VIEWP(3)) *
     :      (WINDO(4) - WINDO(3)) + WINDO(3)
      END IF

*  Set new cursor position                                
      CALL sgs_SETCU(X,Y)

 9999 CONTINUE

      END
