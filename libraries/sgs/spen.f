      SUBROUTINE sgs_SPEN (NPEN)
*+
*   - - - - -
*    S P E N
*   - - - - -
*
*   Select a pen for line marker and text plotting.
*
*   Given:
*      NPEN       i     pen number (1,2,3...)
*
*   Read from COMMON:
*      IZTW       i()   zone table - workstation
*      IWTID      i()   workstation table - workstation ID
*      IWTCA      i()        "        "   - category
*      ISZID      i     current zone ID
*      NPOLY      i     length of current polyline
*      NTEXT      i     length of current text string
*
*   Written to COMMON:
*      IPEN       i     current SGS pen
*
*   Constants from GKS_PAR:
*      GOUTPT     i     workstation category - output
*      COUTIN     i          "          "    - input/output
*
*   Externals:
*      sgs_1ERR, sgs_OTEXT, sgs_OPOLY, GQPLR, GSTXCI, GSPMCI, GSPLI
*
*   The following ASF flag settings are assumed:
*
*                     Polyline colour index:   GBUNDL
*                     Polymarker colour index: GINDIV
*                     Text colour index:       GINDIV
*
*   P.T.Wallace, D.L.Terrett   Starlink   14 September 1991
*-

      IMPLICIT NONE

      INTEGER NPEN

      INCLUDE 'sgscom'

      INCLUDE 'GKS_PAR'

      INCLUDE 'SGS_ERR'


      INTEGER IERR,LNTYPE,LWIDTH,ICOL,JSTAT,IWKID
      CHARACTER*4 RNAME
      PARAMETER (RNAME='SPEN')



*  Flush any pending output
      IF (NPOLY.GT.1) CALL sgs_OPOLY
      IF (NTEXT.GT.0) CALL sgs_OTEXT

*  Current workstation ID
      IWKID = ABS(IZTW(ISZID))

*  If not an output workstation skip setting the text and marker colour
      IF (IWTCA(IWKID).NE.GOUTPT.AND.IWTCA(IWKID).NE.GOUTIN) GO TO 9000

*  Inquire the colour index for this pen on the current workstation
      CALL GQPLR(IWTID(IWKID),NPEN,GSET,IERR,LNTYPE,LWIDTH,ICOL)
      IF (IERR.EQ.61) THEN
         CALL sgs_1ERR(SGS__PENER,RNAME,'SGS pen does not exist',JSTAT)
         GO TO 9999
      ELSE IF (IERR.NE.0) THEN
         CALL sgs_1ERR(SGS__INQER,RNAME,'Error returned by GQPLR',JSTAT)
         GO TO 9999
      END IF

*  Set the marker and text attribute to this colour
      CALL GSTXCI(ICOL)
      CALL GSPMCI(ICOL)

9000  CONTINUE

*  Set the pen (for line drawing)
      CALL GSPLI(NPEN)

*  Remember it
      IPEN=NPEN

9999  CONTINUE

      END
