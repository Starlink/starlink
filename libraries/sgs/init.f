      SUBROUTINE sgs_INIT (LUN, JSTAT)
*+
*   - - - - -
*    I N I T
*   - - - - -
*
*   Open SGS.
*
*   Open GKS (unless already open), Set default values in the SGS
*   common block.
*
*   Given:
*      LUN      i     logical unit for error messages
*
*   Returned:
*      JSTAT    i     status: 0=OK
*
*   Externals:
*      sgs_ISTAT, sgs_1GKSIN, sgs_1SGSIN
*
*   P.T.Wallace, D.L.Terrett   Starlink   13 January 1992
*-

      IMPLICIT NONE

      INTEGER LUN,JSTAT



*  Initialise status handling mode if not already set
      CALL sgs_ISTAT(-1, JSTAT)

*  Initialise GKS
      CALL sgs_1GKSIN(LUN,JSTAT)

*  Initialise SGS
      CALL sgs_1SGSIN(JSTAT)

      END
