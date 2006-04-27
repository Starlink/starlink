      SUBROUTINE sgs_OPEN (WKSTN, IZONID, JSTAT)
*+
*   - - - - -
*    O P E N
*   - - - - -
*
*   Open SGS with logical unit 6 for errors, and one workstation.
*
*   When the "Starlink" GKS error handler is being used, all errors are
*   sent to the Starlink error reporting system, and the error channel
*   passed to GOPKS is never used - except that the channel may be
*   opened which causes an empty file to be created on some systems.
*   This can be avoided by using a channel which is pre-connected to the
*   terminal (usually 6).  This may not be appropriate on all systems.
*
*   Given:
*      WKSTN      c      workstation name
*
*   Returned:
*      IZONID     i      zone identifier
*      JSTAT      i      status: 0=OK
*
*   Externals:
*      sgs_INIT, sgs_OPNWK
*
*   P.T.Wallace, D.L.Terrett   Starlink   14 September 1991
*-

      IMPLICIT NONE

      CHARACTER*(*) WKSTN
      INTEGER IZONID,JSTAT


*  Initialise SGS
      CALL sgs_INIT(6,JSTAT)

*  Open workstation
      CALL sgs_OPNWK(WKSTN,IZONID,JSTAT)

      END
