      SUBROUTINE sgs_1GKERR (RNAME, JSTAT)
*+
*   - - - - -
*    G K E R R       (Internal routine)
*   - - - - -
*
*   Test for GKS error.
*
*   If the status is bad on entry, it is returned unchanged.  If
*   not, and if there has been a GKS error since this routine
*   was last called, an error value is returned and an error
*   reported.
*
*   This routine will only detect a GKS error if it is the last
*   error reported and the GKS error has not been flushed - i.e. only
*   GKS errors that occured after the error stack was marked can be
*   detected.
*
*   It will only work if the Starlink GKS error handler which reports
*   errors via EMS is present.
*
*   Given:
*      RNAME     c      name of calling routine
*      JSTAT     i      status
*
*   Returned:
*      JSTAT     i      status
*
*   Externals:
*       sgs_1ERR, ems_STAT
*
*   Errors:
*      GKS error
*
*   P.T.Wallace, D.L.Terrett   Starlink   7 September 1991
*-

      IMPLICIT NONE

      CHARACTER*(*) RNAME
      INTEGER JSTAT

      INCLUDE 'SGS_ERR'

      INCLUDE 'GKS_ERR'


      INTEGER LASTER



      IF (JSTAT.EQ.0) THEN

*     Test error stack for the presence of an error
         CALL ems_STAT(LASTER)
         IF ( LASTER.EQ.GKS__ERROR)
     :      CALL sgs_1ERR(SGS__GKSER,RNAME,'GKS error detected',JSTAT)
      END IF

      END
