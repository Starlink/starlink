      SUBROUTINE sgs_FLUSH
*+
*   - - - - - -
*    F L U S H
*   - - - - - -
*
*   Flush any pending output on all active workstations.
*
*   Externals:
*      sgs_OTEXT, sgs_OPOLY, sgs_1ERR, GQOPS, GQACWK, GUWK
*
*   Constants from GKS_PAR:
*      GWSAC      i       at least one workstation active
*      GSGOP      i       segment open
*      GPOSTP     i       regeneration postponed
*
*   Errors:
*      Error returned by GKS inquiry
*
*   P.T.Wallace, D.L.Terrett   Starlink   14 September 1991
*-

      IMPLICIT NONE

      INCLUDE 'GKS_PAR'

      INCLUDE 'SGS_ERR'


      INTEGER IERR,N,NACT,IWKID,JSTAT,IOPSTA
      CHARACTER*5 RNAME
      PARAMETER (RNAME='FLUSH')



*  Flush the text & polyline buffers
      CALL sgs_OTEXT
      CALL sgs_OPOLY

*  See if any workstations are active
      CALL GQOPS(IOPSTA)
      IF (IOPSTA.EQ.GWSAC.OR.IOPSTA.EQ.GSGOP) THEN

*     Find how many active workstations there are
         N = 1
         CALL GQACWK(N,IERR,NACT,IWKID)
         IF (IERR.NE.0) THEN
            CALL sgs_1ERR(SGS__INQER,RNAME,'Error returned by GQACWK',
     :      JSTAT)
            GO TO 9999
         END IF

*     Update all active workstations
         DO 10 N = 1,NACT
            CALL GQACWK(N,IERR,NACT,IWKID)
            IF (IERR.EQ.0) THEN
               CALL GUWK(IWKID,GPOSTP)
            ELSE
               CALL sgs_1ERR(SGS__INQER,RNAME,
     :                                'Error returned by GQACWK', JSTAT)
            END IF
   10    CONTINUE
      END IF

 9999 CONTINUE

      END
