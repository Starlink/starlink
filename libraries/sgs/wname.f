      SUBROUTINE sgs_WNAME (ACTROU, INTARG, JSTAT)
*+
*   - - - - - -
*    W N A M E
*   - - - - - -
*
*   Call the action routine once for every workstation name found,
*   passing to it the workstation name and its associated description.
*
*   GKS is opened with unit 6 as the error channel if not already open.
*   (See sgs_OPEN for a discussion of the choice of error channel.)
*
*   Given:
*      ACTROU     routine  action routine
*      INTARG     i        argument passed to action routine
*
*   Returned:
*      JSTAT      i        status = 0 if success
*
*   Externals:
*        gns_GWNG, GQOPS, GOPKS
*        
*   P.T.Wallace, D.L.Terrett   Starlink   7 September 1991
*-

      IMPLICIT NONE

      EXTERNAL ACTROU
      INTEGER INTARG,JSTAT

      INCLUDE 'GKS_PAR'


      LOGICAL gns_FILTG
      EXTERNAL gns_FILTG
      INTEGER ICNTX, LDESCR, IOPSTA
      CHARACTER*15 WSNAME
      CHARACTER*72 DESCR
      CHARACTER*5 RNAME
      PARAMETER (RNAME='WNAME')



*  Initialize context variable
      ICNTX = 0
      JSTAT = 0

*  Open GKS (unless already open)
      CALL GQOPS(IOPSTA)
      IF (IOPSTA.EQ.GGKCL) CALL GOPKS(6,-1)

  100    CONTINUE
         CALL gns_GWNG(gns_FILTG, ICNTX, WSNAME, DESCR, LDESCR, JSTAT)
         IF (JSTAT.NE.0) GO TO 9999

         IF (ICNTX.NE.0) THEN
            CALL ACTROU(WSNAME, DESCR(:LDESCR), INTARG, JSTAT)

*        Go back for next name
            GO TO 100
         END IF

 9999 CONTINUE

      END
