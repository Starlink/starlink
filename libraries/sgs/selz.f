      SUBROUTINE sgs_SELZ (IZONID, JSTAT)
*+
*   - - - - -
*    S E L Z
*   - - - - -
*
*   Select a new zone.
*
*   1) If the current and new zones are on different workstations,
*      the current workstation is deactivated and the new one is
*      activated instead.
*
*   2) The window/viewport for the new zone is set.
*
*   Given:
*      IZONID        i        zone identifier
*      JSTAT         i        inherited status (if option selected)
*
*   Returned:
*      JSTAT         i        status: 0=OK
*
*   Read from COMMON:
*      ISZID         i        current zone ID
*      IZTW          i()      zone table - workstation ID
*      ZTW           r()        "    "   - window
*      ZTV           r()        "    "   - viewport
*      IWTID         i()      workstation table - workstation ID
*      IWTCA         i()      workstation table - category
*      IPEN          i        current SGS pen
*
*   Written to COMMON:
*      ISZID         i        current zone ID
*
*   Constants from SGSCOM:
*      MXZ           i        maximum number of zones allowed
*   Constants from GKS_PAR:
*      GMI           i        workstation category - metafile input
*      GINPUT        i             "         "     - input
*      GWISS         i             "         "     - workstation-
*                                                    independent
*                                                    segment Storage
*      GAVTIV        i        workstation state - active
*      GHIGHR        i        transformation priority - higher
*
*   Externals:
*      sgs_1HSTAT, sgs_1ERR, sgs_FLUSH, sgs_1SETTX, sgs_SPEN,
*      sgs_1GKERR, GQWKS, GDAWK, GACWK, GSELNT, GSVPIP, GSWN, GSVP
*
*   Errors:
*      INVALID ZONE ID
*      SPECIFIED ZONE DOES NOT EXIST
*
*   P.T.Wallace, D.L.Terrett   Starlink   14 September 1991
*-

      IMPLICIT NONE

      INTEGER IZONID,JSTAT

      INCLUDE 'sgscom'

      INCLUDE 'GKS_PAR'

      INCLUDE 'SGS_ERR'


      INTEGER IWKID,IWC,IERR,ISTATE

      CHARACTER RNAME*4
      PARAMETER (RNAME='SELZ')



*  Handle incoming status
      CALL sgs_1HSTAT(JSTAT)
      IF (JSTAT.NE.0) GO TO 9999

*  Validate zone ID
      IF (IZONID.LT.1 .OR. IZONID.GT.MXZ) THEN
         CALL sgs_1ERR(SGS__INVZN,RNAME,'Invalid zone ID',JSTAT)
         GO TO 9999
      END IF

*  Flush any pending output
      CALL sgs_FLUSH

*  Old workstation ID
      IWC = ABS(IZTW(ISZID))

*  New workstation ID
      IWKID=ABS(IZTW(IZONID))
      IF (IWKID.EQ.0) THEN
         CALL sgs_1ERR(SGS__ZONNF,RNAME,'Specified zone does not exist',
     :   JSTAT)
         GO TO 9999
      END IF

*  Set new current zone ID
      ISZID=IZONID

*  Switch workstation if appropriate
      IF (IWC.NE.IWKID) THEN
        IF (IWC.NE.0) THEN

*     Deactivate if active
          IF (IWTCA(IWC).NE.GMI.AND.IWTCA(IWC).NE.GINPUT
     :                            .AND.IWTCA(IWC).NE.GWISS) THEN
             CALL GQWKS(IWTID(IWC),IERR,ISTATE)
             IF (IERR.NE.0) THEN
               CALL sgs_1ERR(SGS__INQER,RNAME,'Error returned by GQWKS',
     :         JSTAT)
               GO TO 9999
             END IF

             IF (ISTATE.EQ.GACTIV) CALL GDAWK(IWTID(IWC))
           END IF
        END IF
      END IF
      IF (IWTCA(IWKID).NE.GMI.AND.IWTCA(IWKID).NE.GINPUT
     :                             .AND.IWTCA(IWKID).NE.GWISS) THEN

*     Activate if not active
         CALL GQWKS(IWTID(IWKID),IERR,ISTATE)
         IF (IERR.NE.0) THEN
           CALL sgs_1ERR(SGS__INQER,RNAME,'Error returned by GQWKS',
     :     JSTAT)
           GO TO 9999
         END IF
 
         IF (ISTATE.NE.GACTIV) CALL GACWK(IWTID(IWKID))

*     Recalculate text attributes
         CALL sgs_1SETTX

*     Reselect the current SGS pen to transfer the colour to markers and
*     text
         CALL sgs_SPEN(IPEN)
      END IF

*  Ensure that transformation 1 is selected and that its input priority is
*  greater that transformation zero.
      CALL GSELNT(1)
      CALL GSVPIP(1,0,GHIGHR)

*  Window
      CALL GSWN(1,ZTW(1,IZONID),ZTW(2,IZONID),
     :            ZTW(3,IZONID),ZTW(4,IZONID))

*  Viewport
      CALL GSVP(1,ZTV(1,IZONID),ZTV(2,IZONID),
     :             ZTV(3,IZONID),ZTV(4,IZONID))

*  Check for GKS errors
      CALL sgs_1GKERR(RNAME,JSTAT)

*  Exit
 9999 CONTINUE

      END
                                                      
