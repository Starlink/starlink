      SUBROUTINE sgs_CLSWK (IZONID, JSTAT)
*+
*   - - - - - -
*    C L S W K
*   - - - - - -
*
*   Close a workstation.
*
*   If there are no other base zones on the workstation: delete the zone
*   and all other zones on the workstation; de-activate and close the
*   workstation. Otherwise just delete the base zone.  If the current
*   zone has been deleted the current zone ID is set to zero.
*
*   The specified zone must be a base zone.
*
*   Given:
*      IZONID    i       zone identifier
*      JSTAT     i       inherited status (if option selected)
*
*   Returned:
*      JSTAT     i       status: 0=OK
*
*   Read from COMMON:
*      IZTW      i()     zone table - workstation ID
*      NTEXT     i       length of current polyline
*      OTEXT     i       length of current text string
*      IWTID     i()     workstation table - GKS workstation ID
*
*   Written to COMMON:
*      IWTID     i()     workstation table - GKS workstation ID
*      IWTTY     i()     workstation table - Type
*      IWTCO     i()     workstation table - Connection ID
*      IWTCA     i()     workstation table - Category
*
*
*   Constants from SGSCOM:
*      MXZ       i       maximum number of zones allowed
*
*   Externals:
*      sgs_1HSTAT, sgs_1ERR, sgs_1CLWK, sgs_OTEXT, sgs_OPOLY
*
*   Errors:
*      Invalid zone ID
*      Specified zone does not exist
*      Specified zone is not a base zone
*                                       
*   P.T.Wallace, D.L.Terrett   Starlink   7 September 1991
*-

      IMPLICIT NONE

      INTEGER IZONID,JSTAT

      INCLUDE 'sgscom'

      INCLUDE 'SGS_ERR'


      INTEGER IWKID,LSTAT,IZONE,IGWKID,IWK

      CHARACTER RNAME*5
      PARAMETER (RNAME='CLSWK')



*  Handle incoming status
      CALL sgs_1HSTAT(JSTAT)
      LSTAT = JSTAT
      JSTAT = 0

*  Validate zone number
      IF (IZONID .LT. 1 .OR. IZONID .GT. MXZ) THEN
         CALL sgs_1ERR(SGS__INVZN,RNAME,'Invalid zone ID',JSTAT)
         GO TO 9999
      END IF

*  Check workstation ID
      IF (IZTW(IZONID) .EQ. 0) THEN
         CALL sgs_1ERR(SGS__ZONNF,RNAME,'Specified zone does not exist',
     :                                                            JSTAT)
         GO TO 9999
      END IF

*  Check that it is a base zone
      IF (IZTW(IZONID) .GT. 0) THEN
         CALL sgs_1ERR(SGS__ZONNB,RNAME,
     :                       'Specified zone is not a base zone', JSTAT)
         GO TO 9999
      END IF

*  Flush any pending output
      IF (NTEXT.GT.0) CALL sgs_OTEXT
      IF (NPOLY.GT.1) CALL sgs_OPOLY

*  Save SGS workstation ID
      IWKID = -IZTW(IZONID)

*  Delete any zones connected to this workstation
      DO 20 IZONE=1,MXZ
         IF (IZTW(IZONE) .EQ. IWKID) THEN
            IZTW(IZONE) = 0

*        If we are deleting the current zone set the current zone to
*        zero
            IF (IZONE.EQ.ISZID) ISZID = 0
         END IF
   20 CONTINUE

*  Delete specified zone
      IZTW(IZONID) = 0

*  Save the GKS workstation ID
      IGWKID = IWTID(IWKID)

*  Reset SGS workstation table entry
      IWTID(IWKID) = 0
      IWTTY(IWKID) = 0
      IWTCO(IWKID) = 0
      IWTCA(IWKID) = 0

*  Check for other SGS workstations open on this GKS workstation
      DO 30 IWK = 1,MXWK
         IF (IWTID(IWK).EQ.IGWKID) GO TO 9999
   30 CONTINUE

*  Close the GKS workstation
      CALL SGS_1CLWK(IGWKID)

*    Exit
9999  CONTINUE

*    Reinstate incoming status
      IF (LSTAT .NE. 0) THEN
         JSTAT = LSTAT
      END IF

      END
