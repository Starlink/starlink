      SUBROUTINE sgs_1WDTRD (IWKID, JSTAT)
*+
*   - - - - - -
*    W D T R D    (Internal routine)
*   - - - - - -
*
*   Read WDT entry into common block.
*
*   Given:
*      IWKID       i        SGS Workstation ID
*      JSTAT       i        inherited status (if mode selected)
*
*   Returned:
*      JSTAT       i        status 0=OK
*
*   Read from COMMON:
*      IWTID       i()      GKS workstation ID
*      
*   Written to COMMON:
*      XRES        r()      WDT - x resolution
*      YRES        r()      WDT - y resolution
*      IBLKCL      i()      WDT - block clear mechanism
*      ISPOOL      i()      WDT - workstation spooled
*      NSCLOP      l()      WDT - workstation supports no screen clear
*                                                                   open
*   Externals:
*      sgs_1HSTAT, sgs_1ERR, gns_IWSG, gns_IWCG
*
*   P.T.Wallace, D.L.Terrett   Starlink   7 September 1991
*-

      IMPLICIT NONE

      INTEGER IWKID,JSTAT

      INCLUDE 'sgscom'

      INCLUDE 'GKS_PAR'


      CHARACTER*5 RNAME
      PARAMETER (RNAME='WDTRD')

      REAL SCALE
      CHARACTER*20 CHAR



*  Handle incoming status
      CALL sgs_1HSTAT(JSTAT)
      IF (JSTAT.NE.0) GO TO 9999

*  Fill the table with defaults
      XRES(IWKID) = 1.0
      YRES(IWKID) = 1.0
      IBLKCL(IWKID) = 0
      ISPOOL(IWKID) = 0
      NSCLOP(IWKID) = .FALSE.

*  Resolution
      JSTAT = 0
      CALL gns_IWSG(IWTID(IWKID), SCALE, JSTAT)
      IF (JSTAT.EQ.0) THEN
         XRES(IWKID) = SCALE
         YRES(IWKID) = SCALE
      END IF

*  Spooling
      JSTAT = 0
      CALL gns_IWCG(IWTID(IWKID), 'OUTPUT', CHAR, JSTAT)
      IF (JSTAT.EQ.0 .AND. CHAR.NE.'DIRECT') ISPOOL(IWKID) = 1

*  Background erase
      JSTAT = 0
      CALL gns_IWCG(IWTID(IWKID), 'CLEAR', CHAR, JSTAT)
      IF (JSTAT.EQ.0 .AND. CHAR.EQ.'SELECTIVE') IBLKCL(IWKID) = 1

*  No screen clear
      JSTAT = 0
      CALL gns_IWCG(IWTID(IWKID), 'OPEN', CHAR, JSTAT)
      IF (JSTAT.EQ.0 .AND. CHAR.EQ.'NORESET') NSCLOP(IWKID) = .TRUE.

      JSTAT = 0
9999  CONTINUE

      END
