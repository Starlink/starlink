      SUBROUTINE sgs_1GETZ (IWKID, IZONID)
*+
*   - - - - -
*    G E T Z        (Internal routine)
*   - - - - -
*
*   Allocate a zone table entry.
*
*   A negative IWKID is specified when the base zone for a workstation
*   is being created.
*
*   IZONID = 0 indicates either that the workstation ID is illegal
*   (i.e. zero) or that all zone table entries are in use.
*
*   Given:
*      IWKID     i     workstation ID or its complement
*
*   Returned:
*      IZONID    i     zone ID  (0=failure)
*
*   Read from COMMON:
*      IZTW      i()   zone table - workstation ID
*
*   Written to COMMON:
*      IZTW      i()   zone table - workstation ID
*
*   Constants from SGSCOM:
*      MXZ       i     maximum number of zones allowed
*
*   P.T.Wallace, D.L.Terrett   Starlink   14 September 1991
*-

      IMPLICIT NONE

      INCLUDE 'sgscom'


      INTEGER IWKID,IZONID

      INTEGER NZONE



*  Preset zone ID to failure
      IZONID=0

*  Validate IWKID
      IF (IWKID.EQ.0) GO TO 9999

*  Search for an unused zone table entry
      DO 10 NZONE=1,MXZ
         IF (IZTW(NZONE).EQ.0) THEN
            IZONID=NZONE
            IZTW(NZONE)=IWKID
            GO TO 9999
         END IF
   10 CONTINUE

*  Exit
 9999 CONTINUE

      END
