      SUBROUTINE sgs_RELZ (IZONID)
*+
*   - - - - -
*    R E L Z
*   - - - - -
*
*   Release a zone that is no longer required.
*
*   Neither a base zone nor the current zone can be released.
*
*   Given:
*      IZONID       i      zone identifier
*
*   Read from COMMON:
*      ISZID        i      current zone ID
*      IZTW         i()    zone table - workstation ID
*
*   Written to COMMON:
*      IZTW         i()    zone table - workstation ID
*
*   Constants from SGSCOM:
*      MXZ          i      maximum number of zones allowed
*
*   P.T.Wallace, D.L.Terrett   Starlink   14 September 1991
*-

      IMPLICIT NONE

      INTEGER IZONID
      INCLUDE 'sgscom'



*  Release the zone (unless it is the current zone, a base zone or junk)
      IF (IZONID.NE.ISZID .AND.
     :    IZONID.GT.0 .AND.
     :    IZONID.LE.MXZ .AND.
     :    IZTW(IZONID).GE.0) IZTW(IZONID)=0

      END
