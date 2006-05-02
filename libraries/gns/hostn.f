      SUBROUTINE gns_1HOSTN( NAME, LNAME, STATUS)
*+
*  Name:
*     gns_1HOSTN

*  Purpose:
*     returns the name of the system

*  Language:
*     Starlink Fortran 77

*  Description:
*     Gets the system name; eg the DECnet node name or IP host name

*  Arguments:
*     NAME = CHAR (Returned)
*         The system name
*     LNAME = INTEGER (Returned)
*         The length of the name
*     STATUS = INTEGER (Given & Returned)
*         Status

*  Authors:
*     DLT: D L Terrett (Starlink)
*     {enter_new_authors_here}

*  History:
*     06-JUN-1991 (DLT):
*        Modified.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      IMPLICIT NONE

      CHARACTER*(*) NAME
      INTEGER LNAME, STATUS

      INCLUDE 'SAE_PAR'

      INTEGER CHR_LEN
      CHARACTER*1 SYSNAM, REL, VER, MACH

      IF (STATUS.EQ.SAI__OK) THEN

*      Get the node name (all the other stuff is ignored)
         CALL PSX_UNAME(SYSNAM , NAME, REL, VER, MACH, STATUS)

*      Find the length of the string
         IF (STATUS.EQ.SAI__OK) THEN
            LNAME = CHR_LEN( NAME )
         ELSE
            LNAME = 0
         END IF

      END IF

      END
