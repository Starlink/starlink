      SUBROUTINE gns_1HOSTN( NAME, LNAME, STATUS)
*++
*   gns_1HOSTN  returns the name of the system
*
*   Description:
*       Gets the system name; eg the DECnet node name or IP host name
*       
*   Input arguments:
*      STATUS  i                 Inherited status
*
*   Output arguments:
*      NAME    c                 The system name
*      LNAME   i                 The length of the name
*      STATUS  i                 Status
*
*   D L Terrett 06-JUN-1991
*++
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
