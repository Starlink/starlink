      SUBROUTINE GRINIT
*+
*     - - - - - - - -
*       G R I N I T   (GKS emulation of GRPCKG)
*     - - - - - - - -
*
*   Opens pgplot:
*
*   if this is the first call to GRINIT then
*      The font database is initialized
*      All GRPCKG devices are marked as closed
*
*   Written to COMMON
*      GRCIDE   i     Current device id
*      GRDVOP   l()   Device open flag
*
*   Constants from GRECOM
*      MAXDEV   i     Maximum number of open GRPCKG devices
*
*   D.L.Terrett  Starlink  May 1997
*+
      IMPLICIT NONE
      INTEGER I

      INCLUDE 'grecom.inc'

      LOGICAL GROPN
      SAVE GROPN

      DATA GROPN/.FALSE./

*    If first call to GRINIT
      IF (.NOT.GROPN) THEN

*      Open font database
         CALL GRSY00

*      Initialize all device open flags to false
         DO 20 I = 1, MAXDEV
            GRDVOP(I) = .FALSE.
   20    CONTINUE

*      GRPCKG is now open
         GROPN = .TRUE.

*      Device ID not valid
         GRCIDE = 0

      END IF
      END
