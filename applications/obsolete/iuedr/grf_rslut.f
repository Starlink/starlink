      SUBROUTINE grf_RSLUT(STATUS)

*+
*
*   Name:
*      SUBROUTINE grf_RSLUT
*
*   Description:
*      Reset LUT for image display.
*
*   Authors:
*      Jack Giddings
*
*   History:
*      Jack Giddings     01-AUG-82
*         AT4 version.
*      Paul Rees         28-NOV-88     IUEDR Vn. 2.0
*         Conversion to FORTRAN.
*         Conversion to GKS 7.2 graphics.
*      Paul Rees         08-MAY-89     IUEDR Vn. 2.1
*         Some restructuring and final conversion to SGP/16 style.
*
*   Method:
*      Set LUT to grey scale.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Global constants:
      INTEGER COLOUR   ! GKS colour
      INTEGER ERR      ! error status
      INTEGER MAXCOLI  ! maximum number of preset colours
      INTEGER MAXPAL   ! maximum number of palet colours
      INTEGER MONOCH   ! GKS monochrome
      INTEGER OK       ! OK status
      PARAMETER (COLOUR=1, ERR=-3, MAXCOLI=256, MAXPAL=16, MONOCH=0,
     :           OK=0)

*   Export:
      INTEGER STATUS   ! status return

*   CMGRAF:
      INCLUDE 'CMGRAF'

*   Local variables:
      INTEGER COLAV    ! colour availability
      INTEGER CONID    ! GKS connection ID
      INTEGER GFSTAT   ! GKS error index
      INTEGER NCOLI    ! number of colours available
      INTEGER NPCOLI   ! number of preset colours available
      INTEGER WKID     ! GKS workstation ID
      INTEGER WTYPE    ! GKS workstation type

*   Initialise STATUS
      STATUS = OK

*   Inquire current GKS workstation ID
      CALL sgs_ICURW(WKID)
      CALL GQWKC(WKID, GFSTAT, CONID, WTYPE)

      IF (GFSTAT.EQ.OK) THEN

*      Inquire colour availability
         CALL GQCF(WTYPE, GFSTAT, NCOLI, COLAV, NPCOLI)

         IF (GFSTAT.EQ.OK .AND. COLAV.EQ.COLOUR) THEN

*         Check that there are enough colours available
            IF (NPCOLI.GE.MAXPAL) THEN

*            Set up LUT
               LUTS(1) = 2
               LUTS(2) = NPCOLI - MAXPAL - 1

*            Load colour or grey LUT depending upon COLUT
               IF (COLUT) THEN

*               Colour LUT
                  CALL grf_LUTCS(WKID, LUTS, STATUS)
               ELSE

*               Grey smoothed
                  CALL grf_LUTG(WKID, LUTS, STATUS)
               END IF

*            Bad pixel colour
               LUTB = 1
               CALL GSCR(WKID, LUTB, 0.0, 0.0, 0.5)

*            Backwash
               CALL GSCR(WKID, 0, 0.0, 0.0, 0.0)
            ELSE
               STATUS = ERR
            END IF

         ELSE
            STATUS = ERR
         END IF

      ELSE
         STATUS = ERR
      END IF

      END
