      SUBROUTINE grf_PPALET(IPAL)

*+
*
*   Name:
*      SUBROUTINE grf_PPALET
*
*   Description:
*      Select plotting in a particular palet colour.
*
*   Authors:
*      Jack Giddings
*
*   History:
*      Jack Giddings     05-MAY-82
*         AT4 version.
*      Paul Rees         12-JAN-88     IUEDR Vn. 2.0
*         Conversion to FORTRAN.
*         Conversion to GKS 7.2 graphics.
*      Paul Rees         09-MAY-89     IUEDR Vn. 2.1
*         Some restructuring and final conversion to SGP/16 style.
*
*   Method:
*      This selects the colour for palet entry IPAL.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Global constants:
      INTEGER COLOUR   ! GKS colour availability (GQCF)
      INTEGER ERR      ! error status
      INTEGER MONOCH   ! GKS monochrome availability (GQCF)
      INTEGER OK       ! OK status

      PARAMETER (COLOUR=1, ERR=-3, MONOCH=0, OK=0)

*   Import:
      INTEGER IPAL     ! colour index

*   CMGRAF:
      INCLUDE 'CMGRAF'

*   CMPAL:
      INCLUDE 'CMPAL'

*   Local variables:
      INTEGER COLAV    ! GKS colour availability
      INTEGER NCOLI    ! GKS number of colours available
      INTEGER NPCOLI   ! GSS number of preset colours
      INTEGER STATUS   ! status flag

      STATUS = 0
*   Inquire if workstation is COLOUR
      CALL GQCF(DEV, STATUS, NCOLI, COLAV, NPCOLI)

*   Only do colours if the device is a colour plotter
      IF (COLAV.EQ.COLOUR .AND. STATUS.EQ.OK) THEN

         IF (IPAL.GE.1 .AND. IPAL.LE.16) THEN
            CALL GSPLCI(TILUT(IPAL))
            CUPAL = IPAL
         END IF
      END IF

      END
