      SUBROUTINE grf_SELINE(ISTYLE, IPALET)

*+
*
*   Name:
*      SUBROUTINE grf_SELINE
*
*   Description:
*      Get next line style and colour for following graphics.
*
*   Authors:
*      Jack Giddings
*
*   History:
*      Jack Giddings     05-MAY-82
*         AT4 version.
*      Paul Rees         14-JAN-88     IUEDR Vn. 2.0
*         Conversion to FORTRAN.
*         Conversion to GKS 7.2 graphics.
*      Paul Rees         09-MAY-89     IUEDR Vn. 2.1
*         Some restructuring and final conversion to SGP/16 style.
*
*   Method:
*      Select line style and colour for following graphics.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Global constants:
      INTEGER COLOUR   ! colour availability on workstation (GQCF)
      INTEGER ERR      ! error status
      INTEGER MONOCH   ! monochrome availability on workstation (GQCF)
      INTEGER OK       ! OK status

      PARAMETER (COLOUR=1, ERR=-3, MONOCH=0, OK=0)

*   Import:
      INTEGER ISTYLE   ! preselected line-style (if > 0)
      INTEGER IPALET   ! preselected palet (if > 0)

*   CMGRAF:
      INCLUDE 'CMGRAF'

*   CMLINR:
      INCLUDE 'CMLINR'

*   CMCOLR
      INCLUDE 'CMCOLR'

*   Local variables:
      INTEGER I        ! loop index
      INTEGER STATUS   ! status
      INTEGER COLAV    ! colour availability
      INTEGER NCOLI    ! number of colours available
      INTEGER NPCOLI   ! number of predefined colour indices

      STATUS = 0
*   Set LINE style
      IF (ISTYLE.GT.0 .AND. ISTYLE.LT.5) THEN
         CALL GSLN(LINTYP(ISTYLE))
      ELSE
         CALL GSLN(LININD)

         IF (LINROT) THEN
            LININD = MOD(LININD, 4) + 1
         END IF
      END IF

*   Inquire if current device is capable of drawing colours
      CALL GQCF(DEV, STATUS, NCOLI, COLAV, NPCOLI)

*   For COLOUR, set colour
      IF (COLAV.EQ.COLOUR .AND. STATUS.EQ.OK) THEN

         IF (IPALET.GT.0) THEN
            I = MOD(IPALET-1, TICNT) + 1
            CALL grf_PPALET(TIPLOT(I))
         ELSE
            CALL grf_PPALET(TIPLOT(TICUR))

            IF (TIROT) THEN
               TICUR = MOD(TICUR, TICNT) + 1
            END IF
         END IF
      END IF

      END
