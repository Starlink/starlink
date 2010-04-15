      SUBROUTINE grf_RSCOLR

*+
*
*   Name:
*      SUBROUTINE grf_RSCOLR
*
*   Description:
*      Reset image display colour table for use with graphics/images.
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
*      Paul Rees         08-MAY-89     IUEDR Vn. 2.1
*         Some restructuring and final conversion to SGP/16 style.
*
*   Method:
*      The image display is given a colour table consisting
*      of a grey scale from 0 to 239, and 16 individual colours in the
*      range 240 to 255.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Global constants:
      INTEGER COLOUR   ! GKS colour availability (GQCF)
      INTEGER ERR      ! error status
      INTEGER MAXPAL   ! maximum number of palet colours
      INTEGER MONOCH   ! GKS monochrome availability (GQCF)
      INTEGER OK       ! OK status

      PARAMETER (COLOUR=1, ERR=-3, MAXPAL=16, MONOCH=0, OK=0)

*   CMGRAF:
      INCLUDE 'CMGRAF'

*   CMCOLR:
      INCLUDE 'CMCOLR'

*   CMPAL:
      INCLUDE 'CMPAL'

*   Local variables:
      INTEGER COLAV    ! GKS colour availability
      INTEGER I        ! loop index
      INTEGER IPAL     ! colour index
      INTEGER IPMIN    ! minimum level of colour LUT
      INTEGER NCOLI    ! GKS number of colours available
      INTEGER NPCOLI   ! GSS number of preset colours
      INTEGER STATUS   ! status flag
      INTEGER WKID     ! GKS workstation ID

      STATUS = 0
*   Set default TILUT etc.
      DO I = 1, MAXPAL
         TILUT(I) = 1
      END DO

      TIMARK = 1
      TIUSER = 1
      TIAXES = 1
      TITEXT = 1

*   Inquire if workstation is COLOUR
      CALL GQCF(DEV, STATUS, NCOLI, COLAV, NPCOLI)

*   Only do if the device is a colour plotter
      IF (STATUS.EQ.OK .AND. COLAV.EQ.COLOUR) THEN

*      Are there enough colours available?
         IF (NPCOLI.GE.MAXPAL) THEN

*         Inquire workstation identification
            CALL sgs_ICURW(WKID)

*         Set up image display part of LUT
            CALL grf_RSLUT(STATUS)

*         Initialise colour table
            IPMIN = NPCOLI - MAXPAL - 1
            IPAL = 0

            DO I = 1, MAXPAL
               CALL GSCR(WKID, I + IPMIN, 1.0, 1.0, 1.0)
            END DO

*         Define colour table
            IPAL = IPAL + 1

*         White
            TILUT(IPAL) = IPMIN + IPAL
            TIMARK = IPAL
            TIUSER = IPAL
            TIAXES = IPAL
            TITEXT = IPAL
            CALL GSCR(WKID, TILUT(IPAL), 1.0, 1.0, 1.0)

*         Brownish-orange (gold?)
            IPAL = IPAL + 1
            TILUT(IPAL) = IPMIN + IPAL
            CALL GSCR(WKID, TILUT(IPAL), 1.0, 0.63, 0.0)

*         Yellow
            IPAL = IPAL + 1
            TILUT(IPAL) = IPMIN + IPAL
            CALL GSCR(WKID, TILUT(IPAL), 1.0, 1.0, 0.0)

*         Green
            IPAL = IPAL + 1
            TILUT(IPAL) = IPMIN + IPAL
            CALL GSCR(WKID, TILUT(IPAL), 0.0, 0.75, 0.06)

*         Red
            IPAL = IPAL + 1
            TILUT(IPAL) = IPMIN + IPAL
            CALL GSCR(WKID, TILUT(IPAL), 1.0, 0.0, 0.0)

*         Blue
            IPAL = IPAL + 1
            TILUT(IPAL) = IPMIN + IPAL
            CALL GSCR(WKID, TILUT(IPAL), 0.0, 0.0, 0.75)

*         Pink
            IPAL = IPAL + 1
            TILUT(IPAL) = IPMIN + IPAL
            CALL GSCR(WKID, TILUT(IPAL), 1.0, 0.4, 0.4)

*         Violet
            IPAL = IPAL + 1
            TILUT(IPAL) = IPMIN + IPAL
            CALL GSCR(WKID, TILUT(IPAL), 0.79, 0.36, 0.76)

*         Turquoise (?)
            IPAL = IPAL + 1
            TILUT(IPAL) = IPMIN + IPAL
            CALL GSCR(WKID, TILUT(IPAL), 0.08, 1.0, 0.8)

*         Orange
            IPAL = IPAL + 1
            TILUT(IPAL) = IPMIN + IPAL
            CALL GSCR(WKID, TILUT(IPAL), 1.0, 0.38, 0.0)

*         Light green
            IPAL = IPAL + 1
            TILUT(IPAL) = IPMIN + IPAL
            CALL GSCR(WKID, TILUT(IPAL), 0.4, 0.91, 0.27)

*         Olive (gold)
            IPAL = IPAL + 1
            TILUT(IPAL) = IPMIN + IPAL
            CALL GSCR(WKID, TILUT(IPAL), 0.86, 0.67, 0.21)

*         Curve palet indices go from 3 to MAXPAL
            TICNT = IPAL - 2

            DO I = 1, TICNT
               TIPLOT(I) = I + 2
            END DO

*         Adopt white (grey) as a starter
            CALL grf_PPALET(1)
         END IF
      END IF

      END
