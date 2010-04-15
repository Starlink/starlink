      SUBROUTINE MAGNS( STATUS )

*   Arguments:
*
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  History:
*     Sometime (UNK):
*        Original version.
*     2-MAR-1993 (AJJB):
*        STATUS argument added to CROSS call
*     3-MAR-1993 (AJJB):
*       STATUS argument added.
*     5-MAR-1993 (Andrew Broderick (AJJB)):
*        STATUS argument added to all calls to routines within Chart
*        which did'nt already have one.
*

      INCLUDE 'SAE_PAR'          ! Standard SAE constants

      INCLUDE 'MAIN'

      INCLUDE 'PLOTDAT'

*  Status:
      INTEGER STATUS             ! Global status
      INTEGER MAGLIM
*   Set Zone for Magnitude Box

      CALL SGS_SELZ (IZBASE,ISTAT)
      CALL SGS_ZONE (0.0,XMAX,0.0,YDIAM,IZTEMP,ISTAT)
      CALL SGS_SW (0.0,XMAX,0.0,YDIAM,ISTAT)

      POSN=YDIAM-MAGH
      IF (POSN.GE.15.0) THEN
         POSN=15.0
      ELSE
         POSN=0.0
      ENDIF

      XWIDTH=AMIN1(XDIAM+MAGW,XMAX)

      CALL SGS_ZONE (XDIAM,XWIDTH,YDIAM-MAGH-POSN,
     :              YDIAM-POSN,IZMAGNS,ISTAT)

*   Set World Co-ordinates for this Area

      CALL SGS_SW (0.0,MAGW,0.0,MAGH,ISTAT)

*   Select Zone IZMAGNS for plotting

      CALL SGS_SELZ (IZMAGNS,ISTAT)

*   Set Positioning of Characters

      CALL SGS_STXJ ('CL')

*   Write Title

      YP = MAGH - 4.0
      XP = 0.5 * CW

      CALL SGS_BTEXT (XP,YP)
      CALL SGS_ATEXT ('MAGNITUDES')
      CALL SGS_OTEXT
      CALL SGS_FLUSH

*   Set Scaling Factor

      FAC = AMAX1 (270.0*SQRT(FLOAT(NUM))/10.0,270.0)

*   Plot 8 Illustriative Stars

*   If ARGS First Set Pen Colour

      IF (COLOUR) CALL SGS_SPEN (1)

*   Set maximum star size

      RMAX = 9.5 * SIZEMM / 270.0

      XP = 4.5 * CW - (RMAX / 4.0)
      XPN = XP + (3.0/2.0*RMAX)
      YP = MAGH - 8.0 - (AMAX1(RMAX,0.5*CH))


*   Modified bt K.F.hartley at RGO on 11-10-83
*   to plot a magnitude scale only up to the magnitude
*   limit selected.

      MAGLIM=MIN(INT(FAINT+1.0),11)
      DO MAGN = 4,MAGLIM

         RMAG = FLOAT (MAGN)
         R=((13.5-RMAG)*SIZEMM)/FAC

         IF (SYMBOL(1:4).EQ.'CIRC') THEN

            CALL SGS_CIRCL (XP,YP,R)
            CALL SGS_OPOLY

         ELSE IF (SYMBOL(1:4).EQ.'SPOT') THEN

            CALL SPOT(XP,YP,R, STATUS )

         ELSE

            CALL CROSS (XP,YP,R, STATUS )

         ENDIF
         CALL SGS_FLUSH

*   Number Stars

         IF (MAGN.LE.9) THEN

            CALL SGS_TXI (XPN,YP,MAGN,1)

         ELSE

            CALL SGS_TXI (XPN,YP,MAGN,0)

         ENDIF

         CALL SGS_OTEXT
         CALL SGS_FLUSH

         YP = YP - 1.0  - (AMAX1(2.0*RMAX,CH))

      END DO


*   Output Scale Bar

      CALL SBAR( STATUS )

*   Flush SGS Buffer

      CALL SGS_FLUSH

*   Release Plotting Zones

      CALL SGS_SELZ (IZBASE,ISTAT)
      CALL SGS_RELZ (IZMAGNS,ISTAT)
      CALL SGS_RELZ (IZTEMP,ISTAT)

      END
