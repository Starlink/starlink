      SUBROUTINE NUMB(X,Y,R,N, STATUS )

*+
*   NUMB Plots the Star Number
*   Below the Symbol on the Chart
*
*   Gets
*   ----
*      X,Y   - Co-ordinates of the star to be numbered.
*      R     - Half-width of the star.
*      N     - Number for the star
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*
*   History:
*     3-MAR-1993 (AJJB):
*       STATUS argument added.
*-

*  Status:
      INTEGER STATUS             ! Global status

      REAL DISP
      LOGICAL CLASH,DISPLEFT,DISPRIGHT
      COMMON/DISPS/CLASH,DISPLEFT,DISPRIGHT
      INCLUDE 'PLOTDAT'
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

      IF ( STATUS .NE. SAI__OK ) RETURN

      IF ( ( SIZEMM / 80.0 .GT. 1.0 ) .AND. KEYNUMB ) THEN

*   Numbers only Drawn on if the size of the numbers is greater than
*   One Eightyeth of the size of the chart
*   Clipping (of the Chart) is disenabled whilst number is being drawn

         CALL SGS_SELZ (IZAREA,ISTAT)
         IF (PLOTTER) THEN
            CHARSIZ=AMIN1(SIZEMM/80.0,3.0)
         ELSE
            CHARSIZ=AMIN1(SIZEMM/30.0,5.0)
         ENDIF
         CALL SGS_SHTX (CHARSIZ)
         IF (CLASH) THEN
            IF (DISPLEFT) THEN
               CALL SGS_STXJ ('TR')
               DISP=-R/4.0
            ELSEIF (DISPRIGHT) THEN
               CALL SGS_STXJ ('TL')
               DISP=R/4.0
               CLASH=.FALSE.
            ENDIF
         ELSE
            CALL SGS_STXJ ('TC')
            DISP=0.0
         ENDIF
         CALL SGS_TXI (X+5.0+DISP,Y-R-(SIZEMM/100.0),N,0)
         CALL SGS_OTEXT
         CALL SGS_SELZ (IZCHART,ISTAT)

      END IF

      END
