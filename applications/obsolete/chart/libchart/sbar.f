      SUBROUTINE SBAR( STATUS )
*+
*   If Magnitudes and scales are required upon a plot, this routine is
*   called to draw a box which has a side of a specified scale (the
*   scale of this normally being 20 minutes)

*   Arguments:
*
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*   History:
*     3-MAR-1993 (AJJB):
*       STATUS argument added.
*-

      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'MAIN'
      INCLUDE 'PLOTDAT'

*  Status:
      INTEGER STATUS             ! Global status

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   If ARGS Set Pen Colour

      IF (COLOUR) CALL SGS_SPEN (4)

      REP=20.0
      BLEN=1200.0/SCALE
      DO I=1,1000
         IF (BLEN.GT.25.0) THEN
            BLEN=BLEN/2.0
            REP=REP/2.0
         ELSE
            GO TO 100
         ENDIF
      END DO
 100  CONTINUE

      XP = 12.5
      YP = 12.5

      RLEN = BLEN / 2.0

      CALL SGS_BPOLY (XP-RLEN,YP-RLEN)
      CALL SGS_APOLY (XP+RLEN,YP-RLEN)
      CALL SGS_APOLY (XP+RLEN,YP+RLEN)
      CALL SGS_APOLY (XP-RLEN,YP+RLEN)
      CALL SGS_APOLY (XP-RLEN,YP-RLEN)
      CALL SGS_OPOLY

*
*   If ARGS set Pen Colour
*

      CALL SGS_SPEN (1)

      XP=12.5+RLEN+0.5*CW
      YP=12.5+0.6*CH

      CALL SGS_STXJ ('CL')

      IF (REP.GE.10.0) THEN

         CALL SGS_TXR (XP,YP,REP,0,-1)

      ELSE

         CALL SGS_TXR (XP,YP,REP,0,1)

      ENDIF

      CALL SGS_ATEXT ('''')
      CALL SGS_OTEXT

      IF ( EXTENS ) THEN

         YP = YP - 1.2 * CH
         CALL SGS_BTEXT (XP,YP)
         CALL SGS_ATEXT ('SQUARE')
         CALL SGS_OTEXT

      ENDIF

      END
