

*+  KFH_BTTNS - Displays trackerball box menu on an ARGS.
      SUBROUTINE KFH_BTTNS(LG,LWL,LWR,LR,LTX,LTY,USECUR,STATUS)
*    Description :
*     This subroutine displays four boxes at the bottom of
*     the ARGS screen in which the function of the four
*     buttons on the trackerball box are displayed. If the
*     trackerball is also to be used then two arrows with
*     circles around them are plotted beneath the boxes. One
*     points in the y direction (up and down), the message
*     beside it indicating what moving the trackerball in
*     the y direction does; the other points in x (left and
*     right), the message beside it indicating the effect
*     moving the trackerball in x has. If either label is
*     set to '*' then the message and the corresponding
*     arrow will not be plotted.
*    Invocation :
*     CALL KFH_BTTNS(LG,LWL,LWR,LR,LTY,LTX,USECUR,STATUS)
*    Parameters :
*     LG = CHAR*(*)
*           This is the label indicating what effect pressing
*           the green button has.
*     LWL = CHAR*(*)
*           This is the label indicating what effect pressing
*           the left-hand white button has.
*     LWR = CHAR*(*)
*           This is the label indicating what effect pressing
*           the right-hand white button has.
*     LR = CHAR*(*)
*           This is the label indicating what effect pressing
*           the red button has.
*     LTX = CHAR*(*)
*           This is the label indicating what effect moving
*           the trackerball in the x direction has.
*     LTY = CHAR*(*)
*           This is the label indicating what effect moving
*           the trackerball in the y direction has.
*     USECUR = LOGICAL
*           This is a flag which is set only if the trackerball
*           is to be used.
*     STATUS = INTEGER
*           This is the status value on entering this routine.
*    Method :
*     If the trackerball is to be used, then the screen is
*     given world coordinates in the range 0 to 6 in both the
*     x and y directions, otherwise the coordinates are set
*     to the range 0 to 6 in the x direction, and 1 to 7 in
*     the y direction. The effect of this is to provide space
*     at the bottom of the display for the arrows if they are
*     to be plotted, but not otherwise.
*     The boxes are plotted with their labels, then if required
*     so are the arrows and their labels.
*    Authors :
*     K.F.Hartley (RGVAD::KFH)
*     A.P.Horsfield (RGVAD::KFH)
*    History :
*     4 August 1983: Original (RGVAD::KFH)
*    Type Definitions :
      IMPLICIT NONE
*    Global Constants :
      INCLUDE 'SAE_PAR'
*    Status :
      INTEGER STATUS
*    Local Constants :
      INTEGER BLUE			! Number of blue pen.
      PARAMETER (BLUE = 4)
      INTEGER GREEN			! Number of green pen.
      PARAMETER (GREEN = 3)
      INTEGER MAXLEN			! Maximum length of a
      PARAMETER (MAXLEN = 10)		! label.
      INTEGER RED			! Number of red pen.
      PARAMETER (RED = 2)
      INTEGER WHITE			! Number of white pen.
      PARAMETER (WHITE = 1)
      INTEGER YELLOW			! Number of yellow pen.
      PARAMETER (YELLOW = 5)
*    Local variables :
      REAL ASPRAT			! Aspect ratio.
      INTEGER FONT			! Font number.
      REAL HM				! Marker height.
      REAL HTCHR			! Character height.
      CHARACTER*(*) LG			! Label for green
*					! button.
      CHARACTER*(*) LR			! Label for red button.
      CHARACTER*(*) LTX			! Label indicating the
*					! effect of moving the
*					! trackerball in x.
      CHARACTER*(*) LTY			! Label indicating the
*					! effect of moving the
*					! trackerball in y.
      CHARACTER*(*) LWL			! Label for left-hand
*					! white button.
      CHARACTER*(*) LWR			! Label for right-hand
*					! white button.
      INTEGER OLDPEN			! Pen number.
      CHARACTER*2 TXJ			! Text justification
*					! code.
      INTEGER TXTPRC			! Text precision.
      REAL TXTSPC			! Text spacing.
      LOGICAL USECUR			! Flag set if the
*					! trackerball is to
*					! be used.
      INTEGER WKID			! Work station ident-
*					! ifier.
      REAL XBL				! X world coordinate of
*					! the bottom left-hand
*					! corner of the sceen.
      REAL XM				! X dimension of zone
*					! in metres.
      REAL XTR				! X world coordinate of
*					! the top right-hand
*					! corner of the screen.
      REAL XU				! X direction cosine
*					! of text orientation.
      REAL YBL				! Y world coordinate of
*					! the bottom left-hand
*					! corner of the screen.
      REAL YM				! The y dimension of
*					! the zone in metres.
      REAL YTR				! Y world coordinate of
*					! the top right-hand
*					! corner of the screen.
      REAL YU				! Y direction cosine
*					! of text orientation.
*-

*
*    If the status value is bad, then return to the calling program.
*

      IF (STATUS.NE.SAI__OK) THEN

         RETURN

      ELSE

*
*       Save parameters defining the text and coordinates.
*

         CALL SGS_IZONE(XBL,XTR,YBL,YTR,XM,YM)
         CALL SGS_IMTX(HM,FONT,TXTPRC,HTCHR,ASPRAT,XU,YU,TXTSPC,
     :    TXJ)
         CALL SGS_IPEN(OLDPEN)

*
*       Clear the current work station.
*

         CALL SGS_ICURW(WKID)
         CALL GKS_CLRWK(WKID)

*
*       If the trackerball is not to be used then put the
*       boxes at the bottom of the screen, otherwise leave
*       space for the arrows.
*

         IF (USECUR) THEN
            CALL SGS_SW(0.0,6.0,0.5,6.5,STATUS)
         ELSE
            CALL SGS_SW(0.0,6.0,1.0,7.0,STATUS)
         ENDIF

*
*       Draw the boxes for the button labels in blue.
*
*       First the perimeter is drawn.
*

         CALL SGS_SPEN(BLUE)

         CALL SGS_BOX(1.0,5.0,1.0,1.5)

*
*       Draw in vertical bars to divide up the large box into
*       four small ones.
*

         CALL SGS_LINE(2.0,1.0,2.0,1.5)
         CALL SGS_LINE(3.0,1.0,3.0,1.5)
         CALL SGS_LINE(4.0,1.0,4.0,1.5)

         CALL SGS_FLUSH

*
*       Set up the text parameters for the labels in the boxes.
*
*       Set the text height to 0.5 units of the world coordinate
*       system.
*

         CALL SGS_SHTX(0.125)

*
*       Set the aspect ratio to 5 to 1.
*
         CALL SGS_SARTX(0.6)

*
*       Set the text justification to the centre of the string.
*

         CALL SGS_STXJ('CC')

*
*       Write the labels into the boxes.
*
*       Write the first label in green.
*

         CALL SGS_SPEN(GREEN)

         CALL SGS_BTEXT(1.5,1.25)
         CALL SGS_ATXL(LG(1:MIN(MAXLEN,LEN(LG))))

         CALL SGS_FLUSH

*
*       Write the second label in white.
*

         CALL SGS_SPEN(WHITE)

         CALL SGS_BTEXT(2.5,1.25)
         CALL SGS_ATXL(LWL(1:MIN(MAXLEN,LEN(LWL))))

*
*       Write the third label in white.
*

         CALL SGS_BTEXT(3.5,1.25)
         CALL SGS_ATXL(LWR(1:MIN(MAXLEN,LEN(LWR))))

         CALL SGS_FLUSH

*
*       Write the fourth label in red.
*

         CALL SGS_SPEN(RED)

         CALL SGS_BTEXT(4.5,1.25)
         CALL SGS_ATXL(LR(1:MIN(MAXLEN,LEN(LR))))

         CALL SGS_FLUSH

*
*       If the trackerball is to be used then draw in the arrows
*       and their labels.
*       If a label is '*' then the arrow and the label are not
*       drawn.
*

         IF (USECUR) THEN

*
*          Draw the arrows with circles around them.
*
*          Draw the horizontal arrow in yellow
*

            CALL SGS_SPEN(YELLOW)

            IF (LTX.NE.'*') THEN

               CALL SGS_LINE(1.3,0.75,1.7,0.75)
               CALL SGS_LINE(1.7,0.75,1.6,0.85)
               CALL SGS_LINE(1.7,0.75,1.6,0.65)
               CALL SGS_LINE(1.3,0.75,1.4,0.85)
               CALL SGS_LINE(1.3,0.75,1.4,0.65)

*
*             Draw a circle around the horizontal arrow.
*

               CALL SGS_CIRCL(1.5,0.75,0.22)

            ENDIF

*
*          Draw the vertical arrow.
*

            IF (LTY.NE.'*') THEN

               CALL SGS_LINE(3.5,0.55,3.5,0.95)
               CALL SGS_LINE(3.5,0.95,3.4,0.85)
               CALL SGS_LINE(3.5,0.95,3.6,0.85)
               CALL SGS_LINE(3.5,0.55,3.4,0.65)
               CALL SGS_LINE(3.5,0.55,3.6,0.65)

*
*             Draw the circle around the vertical arrow.
*

               CALL SGS_CIRCL(3.5,0.75,0.22)

            ENDIF

            CALL SGS_FLUSH

*
*          Set the text attributes for the labels.
*
*          Set the character height to 0.4 world coordinate units.
*

            CALL SGS_SHTX(0.125)

*
*          Set the aspect ratio to 4.
*

            CALL SGS_SARTX(0.7)

*
*          Set the text justification to bottom left.
*

            CALL SGS_STXJ('BL')

*
*          Set the pen to white.
*

            CALL SGS_SPEN(WHITE)

*
*          Write the first label.
*

            IF (LTX.NE.'*') THEN

               CALL SGS_BTEXT(2.0,0.7)
               CALL SGS_ATXL(LTX(1:MIN(MAXLEN,LEN(LTX))))

            ENDIF

*
*          Write the second label.
*

            IF (LTY.NE.'*') THEN

               CALL SGS_BTEXT(4.0,0.7)
               CALL SGS_ATXL(LTY(1:MIN(MAXLEN,LEN(LTY))))

            ENDIF

            CALL SGS_FLUSH

         ENDIF

*
*       Reset all the attributes to what they were on enterring
*       the subroutine.
*
*       Reset the world coordinates.
*

         CALL SGS_SW(XBL,XTR,YBL,YTR,STATUS)

*
*       Reset the character height.
*

         CALL SGS_SHTX(HTCHR)

*
*       Reset the aspect ratio.
*

         CALL SGS_SARTX(ASPRAT)

*
*       Reset the text justification
*

         CALL SGS_STXJ(TXJ)

*
*       Reset the pen.
*

         CALL SGS_SPEN(OLDPEN)

      ENDIF

      END
