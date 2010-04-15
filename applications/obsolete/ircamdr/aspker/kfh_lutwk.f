*+  KFH_LUTWK - Routine for user to adjust args lookup table.
      SUBROUTINE KFH_LUTWK(LUT,MXLUTS,TABLE,STATUS)
*    Description :
*     This routine controls the adjustment of the ARGS lookup
*     table when the user moves the trackerball and presses
*     the buttons. The trackerball controls two aspects of
*     the lookup table. First the motion in the x-direction
*     shifts the origin of the colour set relative to that
*     of the pens. If the colour set is to the right of the
*     the start of the pens then all the pens up to the
*     beginning of the colour set are given the value of the
*     first element of the colour set. Should the colour set
*     begin to the left of the lookup table then the section
*     that 'overhangs' is just ignored. Equally if the colour
*     set 'overhangs' at the right-hand end the extra is just
*     ignored.
*      The second aspect that can be changed is how many pens
*     the colour set is to be distributed over. The pens in
*     the given range are allocated the nearest value from the
*     colour set.
*      The buttons on the trackerball box do the following :
*      Green - Sets the two tables so that they start at the
*              same position, and have the same length.
*      White - Advances the program onto the next colour set
*              causing it to start again at the first one
*              when the end has been reached.
*      White - Not used.
*      Red   - Causes the lookup table to be rewritten using
*            - GKS rather than the usual ARGS-specific routine
*              and then closes SGS and returns control to the
*              main program (i.e. TWEAK, NOT SAI_MAIN).
*    Invocation :
*     CALL KFH_LUTWK(LUT,MXLUTS,TABLE,STATUS)
*    Parameters :
*     LUT(3,0:255,NLUTS) = _INTEGER	! Colour set array.
*     MXLUTS = _INTEGER			! Maximum number of
*					! available colour
*					! sets.
*     TABLE(3,0:255) = _INTEGER		! Lookup table.
*     STATUS = _INTEGER			! Status.
*    Method :
*     Firstly three predefined standard colour sets are defined
*     and stored in LUT. These are :
*      1) The greyscale ramp.
*      2) The colour ramps.
*      3) The colour blocks.
*     The greyscale is loaded into the ARGS lookup table to
*     begin with. The trackerball box is then continuously
*     monitored and the lookup table adjusted according to
*     what the user enters.
*      If the trackerball is rotated in the x-directon the
*     new offset is calculated from:
*     OFFSET = PREVIOUS OFFSET + ( X - PREVIOUS X ) * 1/2
*     The factor of 1/2 was chosen so that the offset goes
*     from pen zero to pen 255 for a sweep of the cursor
*     from the left hand side of the screen to the right
*     hand side.
*      The range of pens of pens in which the colour set
*     is fitted is changed when the ball is changed when the
*     trackerball is moved in the y-direction according to :
*     RANGE = PREVIOUS RANGE + ( Y - PREVIOUS Y ) * 1/2
*     The factor of 1/2 was chosen so that the colour set is
*     to ranges of 256 pens to zero pens when the cursor moves
*     from the top of the screen to the bottom.
*     When fitting the colour set to the pens the compression
*     is carried out using the equaton :
*     PEN = OFFSET + [ RANGE * COLOUR NUMBER / 255 ]
*    Deficiencies :
*     The known deficiencies are :
*      1) The call to SGS_ASSOC will clear the ARGS.
*      2) SRCOLS has to be used to load the ARGS lookup table
*         instead of a GKS/SGS routine as it is so much faster.
*    Authors :
*     A.P.Horsfield (RGVAD::KFH)
*    History :
*     Derived from the Aspic routine LUTTWEAK written by
*     W.Pence at AAO.
*     18 July 1983: Original (RGVAD::KFH)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Status :
      INTEGER STATUS
*    Local Constants :
      REAL DELORG			! Factor in offset
      PARAMETER (DELORG = 0.5)		! calculation.
      REAL INTSTP			! Step between relative
      PARAMETER (INTSTP = 1.0/32.0)	! intensities for ramp.
      REAL MNINTS			! Minimum intensity of
      PARAMETER (MNINTS = 1.0/32.0)	! ramp scale.
      REAL MXINTS			! Maximum intensity of
      PARAMETER (MXINTS = 1.0)		! ramp scale.
      INTEGER NLUTS			! Maximum number of
      PARAMETER (NLUTS = 4)		! possible colour sets
      REAL RNGFCT			! Factor in range
      PARAMETER (RNGFCT = 0.5)		! calculation.
*    Local variables :
      LOGICAL BUTTON(4)		! Map of buttons on
*					! trackerball box.
*					! true =>pressed
*					! false=>unpressed.
      LOGICAL CHANGE			! Indicates whether
*					! user has done
*					! anything or not.
      INTEGER COL(3,8)			! Colours for ramp and
*					! for block scale.
      INTEGER COLOUR			! Colour number - used
*					! to point to colour in
*					! array COL.
      INTEGER DSPLUT			! Number of colour set
*					! used in lookup table.
*					! being displayed.
      REAL GKSTAB(3)			! Dump array used for
*					! converting the lookup
*					! table from ARGS format
*					! to GKS format.
      INTEGER I				! General variable.
      REAL INTENS			! Intensity of colour
*					! in ramp scale.
      INTEGER J				! General variable.
      INTEGER K				! General variable.
      REAL LASTX			! X-coordinate of cursor
*					! before user last moved
*					! the trackerball.
      REAL LASTY			! Y-coordinate of cursor
*					! before user last moved
*					! the trackerball.
      LOGICAL LEAVE			! Flag which is set to
*					! true when the user
*					! wishes to leave the
*					! program.
      INTEGER LOWER			! Lower limit of range
*					! of pens into which the
*					! colour set is to be
*					! placed.
      INTEGER LUT(3,0:255,NLUTS)	! Table containing all
*					! available colour sets.
      INTEGER MXLUTS			! Number of available
*					! colour sets.
      REAL NEWX				! X-coordinate of cursor
*					! which has just been
*					! sampled.
      REAL NEWY				! Y-coordinate of cursor
*					! which has just been
*					! sampled.
      REAL ORIG				! Offset of pens.
      REAL RANGE			! Range of pens into
*					! which colour set has
*					! to be put.
      INTEGER TABLE(3,0:255)		! Lookup table in use.
      INTEGER UPPER			! Last pen in range
*					! containing used part
*					! of the colour set.
      INTEGER WKID			! Work station
*					! identification.
      INTEGER ZONEID			! Zone identification.
*    Local data :
      DATA COL/255,  0,  0,
     :           0,255,  0,
     :         255,255,128,
     :           0,  0,255,
     :         255,255,  0,
     :         255,  0,255,
     :           0,255,255,
     :         255,255,255/
*-

*
*    If the status is bad on entry, then return to the main program.
*

      IF (STATUS.NE.SAI__OK) THEN

         RETURN

      ELSE

*
*       Create standard lookup tables :
*
*       Grey scale :
*

         DO I = 0,255,1
            DO J = 1,3,1
               LUT(J,I,1) = I
            ENDDO
         ENDDO

*
*       Ramped colour scale :
*

         I = -1
         DO COLOUR = 1,8,1
            DO INTENS = MNINTS,MXINTS,INTSTP
               I = I+1
               DO J = 1,3,1
                  LUT(J,I,2) = NINT(REAL(COL(J,COLOUR))*INTENS)
               ENDDO
            ENDDO
         ENDDO

*
*       Blocked colour scale :
*

         I = -1
         DO COLOUR = 1,8,1
            DO K = 1,32,1
               I = I+1
               DO J = 1,3,1
                  LUT(J,I,3) = COL(J,COLOUR)
               ENDDO
            ENDDO
         ENDDO

*
*       Set up initial values :
*

         ORIG = 1.0
         RANGE = 255.0
         DSPLUT = 1
         LASTX = 256.0
         LASTY = 256.0

*
*       Open ARGS.
*

         CALL SGS_ASSOC('DEVICE','UPDATE',ZONEID,STATUS)

         IF (STATUS.NE.SAI__OK) THEN

            CALL ERR_ANNUL(STATUS)
            CALL MSG_OUT('ARGERR','UNABLE TO OPEN ARGS.',STATUS)
            RETURN

         ELSE

*
*          The world coordinate system for the ARGS is set at
*          0-511 by 0-511, with (0,0) being the bottom left-hand
*          corner.
*

            CALL SGS_SW(0.0,511.0,0.0,511.0,STATUS)

*
*          The workstation identifier for the ARGS is obtained
*          for the benefit of the GKS routines used.
*

            CALL SGS_ICURW(WKID)

*
*          The ARGS trackerball box is set up as the choice
*          device, and is put into sample mode.
*

            CALL GKS_ENCH(WKID,1,2)

*
*          The ARGS cursor is made invisible.
*

            CALL SGS_CUVIS(.FALSE.)

*
*          The cursor is set up to be read in sample mode.
*

            CALL SGS_ENSCU

*
*          The lookup table is set to its initial state.
*

            DO I = 0,255,1
               DO J = 1,3,1
                  TABLE(J,I) = LUT(J,I,DSPLUT)
               ENDDO
            ENDDO

*
*          ************* NON-STANDARD FEATURE **************
*          This routine was used to set up the ARGS lookup
*          table in preference to the  GKS/SGS ones as it is
*          a great deal faster.
*          *************************************************
*

            CALL SRCOLS(0,256,TABLE)

*
*          This call is to a subroutine which plots a band across
*          the top of the screen, making use of all the pens in
*          sequence.
*          It is only a temporary routine used to test the rest
*          of the program.
*

*            CALL KFH_SCALE

            LEAVE = .FALSE.

            DO WHILE (.NOT.LEAVE)

*
*             The trackerball box is sampled to see which button
*             has been pressed, and the position of the invisible
*             cursor is obtained. Unfortunately it is impossible
*             at present to reset the buttons which have been,
*             thus this routine will continue to return that the
*             same button has been pressed until another is.
*

               CALL KFH_RDBOX(BUTTON,NEWX,NEWY,WKID)

               CHANGE = .FALSE.

*
*             This section operates if the cursor has been moved
*             in the x-directon.
*

               IF (NEWX.NE.LASTX) THEN

*
*                If the trackerball has been moved in the
*                x-direction then the point in the lookup
*                table corresponding to the beginning of the
*                colour set is shifted.
*

                  ORIG = ORIG + (NEWX-LASTX)*DELORG
                  CHANGE = .TRUE.

                  IF (NEWX.GT.510.0) THEN

*
*                   If the cursor has gone off the right hand
*                   side of the screen then it is brought round
*                   to the left hand side.
*

                     LASTX = 1.0
                     CALL KFH_SETCU(LASTX,NEWY)

                  ELSEIF (NEWX.LT.1.0) THEN

*
*                   If the cursor has gone off the left hand
*                   side of the screen then it is brought round
*                   to the right hand side.
*

                     LASTX = 510.0
                     CALL KFH_SETCU(LASTX,NEWY)

                  ELSE

                     LASTX = NEWX

                  ENDIF

               ENDIF

*
*             If the cursor has moved in the y-direction then
*             the next section is executed.
*

               IF (NEWY.NE.LASTY) THEN

*
*                As the cursor has been moved in the y-direction
*                the range of pens into which the colour set is
*                to be fitted is changed.
*

                  RANGE = MAX(RANGE+(NEWY-LASTY)*RNGFCT,0.0)
                  CHANGE = .TRUE.

                  IF (NEWY.GT.510.0) THEN

*
*                   If the cursor has gone off the top then
*                   it is brought round to the bottom of the
*                   ARGS screen.
*

                     LASTY = 1.0
                     CALL KFH_SETCU(LASTX,LASTY)

                  ELSEIF (NEWY.LT.1.0) THEN

*
*                   If the cursor has gone off the bottom of the
*                   screen then it is brought round to the top.
*

                     LASTY = 510.0
                     CALL KFH_SETCU(LASTX,LASTY)

                  ELSE

                     LASTY = NEWY

                  ENDIF

               ENDIF

               IF (BUTTON(1)) THEN

*
*                If the green button is pressed then the lookup
*                table is reset to its original state.
*

                  ORIG = 1.0
                  RANGE = 255.0
                  CHANGE = .TRUE.

               ELSEIF (BUTTON(2)) THEN

*
*                If the left hand white button is pressed then
*                the next lookup table in the series is used.
*

                  DSPLUT = MOD(DSPLUT,MXLUTS) + 1
                  CHANGE = .TRUE.

               ELSEIF (BUTTON(4)) THEN

*
*                When the red button is pressed the program
*                is terminated. This involves the lookup table
*                being stored using the GKS routine so that the
*                relevant GKS tables will be set up so that
*                they can be accessed by later programs (N.B.
*                GKS requires the intensities to be in the
*                range 0 to 1 whereas SRCOLS requires them
*                in the range 0 to 255). SGS (and hence GKS)
*                is then closed.
*

                  DO I = 0,255,1
                     DO J = 1,3,1
                        GKSTAB(J)=REAL(TABLE(J,I))/255.0
                     ENDDO
                     CALL GKS_SPRP(WKID,I,1,0.01,GKSTAB)
                  ENDDO

                  CALL SGS_DISCU
                  CALL GKS_DSCH(WKID,1)
                  CALL SGS_ANNUL(ZONEID,STATUS)

                  LEAVE = .TRUE.

               ENDIF

               IF (.NOT.LEAVE.AND.CHANGE) THEN

*
*                Provided that the program is not to be terminated
*                and that the user has done something the lookup
*                table is updated
*
*                The lower and upper limits of the colour set part
*                of the lookup table are set.
*
*                The lower point is set to the point in the
*                pen set where the colour set starts, unless
*                this is outside the region 0 to 255 when it
*                set to the appropriate limit.
*

                  LOWER = NINT(MIN(MAX(ORIG,0.0),255.0))

*
*                The upper point is set to the point in the pen
*                set where the end of the colour set lies, unless
*                this is outside the region 0 to 255 when it is
*                set to the appropriate limit.
*

                  UPPER = NINT(MAX(MIN(ORIG+RANGE,255.0),0.0))

                  IF (LOWER.GT.0) THEN

*
*                   Set all pens which appear before the colour
*                   table to the first value in the table.
*

                     DO I = 0,LOWER-1,1
                        DO J = 1,3,1
                           TABLE(J,I) = LUT(J,0,DSPLUT)
                        ENDDO
                     ENDDO

                  ENDIF

                  IF (LOWER.LT.UPPER) THEN

*
*                   Compress and store the colour set in the
*                   lookup table.
*

                     DO I = LOWER,UPPER,1
                        DO J =1,3,1
                           TABLE(J,I) = LUT(J,NINT((REAL(I)-ORIG)/
     :                      RANGE*255.0),DSPLUT)
                        ENDDO
                     ENDDO

                  ENDIF

                  IF (UPPER.LT.255) THEN

*
*                   Set all pens after the colour set are set to
*                   the last entry in the colour set.
*

                     DO I = UPPER+1,255,1
                        DO J = 1,3,1
                           TABLE(J,I) = LUT(J,255,DSPLUT)
                        ENDDO
                     ENDDO

                  ENDIF

*
*                Set the background to black.
*

                  TABLE(1,0) = 0
                  TABLE(2,0) = 0
                  TABLE(3,0) = 0

*
*                ************ NON-STANDARD CALL ***********
*                SRCOLS used instead of GKS/SGS routines
*                because of much greater speed.
*                ******************************************
*
*                Load up the lookup table.
*

                  CALL SRCOLS(0,256,TABLE)

               ENDIF

            ENDDO

         ENDIF

      ENDIF

      END
