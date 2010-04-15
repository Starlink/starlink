      SUBROUTINE SPD_PBAC( XKEY, YKEY, T_CKEY, MODX, MODX2,
     :   MODF, MODF2, NPAR, MODPAR, MSKDIM, MSKUSE, MASK, STATUS )
*+
*  Name:
*     SPD_PBAC

*  Purpose:
*     PGCURS with pan and zoom through spectrum and model.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_PBAC( XKEY, YKEY, CKEY, MODX, MODX2,
*        MODF, MODF2, NPAR, MODPAR, MSKDIM, MSKUSE, MASK, STATUS )

*  Description:
*     This routine is a wrap around PGCURS. It will return a position
*     and the character pressed, just like PGCURS. However, this routine
*     also knows about the spectrum and model and how they are currently
*     displayed. The spectrum and model should be displayed before this
*     routine is called. At least this routine assumes it is displayed
*     and its viewport is active.
*
*     Certain key strokes will be intercepted and handled by this routine.
*     These key strokes are not returned to the calling routine, they are:
*
*     W: Reset display to show the whole finder image with original
*        contrast.
*     X: Zoom twofold in x direction with the indicated x position as
*        the new centre.
*     Y: Zoom twofold in y direction with the indicated y position as
*        the new centre.
*     Z: Zoom twofold in x and y direction with the indicated position
*        as the new centre.
*     P: Pan around the image, i.e. move by 3/4 of the current display
*        range. Movement can be in x, in y, or both. A left/right
*        movement occurs if and only if the key is hit in the left/right
*        quarter of the x range. Similarly for y movement. Diagonal
*        movement occurs if the key is hit in an intersection area for x
*        and y movement, i.e. close to the corners of the viewport.
*
*     These keys are intercepted regardless of their case.
*
*     PGCURS seems to treat the first mouse button as a space bar and
*     the second mouse button as a return key. The third mouse button
*     produces an error message and reversion to the given coordinates.
*     Control keys are not detected at all, neither are function keys or
*     keypad keys.

*  Arguments:
*     XKEY = REAL (Given and Returned)
*        The x coordinate of the last key stroke.
*     YKEY = REAL (Given and Returned)
*        The y coordinate of the last key stroke.
*     CKEY = CHARACTER * ( 1 ) (Returned)
*        The character of the last key stroke.
*     MODX = LOGICAL (Given)
*        True if MODF given.
*     MODX2 = LOGICAL (Given)
*        True if MODF2 given.
*     MODF = EXTERNAL (Given)
*        Routine to evaluate model for a given x value array.
*        SUBROUTINE MODF( NX, X, NPAR, MODPAR, Y, STATUS )
*           NX = INTEGER (Given)
*           X( NX ) = REAL (Given)
*           NPAR = INTEGER (Given)
*           MODPAR( NPAR ) = REAL (Given)
*           Y( NX ) = REAL (Returned)
*           STATUS = INTEGER (Given and Returned)
*        This routine is used for the principal model, most obviously
*        the dashed line in the bottom view port.
*     MODF2 = EXTERNAL (Given)
*        Routine to evaluate model for a given x value array.
*        SUBROUTINE MODF2( NX, X, NPAR, MODPAR, Y, STATUS )
*           NX = INTEGER (Given)
*           X( NX ) = REAL (Given)
*           NPAR = INTEGER (Given)
*           MODPAR( NPAR ) = REAL (Given)
*           Y( NX ) = REAL (Returned)
*           STATUS = INTEGER (Given and Returned)
*        This routine is used for the secondary model, i.e. the
*        full-drawn line in the top viewport.
*     NPAR = INTEGER (Given)
*        The size of the MODPAR array, passed on to MODF, MODF2.
*     MODPAR( NPAR ) = REAL (Given)
*        The model parameters, passed on to MODF, MODF2. Note that both
*        routines have to use the same set of parameters.
*     MSKDIM = INTEGER (Given)
*        Size of MASK array. Must be even.
*     MSKUSE = INTEGER (Given)
*        Number of valid mask intervals. Give zero to indicate that no
*        mask array is available.
*     MASK( MSKDIM ) = REAL (Given)
*        The mask is used to draw a number of pedestals at the bottom of
*        the bottom viewport. The pedestals extend
*           from MASK(1) to MASK(1+MSKDIM/2),
*           from MASK(2) to MASK(2+MSKDIM/2),
*              ...
*           from MASK(MSKUSE) to MASK(MSKUSE+MSKDIM/2),
*        provided that the leading end is actually to the left of the
*        trailing end. Pedestals may overlap.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     29 Apr 1994 (hme):
*        Original version.
*     06 May 1994 (hme):
*        Added all the extra arguments needed for the display routine.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Global Variables:
      INCLUDE 'SPD_PCOM'         ! Specdre SPLOOP common block

*  Arguments Given:
      LOGICAL MODX
      LOGICAL MODX2
      EXTERNAL MODF
      EXTERNAL MODF2
      INTEGER NPAR
      REAL MODPAR( NPAR )
      INTEGER MSKDIM
      INTEGER MSKUSE
      REAL MASK( MSKDIM )

*  Arguments Given and Returned:
      REAL XKEY
      REAL YKEY

*  Arguments Returned:
      CHARACTER * ( 1 ) T_CKEY

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL PLOT               ! True if re-display necessary
      REAL DELTA                 ! Current range
      CHARACTER * ( 1 ) CKEY     ! Local (upper case) version of key

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  The following loop is broken as soon as PGCURS returns a key that
*  this routine cannot handle.
      CKEY = 'W'
 1    CONTINUE                   ! Start of 'DO WHILE' loop
      IF ( 0 .NE. INDEX('WXYZP',CKEY)
     :     .AND. STATUS .EQ .SAI__OK   ) THEN

*     Wait for next PGCURS event.
         CALL PGCURS( XKEY, YKEY, T_CKEY )
         CKEY = T_CKEY
         CALL CHR_UCASE( CKEY )

*     If the cursor is outside the x range, ignore the event.
*     Outside the y range is allowed so that the user can use the top
*     (inactive) viewport for x marking.
         IF ( XKEY .LT. SPWIN(1) .OR. XKEY .GT. SPWIN(2) ) THEN
            PLOT = .FALSE.

*     Else if "X" zoom chosen.
*     The window does never exceed the full x range, but the zoom factor
*     may be higher than 2 and XKEY may not be in the centre.
         ELSE IF ( CKEY .EQ. 'X' ) THEN
            DELTA = ( SPWIN(2) - SPWIN(1) ) / 2.
            SPWIN(1) = XKEY - DELTA / 2.
            SPWIN(2) = XKEY + DELTA / 2.
            PLOT = .TRUE.

*     Else if "Y" zoom chosen.
         ELSE IF ( CKEY .EQ. 'Y' ) THEN
            DELTA = ( SPWIN(4) - SPWIN(3) ) / 2.
            SPWIN(3) = YKEY - DELTA / 2.
            SPWIN(4) = YKEY + DELTA / 2.
            PLOT = .TRUE.

*     Else if "Z" zoom chosen.
         ELSE IF ( CKEY .EQ. 'Z' ) THEN
            DELTA = ( SPWIN(2) - SPWIN(1) ) / 2.
            SPWIN(1) = XKEY - DELTA / 2.
            SPWIN(2) = XKEY + DELTA / 2.
            DELTA = ( SPWIN(4) - SPWIN(3) ) / 2.
            SPWIN(3) = YKEY - DELTA / 2.
            SPWIN(4) = YKEY + DELTA / 2.
            PLOT = .TRUE.

*     Else if "W" whole image chosen, reset window and contrast.
         ELSE IF ( CKEY .EQ. 'W' ) THEN
            SPWIN(1) = SPRNG(1)
            SPWIN(2) = SPRNG(2)
            SPWIN(3) = SPRNG(3)
            SPWIN(4) = SPRNG(4)
            PLOT = .TRUE.

*     Else if "P" pan chosen.
         ELSE IF ( CKEY .EQ. 'P' ) THEN
            PLOT = .FALSE.
            DELTA = SPWIN(2) - SPWIN(1)
            IF ( XKEY .LE. SPWIN(1) + DELTA / 4. ) THEN
               SPWIN(1) = SPWIN(1) - 0.75 * DELTA
               SPWIN(2) = SPWIN(1) + DELTA
               PLOT = .TRUE.
            ELSE IF ( XKEY .GE. SPWIN(2) - DELTA / 4. ) THEN
               SPWIN(2) = SPWIN(2) + 0.75 * DELTA
               SPWIN(1) = SPWIN(2) - DELTA
               PLOT = .TRUE.
            END IF
            DELTA = SPWIN(4) - SPWIN(3)
            IF ( YKEY .LE. SPWIN(3) + DELTA / 4. ) THEN
               SPWIN(3) = SPWIN(3) - 0.75 * DELTA
               SPWIN(4) = SPWIN(3) + DELTA
               PLOT = .TRUE.
            ELSE IF ( YKEY .GE. SPWIN(4) - DELTA / 4. ) THEN
               SPWIN(4) = SPWIN(4) + 0.75 * DELTA
               SPWIN(3) = SPWIN(4) - DELTA
               PLOT = .TRUE.
            END IF
         END IF

*     If re-display necessary, do so.
         IF ( PLOT )
     :      CALL SPD_PBAB( MODX, MODX2, MODF, MODF2, NPAR, MODPAR,
     :         MSKDIM, MSKUSE, MASK, STATUS )
         PLOT = .FALSE.

      GO TO 1
      END IF                     ! End of 'DO WHILE' loop

*  Return.
      END
