      SUBROUTINE SPD_PDAC( XKEY, YKEY, T_CKEY, STATUS )
*+
*  Name:
*     SPD_PDAC

*  Purpose:
*     PGCURS with pan and zoom through finder image.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_PDAC( XKEY, YKEY, CKEY, STATUS )

*  Description:
*     This routine is a wrap around PGCURS. It will return a position
*     and the character pressed, just like PGCURS. However, this routine
*     also knows about the finder image and how it is currently
*     displayed. The finder image should be displayed before this
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
*     C: Increase the contrast twofold with the brightness under the
*        cursor becoming the new medium grey brightness.
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
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     29 Apr 1994 (hme):
*        Original version.
*     2005 June 2 (MJC):
*        Use CNF_PVAL for pointers to mapped data.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Global Variables:
      INCLUDE 'SPD_PCOM'         ! Specdre SPLOOP common block

*  Arguments Given and Returned:
      REAL XKEY
      REAL YKEY

*  Arguments Returned:
      CHARACTER * ( 1 ) T_CKEY

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL PLOT               ! True if re-display necessary
      INTEGER I, J, K            ! Array indices
      REAL DELTA                 ! Current range
      REAL ZKEY                  ! Brightness under cursor
      CHARACTER * ( 1 ) CKEY     ! Local (upper case) version of key

*  Internal References:
      REAL SPD_PEABR             ! Get an array elements

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  The following loop is broken as soon as PGCURS returns a key that
*  this routine cannot handle.
      CKEY = 'W'
 1    CONTINUE                   ! Start of 'DO WHILE' loop
      IF ( 0 .NE. INDEX('WXYZCP',CKEY)
     :     .AND. STATUS .EQ .SAI__OK   ) THEN

*     Wait for next PGCURS event.
         CALL PGCURS( XKEY, YKEY, T_CKEY )
         CKEY = T_CKEY
         CALL CHR_UCASE( CKEY )

*     If the cursor is outside the x or y range, ignore the event.
         IF ( XKEY .LT. IMWIN(1) .OR. XKEY .GT. IMWIN(2) .OR.
     :        YKEY .LT. IMWIN(3) .OR. YKEY .GT. IMWIN(4)      ) THEN
            PLOT = .FALSE.

*     Else if "X" zoom chosen.
*     The window does never exceed the full x range, but the zoom factor
*     may be higher than 2 and XKEY may not be in the centre.
         ELSE IF ( CKEY .EQ. 'X' ) THEN
            DELTA = ( IMWIN(2) - IMWIN(1) ) / 2.
            IMWIN(1) = MAX( XKEY-DELTA/2., IMZOOM(1)+1. )
            IMWIN(2) = MIN( XKEY+DELTA/2., IMZOOM(1)+FLOAT(IMDIM(1)) )
            PLOT = .TRUE.

*     Else if "Y" zoom chosen.
         ELSE IF ( CKEY .EQ. 'Y' ) THEN
            DELTA = ( IMWIN(4) - IMWIN(3) ) / 2.
            IMWIN(3) = MAX( YKEY-DELTA/2., IMZOOM(4)+1. )
            IMWIN(4) = MIN( YKEY+DELTA/2., IMZOOM(4)+FLOAT(IMDIM(2)) )
            PLOT = .TRUE.

*     Else if "Z" zoom chosen.
         ELSE IF ( CKEY .EQ. 'Z' ) THEN
            DELTA = ( IMWIN(2) - IMWIN(1) ) / 2.
            IMWIN(1) = MAX( XKEY-DELTA/2., IMZOOM(1)+1. )
            IMWIN(2) = MIN( XKEY+DELTA/2., IMZOOM(1)+FLOAT(IMDIM(1)) )
            DELTA = ( IMWIN(4) - IMWIN(3) ) / 2.
            IMWIN(3) = MAX( YKEY-DELTA/2., IMZOOM(4)+1. )
            IMWIN(4) = MIN( YKEY+DELTA/2., IMZOOM(4)+FLOAT(IMDIM(2)) )
            PLOT = .TRUE.

*     Else if "W" whole image chosen, reset window and contrast.
         ELSE IF ( CKEY .EQ. 'W' ) THEN
            IMWIN(1) = IMZOOM(1) + 1.
            IMWIN(2) = IMZOOM(1) + FLOAT( IMDIM(1) )
            IMWIN(3) = IMZOOM(4) + 1.
            IMWIN(4) = IMZOOM(4) + FLOAT( IMDIM(2) )
            CALL SPD_PEAAR( .FALSE., IMDIM(1)*IMDIM(2),
     :                      %VAL( CNF_PVAL(IMAGE) ),
     :                      IMRNG(1), IMRNG(2), STATUS )
            PLOT = .TRUE.

*     Else if "C" increase contrast chosen.
         ELSE IF ( CKEY .EQ. 'C' ) THEN
            I = INT( (XKEY-IMZOOM(1))/IMZOOM(2) + 0.5 )
            J = INT( (YKEY-IMZOOM(4))/IMZOOM(6) + 0.5 )
            K = I + (J-1) * IMDIM(1)
            ZKEY = SPD_PEABR( %VAL( CNF_PVAL(IMAGE) ), K, STATUS )
            DELTA = ( IMRNG(2) - IMRNG(1) ) / 2.
            IMRNG(1) = ZKEY - DELTA / 2.
            IMRNG(2) = ZKEY + DELTA / 2.
            PLOT = .TRUE.

*     Else if "P" pan chosen.
         ELSE IF ( CKEY .EQ. 'P' ) THEN
            PLOT = .FALSE.
            DELTA = IMWIN(2) - IMWIN(1)
            IF ( XKEY .LE. IMWIN(1) + DELTA / 4. ) THEN
               IMWIN(1) = MAX( IMWIN(1)-0.75*DELTA, IMZOOM(1)+1. )
               IMWIN(2) = IMWIN(1) + DELTA
               PLOT = .TRUE.
            ELSE IF ( XKEY .GE. IMWIN(2) - DELTA / 4. ) THEN
               IMWIN(2) =
     :            MIN( IMWIN(2)+0.75*DELTA, IMZOOM(1)+FLOAT(IMDIM(1)) )
               IMWIN(1) = IMWIN(2) - DELTA
               PLOT = .TRUE.
            END IF
            DELTA = IMWIN(4) - IMWIN(3)
            IF ( YKEY .LE. IMWIN(3) + DELTA / 4. ) THEN
               IMWIN(3) = MAX( IMWIN(3)-0.75*DELTA, IMZOOM(4)+1. )
               IMWIN(4) = IMWIN(3) + DELTA
               PLOT = .TRUE.
            ELSE IF ( YKEY .GE. IMWIN(4) - DELTA / 4. ) THEN
               IMWIN(4) =
     :            MIN( IMWIN(4)+0.75*DELTA, IMZOOM(4)+FLOAT(IMDIM(2)) )
               IMWIN(3) = IMWIN(4) - DELTA
               PLOT = .TRUE.
            END IF
         END IF

*     If re-display necessary, do so.
         IF ( PLOT ) CALL SPD_PDAB( STATUS )
         PLOT = .FALSE.

      GO TO 1
      END IF                     ! End of 'DO WHILE' loop

*  Return.
      END
