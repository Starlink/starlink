      SUBROUTINE KPS1_AGNCP( PALNUM, POICOL, MXPOL, NPTS, ARDDEF, XLOW,
     :                       XHIGH, YLOW, YHIGH, XCUR, YCUR, NOBT, X, Y, 
     :                       STATUS )
*+
*   Name:
*     KPS1_AGNCP

*  Purpose:
*     Get screen positions defining an ARD region for ARDGEN.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_AGNCP( PALNUM, POICOL, MXPOL, NPTS, ARDDEF, XLOW,
*                      XHIGH, YLOW, YHIGH, XCUR, YCUR, NOBT, X, Y,
*                      STATUS )

*  Description:
*     This subroutine enables use of the cursor on the current graphics
*     device to select a defined number of points whose co-ordinates
*     are returned.  Only positions within the defined limits are
*     accepted.

*  Arguments:
*     PALNUM = INTEGER (Given)
*        Pen number to be used.
*     POICOL = INTEGER (Given)
*        Pen number to be used to show the defining points.
*     MXPOL = INTEGER (Given)
*        Maximum number of points in a polygon.
*     NPTS = INTEGER (Given)
*        Number of point locations to be determined using the cursor.
*     ARDDEF = CHARACTER * ( * ) (Given)
*        User selected ARD region keyword.
*     XLOW = REAL (Given)
*        Lower x co-ordinate that defines the region in which all
*        points selected by the cursor must lie.
*     XHIGH = REAL (Given)
*        Upper x co-ordinate that defines the region in which all
*        points selected by the cursor must lie.
*     YLOW = REAL (Given)
*        Lower y co-ordinate that defines the region in which all
*        points selected by the cursor must lie.
*     YHIGH = REAL (Given)
*        Upper y co-ordinate that defines the region in which all
*        points selected by the cursor must lie.
*     XCUR = REAL (Given and Returned)
*        X co-ordinate of the position where the cursor will first
*        appear.  This is usually the last location of the cursor
*        or the centre of the picture.
*     YCUR = REAL (Given and Returned)
*        Y co-ordinate of the position where the cursor will first
*        appear.
*     NOBT = INTEGER (Returned)
*        Number of points obtained with the cursor.  This is usually
*        the same as NPTS, but can be different if a premature exit
*        was made.
*     X( MXPOL ) = REAL (Returned)
*        The x co-ordinates of the points measured by the cursor.
*     Y( MXPOL ) = REAL (Returned)
*        The y co-ordinates of the points measured by the cursor.
*     STATUS  = INTEGER (Given and Returned)
*        Global status value.

*  Authors:
*    GJP: Grant Privett (STARLINK)
*    DSB: David Berry (STARLINK)
*    MJC: Malcolm J. Currie (STARLINK)
*    {enter_new_authors_here}

*  History:   
*     6-JUN-1994 (GJP)
*        Original version based on CURPTS by Malcolm Currie.
*     11-NOV-1994 (GJP)
*        Added ARD further keywords.
*     5-DEC-1994 (DSB):
*        Tidied up.  Name changed from ARDG1_CURPTS to KPS1_AGNCP.
*        Button assignments changed.  MXPOL argument added.
*        Re-structured.
*     1995 March 15 (MJC):
*        Shortened long lines, sorted the variables, corrected typo's,
*        and prologue indentation.
*     1995 November 24 (MJC):
*        Now also exits if the choice is less than or equal to zero, so
*        will exit if the right-hand button of the mouse is pressed on
*        a workstation or X-terminal, and so bring it into line with
*        other tasks.
*     1995 December 16 (MJC):
*        No longer exits if choice 0 is returned.  This is equivalent to
*        the middle button of a mouse.  Switched the meanings of choice
*        two and three.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! SSE definitions
      INCLUDE 'DAT_PAR'	         ! HDS public onstants
      INCLUDE 'PAR_PAR'          ! Parameter system public constants

*  Arguments Given:
      INTEGER PALNUM             ! Pen number required
      INTEGER POICOL             ! Erase pen number
      INTEGER MXPOL              
      INTEGER NPTS               ! Number of points to be obtained
      INTEGER MXCHO              ! End of selection signal
      CHARACTER * ( * ) ARDDEF   ! Suggested default ARD keyword
      REAL XLOW, YLOW            ! Lower bound of the region for
                                 ! valid cursor positions
      REAL XHIGH, YHIGH          ! Upper bound of the region for
                                 ! valid cursor positions

*  Arguments Given and Returned:
      REAL XCUR, YCUR            ! Start position of the cursor

*  Arguments Returned:
      INTEGER NOBT               ! Number of points obtained
      REAL X( MXPOL ), Y( MXPOL ) ! Information of points selected

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER COLI               ! Polyline colour index
      LOGICAL DUPLIC             ! The current cursor position has
                                 ! already been selected and points must
                                 ! be distinct
      LOGICAL DYNAMC             ! Polyline colour representation is
                                 ! dynamic
      INTEGER GSTAT              ! GKS status
      INTEGER HITVAL             ! Number of button pressed on the
                                 ! input device to get a position
      INTEGER I                  ! Loop counter
      LOGICAL MORE               ! More points to be defined?
      INTEGER N                  ! Loop counter
      INTEGER LASF( 13 )         ! GKS list of aspect source flags
      INTEGER LNTYPE             ! Polyline type
      INTEGER PEN                ! Input SGS pen
      INTEGER WKID               ! Workstation identifier
      REAL XIN                   ! x co-ordinate from the cursor
      REAL YIN                   ! y co-ordinate from the cursor
      REAL XL, YL                ! Last valid cursor position

*.
 
*  Check the global inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set the end of selection flag value.
      MXCHO = 3

*  Initialise the number of points obtained here in case there is a GKS
*  error.
      N = 0
      DYNAMC = .FALSE.

*  Store the current SGS pen as the crosses will be drawn in the colour
*  requested (if colour is available).
      CALL SGS_IPEN( PEN )

*  Set the pen colour.
      CALL SGS_SPEN( PALNUM )

*  Does the device have a dynamic colour representation?
      CALL SGS_ICURW( WKID )
      CALL DYNCLR( WKID, DYNAMC, STATUS )

*  Display surface will clear at the end of an application unless it is
*  dynamic.
      IF ( DYNAMC .AND. STATUS .EQ. SAI__OK ) THEN

*  Inquire the GKS aspect source flags.
         CALL GQASF( GSTAT, LASF )

*  Watch out for any error.
         CALL GKS_GSTAT( STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL MSG_OUT( 'KPS1_AGNCP_MSG1', 'Problem reading GKS '/
     :                    /'aspect flags.', STATUS )
            GO TO 999
         END IF

      END IF

*  Get the initial cursor position.
      XL = XCUR
      YL = YCUR

*  Loop until the the region is defined or the user cancels it, or an
*  error occurs. Do not loop if no points are required (i.e. if
*  NPTS=0).
      MORE = ( N .NE. NPTS )
      DO WHILE ( MORE .AND. STATUS .EQ. SAI__OK )

*  Start a new error context.
         CALL ERR_MARK

*  If a message has already been displayed, and then the cursor is
*  used, the next message is no longer in synchronisation with the
*  cursor.  So synchronise the message system just in case.
         CALL MSG_SYNC( STATUS )

*  If a message has already been displayed, and then the cursor is
*  used, the next message is no longer in synchronisation with the
*  cursor.  So synchronise the message system just in case.  FOr obscure
*  reasons this needs to be done twice to be confident of
*  synchronisation.
         CALL MSG_SYNC( STATUS )

*  Wait for the user to press a button and then get the cursor position 
*  and the button value...
*     HITVAL = 1 or 3  take as the select position.
*     HITVAL = 2 show cursor co-ordinates.
*     HITVAL = <0 or 4 user ends input.
         CALL SGS_SETCU( XL, YL )
         CALL SGS_REQCU( XIN, YIN, HITVAL )

*  This is strictly not portable, but since it is impossible to
*  determine the number of mouse buttons of a workstation/X-terminal
*  available through GKS, we assume a three-button mouse.  Convert the
*  'break' button into the second choice, as it was under GKS 7.2.
         IF ( HITVAL .EQ. 0 ) HITVAL = 2
  
*  If the user has selected a point...
         IF ( HITVAL .EQ. 1 .OR. HITVAL .EQ. 3 ) THEN

*  Ignore positions which are outside the allowed area.
            IF ( ( XIN .GE. XLOW ) .AND. ( XIN .LE. XHIGH ) .AND. 
     :           ( YIN .GE. YLOW ) .AND. ( YIN .LE. YHIGH ) ) THEN

*  Save the new cursor position.
               XL = XIN
               YL = YIN

*  Check for previously defined duplicate point.
               DUPLIC = .FALSE.
               IF ( N .GT. 0 ) THEN

                 DO I = 1, N

                    IF ( ( XIN .EQ. X( I ) ) .AND.
     :                  ( YIN .EQ. Y( I ) ) ) THEN
                       DUPLIC = .TRUE.
                       CALL MSG_OUT( 'KPS1_AGNCP_MSG2', 
     :                      'Duplicate point ignored.', STATUS )
                    END IF

                 END DO

               END IF

*  Deal with new points.
               IF ( .NOT. DUPLIC ) THEN

*  Increment the count of points and store the co-ordinates in the
*  co-ordinate arrays.  Cancel the current region and prompt for a new
*  shape if too many points have been given.
                  N = N + 1
                  IF ( N .LE. MXPOL ) THEN
                     X( N ) = XIN
                     Y( N ) = YIN

                  ELSE
                     CALL MSG_OUT( 'KPS1_AGNCP_MSG3', 'Too many '/
     :                             /'points given!', STATUS )
                     MORE = .FALSE.

                  END IF

*  Draw a line from current point to last point if a polygon is being
*  defined.
                  IF ( ( ARDDEF .EQ. 'POLYGON') .AND.
     :                 ( N .GT. 1 ) ) THEN
                     CALL SGS_LINE( X( N - 1 ), Y( N - 1 ),
     :                              X( N ),     Y( N ) )

*  Otherwise, mark the position with a point.
                  ELSE
                     CALL SGS_LINE( XIN, YIN, XIN, YIN )

*  If all the positions needed to defined the non-polygon region have
*  been given, leave the loop.
                     IF ( N .EQ. NPTS ) MORE = .FALSE.

                  END IF

               END IF

            ELSE

*  Indicate why the point is being disregarded.
               CALL MSG_OUT( 'KPS1_AGNCP_MSG4', 'Chosen point lies '/
     :                       /'outside the allowed region.', STATUS )
            END IF

*  If the user wants to see the current cursor co-ordinates...
         ELSE IF ( HITVAL .EQ. 2 ) THEN
            CALL KPS1_AGNCV( XLOW, YLOW, XIN, YIN, STATUS )
            XL = XIN
            YL = YIN

*  If the user wants to stop giving points...
         ELSE IF ( HITVAL .LT. 0 .OR. HITVAL .EQ. 4 ) THEN 
            MORE = .FALSE.

*  If a polygon has been defined, close it.
            IF ( ( ARDDEF .EQ. 'POLYGON' ) .AND. ( N .GE. 3 ) ) THEN
               CALL SGS_SPEN( PALNUM )
               CALL SGS_LINE( X( 1 ), Y( 1 ), X( N ), Y( N ) )
            END IF

         END IF

*  Release the new error context.
         CALL ERR_RLSE

*  Flush the graphics buffer.
         CALL SGS_FLUSH

*  End of loop to get a series of points.
      END DO

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  If all the positions needed to defined the non-polygon region have
*  been given, draw it.
      IF ( N .EQ. NPTS ) CALL KPS1_AGNDR( MXPOL, XLOW, XHIGH, YLOW, 
     :                                    YHIGH, ARDDEF, X, Y, STATUS )

*  Erase the marks used to identify the selected positions (if
*  possible).
      IF ( DYNAMC ) THEN

*  Set the linetype and colour index aspect source flags to individual.
         LASF( 1 ) = 1
         LASF( 3 ) = 1
         CALL GSASF( LASF )

*  Store the current linetype and colour index.
         CALL GQLN( GSTAT, LNTYPE )
         CALL GQPLCI( GSTAT, COLI )

*  Watch out for any error.
         CALL GKS_GSTAT( STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Set the colour for erasing the crosses.
         CALL GSLN( 1 )
         CALL GSPLCI( POICOL )

*  Erase the crosses ( unless the requested KEYWORD in use required
*  only one point ie point, row or column).
         IF ( N .GT. 1 ) THEN
            DO I = 1, N
               CALL SGS_LINE( X( I ), Y( I ), X( I ), Y( I ) )
            END DO
         END IF

*  Restore the input linetype and colour index.
         CALL GSLN( LNTYPE )
         CALL GSPLCI( COLI )

*  Set the linetype and colour-index aspect source flags to bundled.
         LASF( 1 ) = 0
         LASF( 3 ) = 0
         CALL GSASF( LASF )

*  Watch out for any error.
         CALL GKS_GSTAT( STATUS )

      END IF

*  Reset the SGS pen to its value on input.
      CALL SGS_SPEN( PEN )

  999 CONTINUE

*  Set the value of NOBT to show if the selection process was
*  completed.
      NOBT = N

      END
