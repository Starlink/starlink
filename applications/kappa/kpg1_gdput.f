      SUBROUTINE KPG1_GDPUT( IPIC, IPLOT, STATUS )
*+
*  Name:
*     KPG1_GDPUT

*  Purpose:
*     Save an AST Plot with a graphics database picture.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_GDPUT( IPIC, IPLOT, STATUS )

*  Description:
*     This routine saves the supplied AST Plot (see SUN/210) in the AGI
*     database (see SUN/48) within the MORE structure of the specified 
*     picture. It can be retrieved if necessary using KPG1_GDGET (see the 
*     prologue of KPG1_GDGET for more information about using these two 
*     routines). 

*  Arguments:
*     IPIC = INTEGER (Given)
*        The AGI identifier for the picture. A value of -1 causes the
*        Plot to be stored with the current picture.
*     IPLOT = INTEGER (Given)
*        An AST pointer to the Plot to be stored with the picture. If
*        AST__NULL is supplied, any existing Plot stored with the picture is
*        deleted.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The Base (GRAPHICS) Frame in the Plot should represent millimetres
*     from the bottom left corner of the view surface.
*     -  An error is reported if the Plot contains any Frames which have the 
*     Domain AGI_DATA.
*     -  The PGPLOT interface to the AGI library should be opened before
*     calling this routine.  
*     -  The Plot is stored in a component of the MORE structure named 
*     "AST_PLOT" and with type "WCS".

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2-MAR-1998 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants
      INCLUDE 'DAT_PAR'          ! DAT constants

*  Arguments Given:
      INTEGER IPIC
      INTEGER IPLOT

*  Status:
      INTEGER STATUS               ! Global status

*  Local Variables:
      CHARACTER MODE*6             ! Access mode for picture's MORE structure
      CHARACTER MORLOC*(DAT__SZLOC)! HDS locator for picture's MORE structure
      INTEGER FRM                  ! Pointer to next Frame
      INTEGER I                    ! Frame index
      INTEGER IFRM                 ! Index of AGI_DATA Frame
      INTEGER IPICL                ! AGI identifier for picture to use
      LOGICAL MORE                 ! Does picture have a MORE structure?
      LOGICAL THERE                ! Does MORE have an AST_PLOT component?
*.

*  Check the inherited status. 
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that an AST Plot has been supplied.
      IF( IPLOT .NE. AST__NULL ) THEN 
         IF( .NOT. AST_ISAPLOT( IPLOT, STATUS ) ) THEN
            IF( STATUS .EQ. SAI__OK ) THEN
               CALL MSG_SETC( 'CLASS', AST_GETC( IPLOT, 'CLASS', 
     :                                           STATUS ) )
               STATUS = SAI__ERROR
               CALL ERR_REP( 'KPG1_GDPUT_1', 'KPG1_GDPUT: Programming'//
     :                    ' error - an AST ^CLASS has been supplied '//
     :                    'when a Plot was needed.', STATUS )
               GO TO 999
            END IF
         END IF
      END IF

*  Get the identifier for the picture to use.
      IF( IPIC .EQ. -1 ) THEN
         CALL AGI_ICURP( IPICL, STATUS )
      ELSE
         IPICL = IPIC
      END IF

*  Set the access mode required for the MORE structure, depending on
*  whether or not the picture already has a more structure.
      CALL AGI_IMORE( IPICL, MORE, STATUS ) 
      IF( MORE ) THEN
         MODE = 'UPDATE'
      ELSE
         MODE = 'WRITE'
      END IF

*  Get an HDS locator to the MORE structure.
      CALL AGI_MORE( IPICL, MODE, MORLOC, STATUS ) 

*  If supplied, see if the Plot contains any AGI_DATA frames. Component
*  Frames within CmpFrames are also checked.
      IF( IPLOT .NE. AST__NULL ) THEN
         CALL KPG1_ASFFR( IPLOT, 'AGI_DATA', IFRM, STATUS )

*  Report an error if an AGI_DATA Frame was found.
         IF( IFRM .NE. AST__NOFRAME .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'KPG1_GDPUT_2', 'KPG1_GDPUT: An attempt has'//
     :                    ' been made to store an AST Plot containing'//
     :                    ' an AGI_DATA Frame in the graphics '//
     :                    'database (programming error).', STATUS )

*  Otherwise, save the Plot in the database.
         ELSE
            CALL KPG1_WWRT( IPLOT, 'AST_PLOT', MORLOC, STATUS )
         END IF

*  If no Plot was supplied, delete the AST_PLOT component within MORE if it 
*  exists.
      ELSE
         CALL DAT_THERE( MORLOC, 'AST_PLOT', THERE, STATUS )
         IF( THERE ) CALL DAT_ERASE( MORLOC, 'AST_PLOT', STATUS )
      END IF

 999  CONTINUE

*  Annul the locator to the MORE structure.
      CALL DAT_ANNUL( MORLOC, STATUS )

      END
