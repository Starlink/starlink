      SUBROUTINE KPG1_PLOTS( IPLOT, IPICD, DATREF, ICURR, STATUS )
*+
*  Name:
*     KPG1_PLOT

*  Purpose:
*     Save an AST Plot with an AGI DATA picture.

*  Language:
*     Starlink Fortran 77

*  Invocation:

*     CALL KPG1_PLOTS( IPLOT, IPICD, DATREF, ICURR, STATUS )

*  Description:
*     This routine saves a specified Plot and data reference in the AGI 
*     database with a specified DATA picture. A specified Frame can be 
*     made current before saving the Plot.

*  Arguments:
*     IPLOT = INTEGER (Given)
*        An AST pointer to the Plot. The current Frame is unchanged on
*        exit (even if a Frame is specified using ICURR).
*     IPICD = INTEGER (Given)
*        An AGI identifier for the DATA picture.
*     DATREF = CHARACTER * ( * ) (Given)
*        A data reference to store with the picture.
*     ICURR = INTEGER (Returned)
*        The index of a Frame to make current before storing the Plot. The
*        supplied current Frame is used if ICURR is AST__NOFRAME.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     24-SEP-1998 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations

*  Arguments Given:
      INTEGER IPLOT
      INTEGER IPICD
      CHARACTER DATREF*(*)
      INTEGER ICURR

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER ICURR0              ! Index of original current Frame
*.

*  Check the inherited status. 
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If a non-blank data reference string was supplied, put it into the 
*  AGI database for the DATA picture.
      IF( DATREF .NE. ' ' ) CALL AGI_PTREF( DATREF, IPICD, STATUS )

*  Save the Plot with the DATA picture in the database. If a Frame has
*  been specified, save the index of the Current Frame in the Plot, and 
*  temporarily make the specified Frame the Current Frame.
      IF( ICURR .NE. AST__NOFRAME ) THEN
         ICURR0 = AST_GETI( IPLOT, 'CURRENT', STATUS )
         CALL AST_SETI( IPLOT, 'CURRENT', ICURR, STATUS )
      END IF

*  Save the Plot in the AGI database.
      CALL KPG1_GDPUT( IPICD, IPLOT, STATUS )

*  Re-instate the original Current Frame in the Plot, if required.
      IF( ICURR .NE. AST__NOFRAME ) CALL AST_SETI( IPLOT, 'CURRENT', 
     :                                              ICURR0, STATUS )

      END
