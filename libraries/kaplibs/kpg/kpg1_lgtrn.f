      SUBROUTINE KPG1_LGTRN( XLOG, YLOG, STATUS )
*+
*  Name:
*     KPG1_LGTRN

*  Purpose:
*     Saves a transformation for a base-10 logarithmic plot in the AGI
*     database.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_LGTRN( XLOG, YLOG, STATUS )

*  Description:
*     This routine defines the transformations between world and a
*     log10-log10 or log10-linear world co-ordinate system, and saves
*     the transformation in the AGI database with the current picture.

*  Arguments:
*     XLOG = LOGICAL (Given)
*        If true the x-axis is logarithmic in world co-ordinates,
*        otherwise it is linear.
*     YLOG = LOGICAL (Given)
*        If true the y-axis is logarithmic in world co-ordinates,
*        otherwise it is linear.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Prior Requirements:
*     -  There must be a current AGI picture.

*  [optional_subroutine_items]...
*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 February 15 (MJC):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      LOGICAL
     :  XLOG,
     :  YLOG

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER NCD                ! Number of data co-ordinates
      PARAMETER ( NCD = 2 )
      INTEGER NCW                ! Number of world co-ordinates
      PARAMETER ( NCW = 2 )

*  Local Variables:
      CHARACTER * ( 15 )
     :  DTOW( NCW ),             ! Expressions for converting data to
                                 ! world co-ordinates
     :  WTOD( NCD )              ! Expressions for converting world to
                                 ! data co-ordinates

*.

*    Check the inherited global status.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Assign the transformations for the x co-ordinates.

      IF ( XLOG ) THEN

*       Logarithmic (base 10).

         WTOD( 1 ) = 'X = 10.**( XL )'
         DTOW( 1 ) = 'XL = LOG10( X )'
      ELSE

*       Linear. 

         WTOD( 1 ) = 'X = XL'
         DTOW( 1 ) = 'XL = X'
      END IF

*    Assign the transformations for the y co-ordinates.

      IF ( YLOG ) THEN

*       Logarithmic (base 10).

         DTOW( 2 ) = 'YL = LOG10( Y )'
         WTOD( 2 ) = 'Y = 10.**( YL )'
      ELSE

*       Linear. 

         WTOD( 2 ) = 'Y = YL'
         DTOW( 2 ) = 'YL = Y'
      END IF

*    Save the transformation in the database associated with the current
*    picture.

      CALL AGI_TNEW( NCD, NCW, DTOW, WTOD, -1, STATUS )

      END
