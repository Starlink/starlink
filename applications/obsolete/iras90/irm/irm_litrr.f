      SUBROUTINE IRM_LITRR( SCALE, OFFSET, STATUS )
*+
*  Name:
*     IRM_LITRR

*  Purpose:
*     Saves a transformation for a linear plot in the AGI database.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRM_LITRR( SCALE, OFFSET, STATUS )

*  Description:
*     This routine defines the transformations between world and a
*     linear data co-ordinate system that has either, neither or both
*     axes with reversed polarity (increasing right to left or top to
*     bottom), and saves the transformation in the AGI database with
*     the current picture.

*  Arguments:
*     SCALE( 2 ) = REAL (Given)
*        The scale factors of each linear axis to transform from world
*        co-ordinates to data co-ordinates.
*     OFFSET( 2 ) = REAL (Given)
*        The offsets of each linear axis at pixel 0 to transform from
*        world co-ordinates to data co-ordinates.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Prior Requirements:
*     -  There must be a current AGI picture.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     4-NOV-1992 (DSB):
*        Original version, copied from KAPPA routine KPG1_LITRx.GEN
*        written by MJC.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT constants

*  Arguments Given:
      REAL
     :  SCALE( 2 ),
     :  OFFSET( 2 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER NCD                ! Number of data co-ordinates
      PARAMETER ( NCD = 2 )
      INTEGER NCW                ! Number of world co-ordinates
      PARAMETER ( NCW = 2 )

*  Local Variables:
      CHARACTER * ( 24 + 2 * VAL__SZR )
     :  DTOW( NCW ),             ! Expressions for converting data to
                                 ! world co-ordinates
     :  WTOD( NCD )              ! Expressions for converting world to
                                 ! data co-ordinates

      INTEGER
     :  I,                       ! Loop counter
     :  NSUBS                    ! Number of token substitutions
*.

*    Check the inherited global status.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Assign the transformations for the x co-ordinates.

      WTOD( 1 ) = 'XL = X * x_scale + x_offset'
      DTOW( 1 ) = 'X = ( XL - x_offset ) / x_scale'

*    Assign the transformations for the y co-ordinates.

      WTOD( 2 ) = 'YL = Y * y_scale + y_offset'
      DTOW( 2 ) = 'Y = ( YL - y_offset ) / y_scale'

*    Substitute the actual scales for the tokens.

      CALL TRN_STOKR( 'x_scale', SCALE( 1 ), WTOD( 1 ), NSUBS,
     :                  STATUS )
      CALL TRN_STOKR( 'y_scale', SCALE( 2 ), WTOD( 2 ), NSUBS,
     :                  STATUS )
      CALL TRN_STOKR( 'x_scale', SCALE( 1 ), DTOW( 1 ), NSUBS,
     :                  STATUS )
      CALL TRN_STOKR( 'y_scale', SCALE( 2 ), DTOW( 2 ), NSUBS,
     :                  STATUS )

*    Substitute the actual offsets for the tokens.

      CALL TRN_STOKR( 'x_offset', OFFSET( 1 ), WTOD( 1 ), NSUBS,
     :                  STATUS )
      CALL TRN_STOKR( 'y_offset', OFFSET( 2 ), WTOD( 2 ), NSUBS,
     :                  STATUS )
      CALL TRN_STOKR( 'x_offset', OFFSET( 1 ), DTOW( 1 ), NSUBS,
     :                  STATUS )
      CALL TRN_STOKR( 'y_offset', OFFSET( 2 ), DTOW( 2 ), NSUBS,
     :                  STATUS )

*    Save the transformation in the database associated with the current
*    picture.

      CALL AGI_TNEW( NCD, NCW, DTOW, WTOD, -1, STATUS )

      END
