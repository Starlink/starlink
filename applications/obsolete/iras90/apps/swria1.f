      SUBROUTINE SWRIA1( XDIM, DIR, HEIGHT, RATIO, JSTFCT, SPACE, FONT,
     :                   PEN, STATUS )
*+
*  Name:
*     SWRIA1

*  Purpose:
*     Set text attribute.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SWRIA1( XDIM, DIR, HEIGHT, RATIO, JSTFCT, SPACE, FONT,
*                  PEN, STATUS )

*  Description:
*     This subroutine is used by application SKYWRITE to set text
*     attributes.

*  Arguments:
*     XDIM = REAL (Given)
*        The wide of the current SGS zone.
*     DIR( 2 ) = REAL (Given)
*        The setting of the text direction.
*     HEIGHT = REAL (Given)
*        The setting of the text hight as HEIGHT*IMGWID.
*     RATIO = REAL (Given)
*        The setting of the text aspect ratio.
*     JSTFCT = CHARACTER*( 2 ) (Given)
*        The setting of the text justification.
*     SPACE = REAL (Given)
*        The setting of the space between characters.
*     FONT = INTEGER (Given)
*        The setting of the text font.
*     PEN = INTEGER (Given)
*        The setting of the SGS pen to be used.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     {enter_new_authors_here}

*  History:
*     12-FEB-1993 (WG):
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
      REAL XDIM
      REAL DIR( 2 ), HEIGHT, RATIO
      CHARACTER*( 2 ) JSTFCT
      REAL SPACE
      INTEGER FONT, PEN

*  Status:
      INTEGER STATUS             ! Global status

*  Local variables:
      LOGICAL COLOUR             ! Colour available flag

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set text upward diretion
      IF( DIR( 1 )*DIR( 2 ) .NE. 0.0 ) THEN
         CALL SGS_SUPTX( DIR( 1 ), DIR( 2 ) )
      ELSE
         CALL SGS_SUPTX( 0.0, 1.0 )
      END IF

*  Set height as the fraction of the X dimension of the image.
      CALL SGS_SHTX( MAX( 0.00001, HEIGHT ) * XDIM )

*  Set Aspect ratio.
      CALL SGS_SARTX( ABS( RATIO ) )

*  Set text justification.
      CALL SGS_STXJ( JSTFCT )

*  Set space between characters.
      CALL SGS_SSPTX( SPACE )

*  Set font number.
      CALL SGS_SFONT( MAX( 1, FONT ) )

*  If colour is available, set the pen number, otherwise use pen 1.
      CALL IRM_QCOL( COLOUR, STATUS )

      IF( COLOUR ) THEN
         CALL SGS_SPEN( MAX( 0, PEN ) )
      ELSE
         CALL SGS_SPEN( 1 )
      END IF

*  See if an error was reported by SGS/GKS.
      CALL GKS_GSTAT( STATUS )

      END
