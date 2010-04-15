      SUBROUTINE SWRIA2( PTXT, PLON, PLAT, MODE, IRA, SCS, X1, X2, Y1,
     :                   Y2, MXNTXT, NTXT, LON, LAT, TXT, DIRX, DIRY,
     :                   HIGT, RTIO, JSTF, SPAC, FONT, PEN, STATUS )
*+
*  Name:
*     SWRIA2

*  Purpose:
*     Write text string interactively.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SWRIA2( PTXT, PLON, PLAT, MODE, IRA, SCS, X1, X2, Y1, Y2,
*                  MXNTXT, NTXT, LON, LAT, TXT, DIRX, DIRY, HIGT, RTIO,
*                  JSTF, SPAC, FONT, PEN, STATUS )

*  Description:
*     This subroutine is used by applicaition SKYWRITE to write text
*     string in the current SGS zone interactively. The positions of
*     texts to be written to are either specified by the cursor or by
*     their sky coordinates specified via the keyboard. The subroutine
*     will keep prompting the user for the specification of the next
*     text until a null. '!' response is obtained or the position
*     specified by the cursor is outside the zone. The positions,
*     texts and their attributes are retruned in arrays.

*  Arguments:
*     PTXT = CHARACTER (Given)
*        The name of the parameter used to get the text string to be
*        written to the image.
*     PLON = CHARACTER (Given)
*        The name of the parameter used to get the longitude of the
*        position at which the text is to be written.
*     PLAT = CHARACTER (Given)
*        The name of the parameter used to get the latitude of the
*        position at which the text is to be written.
*     MODE = CHARACTER (Given)
*        Application's working mode. It can be either 'KEYBOARD', the
*        position should be specified via keyboard, or 'CURSOR', the
*        position should be specified by cursor.
*     IRA = INTEGER (Given)
*        The id of the IRA system.
*     SCS = CHARACTER (Given)
*        The name of the sky coordinate system in use.
*     X1, X2, Y1, Y2 = REAL (Given)
*        Extension of the current SGS zone.
*     MXNTXT = INTEGER (Given)
*        The max. number of text string can be written to the image.
*     NTXT = INTEGER (Given and Returned)
*        Number of text strings have been written to the image.
*     LON( MXNTXT ), LAT( MXNTXT ) = DOUBLE PRECISION (Given and Returned)
*        Longitude and latitude of the positions at which texts has been
*        written.
*     TXT( MXNTXT ) = CHARACTER (Given and Returned)
*        The text strings have been written to the image so far.
*     DIRX( MXNTXT ), DIRY( MXNTXT ) = REAL (Given and Returned)
*        The up directions of the texts have been written so far.
*     HIGT( MXNTXT ) = REAL (Given and Returned)
*        The heights of the texts have been written so far.
*     RTIO( MXNTXT ) = REAL (Given and Returned)
*        The aspect ratio of the texts have been written so far.
*     JSTF( MXNTXT ) = CHARACTER (Given and Returned)
*        The justifications of the texts have been written so far.
*     SPAC( MXNTXT ) = CHARACTER (Given and Returned)
*        The space of the characters in the texts have been written so
*        far.
*     FONT( MXNTXT ) = INTEGER (Given and Returned)
*        The fonts of the texts have been written so far.
*     PEN( MXNTXT ) INTEGER (Given and Returned)
*        The pen of the texts have been written so far.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     {enter_new_authors_here}

*  History:
*     15-FEB-1993 (WG):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_ERR'          ! Parameter error constants

*  Arguments Given:
      CHARACTER*( * ) PTXT, PLON, PLAT
      CHARACTER*( * ) MODE
      INTEGER IRA
      CHARACTER*( * ) SCS
      REAL X1, X2, Y1, Y2
      INTEGER MXNTXT

*  Arguments Given and Returned:
      INTEGER NTXT
      DOUBLE PRECISION LON( MXNTXT ), LAT( MXNTXT )
      CHARACTER*( * ) TXT( MXNTXT )
      REAL DIRX( MXNTXT ), DIRY( MXNTXT )
      REAL HIGT( MXNTXT )
      REAL RTIO( MXNTXT )
      CHARACTER*( * ) JSTF( MXNTXT )
      REAL SPAC( MXNTXT )
      INTEGER FONT( MXNTXT ), PEN( MXNTXT )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      REAL HT, AR                ! Height and aspect ratio
      INTEGER I                  ! Do loop index
      REAL LBND( 2 ), UBND( 2 )  ! Bounds of the current SGS zone
      INTEGER NF, NPR, NPEN      ! Font, precision and pen number
      INTEGER PRNTXT             ! Number of previously written texts
      REAL SP                    ! Space between characters
      CHARACTER*( 2 ) TXJ        ! Text justification
      REAL XU, YU                ! Up dirction of text

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      PRNTXT = 0

*  Inquire the present attribute setting.
      CALL SGS_ITXA( NF, NPR, HT, AR, XU, YU, SP, TXJ )
      CALL SGS_IPEN( NPEN )

*  If working in keyboard mode, write text at the given sky position.
      IF ( MODE( : 8 ) .EQ. 'KEYBOARD' ) THEN
         PRNTXT = NTXT
         CALL SWRIB0( PLON, PLAT, IRA, SCS, PTXT, MXNTXT, LON, LAT, TXT,
     :                NTXT, STATUS )

*  If 'CURSOR' is selected, write text at the cursor position.
      ELSE IF ( MODE( : 6 ) .EQ. 'CURSOR' ) THEN

*  Set the cursor visible.
         CALL SGS_CUVIS( .TRUE. )

*  Write the texts at the cursor positions.
         LBND( 1 ) = X1
         UBND( 1 ) = X2
         LBND( 2 ) = Y1
         UBND( 2 ) = Y2
         PRNTXT = NTXT
         CALL SWRIB1( LBND, UBND, IRA, SCS, PTXT, MXNTXT, LON, LAT,
     :                TXT, NTXT, STATUS )
      END IF

*  Record the text attributes for the text written above.
      DO I = PRNTXT + 1, NTXT
         DIRX( I ) = XU
         DIRY( I ) = YU
         HIGT( I ) = HT / ( X2 - X1 )
         RTIO( I ) = AR
         JSTF( I ) = TXJ
         SPAC( I ) = SP
         FONT( I ) = NF
         PEN( I ) = NPEN
      END DO

*  See if GKS/SGS has reported an error.
      CALL GKS_GSTAT( STATUS )

      END
