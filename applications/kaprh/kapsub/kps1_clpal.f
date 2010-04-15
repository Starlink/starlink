      SUBROUTINE KPS1_CLPAL( DIM1, DIM2, PALETT, STATUS )
*+
*  Name:
*     KPS1_CLPAL

*  Purpose:
*     Initialises the standard CRELUT palette.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_CLPAL( DIM1, DIM2, PALETT, STATUS )

*  Description:
*     This routine assigns the default, standard palette for the CRELUT
*     application.  The palette comprises these colours in the following
*     order: black, white, red, green, blue, yellow, magenta, cyan;
*     a (DIM1-8)-level greyscale, followed by a DIM1-level series of
*     greyscales.

*  Arguments:
*     DIM1 = INTEGER (Given)
*        The number of palette entries in a line of the displayed
*        palette.  It specifies the length of the greyscale sections,
*        and must be at least 10.
*     DIM2 = INTEGER (Given)
*        The number of lines in the displayed palette.  DIM2-1
*        specifies the number of long greyscale sections, and must be
*        at least 1.
*     PALETT( 3, 0: DIM1 * DIM2 - 1 ) = REAL (Returned)
*        The palette array to hold the RGB values of the palette
*        colours.
*     STATUS = INTEGER (Returned)
*        The global status.

*  Notes:
*     The input dimensions are not validated.

*  [optional_subroutine_items]...
*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 June 27 (MJC):
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
      INTEGER
     :  DIM1,
     :  DIM2

*  Arguments Returned:
      REAL PALETT( 3, 0 : DIM1 * DIM2 - 1 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER
     :  I, J, K,                 ! Loop counters
     :  NCPENS                   ! Number of coloured pens

      REAL
     :  NORM                     ! Normalisation factor for initial
                                 ! palette greyscales

*.

*    Check the inherited global status.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Create the pre-defined palette colours.
*    =======================================

*    The first line of the palette is fixed.  It comprises primary and
*    secondary colours and a mini-greyscale.  The second and subsequent
*    lines of the palette are greyscales.

*    Insert into the basic grey-scale table, the colours which
*    are predefined.

*    Black

      PALETT( 1, 0 ) = 0.0
      PALETT( 2, 0 ) = 0.0
      PALETT( 3, 0 ) = 0.0

*    White

      PALETT( 1, 1 ) = 1.0
      PALETT( 2, 1 ) = 1.0
      PALETT( 3, 1 ) = 1.0

*    Red

      PALETT( 1, 2 ) = 1.0
      PALETT( 2, 2 ) = 0.0
      PALETT( 3, 2 ) = 0.0

*    Green

      PALETT( 1, 3 ) = 0.0
      PALETT( 2, 3 ) = 1.0
      PALETT( 3, 3 ) = 0.0

*    Blue

      PALETT( 1, 4 ) = 0.0
      PALETT( 2, 4 ) = 0.0
      PALETT( 3, 4 ) = 1.0

*    Yellow

      PALETT( 1, 5 ) = 1.0
      PALETT( 2, 5 ) = 1.0
      PALETT( 3, 5 ) = 0.0

*    Magenta

      PALETT( 1, 6 ) = 1.0
      PALETT( 2, 6 ) = 0.0
      PALETT( 3, 6 ) = 1.0

*    Cyan

      PALETT( 1, 7 ) = 0.0
      PALETT( 2, 7 ) = 1.0
      PALETT( 3, 7 ) = 1.0

*    Number of special pens so far.

      NCPENS = 8

*    Set up a fixed greyscale to complete the first line of the palette.
*    Allow for the pens already used.

      NORM = REAL( DIM1 - NCPENS - 1 )
      DO  I = NCPENS, DIM1 - 1, 1
         DO  J = 1, 3
             PALETT( J, I ) = REAL( I - NCPENS ) / NORM
         END DO
      END DO

*    Set up a greyscale in the second and subsequent lines which may be
*    overwritten by user-specified colours.  Allow for the pens already
*    used.

      IF ( DIM2 .GT. 1 ) THEN
         NORM = REAL( DIM1 - 1 )
         DO  K = 2, DIM2
            NCPENS = ( K - 1 ) * DIM1
            DO  I = NCPENS, K * DIM1 - 1
               DO  J = 1, 3
                  PALETT( J, I ) = REAL( I - NCPENS ) / NORM
               END DO
            END DO
         END DO
      END IF

      END
