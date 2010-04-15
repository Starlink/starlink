      SUBROUTINE SPD_PDAA( IMPTR, DIM1, DIM2, LBND1, LBND2, STATUS )
*+
*  Name:
*     SPD_PDAA

*  Purpose:
*     Register finder image.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_PDAA( IMPTR, DIM1, DIM2, LBND1, LBND2, STATUS )

*  Description:
*     This routine registers the given array, its bounds etc. as the
*     finder image. This includes intialisation of the controls for the
*     interactive PGPLOT graphics. A finder image is an image to be used
*     in interactive display.
*
*     This routine only registers the array, it does not allocate the
*     array. The array must have been mapped by the calling routine.
*
*     The finder image should not contain bad values, as these cause
*     extremely low contrast, they may also cause a floating point
*     exception when contrast is enhanced to cover an image value range
*     smaller than 1.
*
*     It is recommended that bad values be given a value somewhat lower
*     than the minimum value. This ensures they are always displayed
*     at least as dark as the minimum value.

*  Arguments:
*     IMPTR = INTEGER (Given)
*        The pointer to the array that is the finder image. The array
*        pointed to must be of type REAL and with dimensions DIM1 and
*        DIM2.
*     DIM1 = INTEGER (Given)
*        The first dimension of the finder image.
*     DIM2 = INTEGER (Given)
*        The second dimension of the finder image.
*     LBND1 = INTEGER (Given)
*        The lower NDF bound along the first image axis. If in doubt,
*        give 1.
*     LBND2 = INTEGER (Given)
*        The lower NDF bound along the second image axis. If in doubt,
*        give 1.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     27 Apr 1994 (hme):
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

*  Arguments Given:
      INTEGER IMPTR
      INTEGER DIM1
      INTEGER DIM2
      INTEGER LBND1
      INTEGER LBND2

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  De-register any existing image.
      IMXST = .FALSE.

*  Store image pointer.
      IMAGE = IMPTR

*  Store image shape.
      IMDIM(1) = DIM1
      IMDIM(2) = DIM2

*  Store world coordinates and transform matrix.
      IMWIN(1) = FLOAT( LBND1 )
      IMWIN(2) = FLOAT( LBND1 + DIM1 - 1 )
      IMWIN(3) = FLOAT( LBND2 )
      IMWIN(4) = FLOAT( LBND2 + DIM2 - 1 )
      IMZOOM(1) = FLOAT( LBND1 - 1 )
      IMZOOM(2) = 1.0
      IMZOOM(3) = 0.0
      IMZOOM(4) = FLOAT( LBND2 - 1 )
      IMZOOM(5) = 0.0
      IMZOOM(6) = 1.0

*  Work out the image data range.
      CALL SPD_PEAAR( .FALSE., DIM1*DIM2, %VAL( CNF_PVAL(IMPTR) ),
     :                IMRNG(1), IMRNG(2), STATUS )

*  Set flag.
      IF ( STATUS .EQ. SAI__OK ) IMXST = .TRUE.

*  Return.
      END
