      SUBROUTINE KPS1_WIECP( XMARG, YMARG, DIM1, DIM2, IN, NPIX, NLIN, 
     :                       OUT, MEAN, MEAN2, BAD, STATUS )
*+
*  Name:
*     KPS1_WIECP

*  Purpose:
*     Copies an input array to a larger output array, and get
*     information about the array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_WIECP( XMARG, YMARG, DIM1, DIM2, IN, NPIX, NLIN, OUT, 
*                      MEAN, MEAN2, BAD, STATUS )

*  Description:
*     The IN array is copied to the OUT array, leaving a margin of
*     XMARG pixels at the left hand edge, and YMARG pixels at the
*     bottom.  The margins are filled with the nearest edge values.
*     The mean data value, the mean squared data value, and a flag
*     indicating if any bad values are present, are returned.

*  Arguments:
*     XMARG = INTEGER (Given)
*        The width of the left-hand x-axis margin, in pixels.
*     YMARG = INTEGER (Given)
*        The width of the bottom y-axis margin, in pixels.
*     DIM1 = INTEGER (Given)
*        Number of pixels per line in the input data.
*     DIM2 = INTEGER (Given)
*        Number of pixels per line in the input data.
*     IN( DIM1, DIM2 ) = REAL (Given)
*        The input array.
*     NPIX = INTEGER (Given)
*        Number of pixels per line in the output data.
*     NLIN = INTEGER (Given)
*        Number of pixels per line in the output data.
*     OUT( NPIX, NLIN ) = REAL (Returned)
*        The output array.
*     MEAN = REAL (Returned)
*        The mean value in IN.  Returned equal to VAL__BADR if there
*        is no good data in the array.
*     MEAN2 = REAL (Returned)
*        The mean squared value in IN.  Returned equal to VAL__BADR if
*        there is no good data in the array.
*     BAD = LOGICAL (Returned)
*        Returned .TRUE. if any bad values are found in IN.  Returned
*        .FALSE. otherwise.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     16-MAR-1995 (DSB):
*        Original version.
*     1995 March 28 (MJC):
*        Shortened long lines and minor stylistic changes.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Data constants.

*  Arguments Given:
      INTEGER  XMARG
      INTEGER  YMARG
      INTEGER  DIM1
      INTEGER  DIM2
      REAL     IN( DIM1, DIM2 )
      INTEGER  NPIX
      INTEGER  NLIN
      
*  Arguments Returned:
      REAL     OUT( NPIX, NLIN )
      REAL     MEAN
      REAL     MEAN2
      LOGICAL  BAD

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER  LIN               ! Line counter
      INTEGER  NGOOD             ! Good pixel count
      INTEGER  PIX               ! Pixel counter
      REAL VAL                   ! Pixel value

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the returned bad-pixel flag.
      BAD = .FALSE.

*  Initialise the number of good values found so far, and the sum of
*  the good values, and the sum of the squared good values.
      NGOOD = 0
      MEAN = 0.0
      MEAN2 = 0.0

*  Copy the input array to the output array leaving the required margins.
      DO LIN  = 1, DIM2
         DO PIX = 1, DIM1
            VAL = IN( PIX, LIN )
            OUT( XMARG + PIX, YMARG + LIN ) = VAL

*  If this is a good value, increment the statistics.
            IF ( VAL .NE. VAL__BADR ) THEN
               MEAN = MEAN + VAL
               MEAN2 = MEAN2 + VAL**2
               NGOOD = NGOOD + 1

*  Otherwise, indicate that at least one bad pixel has been found.
            ELSE
               BAD = .TRUE.
            END IF
            
         END DO
      END DO

*  Replicate the edge pixels into the margins.
      DO LIN = YMARG + 1, YMARG + DIM2

         VAL = OUT( XMARG + 1, LIN )
         DO PIX = 1, XMARG
            OUT( PIX, LIN ) = VAL
         END DO

         VAL = OUT( XMARG + DIM1, LIN )
         DO PIX = XMARG + DIM1 + 1, NPIX
            OUT( PIX, LIN ) = VAL
         END DO

      END DO

      DO LIN = YMARG, 1, -1
         DO PIX = 1, NPIX
            OUT( PIX, LIN ) = OUT( PIX, LIN + 1 )
         END DO
      END DO

      DO LIN = YMARG + DIM2 + 1, NLIN
         DO PIX = 1, NPIX
            OUT( PIX, LIN ) = OUT( PIX, LIN - 1 )
         END DO
      END DO

*  Return the mean value and the mean squared value (excluding the
*  margins).
      IF ( NGOOD .GT. 0 ) THEN
         MEAN = MEAN / NGOOD
         MEAN2 = MEAN2 / NGOOD
      ELSE
         MEAN = VAL__BADR
         MEAN2 = VAL__BADR
      END IF

      END
