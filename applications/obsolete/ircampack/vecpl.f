      SUBROUTINE VECPL( LXBND1, UXBND1, LYBND1, UYBND1, VECMAG, LXBND2,
     :                  UXBND2, LYBND2, UYBND2, VECORN, ANGFAC, ANGROT,
     :                  STEP, VSCALE, AHSIZE, JUST, STATUS )
*+
*  Name:
*     VECPL

*  Purpose:
*     Plot a vector map

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL VECPL( LXBND1, UXBND1, LYBND1, UYBND1, VECMAG, LXBND2,
*                 UXBND2, LYBND2, UYBND2, VECORN, ANGFAC, ANGROT,
*                 STEP, VSCALE, AHSIZE, JUST, STATUS )

*  Description:
*     Vectors are plotted within the region of overlap of the two input
*     NDFs and the current SGS zone, at increments of STEP pixels along
*     both axes. The number of plotted vectors is reported to the user.

*  Arguments:
*     LXBND1 = INTEGER (Given)
*        The lower bound of pixel indices on the X axis of VECMAG.
*     UXBND1 = INTEGER (Given)
*        The upper bound of pixel indices on the X axis of VECMAG.
*     LYBND1 = INTEGER (Given)
*        The lower bound of pixel indices on the Y axis of VECMAG.
*     UYBND1 = INTEGER (Given)
*        The upper bound of pixel indices on the Y axis of VECMAG.
*     VECMAG( LXBND1:UXBND1, LYBND1:UYBND1 ) = REAL (Given)
*        The data values defining the vector magnitudes.
*     LXBND2 = INTEGER (Given)
*        The lower bound of pixel indices on the X axis of VECORN.
*     UXBND2 = INTEGER (Given)
*        The upper bound of pixel indices on the X axis of VECORN.
*     LYBND2 = INTEGER (Given)
*        The lower bound of pixel indices on the Y axis of VECORN.
*     UYBND2 = INTEGER (Given)
*        The upper bound of pixel indices on the Y axis of VECORN.
*     VECORN( LXBND2:UXBND2, LYBND2:UYBND2 ) = REAL (Given)
*        The data values defining the vector orientations. Positive
*        values correspond to rotation in the same sense as from the X
*        axis to the Y axis. Zero corresponds to the Y axis. The units
*        are defined by argument ANGFAC.
*     ANGFAC = REAL (gIVEN)
*        The factor which converts values from VECORN into units of
*        radians.
*     ANGROT = REAL (Given)
*        A value (in radians) to be added on to the vector orientation
*        angles given by VECORN.
*     STEP = INTEGER (Given)
*        The increment (in pixels) between plotted vectors. The same
*        value is used for both axes.
*     VSCALE = REAL (Given)
*        A factor which converts data values in VECMAG into
*        corresponding vector lengths in millimetres.
*     AHSIZE = REAL (Given)
*        The length of each stroke of the arrow head placed at the end
*        of the vector, in pixels. A value of zero causes no arrow head
*        to be drawn and the vectors to be centred on the corresponding
*        pixel centres. Otherwise, the vectors start at the centre of
*        the corresponding pixel.
*     JUST = CHARACTER * ( * ) (Given)
*        CENTRE, START or END. CENTER causes vectors to be drawn
*        centred on the corresponding pixel. START causes vectors to be
*        drawn starting at the corresponding pixel. END causes vectors
*        to be drawn ending at the corresponding pixel.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  It is assumed that the current SGS zone has a uniform
*     world coordinate system corresponding to pixels coordinates.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     12-AUG-1993 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL_ constants

*  Arguments Given:
      INTEGER LXBND1
      INTEGER UXBND1
      INTEGER LYBND1
      INTEGER UYBND1
      REAL VECMAG( LXBND1:UXBND1, LYBND1:UYBND1 )
      INTEGER LXBND2
      INTEGER UXBND2
      INTEGER LYBND2
      INTEGER UYBND2
      REAL VECORN( LXBND2:UXBND2, LYBND2:UYBND2 )
      REAL ANGFAC
      REAL ANGROT
      INTEGER STEP
      REAL VSCALE
      REAL AHSIZE
      CHARACTER JUST*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER
     :          I,               ! X pixel index
     :          J,               ! Y pixel index
     :          LBNDX,           ! Low X pixel index bound of plot
     :          LBNDY,           ! Low Y pixel index bound of plot
     :          LX1,             ! Low X pixel index bound of SGS zone
     :          LX2,             ! High X pixel index bound of SGS zone
     :          LY1,             ! Low Y pixel index bound of SGS zone
     :          LY2,             ! High Y pixel index bound of SGS zone
     :          NPLOT,           ! No. of vectors plotted
     :          UBNDX,           ! Upper X pixel index bound of plot
     :          UBNDY            ! Upper Y pixel index bound of plot

      REAL
     :          SCALE,           ! vector scale (pixels per data unit)
     :          VECANG,          ! Vector position angle in radians
     :          VECLEN,          ! Vector length in pixels
     :          X,               ! X pixel coordinate
     :          X1,              ! Lower X bound of SGS zone
     :          X2,              ! Upper X bound of SGS zone
     :          XM,              ! X extent of SGS zone, in metres
     :          Y,               ! Y pixel coordinate
     :          Y1,              ! Lower Y bound of SGS zone
     :          Y2,              ! Upper Y bound of SGS zone
     :          YM               ! Y extent of SGS zone, in metres



*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the extent of the currrent SGS zone in terms of world coordinates
*  (pixels) and metres.
      CALL SGS_IZONE( X1, X2, Y1, Y2, XM, YM )

*  Get the corresponding pixel index bounds.
      LX1 = NINT( X1 + 0.5 )
      LX2 = NINT( X2 + 0.5 )
      LY1 = NINT( Y1 + 0.5 )
      LY2 = NINT( Y2 + 0.5 )

*  Calculate the bounds of the region of overlap between the two
*  images, and the current SGS zone.
      LBNDX = MAX( MAX( LXBND1, LXBND2 ), LX1 )
      LBNDY = MAX( MAX( LYBND1, LYBND2 ), LY1 )
      UBNDX = MIN( MIN( UXBND1, UXBND2 ), LX2 )
      UBNDY = MIN( MIN( UYBND1, UYBND2 ), LY2 )

*  Check that there is some overlap. Return without action otherwise.
      IF( LBNDX .LE. UBNDX .AND. LBNDY .LE. UBNDY ) THEN

*  Calculate the scaling factor which converts data values into vector
*  lengths in units of pixels. It is assumed that the zone has a
*  uniform coordinate system, and so this value should be the same for
*  both axes.
         SCALE = ( X2 - X1 )/( VSCALE*100.0*XM )

*  Initialise the count of plotted vectors.
         NPLOT = 0

*  Loop round the overlap region, plotting a vector at increments given
*  by argument STEP. The bottom left vector is placed in the middle of
*  the bottom left increment box.
         DO J = LBNDY + STEP/2, UBNDY, STEP
            Y = REAL( J ) - 0.5

            DO I = LBNDX + STEP/2, UBNDX, STEP
               X = REAL( I ) - 0.5

*  Skip over bad data values.
               IF( VECMAG( I, J ) .NE. VAL__BADR .AND.
     :             VECORN( I, J ) .NE. VAL__BADR ) THEN

*  Calculate the length of the vector in units of pixels.
                  VECLEN = VECMAG( I, J )*SCALE

*  Calculate the vector orientation, in radians.
                  VECANG = ANGFAC*VECORN( I, J ) + ANGROT

*  Plot the vector.
                  CALL VECT( X, Y, JUST, VECLEN, VECANG, AHSIZE,
     :                       STATUS )

*  Abort if an error has occurred.
                  IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Increment the count of plotted vectors.
                  NPLOT = NPLOT + 1

               END IF

            END DO

         END DO

*  If no vectors have been plotted, warn the user.
         CALL MSG_BLANK( STATUS )

         IF( NPLOT .EQ. 0 ) THEN
            CALL MSG_OUT('VECPL_NOPLT', '  No vectors plotted due to'//
     :                   'lack of good data in input NDFs', STATUS )

*  Otherwise, tell the user how many vectors were plotted.
         ELSE
            CALL MSG_SETI( 'NP', NPLOT )
            CALL MSG_OUT('VECPL_NPLT', '  ^NP vectors plotted', STATUS )
         END IF

         CALL MSG_BLANK( STATUS )

      ENDIF

*  Arrive here if an error occurs.
 999  CONTINUE

      END
