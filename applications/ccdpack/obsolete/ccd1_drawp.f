      SUBROUTINE CCD1_DRAWP( X, Y, N, IMARK, IDS, STATUS )
*+
*  Name:
*     CCD1_DRAWP

*  Purpose:
*     Draws PGPLOT markers at the given positions

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_DRAWP( X, Y, N, IMARK, IDS, STATUS )

*  Description:
*     This routine draws markers of the type decided by IMARK at the
*     positions determined by the X and Y arrays. If IMARK is less than
*     zero then the values in ID are plotted at the positions instead
*     of a `normal' marker. The X and Y positions are given as double
*     precision values (the CCDPACK standard for position lists) which
*     require conversion before plotting by PGPLOT.

*  Arguments:
*     X( N ) = DOUBLE PRECISION (Given)
*        The X positions of the markers.
*     Y( N ) = DOUBLE PRECISION (Given)
*        The Y positions of the markers.
*     N = INTEGER (Given)
*        Number of values in X and Y arrays.
*     IMARK = INTEGER (Given)
*        The type of the markers to be drawn. This corresponds to the
*        PGPLOT marker number.
*     IDS( N ) = INTEGER (Given)
*        The identifiers of the X and Y data. These values are plotted
*        at the X and Y positions if the marker type is less than zero.
*        If IMARK is greater than zero this array is not used.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     24-SEP-1992 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT constants

*  External References:
      EXTERNAL VAL_DTOR
      REAL VAL_DTOR              ! Converts DBLE to REAL with protection

*  Arguments Given:
      INTEGER N
      DOUBLE PRECISION X( N )
      DOUBLE PRECISION Y( N )
      INTEGER IMARK
      INTEGER IDS( N )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( VAL__SZI ) IDENT ! Identifier as character
      INTEGER I                  ! Loop variable
      INTEGER NCHAR              ! Number of characters in IDENT
      REAL XR                    ! X value converted to REAL
      REAL YR                    ! Y value converted to REAL

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set an ERROR context.
      CALL ERR_MARK

*  Draw the markers converting double precision positions to
*  single precision.
      DO 1 I = 1, N
         XR = VAL_DTOR( .FALSE., X( I ), STATUS )
         YR = VAL_DTOR( .FALSE., Y( I ), STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN

*  Couldn't convert the value to real.
            CALL ERR_REP( 'DBLE2REALERR',
     :      'Failed to convert the value ^THSVAL to single'//
     :      ' precision; position not plotted', STATUS )
            CALL ERR_FLUSH( STATUS )
         ELSE

*  Plot the position.
            IF ( IMARK .LT. 0 ) THEN 

*  Write the position identifier
               CALL CHR_ITOC( IDS( I ), IDENT, NCHAR )
               CALL PGPTEXT( XR, YR, 0.0, 0.5, IDENT( 1: NCHAR ) )
            ELSE
               CALL PGPOINT( 1, XR, YR, IMARK )
            END IF
         END IF      
 1    CONTINUE

*  End the ERROR context.
      CALL ERR_RLSE

      END
* $Id$
