      SUBROUTINE CCD1_DRAWA( PLOT, IDS, POS, NMARK, NVAL, MTYPE, 
     :                       STATUS )
*+
*  Name:
*     CCD1_DRAWA

*  Purpose:
*     Draws markers at the given positions on an AST Plot.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_DRAWA( PLOT, IDS, POS, NMARK, NVAL, MTYPE, STATUS )

*  Description:
*     This routine draws markers of the type decided by MTYPE at the
*     positions determined by the X and Y arrays. If MTYPE is less than
*     zero then the values in ID are plotted at the positions instead
*     of a `normal' marker.

*  Arguments:
*     PLOT = INTEGER (Given)
*        The AST identifier for the Plot object into which plots are
*        to be made.
*     IDS( NMARK ) = INTEGER (Given)
*        The identifiers of the X and Y data. These values are plotted
*        at the X and Y positions if the marker type is less than zero.
*        If MTYPE is greater than zero this array is not used.
*     POS( NMARK, NVAL ) = DOUBLE PRECISION (Given)
*        The positions at which the markers are to be plotted.  For 
*        point number IMARK, the X coordinate is POS( IMARK, 1 ) and
*        the Y coordinate is POS( IM, 2 ).
*     NMARK = INTEGER (Given)
*        Number of markers to be plotted.
*     NVAL = INTEGER (Given)
*        The second dimension of the POS array.  Must be at least 2.
*     MTYPE = INTEGER (Given)
*        The type of the markers to be drawn. This corresponds to the
*        PGPLOT marker number.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     9-JAN-2001 (MBT):
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

*  Arguments Given:
      INTEGER PLOT
      INTEGER NMARK
      INTEGER NVAL
      INTEGER IDS( NMARK )
      DOUBLE PRECISION POS( NMARK, NVAL )
      INTEGER MTYPE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( VAL__SZI ) IDENT ! Identifier as character
      INTEGER I                  ! Loop variable
      INTEGER NCHAR              ! Number of characters in IDENT
      REAL UP( 2 )               ! Normal vector for text
      DOUBLE PRECISION PT( 2 )   ! Position of current marker

*  Local Data:
      DATA UP / 0.0, 1.0 /

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Draw all the markers.
      CALL AST_MARK( PLOT, NMARK, 2, NMARK, POS, ABS( MTYPE ), STATUS )

*  Draw numeric labels if required.
      IF ( MTYPE .LT. 0 ) THEN
         DO I = 1, NMARK
            CALL CHR_ITOC( IDS( I ), IDENT, NCHAR )
            PT( 1 ) = POS( I, 1 )
            PT( 2 ) = POS( I, 2 )
            CALL AST_TEXT( PLOT, IDENT( 1:NCHAR ), PT, UP, 'CL',
     :                     STATUS )
         END DO
      END IF

      END
* $Id$
