      SUBROUTINE IRA_DROPT( ITEM, VALUE, STATUS )
*+
*  Name:
*     IRA_DROPT

*  Purpose:
*     Set an option for the IRA graphics routines.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA_DROPT( ITEM, VALUE, STATUS )

*  Description:
*     This routine allows several aspects of the graphics produced by
*     the routines IRA_DRxxx (eg IRA_DRGRD, etc) to be controlled by
*     setting new values for various "options". These option take
*     default values (described in the "Notes:" section below unless a
*     new value is assigned to them using this routine.

*  Arguments:
*     ITEM = CHARACTER * ( * ) Given)
*        The name of the option (see the "Notes:" section below). An
*        unambiguous abbreviation may be supplied. Case is ignored.
*     VALUE = DOUBLE PRECISION (Given)
*        The new value for the option.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     The argument ITEM may take any of the following values (default
*     values are shown in square brackets at the end of each
*     description):
*
*     -  'TEXT_SIZE'  the height of axis titles, expressed as a
*     fraction of the maximum dimension of the area containing the
*     plot. A negative value suppresses text labels. [0.0125]
*     -  'COORD_SIZE'  the height of formatted coordinate values,
*     expressed as a fraction of the maximum dimension of the SGS zone
*     selected on entry to the plotting routine. A negative value
*     supresses coordinate labels. [0.0125]
*     -  'TOLERANCE'  a measure of the accuracy required when drawing
*     curves. The value should be between 0.0 (for maximum accuracy)
*     and 10.0 (for minimum accuracy). Greater accuracy is bought at the
*     cost of much greater processing time. The nearest integer value
*     is used. [6.0]
*     -  'LINES'  if positive, then complete curves will be drawn. If
*     negative, no curves will be drawn and tick marks may be drawn
*     instead. [+1.0]
*     -  'LONG_GAP'  the gap in longitude between meridians, in radians.
*     A negative or zero value causes an internally calculated value to
*     be used. [-1.0]
*     -  'LAT_GAP'  the gap in latitude between meridians, in radians.
*     A negative or zero value causes an internally calculated value to
*     be used. [-1.0]
*     -  'LONG_ACC'  specifies the accuracy to which a longitude value
*     should be displayed, in radians.  The displayed value is such that
*     a change of 1 in the least significant field corresponds to the
*     largest value which is smaller than (or equal to) the supplied
*     value. For instance, if a seconds field is not required in the
*     displayed text, then LONG_ACC could be given the radian equivalent
*     of 1 minute. A negative or zero value causes an internally
*     calculated value to be used. [-1.0]
*     -  'LAT_ACC'  specifies the accuracy to which a latitude value
*     should be displayed, in radians. See LONG_ACC. [-1.0]
*     -  'PEN1'  the SGS pen to use when drawing the boundary (see
*     IRA_DRBND). The nearest integer value is used. [1.0]
*     -  'PEN2'  the SGS pen to use when drawing curves or ticks. The
*     nearest integer value is used. [1.0]
*     -  'PEN3'  the SGS pen to use when drawing text labels. The
*     nearest integer value is used. [1.0]
*     -  'PEN4'  the SGS pen to use when drawing coordinate labels. The
*     nearest integer value is used. [1.0]

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     9-NOV-1992 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants.
      INCLUDE 'IRA_PAR'          ! IRA constants.

*  Global Variables:
      INCLUDE 'IRA_COM'          ! IRA common blocks.
*        ACM_DROPT( IRA__NOPT ) = DOUBLE PRECISION (Write)
*           The graphics options values.

*  Arguments Given:
      CHARACTER ITEM*(*)
      DOUBLE PRECISION VALUE

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL IRA1_INIT         ! IRA common block initialisation.

*  Local Variables:
      INTEGER INDX               ! Index into the common options array.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the index at which the option is stored in the common array
*  ACM_DROPT.
      CALL IRA1_OPTID( ITEM, INDX, STATUS )

*  If all was OK, store the supplied value.
      IF( STATUS .EQ. SAI__OK ) THEN
         ACM_DROPT( INDX ) = VALUE

*  Otherwise, add a context message.
      ELSE
         CALL ERR_REP( 'IRA_DROPT_ERR1',
     : 'IRA_DROPT: Unable to assign a new value to an astrometric '//
     : 'graphics option', STATUS )
      END IF

      END
