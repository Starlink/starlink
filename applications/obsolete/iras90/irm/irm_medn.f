      SUBROUTINE IRM_MEDN( EL, DATA, MEDIAN, NGOOD, STATUS )
*+
*  Name:
*     IRM_MEDN

*  Purpose:
*     To find the median value of a data set.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRM_MEDN( EL, DATA, MEDIAN, STATUS )

*  Description:
*     This routine finds the median value of the supplied data array,
*     using the facilities of the routine IRM_QNTLR. A median value of
*     VAL__BADR is returned if all the input data is bad, but no error
*     is reported. The number of good data values is also returned.

*  Arguments:
*     EL = INTEGER (Given)
*        The number of elements in the data array.
*     DATA( EL ) = REAL (Given)
*        The data array.
*     MEDIAN = REAL (Returned)
*        The median value. Returned equal to VAD__BADR if an error
*        occurs.
*     NGOOD = INTEGER (Returned)
*        The number of good data values in DATA.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     29-SEP-1992 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Starlink data constants

*  Arguments Given:
      INTEGER EL
      REAL DATA( EL )

*  Arguments Returned:
      REAL MEDIAN
      INTEGER NGOOD

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IPIP               ! Pointer to array of pixel indices.
      REAL    W                  ! Dummy weight array.
*.

*  Initialise the returned median value to be VAL__BADR
      MEDIAN = VAL__BADR

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get work space to hold the pointers to the elements of the data array
*  which are to be included in the calculation.
      CALL PSX_CALLOC( EL, '_INTEGER', IPIP, STATUS )

*  Set up the array of pixel indices to be included in the median
*  estimation. Any bad pixels are omitted.
      CALL IRM1_SETIP( EL, DATA, %VAL( IPIP ), NGOOD, STATUS )

*  If there are some good data values, call IRM_QNTLR to do the work.
*  Uniform weighting is used, and the resulting value is produced by linear
*  interpolation.
      IF( NGOOD .NE. 0 ) THEN
         CALL IRM_QNTLR( .FALSE., .TRUE., 0.5, NGOOD, DATA, W,
     :                   %VAL( IPIP ), MEDIAN, STATUS )
      END IF

*  Release the work space used to hold the pixel pointers.
      CALL PSX_FREE( IPIP, STATUS )

*  If an error has occurred, give a context message and return a bad
*  median value.
      IF( STATUS .NE. SAI__OK ) THEN
         MEDIAN = VAL__BADR
         CALL ERR_REP( 'IRM_MEDN_ERR2',
     :   'IRM_MEDN: Unable to estimate the median value of an array' ,
     :    STATUS )
      END IF

      END
