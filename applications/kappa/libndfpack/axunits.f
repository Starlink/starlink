      SUBROUTINE AXUNITS( STATUS )
*+
*  Name:
*     AXUNITS

*  Purpose:
*     Sets a new units value for an axis within an NDF data structure.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL AXUNITS( STATUS )

*  Description:
*     This routine sets a new value for a units component of an
*     existing NDF AXIS data structure.  The NDF is accessed in update
*     mode and any pre-existing units component is over-written with a
*     new value.  Alternatively, if a `null' value (!) is given for the
*     UNITS parameter, then the NDF's axis units component will be
*     erased.  If an AXIS structure does not exist, a new one whose
*     centres are pixel co-ordinates is created.

*  Usage:
*     axunits ndf units dim 

*  ADAM Parameters:
*     DIM = _INTEGER (Read)
*        The axis dimension for which the units is to be modified.
*        There are separate units for each NDF dimension.  The value
*        must lie between 1 and the number of dimensions of the NDF.
*        This defaults to 1 for a 1-dimensional NDF.  The suggested
*        default is the current value. []
*     NDF = NDF (Read and Write)
*        The NDF data structure in which an axis units component is to
*        be modified.
*     UNITS = LITERAL (Read)
*        The value to be assigned to the NDF's axis units component
*        (e.g. "Pixels" or "km/s").  UNITS describes the physical units
*        of the quantity measured along the axis.  This value may later
*        be used by other applications for labelling graphs and other
*        forms of display where the NDF's axis co-ordinates are shown.
*        The suggested default is the current value.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Examples:
*     axunits ngc253 "arcsec" 2
*        Sets the units component of the second axis dimension of the
*        NDF structure ngc253 to have the value "arcsec".
*     axunits ndf=spect units=Angstrom
*        Sets the axis units component of the 1-dimensional NDF
*        structure spect to have the value "Angstrom".
*     axunits datafile units=! dim=3
*        By specifying a null value (!), this example erases any
*        previous value of the units component for the third dimension
*        in the NDF structure datafile.

*  Related Applications:
*     KAPPA: AXLABEL, SETAXIS, SETUNITS.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1995 April 21 (MJC):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF__ constants
      INCLUDE 'PAR_ERR'          ! PAR__ error constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER DIMS( NDF__MXDIM ) ! Dimensions of the NDF
      INTEGER IAXIS              ! Dimension to modify
      INTEGER NDF                ! NDF identifier
      INTEGER NDIM               ! Number of dimensions of NDF
      CHARACTER * ( 132 ) UNITS  ! Axis units

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain an identifier for the NDF to be modified.
      CALL NDG_ASSOCL( 'NDF', 'UPDATE', NDF, STATUS )

*  Find the number of dimensions in the NDF.
      CALL NDF_DIM( NDF, NDF__MXDIM, DIMS, NDIM, STATUS )

*  Find which axis to modify only if there is more than one.
      IAXIS = 1
      IF ( NDIM .GT. 1 ) CALL PAR_GDR0I( 'DIM', 1, 1, NDIM, .FALSE.,
     :                   IAXIS, STATUS )

*  Reset any existing units component.
      CALL NDF_AREST( NDF, 'Units', IAXIS, STATUS )

*  Obtain a new value for the units component.  A null value indicates
*  that the units component is to be deleted.  So use a new error
*  context and annul a null status.
      CALL ERR_MARK
      CALL PAR_GET0C( 'UNITS', UNITS, STATUS )
      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )

*  Place the new value in the AXIS structure.  This has an implied call
*  to NDF_ACRE if the axis structure does not exist.
      ELSE
         CALL NDF_ACPUT( UNITS, NDF, 'Units', IAXIS, STATUS )
      END IF

*  Annul the NDF identifier.
      CALL NDF_ANNUL( NDF, STATUS )

*  Write the closing error message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'AXUNITS_ERR',
     :     'AXUNITS: Error modifying the units of an NDF AXIS.',
     :     STATUS )
      END IF

      END
