      SUBROUTINE AXLABEL( STATUS )
*+
*  Name:
*     AXLABEL

*  Purpose:
*     Sets a new label value for an axis within an NDF data structure.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL AXLABEL( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine sets a new value for a LABEL component of an
*     existing NDF AXIS data structure.  The NDF is accessed in update
*     mode and any pre-existing LABEL component is over-written with a
*     new value.  Alternatively, if a `null' value (!) is given for the
*     LABEL parameter, then the NDF's axis LABEL component will be
*     erased.  If an AXIS structure does not exist, a new one whose
*     centres are pixel co-ordinates is created.

*  Usage:
*     axlabel ndf label dim

*  ADAM Parameters:
*     DIM = _INTEGER (Read)
*        The axis dimension for which the label is to be modified.
*        There are separate labels for each NDF dimension.  The value
*        must lie between 1 and the number of dimensions of the NDF.
*        This defaults to 1 for a 1-dimensional NDF.  The suggested
*        default is the current value. []
*     NDF = NDF (Read and Write)
*        The NDF data structure in which an axis LABEL component is to
*        be modified.
*     LABEL = LITERAL (Read)
*        The value to be assigned to the NDF's axis LABEL component
*        (e.g. "Wavelength" or "Fibre index").  LABEL describes the
*        quantity measured along the axis.  This value may later be
*        used by other applications for labelling graphs or as a
*        heading for columns in tabulated output.  The suggested
*        default is the current value.

*  Examples:
*     axlabel ngc253 "Offset from nucleus" 2
*        Sets the LABEL component of the second axis dimension of the
*        NDF structure ngc253 to have the value "Offset from nucleus".
*     axlabel ndf=spect label=Wavelength
*        Sets the axis LABEL component of the 1-dimensional NDF
*        structure spect to have the value "Wavelength".
*     axlabel datafile label=! dim=3
*        By specifying a null value (!), this example erases any
*        previous value of the LABEL component for the third dimension
*        in the NDF structure datafile.

*  Related Applications:
*     KAPPA: AXUNITS, SETAXIS, SETLABEL.

*  Copyright:
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1995 April 21 (MJC):
*        Original version.
*     {enter_further_changes_here}

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
      CHARACTER * ( 132 ) LABEL  ! Axis label
      INTEGER NDF                ! NDF identifier
      INTEGER NDIM               ! Number of dimensions of NDF

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain an identifier for the NDF to be modified.
      CALL LPG_ASSOC( 'NDF', 'UPDATE', NDF, STATUS )

*  Find the number of dimensions in the NDF.
      CALL NDF_DIM( NDF, NDF__MXDIM, DIMS, NDIM, STATUS )

*  Find which axis to modify only if there is more than one.
      IAXIS = 1
      IF ( NDIM .GT. 1 ) CALL PAR_GDR0I( 'DIM', 1, 1, NDIM, .FALSE.,
     :                   IAXIS, STATUS )

*  Reset any existing LABEL component.
      CALL NDF_AREST( NDF, 'label', IAXIS, STATUS )

*  Obtain a new value for the LABEL component.  A null value indicates
*  that the LABEL component is to be deleted.  So use a new error
*  context and annul a null status.
      CALL ERR_MARK
      CALL PAR_GET0C( 'LABEL', LABEL, STATUS )
      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )

*  Place the new value in the AXIS structure.  This has an implied call
*  to NDF_ACRE if the axis structure does not exist.
      ELSE
         CALL NDF_ACPUT( LABEL, NDF, 'Label', IAXIS, STATUS )
      END IF

*  Annul the NDF identifier.
      CALL NDF_ANNUL( NDF, STATUS )

*  Write the closing error message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'AXLABEL_ERR',
     :     'AXLABEL: Error modifying the label of an NDF AXIS.',
     :     STATUS )
      END IF

      END
