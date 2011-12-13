      SUBROUTINE NDF_ACLEN( INDF, COMP, IAXIS, LENGTH, STATUS )
*+
*  Name:
*     NDF_ACLEN

*  Purpose:
*     Determine the length of an NDF axis character component.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_ACLEN( INDF, COMP, IAXIS, LENGTH, STATUS )

*  Description:
*     The routine returns the length of the specified axis character
*     component of an NDF (i.e. the number of characters in the LABEL
*     or UNITS component of an NDF axis).

*  Arguments:
*     INDF = INTEGER (Given)
*        NDF identifier.
*     COMP = CHARACTER * ( * ) (Given)
*        Name of the axis character component whose length is required:
*        'LABEL' or 'UNITS'.
*     IAXIS = INTEGER (Given)
*        Number of the NDF axis.
*     LENGTH = INTEGER (Returned)
*        The component's length in characters.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The length of an NDF axis character component is normally
*     determined by the length of the VALUE string assigned to it by a
*     previous call to NDF_ACPUT (note that this could include trailing
*     blanks).
*     -  If the requested axis component is in an undefined state, then
*     the length returned will be the number of characters in the
*     default value which would be returned by the NDF_ACGET routine.
*     -  A value of zero may be supplied for the IAXIS argument, in
*     which case the routine will return the maximum component length
*     for all the NDF axes.

*  Algorithm:
*     -  Import the NDF identifier.
*     -  Validate the axis character component name.
*     -  Validate the axis number.
*     -  Obtain an index to the data object entry in the DCB.
*     -  Obtain the number of dimensions of the actual data object from
*     the ARY_ system identifier for its data array.
*     -  Loop to process each axis required.
*     -  See if the current axis exists within the actual data object.
*     If so, then ensure that axis character component information is
*     available for this axis.
*     -  Note if the required component exists for this axis.
*     -  If the component does not exist, then generate the length of
*     the default component value (for the label component, this
*     requires calculation of the number of decimal digits needed to
*     store the axis number).
*     -  If the required component exists, then obtain its length.
*     -  Quit considering axes if an error occurs.
*     -  Find the maximum length for all the axes considered.
*     -  If an error occurred, then report context information.

*  Copyright:
*     Copyright (C) 1990, 1991 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     4-JUL-1990 (RFWS):
*        Original version.
*     5-JUL-1990 (RFWS):
*        Fixed bug caused by incorrect use of IAX1 instead of IAX.
*     14-OCT-1991 (RFWS):
*        Minor improvements to routine prologue.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants

*  Global Variables:
      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_ACLOC( NDC__MXDIM, NDF__MXACN, NDF__MXDCB ) = CHARACTER * (
*        DAT__SZLOC ) (Read)
*           Locators to axis character components.
*        DCB_DID( NDF__MXDCB ) = INTEGER (Read)
*           ARY_ system identifier for the NDF's data array.

      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_IDCB( NDF__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.

*  Arguments Given:
      INTEGER INDF
      CHARACTER * ( * ) COMP
      INTEGER IAXIS

*  Arguments Returned:
      INTEGER LENGTH

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IACB               ! Index to the NDF entry in the ACB
      INTEGER IAX                ! Loop counter for axes
      INTEGER IAX1               ! First axis to process
      INTEGER IAX2               ! Last axis to process
      INTEGER ICCOMP             ! Character component identifier
      INTEGER IDCB               ! Index to data object entry in the DCB
      INTEGER L                  ! Individal component length
      INTEGER LBND( NDF__MXDIM ) ! Data object lower bounds
      INTEGER NDIM               ! Number of data object dimensions
      INTEGER UBND( NDF__MXDIM ) ! Data object upper bounds
      LOGICAL THERE              ! Whether component exists

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the NDF identifier.
      CALL NDF1_IMPID( INDF, IACB, STATUS )

*  Validate the axis character component name.
      CALL NDF1_VACCN( COMP, ICCOMP, STATUS )

*  Validate the axis number.
      CALL NDF1_VAN( IACB, IAXIS, .TRUE., IAX1, IAX2, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Obtain an index to the data object entry in the DCB.
         IDCB = ACB_IDCB( IACB )

*  Obtain the number of dimensions of the actual data object from the
*  ARY_ system identifier for its data array.
         CALL ARY_BOUND( DCB_DID( IDCB ), NDF__MXDIM, LBND, UBND, NDIM,
     :                   STATUS )

*  Loop to process each axis required.
         IF ( STATUS .EQ. SAI__OK ) THEN
            LENGTH = 0
            DO 1 IAX = IAX1, IAX2

*  See if the current axis exists within the actual data object. If so,
*  then ensure that axis character component information is available
*  for this axis.
               THERE = IAX .LE. NDIM
               IF ( THERE ) THEN
                  CALL NDF1_DAC( IAX, ICCOMP, IDCB, STATUS )

*  Note if the required component exists for this axis.
                  IF ( STATUS .EQ. SAI__OK ) THEN
                     THERE = DCB_ACLOC( IAX, ICCOMP, IDCB ) .NE.
     :                       DAT__NOLOC
                  END IF
               END IF

*  If the component does not exist, then generate the length of the
*  default component value (for the label component, this requires
*  calculation of the number of decimal digits needed to store the axis
*  number).
               IF ( STATUS .EQ. SAI__OK ) THEN
                  IF ( .NOT. THERE ) THEN

*  Label component...
                     IF ( ICCOMP .EQ. NDF__ALAB ) THEN
                        L = 6 + INT( LOG10( 0.5 + REAL( IAX ) ) )

*  Units component...
                     ELSE IF ( ICCOMP .EQ. NDF__AUNI ) THEN
                        L = 5
                     END IF

*  If the required component exists, then obtain its length.
                  ELSE
                     CALL DAT_LEN( DCB_ACLOC( IAX, ICCOMP, IDCB ), L,
     :                             STATUS )
                  END IF
               END IF

*  Quit considering axes if an error occurs.
               IF ( STATUS .NE. SAI__OK ) GO TO 2

*  Find the maximum length for all the axes considered.
               LENGTH = MAX( LENGTH, L )
1           CONTINUE
2           CONTINUE
         END IF
      END IF

*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDF_ACLEN_ERR',
     :   'NDF_ACLEN: Error determining the length of an NDF axis ' //
     :   'character component.', STATUS )
         CALL NDF1_TRACE( 'NDF_ACLEN', STATUS )
      END IF

      END
