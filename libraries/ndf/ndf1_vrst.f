      SUBROUTINE NDF1_VRST( IACB, STATUS )
*+
*  Name:
*     NDF1_VRST

*  Purpose:
*     Reset the variance component of an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_VRST( IACB, STATUS )

*  Description:
*     The routine sets the variance component of an NDF into the
*     "undefined" state. No action is taken if it is already in this
*     state or if the specified NDF is not a base NDF.  The NDF is
*     identified by its ACB entry.

*  Arguments:
*     IACB = INTEGER (Given)
*        Index to the NDF's ACB entry.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  Resetting this component involves the deletion of the
*     associated array (if it exists).

*  Algorithm:
*     -  Ensure that variance information is available in the DCB and
*     ACB.
*     -  Obtain an index to the data object entry in the DCB.
*     -  Check that the variance component is not mapped through the
*     current ACB entry. Report an error if it is.
*     -  Only take further action if this is a base NDF. Check that the
*     variance component is not mapped at all. Report an error if it
*     is.
*     -  See if the DCB variance array identifier is valid. If not,
*     then the variance component is already undefined.
*     -  If it is defined, then obtain its attributes and save these in
*     the DCB for use if the array is later re-created.
*     -  Delete the variance array.

*  Copyright:
*     Copyright (C) 1989, 1990 Science & Engineering Research Council.
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
*     8-DEC-1989 (RFWS):
*        Original version.
*     8-DEC-1989 (RFWS):
*        Installed the NDF1_VIMP routine.
*     21-MAR-1990 (RFWS):
*        Installed checks that the variance array is not mapped.
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
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Global Variables:
      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_NVMAP( NDF__MXDCB ) = INTEGER (Read)
*           Number of mappings to the NDF's variance array.
*        DCB_VCPX( NDF__MXDCB ) = LOGICAL (Write)
*           Whether the NDF's variance component holds complex data by
*           default.
*        DCB_VFRM( NDF__MXDCB ) = CHARACTER * ( NDF__SZFRM ) (Write)
*           The default form of array used to store data in the NDF's
*           variance component.
*        DCB_VTYP( NDF__MXDCB ) = CHARACTER * ( NDF__SZTYP ) (Write)
*           Default numeric data type of the NDF's variance component.
*        DCB_VID( NDF__MXDCB ) = INTEGER (Read and Write)
*           ARY_ system identifier for the NDF's variance array.

      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_CUT( NDF_MMXACB ) = LOGICAL (Read)
*           Whether the NDF is a cut (i.e. section).
*        ACB_IDCB( NDF__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.
*        ACB_VMAP( NDF__MXACB ) = LOGICAL (Read)
*           Whether the NDF's variance component is mapped.

*  Arguments Given:
      INTEGER IACB

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IDCB               ! Index to data object entry in the DCB
      LOGICAL VALID              ! Whether variance array exists

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Ensure that variance information is available in the DCB and ACB.
      CALL NDF1_VIMP( IACB, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Obtain an index to the data object entry in the DCB.
         IDCB = ACB_IDCB( IACB )

*  Check that the variance component is not mapped through the current
*  ACB entry. Report an error if it is.
         IF ( ACB_VMAP( IACB ) ) THEN
            STATUS = NDF__ISMAP
            CALL NDF1_AMSG( 'NDF', IACB )
            CALL ERR_REP( 'NDF1_VRST_MAP',
     :      'The variance component in the NDF structure ^NDF is ' //
     :      'already mapped for access through the specified ' //
     :      'identifier (possible programming error).', STATUS )

*  Only take further action if this is a base NDF. Check that the
*  variance component is not mapped at all. Report an error if it is.
         ELSE IF ( .NOT. ACB_CUT( IACB ) ) THEN
            IF ( DCB_NVMAP( IDCB ) .NE. 0 ) THEN
               STATUS = NDF__ISMAP
               CALL NDF1_DMSG( 'NDF', IDCB )
               CALL ERR_REP( 'NDF1_VRST_BMAP',
     :         'The variance component in the NDF structure ^NDF is ' //
     :         'already mapped for access through another ' //
     :         'identifier (possible programming error).', STATUS )

*  See if the DCB variance array identifier is valid. If not, then the
*  variance component is already undefined.
            ELSE
               CALL ARY_VALID( DCB_VID( IDCB ), VALID, STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN

*  If it is defined, then obtain the variance array attributes, which
*  may have changed since it was created. Save these as the defaults for
*  use if the array is re-created.
                  IF ( VALID ) THEN
                     CALL ARY_TYPE( DCB_VID( IDCB ), DCB_VTYP( IDCB ),
     :                              STATUS )
                     CALL ARY_CMPLX( DCB_VID( IDCB ), DCB_VCPX( IDCB ),
     :                               STATUS )
                     CALL ARY_FORM( DCB_VID( IDCB ), DCB_VFRM( IDCB ),
     :                              STATUS )

*  Delete the variance array. Note that all identifiers referring to it
*  (e.g. those in the ACB) thereby become invalid.
                     CALL ARY_DELET( DCB_VID( IDCB ), STATUS )
                  END IF
               END IF
            END IF
         END IF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_VRST', STATUS )

      END
