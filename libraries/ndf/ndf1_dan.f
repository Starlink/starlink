      SUBROUTINE NDF1_DAN( IAX, IDCB, STATUS )
*+
*  Name:
*     NDF1_DAN

*  Purpose:
*     Ensure that DCB information is available for an axis normalisation
*     flag.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_DAN( IAX, IDCB, STATUS )

*  Description:
*     The routine ensures that information is available in the DCB for
*     an NDF's axis normalisation flag. If this information is already
*     available, then it returns without action. Otherwise, it inspects
*     the actual data object to obtain this information, which it then
*     stores in the DCB. Only those checks necessary to obtain and
*     validate this information are performed.

*  Arguments:
*     IAX = INTEGER (Given)
*        Axis number for which the information is required.
*     IDCB = INTEGER (Given)
*        Index to the data object entry in the DCB.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  See whether axis normalisation information is already
*     available. There is nothing to do if it is.
*     -  Set a default value of .FALSE. for the axis normalisation
*     flag.
*     -  Ensure that axis structure information is available in the
*     DCB.
*     -  If an axis structure exists, then see whether a NORMALISED
*     component is present.
*     -  If the component is present, then obtain a locator for it and
*     determine its type and shape.
*     -  Check that the type is _LOGICAL and report an error if it is
*     not.
*     -  Check that it is a scalar object and report an error if it is
*     not.
*     -  Obtain the logical value of the component.
*     -  Annul the component locator.
*     -  Note whether axis normalisation information is now available.

*  Copyright:
*     Copyright (C) 1990 Science & Engineering Research Council.
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
*     5-JUL-1990 (RFWS):
*        Original version.
*     14-DEC-1990 (RFWS):
*        Changed to test the axis structure locator for the axis being
*        accessed, rather than the first axis, to determine if an axis
*        structure is available.
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
*        DCB_ALOC( NDF__MXDIM, NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC )
*        (Read)
*           Locators to axis structure elements.
*        DCB_ANRM( NDF__MXDIM, NDF__MXDCB ) = LOGICAL (Write)
*           Axis normalisation value.
*        DCB_KAN( NDF__MXDIM, NDF__MXDCB ) = LOGICAL (Read and Write)
*           Whether information is available about axis normalisation.

*  Arguments Given:
      INTEGER IAX
      INTEGER IDCB

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) LOC ! Component locator
      CHARACTER * ( DAT__SZTYP ) TYPE ! Component type
      INTEGER DIM( DAT__MXDIM )  ! Component dimensions
      INTEGER NDIM               ! Number of component dimensions
      LOGICAL THERE              ! Whether component exists

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  See whether axis normalisation information is already available.
*  There is nothing to do if it is.
      IF ( .NOT. DCB_KAN( IAX, IDCB ) ) THEN

*  Set a default value of .FALSE. for the axis normalisation flag.
         DCB_ANRM( IAX, IDCB ) = .FALSE.

*  Ensure that axis structure information is available in the DCB.
         CALL NDF1_DA( IDCB, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  If an axis structure exists, then see whether a NORMALISED component
*  is present.
            IF ( DCB_ALOC( IAX, IDCB ) .NE. DAT__NOLOC ) THEN
               CALL DAT_THERE( DCB_ALOC( IAX, IDCB ), 'NORMALISED',
     :                         THERE, STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN

*  If the component is present, then obtain a locator for it and
*  determine its type and shape.
                  IF ( THERE ) THEN
                     CALL DAT_FIND( DCB_ALOC( IAX, IDCB ), 'NORMALISED',
     :                              LOC, STATUS )
                     CALL DAT_TYPE( LOC, TYPE, STATUS )
                     CALL DAT_SHAPE( LOC, DAT__MXDIM, DIM, NDIM,
     :                               STATUS )
                     IF ( STATUS .EQ. SAI__OK ) THEN

*  Check that the type is _LOGICAL and report an error if it is not.
                        IF ( TYPE .NE. '_LOGICAL' ) THEN
                           STATUS = NDF__TYPIN
                           CALL DAT_MSG( 'AXIS',
     :                                   DCB_ALOC( IAX, IDCB ) )
                           CALL MSG_SETC( 'BADTYPE', TYPE )
                           CALL ERR_REP( 'NDF1_DAN_TYPE',
     :                     'The NORMALISED component in the NDF ' //
     :                     'axis structure ^AXIS has an invalid ' //
     :                     'type of ''^BADTYPE''; it should be of ' //
     :                     'type ''_LOGICAL''.', STATUS )

*  Check that it is a scalar object and report an error if it is not.
                        ELSE IF ( NDIM .NE. 0 ) THEN
                           STATUS = NDF__NDMIN
                           CALL DAT_MSG( 'AXIS',
     :                                   DCB_ALOC( IAX, IDCB ) )
                           CALL MSG_SETI( 'BADNDIM', NDIM )
                           CALL ERR_REP( 'NDF1_DAN_NDIM',
     :                     'The NORMALISED component in the NDF ' //
     :                     'axis structure ^AXIS is ' //
     :                     '^BADNDIM-dimensional; it should be ' //
     :                     'scalar.', STATUS )
                        END IF
                     END IF

*  Obtain the logical value of the component.
                     CALL DAT_GET0L( LOC, DCB_ANRM( IAX, IDCB ),
     :                               STATUS )

*  Annul the component locator.
                     CALL DAT_ANNUL( LOC, STATUS )
                  END IF
               END IF
            END IF
         END IF

*  Note whether axis normalisation information is now available.
         DCB_KAN( IAX, IDCB ) = STATUS .EQ. SAI__OK
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_DAN', STATUS )

      END
