      SUBROUTINE NDF1_DV( IDCB, STATUS )
*+
*  Name:
*     NDF1_DV

*  Purpose:
*     Ensure that variance information is available in the DCB.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_DV( IDCB, STATUS )

*  Description:
*     The routine ensures that information about a data object's
*     VARIANCE component is available in the DCB. It does nothing if
*     this information is already available. Otherwise, it obtains this
*     information by inspecting the actual data object, performing
*     necessary validation checks in the process.

*  Arguments:
*     IDCB = INTEGER (Given)
*        Index to the DCB entry for which variance information is
*        required.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  See if variance information is already available. There is
*     nothing to do if it is.
*     -  Ensure that data array information is available for the data
*     object in the DCB.
*     -  See if the variance component is present in the NDF. If not,
*     then signify this by storing the ARY__NOID value in the DCB.
*     -  If it is present, then import the variance component into the
*     ARY_ system, storing the resulting identifier in the DCB.
*     -  Obtain the number of dimensions and the pixel index bounds of
*     the NDF's variance and data array components. Report an error if
*     those of the variance component do not match those of the data
*     array component.
*     -  Set the default attributes of the variance component.
*     -  If the component was not suitable, then annul its identifier
*     in the DCB.
*     -  Initialise the variance mapping count and note whether
*     variance information is now available in the DCB.

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
*     25-SEP-1989 (RFWS):
*        Original version.
*     6-DEC-1989 (RFWS):
*        Updated to reflect changes made to the DCB contents.
*     7-DEC-1989 (RFWS):
*        Added initialisation of the default attributes for the variance
*        component.
*     12-DEC-1989 (RFWS):
*        Added initialisation of the variance mapping count.
*     15-NOV-1990 (RFWS):
*        Removed unnecessary DCB initialisation.
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
      INCLUDE 'ARY_PAR'          ! ARY_ public constants

      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_DECPX( NDF__MXDCB ) = LOGICAL (Read)
*           Default complex value flag for NDF components.
*        DCB_DEFRM( NDF__MXDCB ) = CHARACTER * ( NDF__SZFRM ) (Read)
*           Default storage form for NDF components.
*        DCB_DETYP( NDF__MXDCB ) = CHARACTER * ( NDF__SZTYP ) (Read)
*           Default numeric data type for NDF components.
*        DCB_DID( NDF__MXDCB ) = INTEGER (Read)
*           ARY_ system identifier for the NDF's data array.
*        DCB_KV( NDF__MXDCB ) = LOGICAL (Read and Write)
*           Whether information about the NDF's variance component is
*           available in the DCB.
*        DCB_LOC( NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Data object locator.
*        DCB_VCPX( NDF__MXDCB ) = LOGICAL (Write)
*           Default complex value flag for the variance component.
*        DCB_VFRM( NDF__MXDCB ) = CHARACTER * ( NDF__SZFRM ) (Write)
*           Default storage form for the variance component.
*        DCB_VTYP( NDF__MXDCB ) = CHARACTER * ( NDF__SZTYP ) (Write)
*           Default numeric data type for the variance component.
*        DCB_VID( NDF__MXDCB ) = INTEGER (Write)
*           ARY_ system identifier for the NDF's variance array.

*  Arguments Given:
      INTEGER IDCB

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop counter for dimensions
      INTEGER LBNDD( NDF__MXDIM ) ! Data component lower bounds
      INTEGER LBNDV( NDF__MXDIM ) ! Variance component lower bounds
      INTEGER NDIMD              ! Number of data component dimensions
      INTEGER NDIMV              ! Number of variance dimensions
      INTEGER UBNDD( NDF__MXDIM ) ! Data component upper bounds
      INTEGER UBNDV( NDF__MXDIM ) ! Variance component upper bounds
      LOGICAL THERE              ! Whether variance component exists

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  See if variance information is already available. There is nothing to
*  do if it is.
      IF ( .NOT. DCB_KV( IDCB ) ) THEN

*  Ensure that information about the data array is available in the DCB.
         CALL NDF1_DD( IDCB, STATUS )

*  See if the VARIANCE component is present. If not, then signify this
*  by storing the ARY__NOID value in the DCB.
         CALL DAT_THERE( DCB_LOC( IDCB ), 'VARIANCE', THERE, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN
            IF ( .NOT. THERE ) THEN
               DCB_VID( IDCB ) = ARY__NOID

*  If it is present, then import the VARIANCE component into the ARY_
*  system, storing the resulting identifier in the DCB.
            ELSE
               CALL ARY_FIND( DCB_LOC( IDCB ), 'VARIANCE',
     :                        DCB_VID( IDCB ), STATUS )

*  Obtain the number of dimensions and the pixel index bounds of the
*  NDF's data array and variance components.
               CALL ARY_BOUND( DCB_DID( IDCB ), NDF__MXDIM, LBNDD,
     :                         UBNDD, NDIMD, STATUS )
               CALL ARY_BOUND( DCB_VID( IDCB ), NDF__MXDIM, LBNDV,
     :                         UBNDV, NDIMV, STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN

*  Report an error if the number of variance dimensions does not match
*  that of the data array.
                  IF ( NDIMV .NE. NDIMD ) THEN
                     STATUS = NDF__NDMIN
                     CALL NDF1_DMSG( 'NDF', IDCB )
                     CALL MSG_SETI( 'BADNDIM', NDIMV )
                     CALL MSG_SETI( 'NDIM', NDIMD )
                     CALL ERR_REP( 'NDF1_DV_NDIM',
     :               'The VARIANCE array in the NDF structure ' //
     :               '^NDF has an invalid number of dimensions ' //
     :               '(^BADNDIM); it should be ^NDIM-dimensional.',
     :               STATUS )

*  Check that the variance pixel index bounds in each dimension match
*  those of the data array.
                  ELSE
                     DO 1 I = 1, NDIMD
                        IF ( ( LBNDV( I ) .NE. LBNDD( I ) ) .OR.
     :                       ( UBNDV( I ) .NE. UBNDD( I ) ) ) THEN

*  Report an error if a discrepancy is found.
                           STATUS = NDF__BNDIN
                           CALL MSG_SETI( 'DIM', I )
                           CALL NDF1_DMSG( 'NDF', IDCB )
                           CALL ERR_REP( 'NDF1_DV_BND',
     :                     'The pixel-index bounds of dimension ' //
     :                     '^DIM of the VARIANCE array in the NDF ' //
     :                     'structure ^NDF do not match those of ' //
     :                     'the NDF''s DATA_ARRAY component.', STATUS )
                           GO TO 2
                        END IF
1                    CONTINUE
2                    CONTINUE
                  END IF
               END IF
            END IF

*  Set the default attributes of the variance component using the values
*  initially derived from the data array component.
            DCB_VTYP( IDCB ) = DCB_DETYP( IDCB )
            DCB_VCPX( IDCB ) = DCB_DECPX( IDCB )
            DCB_VFRM( IDCB ) = DCB_DEFRM( IDCB )

*  If the component is not suitable, then annul the associated ID.
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ARY_ANNUL( DCB_VID( IDCB ), STATUS )
            END IF
         END IF

*  Note whether variance information is now available in the DCB.
         DCB_KV( IDCB ) = STATUS .EQ. SAI__OK
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_DV', STATUS )

      END
