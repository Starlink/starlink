      SUBROUTINE NDF_XNEW( INDF, XNAME, TYPE, NDIM, DIM, LOC, STATUS )
*+
*  Name:
*     NDF_XNEW

*  Purpose:
*     Create a new extension in an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_XNEW( INDF, XNAME, TYPE, NDIM, DIM, LOC, STATUS )

*  Description:
*     The routine creates a new named extension of specified type and
*     shape in an NDF structure, and returns an HDS locator to it.

*  Arguments:
*     INDF = INTEGER (Given)
*        NDF identifier.
*     XNAME = CHARACTER * ( * ) (Given)
*        Extension name.
*     TYPE = CHARACTER * ( * ) (Given)
*        HDS data type of the extension.
*     NDIM = INTEGER (Given)
*        Number of extension dimensions.
*     DIM( NDIM ) = INTEGER (Given)
*        Extension dimension sizes.
*     LOC = CHARACTER * ( * ) (Returned)
*        Locator to the newly created extension.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If this routine is called with STATUS set, then an invalid
*     locator will be returned for the LOC argument, although no
*     further processing will occur. The same value will also be
*     returned if the routine should fail for any reason.

*  Algorithm:
*     -  Set an initial value for the LOC argument, before checking the
*     inherited status.
*     -  Import the NDF identifier.
*     -  Check that WRITE access to the NDF is available.
*     -  Check the extension name.
*     -  Obtain an index to the data object entry in the DCB and ensure
*     that extension information is available in the DCB.
*     -  If an extension (MORE) structure does not exist, then create
*     one and store a locator to it in the DCB.
*     -  Note if the required extension component already exists.
*     -  If so, then report an error.
*     -  Otherwise, create the extension component and obtain a locator
*     to it.

*  Copyright:
*     Copyright (C) 1989 Science & Engineering Research Council.
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
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     20-SEP-1989 (RFWS):
*        Original version.
*     26-SEP-1989 (RFWS):
*        Initialise LOC to DAT__NOLOC.
*     24-SEP-2010 (TIMJ):
*        Include the name of the extension in the error message.
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
*        DCB_LOC( NDF_MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Data object locator.
*        DCB_XLOC( NDF_MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read and
*        Write)
*           Extension (MORE) structure locator.

      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_IDCB( NDF_MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.

*  Arguments Given:
      INTEGER INDF
      CHARACTER * ( * ) XNAME
      CHARACTER * ( * ) TYPE
      INTEGER NDIM
      INTEGER DIM( * )

*  Arguments Returned:
      CHARACTER * ( * ) LOC

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL THERE              ! Whether extension component exists
      INTEGER IACB               ! Index to NDF entry in the ACB
      INTEGER DUMMY( 1 )         ! Dummy dimension array
      INTEGER IDCB               ! Index to data object entry in the DCB

*.

*  Set an initial value for the LOC argument.
      LOC = DAT__NOLOC

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the NDF identifier.
      CALL NDF1_IMPID( INDF, IACB, STATUS )

*  Check that WRITE access to the NDF is available.
      CALL NDF1_CHACC( IACB, 'WRITE', STATUS )

*  Check the extension name.
      CALL NDF1_CHXNM( XNAME, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Obtain an index to the data object entry in the DCB.
         IDCB = ACB_IDCB( IACB )

*  Ensure that extension information is available in the DCB.
         CALL NDF1_DX( IDCB, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  If an extension (MORE) structure does not exist, then create one and
*  obtain a locator to it, storing this in the DCB.
            IF ( DCB_XLOC( IDCB ) .EQ. DAT__NOLOC ) THEN
               DUMMY( 1 ) = 0
               CALL DAT_NEW( DCB_LOC( IDCB ), 'MORE', 'EXT', 0, DUMMY,
     :                       STATUS )
               CALL DAT_FIND( DCB_LOC( IDCB ), 'MORE', DCB_XLOC( IDCB ),
     :                        STATUS )

*  Note whether the required extension component already exists,
*  enquiring this information if necessary.
               THERE = .FALSE.
            ELSE
               CALL DAT_THERE( DCB_XLOC( IDCB ), XNAME, THERE, STATUS )
            END IF

*  If it already exists, then report an error.
            IF ( STATUS .EQ. SAI__OK ) THEN
               IF ( THERE ) THEN
                  STATUS = NDF__XISTS
                  CALL MSG_SETC( 'XNAME', XNAME )
                  CALL NDF1_AMSG( 'NDF', IACB )
                  CALL ERR_REP( 'NDF_XNEW_XIST',
     :            'A ''^XNAME'' extension already exists in the NDF ' //
     :            'structure ^NDF', STATUS )

*  Otherwise, create the extension component and obtain a locator to it.
               ELSE
                  CALL DAT_NEW( DCB_XLOC( IDCB ), XNAME, TYPE, NDIM,
     :                          DIM, STATUS )
                  CALL DAT_FIND( DCB_XLOC( IDCB ), XNAME, LOC, STATUS )
               END IF
            END IF
         END IF
      END IF

*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL MSG_SETC( 'XNAME', XNAME )
         CALL ERR_REP( 'NDF_XNEW_ERR',
     :        'NDF_XNEW: Error creating a new extension named ^XNAME '//
     :        'in an NDF.', STATUS )
         CALL NDF1_TRACE( 'NDF_XNEW', STATUS )
      END IF

      END
