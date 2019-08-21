      SUBROUTINE NDF1_DCREP( FTYPE, NDIM, UBND, IPCB, IACB, STATUS )
*+
*  Name:
*     NDF1_DCREP

*  Purpose:
*     Create a primitive NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_DCREP( FTYPE, NDIM, UBND, IPCB, IACB, STATUS )

*  Description:
*     The routine creates a primitive NDF data object containing just a
*     data array component and returns an ACB index which refers to the
*     resulting new base NDF. The location of the new object is
*     identified by means of an index to a placeholder entry in the
*     PCB. This placeholder should later be annulled.

*  Arguments:
*     FTYPE = CHARACTER * ( * ) (Given)
*        Full data type of the NDF's data array.
*     NDIM = INTEGER (Given)
*        Number of NDF dimensions.
*     UBND( NDIM ) = INTEGER (Given)
*        Upper bounds of the NDF.
*     IPCB = INTEGER (Given)
*        Index to a placeholder entry in the PCB which specifies the
*        location (and certain properties) of the new NDF.
*     IACB = INTEGER (Returned)
*        Index to the ACB entry which refers to the new base NDF.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council

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
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15-FEB-1990 (RFWS):
*        Original, derived rom the NDF1_DCRE routine.
*     26-FEB-1990 (RFWS):
*        Changed the NDF placeholder data type to 'NDF'.
*     1-MAR-1990 (RFWS):
*        Removed un-referenced variable.
*     23-MAR-1990 (RFWS):
*        Tune HDS for the expected number of NDF components.
*     26-MAR-1990 (RFWS):
*        Changed to initialise the quality bad-bits values in the DCB.
*     1-AUG-1990 (RFWS):
*        Changed subscript order in DCB axis character component
*        arrays.
*     15-NOV-1990 (RFWS):
*        Removed unnecessary DCB initialisations, which are now
*        performed by NDF1_FFS.
*     8-JUN-1993 (DSB):
*        Call to DAT_PRMRY included to promote the locator to the data
*        object stored in the DCB to a primary locator.
*     4-NOV-1993 (RFWS):
*        Changed to specify location of new object using a placeholder
*        entry in the PCB.
*     29-APR-1994 (RFWS):
*        Eliminated initialisations now done by NDF1_PLDCB.
*     31-OCT-2007 (DSB):
*        Add call to NDF1_EVENT.
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
      INCLUDE 'NDF_CONST'        ! NDF_ private constant

*  Global Variables:
      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_DECPX( NDF__MXDCB ) = LOGICAL (Write)
*           Default complex value flag for other NDF components.
*        DCB_DEFRM( NDF__MXDCB ) = CHARACTER * ( NDF__SZFRM ) (Write)
*           Default storage form for other NDF components.
*        DCB_DETYP( NDF__MXDCB ) = CHARACTER * ( NDF__SZTYP ) (Write)
*           Default numeric data type for other NDF components.
*        DCB_DID( NDF__MXDCB ) = INTEGER (Write)
*           ARY_ system identifier for the NDF's data array.
*        DCB_KD( NDF__MXDCB ) = LOGICAL (Write)
*           Whether data array information is available in the DCB.
*        DCB_LOC( NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Data object locator.

*  Arguments Given:
      CHARACTER * ( * ) FTYPE
      INTEGER NDIM
      INTEGER UBND( NDIM )
      INTEGER IPCB

*  Arguments Returned:
      INTEGER IACB

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IDCB               ! Index to data object entry in the DCB
      INTEGER NLEV               ! Levels in HDS path name
      INTEGER PLACE              ! ARY_ placeholder for data array

*.

*  Set an initial value for the IACB argument.
      IACB = 0

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain a free slot in the DCB.
      CALL NDF1_FFS( NDF__DCB, IDCB, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Take the default for any wild-carded foreign format information in
*  the PCB entry and then initialise the new DCB entry with information
*  derived from the placeholder.
         CALL NDF1_PRFOR( 0, IPCB, STATUS )
         CALL NDF1_PLDCB( IPCB, IDCB, STATUS )

*  Use HDS_TUNE to set the optimum number of components in the HDS
*  structure.
         CALL HDS_TUNE( 'NCOMP', 10, STATUS )

*  Create the data array component and store an ARY_ system identifier
*  for it in the DCB.
         CALL ARY_PLACE( DCB_LOC( IDCB ), 'DATA_ARRAY', PLACE, STATUS )
         CALL ARY_NEWP( FTYPE, NDIM, UBND, PLACE, DCB_DID( IDCB ),
     :                  STATUS )

*  Derive the data array component attributes which are needed as
*  default values for other components.
         CALL ARY_TYPE( DCB_DID( IDCB ), DCB_DETYP( IDCB ), STATUS )
         DCB_DECPX( IDCB ) = .FALSE.
         DCB_DEFRM( IDCB ) = 'PRIMITIVE'

*  Note whether data array information is available.
         DCB_KD( IDCB ) = STATUS .EQ. SAI__OK

*  Create a new base NDF entry in the ACB to describe the new object.
         CALL NDF1_CRNBN( IDCB, IACB, STATUS )

*  Assign the name of the data file to the MSG token "NDF_EVENT"
         CALL NDF1_EVMSG( 'NDF_EVENT', IDCB )

*  Raise an NDF event, describing the opening of a new NDF.
         CALL NDF1_EVENT( 'OPEN_NEW_NDF', STATUS )

*  If there was an error, then clean up by annulling the identifiers
*  and locators which may have been acquired.
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ARY_ANNUL( DCB_DID( IDCB ), STATUS )
            CALL DAT_ANNUL( DCB_LOC( IDCB ), STATUS )

*  Release the allocated DCB slot.
            CALL NDF1_RLS( NDF__DCB, IDCB, STATUS )
         ENDIF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_DCREP', STATUS )

      END
