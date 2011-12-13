      SUBROUTINE NDF_XNAME( INDF, N, XNAME, STATUS )
*+
*  Name:
*     NDF_XNAME

*  Purpose:
*     Obtain the name of the N'th extension in an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_XNAME( INDF, N, XNAME, STATUS )

*  Description:
*     The routine returns the name of the N'th extension in an NDF. If
*     the requested extension does not exist, then the name is returned
*     blank. The routine may therefore be used to obtain the names of
*     all the extensions present by setting N to 1,2... etc.  until a
*     blank name is returned. Note that the order in which these names
*     are obtained is not defined.

*  Arguments:
*     INDF = INTEGER (Given)
*        NDF identifier.
*     N = INTEGER (Given)
*        The number of the extension whose name is required.
*     XNAME = CHARACTER * ( * ) (Returned)
*        The extension name (in upper case).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The symbolic constant NDF__SZXNM is provided to define the
*     length of character variables which are to hold an NDF extension
*     name. This constant is defined in the include file NDF_PAR.

*  Algorithm:
*     -  Import the NDF identifier.
*     -  Obtain an index to the data object entry in the DCB.
*     -  Ensure that extension (MORE) structure information is available
*     in the DCB.
*     -  If there is no extension (MORE) structure in the NDF, then set
*     a blank name.
*     -  Otherwise, mark the error stack and obtain a locator to the
*     N'th extension structure component.
*     -  If the component was not found, then annul the error and set a
*     blank name.
*     -  Otherwise, obtain the name and copy it to the output argument,
*     checking for possible truncation.
*     -  Annul the locator.
*     -  Release the error stack.
*     -  If an error occurred, then report context information.

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
*     21-NOV-1989 (RFWS):
*        Original version.
*     24-NOV-1989 (RFWS):
*        Added check on the validity of the N argument.
*     15-MAR-1990 (RFWS):
*        Changed to return a blank name if the requested extension does
*        not exist, rather than an error status.
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
      INCLUDE 'DAT_ERR'          ! HDS error codes

*  Global Variables:
      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_LOC( NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Data object locator.
*        DCB_XLOC( NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Locator to extension (MORE) structure.

      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_IDCB( NDF__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.

*  Arguments Given:
      INTEGER INDF
      INTEGER N

*  Arguments Returned:
      CHARACTER * ( * ) XNAME

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) LOC ! Locator to HDS component
      CHARACTER * ( DAT__SZNAM ) NAME ! Name of HDS extension component
      INTEGER IACB               ! Index to NDF entry in the ACB
      INTEGER IDCB               ! Index to data object entry in the DCB

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that the extension number specified is valid and report an
*  error if it is not.
      IF ( N .LT. 1 ) THEN
         STATUS = NDF__XNOIN
         CALL MSG_SETI( 'N', N )
         CALL ERR_REP( 'NDF_XNAME_N',
     :   'Invalid extension number ^N specified (possible ' //
     :   'programming error).', STATUS )

*  Import the NDF identifier.
      ELSE
         CALL NDF1_IMPID( INDF, IACB, STATUS )

*  Obtain an index to the data object entry in the DCB.
         IF ( STATUS .EQ. SAI__OK ) THEN
            IDCB = ACB_IDCB( IACB )

*  Ensure that extension (MORE) structure information is available in
*  the DCB.
            CALL NDF1_DX( IDCB, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN

*  If there is no extension (MORE) structure in the NDF, then set a
*  blank name.
               IF ( DCB_XLOC( IDCB ) .EQ. DAT__NOLOC ) THEN
                  XNAME = ' '

*  Otherwise, mark the error stack and obtain a locator to the N'th
*  extension structure component.
               ELSE
                  CALL ERR_MARK
                  CALL DAT_INDEX( DCB_XLOC( IDCB ), N, LOC, STATUS )

*  If the component was not found, then annul the error and set a blank
*  name.
                  IF ( STATUS .EQ. DAT__OBJNF ) THEN
                     CALL ERR_ANNUL( STATUS )
                     XNAME = ' '

*  Otherwise, obtain the name and copy this to the output argument,
*  checking for possible truncation.
                  ELSE
                     CALL DAT_NAME( LOC, NAME, STATUS )
                     CALL NDF1_CCPY( NAME, XNAME, STATUS )

*  Annul the locator.
                     CALL DAT_ANNUL( LOC, STATUS )
                  END IF
                  LOC = DAT__NOLOC

*  Release the error stack.
                  CALL ERR_RLSE
               END IF
            END IF
         END IF
      END IF

*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDF_XNAME_ERR',
     :   'NDF_XNAME: Error obtaining the name of the N''th ' //
     :   'extension in an NDF.', STATUS )
         CALL NDF1_TRACE( 'NDF_XNAME', STATUS )
      END IF

      END
