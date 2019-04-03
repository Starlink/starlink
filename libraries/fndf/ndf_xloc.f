      SUBROUTINE NDF_XLOC( INDF, XNAME, MODE, LOC, STATUS )
*+
*  Name:
*     NDF_XLOC

*  Purpose:
*     Obtain access to a named NDF extension via an HDS locator.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_XLOC( INDF, XNAME, MODE, LOC, STATUS )

*  Description:
*     The routine returns an HDS locator to a named extension (if
*     present) in an NDF. An error results if the specified extension
*     is not present.

*  Arguments:
*     INDF = INTEGER (Given)
*        NDF identifier.
*     XNAME = CHARACTER * ( * ) (Given)
*        Name of the required extension.
*     MODE = CHARACTER * ( * ) (Given)
*        Mode of access required to the extension: 'READ', 'UPDATE' or
*        'WRITE'.
*     LOC = CHARACTER * ( * ) (Returned)
*        Extension locator.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If WRITE access is specified, then any existing extension
*     contents or values will be erased or reset, so that the extension
*     is ready to receive new values. If UPDATE access is specified,
*     then existing values will be retained so that they may be
*     modified.
*     -  It is the caller's responsibility to annul the HDS locator
*     issued by this routine (e.g. by calling DAT_ANNUL) when it is no
*     longer required. The NDF_ system will not perform this task
*     itself.
*     -  Although this routine will check the access mode value
*     supplied against the available access to the NDF, HDS does not
*     allow the returned locator to be protected against write access
*     in the case where WRITE access to the NDF is available, but only
*     READ access was requested.  In this case it is the responsibility
*     of the caller to respect the locator access restriction.
*     -  If this routine is called with STATUS set, then an invalid
*     locator will be returned for the LOC argument, although no
*     further processing will occur. The same value will also be
*     returned if the routine should fail for any reason.

*  Algorithm:
*     -  Set an initial value for the LOC argument, before checking the
*     inherited status.
*     -  Import the NDF identifier.
*     -  Validate the requested access mode string.
*     -  Check that the requested mode of access is available.
*     -  Check the extension name.
*     -  Ensure that extension information is available in the DCB.
*     -  If there is no extension (MORE) structure, then report an
*     error.
*     -  Otherwise, see if the requested extension component is present.
*     If so, then obtain a locator to it.
*     -  If write access was requested, then reset the extension's
*     value/erase its contents.
*     -  If the extension is not present, then report an error.

*  Implementation Deficiencies:
*     -  At present, although the requested access mode will be checked
*     against the available access to the NDF, the returned locator may
*     allow WRITE access, even when only READ access was requested.
*     This is because HDS does not currently provide a means of
*     explicitly protecting a locator against write operations.

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
*     20-SEP-1989 (RFWS):
*        Original version.
*     26-SEP-1989 (RFWS):
*        Initialise LOC to DAT__NOLOC.
*     7-FEB-1990 (RFWS):
*        Changed the argument order.
*     20-MAR-1990 (RFWS):
*        Changed so that write access causes any pre-existing extension
*        components or values to be erased/reset.
*     20-MAR-1990 (RFWS):
*        Changed the argument order back again.
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
*        DCB_XLOC( NDF_MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Extension (MORE) structure locator.

      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_IDCB( NDF_MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.

*  Arguments Given:
      INTEGER INDF
      CHARACTER * ( * ) XNAME
      CHARACTER * ( * ) MODE

*  Arguments Returned:
      CHARACTER * ( * ) LOC

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( NDF__SZMOD ) VMODE ! Validated access mode
      INTEGER IACB               ! Index to NDF entry in the ACB
      INTEGER IDCB               ! Index to data object entry in the DCB
      LOGICAL THERE              ! Whether the extension is there

*.

*  Set an initial value for the LOC argument.
      LOC = DAT__NOLOC

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the NDF identifier.
      CALL NDF1_IMPID( INDF, IACB, STATUS )

*  Validate the requested access mode string.
      CALL NDF1_VMOD( MODE, VMODE, STATUS )

*  Check that the requested mode of access is available.
      CALL NDF1_CHMOD( IACB, VMODE, STATUS )

*  Check the extension name.
      CALL NDF1_CHXNM( XNAME, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Obtain an index to the data object entry in the DCB.
         IDCB = ACB_IDCB( IACB )

*  Ensure that extension information is available in the DCB.
         CALL NDF1_DX( IDCB, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  If there is no extension (MORE) structure, then the requested
*  extension component cannot be there, so report an error.
            IF ( DCB_XLOC( IDCB ) .EQ. DAT__NOLOC ) THEN
               STATUS = NDF__NOEXT
               CALL MSG_SETC( 'XNAME', XNAME )
               CALL NDF1_AMSG( 'NDF', IACB )
               CALL ERR_REP( 'NDF_XLOC_NO1',
     :         'There is no ''^XNAME'' extension in the NDF ' //
     :         'structure ^NDF', STATUS )

*  Otherwise, see if the requested extension component is present.
            ELSE
               CALL DAT_THERE( DCB_XLOC( IDCB ), XNAME, THERE, STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN

*  If present, obtain a locator to it.
                  IF ( THERE ) THEN
                     CALL DAT_FIND( DCB_XLOC( IDCB ), XNAME, LOC,
     :                              STATUS )

*  If write access was requested, then reset the extension value(s).
                     IF ( VMODE .EQ. 'WRITE' ) THEN
                        CALL NDF1_HRST( LOC, STATUS )
                     END IF

*  If absent, report an error.
                  ELSE
                     STATUS = NDF__NOEXT
                     CALL MSG_SETC( 'XNAME', XNAME )
                     CALL NDF1_AMSG( 'NDF', IACB )
                     CALL ERR_REP( 'NDF_XLOC_NO2',
     :               'There is no ''^XNAME'' extension in the NDF ' //
     :               'structure ^NDF', STATUS )
                  END IF
               END IF
            END IF
         END IF
      END IF

*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDF_XLOC_ERR',
     :   'NDF_XLOC: Error obtaining access to a named NDF extension ' //
     :   'via an HDS locator.', STATUS )
         CALL NDF1_TRACE( 'NDF_XLOC', STATUS )
      END IF

      END
