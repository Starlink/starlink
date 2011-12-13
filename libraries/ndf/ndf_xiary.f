      SUBROUTINE NDF_XIARY( INDF, XNAME, CMPT, MODE, IARY, STATUS )
*+
*  Name:
*     NDF_XIARY

*  Purpose:
*     Obtain access to an array stored in an NDF extension.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_XIARY( INDF, XNAME, CMPT, MODE, IARY, STATUS )

*  Description:
*     The routine locates an array stored in an NDF extension and
*     imports it into the ARY_ system, returning an array identifier
*     for it. If necessary, a section of the array will be selected so
*     that it matches pixel-for-pixel with the main data array of the
*     NDF (or NDF section) supplied.  The returned array identifier may
*     be used to manipulate the array using the ARY_ routines (see
*     SUN/11).

*  Arguments:
*     INDF = INTEGER (Given)
*        NDF identifier.
*     XNAME = CHARACTER * ( * ) (Given)
*        Name of the extension.
*     CMPT = CHARACTER * ( * ) (Given)
*        Name of the array component within the extension.
*     MODE = CHARACTER * ( * ) (Given)
*        Mode of access required: 'READ', 'UPDATE' or 'WRITE'.
*     IARY = INTEGER (Returned)
*        Array identifier.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The value given for the CMPT argument may be an HDS path name,
*     consisting of several fields separated by '.', so that an object
*     can be accessed in a sub-component (or a sub-sub-component...) of
*     an NDF extension. Array subscripts may also be included. Thus a
*     string such as 'FILTER(3).FLATFIELD' could be used as a valid CMPT
*     value.
*     -  This routine will normally generate an array section. However,
*     if the input NDF is a base NDF and the requested array has the
*     same pixel-index bounds, then there is no need to generate a
*     section in order to access the required part of the array. In
*     this case, a base array identifier will be issued instead.
*     -  It is the caller's responsibility to annul the ARY_ system
*     identifier returned by this routine (e.g. by calling ARY_ANNUL)
*     when it is no longer required. The NDF_ system will not perform
*     this task itself.
*     -  The array associated with the returned identifier will have
*     the same number of dimensions as the base array from which it is
*     derived. If the input NDF has fewer dimensions than this, then
*     the pixel-index bounds of the extra array dimensions are
*     preserved unchanged. If the NDF has more dimensions, then the
*     extra ones are ignored.
*     -  This routine takes account of the transfer window of the NDF
*     supplied and will restrict the transfer window of the new array
*     section so as not to grant access to regions of the base array
*     which are not accessible in the input NDF.
*     -  If this routine is called with STATUS set, then a value of
*     ARY__NOID will be returned for the IARY argument, although no
*     further processing will occur. The same value will also be
*     returned if the routine should fail for any reason.
*     -  The ARY__NOID constant is defined in the include file ARY_PAR.

*  Copyright:
*     Copyright (C) 1991, 1994 Science & Engineering Research Council
*     Copyright (C) 2005 Particle Physics and Astronomy Research Council.
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
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     14-OCT-1991 (RFWS):
*        Original version.
*     14-OCT-1991 (RFWS):
*        Fixed bug: wrong data array identifier used.
*     15-OCT-1991 (RFWS):
*        Improved prologue.
*     17-OCT-1991 (RFWS):
*        Fixed bug: missing argument to ERR_REP.
*     8-SEP-1994 (RFWS):
*        Updated prologue.
*     24-DEC-2005 (TIMJ):
*        Use HDS_FIND rather than NDF1_HFIND
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

*  Global Variables:
      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_XLOC( NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Locator to extension (MORE) structure.

      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_DID( NDF__MXACB ) = INTEGER (Read)
*           ARY_ system identifier for the NDF's data array.
*        ACB_IDCB( NDF__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.

*  Arguments Given:
      INTEGER INDF
      CHARACTER * ( * ) XNAME
      CHARACTER * ( * ) CMPT
      CHARACTER * ( * ) MODE

*  Arguments Returned:
      INTEGER IARY

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) LOC ! Extension locator
      CHARACTER * ( DAT__SZLOC ) LOC1 ! Component locator
      CHARACTER * ( NDF__SZMOD ) VMODE ! Validated access mode string
      INTEGER IACB               ! Index to the NDF in the ACB
      INTEGER IARYB              ! Base array identifier
      INTEGER IDCB               ! Index to data object in the DCB
      LOGICAL THERE              ! Extension present?

*.

*  Set an initial null default value for the IARY argument.
      IARY = ARY__NOID

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the NDF identifier.
      CALL NDF1_IMPID( INDF, IACB, STATUS )

*  Check the access mode for validity and determine whether the required
*  mode of NDF access is available.
      CALL NDF1_VMOD( MODE, VMODE, STATUS )
      CALL NDF1_CHMOD( IACB, VMODE, STATUS )

*  Check the extension name for validity.
      CALL NDF1_CHXNM( XNAME, STATUS )

*  If OK, then obtain an index to the data object in the DCB.
      IF ( STATUS .EQ. SAI__OK ) THEN
         IDCB = ACB_IDCB( IACB )

*  Ensure that extension information is available for the NDF.
         CALL NDF1_DX( IDCB, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  If there is no extension (MORE) structure, then the requested
*  extension cannot be there, so report an error.
            IF ( DCB_XLOC( IDCB ) .EQ. DAT__NOLOC ) THEN
               STATUS = NDF__NOEXT
               CALL MSG_SETC( 'XNAME', XNAME )
               CALL NDF1_AMSG( 'NDF', IACB )
               CALL ERR_REP( 'NDF_XIARY_NO1',
     :         'There is no ''^XNAME'' extension in the NDF ' //
     :         'structure ^NDF', STATUS )

*  Otherwise, see if the requested extension is present.
            ELSE
               CALL DAT_THERE( DCB_XLOC( IDCB ), XNAME, THERE, STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN

*  If absent, then report an error.
                  IF ( .NOT. THERE ) THEN
                     STATUS = NDF__NOEXT
                     CALL MSG_SETC( 'XNAME', XNAME )
                     CALL NDF1_AMSG( 'NDF', IACB )
                     CALL ERR_REP( 'NDF_XIARY_NO2',
     :               'There is no ''^XNAME'' extension in the NDF ' //
     :               'structure ^NDF', STATUS )

*  If the required extension is present, then obtain a locator to it.
                  ELSE
                     CALL DAT_FIND( DCB_XLOC( IDCB ), XNAME, LOC,
     :                              STATUS )

*  Locate the required component within the extension.
                     CALL HDS_FIND( LOC, CMPT, VMODE, LOC1, STATUS )
                     CALL DAT_ANNUL( LOC, STATUS )

*  Import the component into the ARY_ system, obtaining an identifier
*  for the base array. Annul the extension locator.
                     CALL ARY_IMPRT( LOC1, IARYB, STATUS )
                     CALL DAT_ANNUL( LOC1, STATUS )

*  Obtain a section from the array which matches the NDF's main data
*  array. Annul the base array identifier.
                     CALL ARY_SSECT( IARYB, ACB_DID( IACB ), IARY,
     :                               STATUS )
                     CALL ARY_ANNUL( IARYB, STATUS )
                  END IF
               END IF
            END IF
         END IF
      END IF

*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDF_XIARY_ERR',
     :   'NDF_XIARY: Error obtaining access to an array stored in ' //
     :   'an NDF extension.', STATUS )
         CALL NDF1_TRACE( 'NDF_XIARY', STATUS )
      END IF

      END
