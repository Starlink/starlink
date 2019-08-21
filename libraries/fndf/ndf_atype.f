      SUBROUTINE NDF_ATYPE( INDF, COMP, IAXIS, TYPE, STATUS )
*+
*  Name:
*     NDF_ATYPE

*  Purpose:
*     Obtain the numeric type of an NDF axis array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_ATYPE( INDF, COMP, IAXIS, TYPE, STATUS )

*  Description:
*     The routine returns the numeric type of an NDF axis array as an
*     upper-case character string (e.g. '_REAL').

*  Arguments:
*     INDF = INTEGER (Given)
*        NDF identifier.
*     COMP = CHARACTER * ( * ) (Given)
*        Name of the axis array component whose numeric type is
*        required: 'CENTRE', 'VARIANCE' or 'WIDTH'.
*     IAXIS = INTEGER (Given)
*        Number of the NDF axis for which information is required.
*     TYPE = CHARACTER * ( * ) (Returned)
*        Numeric type of the axis array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  A comma-separated list of axis array component names may also
*     be supplied to this routine. In this case the result returned
*     will be the lowest precision numeric type to which all the
*     specified axis arrays can be converted without unnecessary loss
*     of information.
*     -  A value of zero may be supplied for the IAXIS argument, in
*     which case the routine will combine the results for all the NDF's
*     axes in the same way as described above.
*     -  The symbolic constant NDF__SZTYP may be used for declaring the
*     length of a character variable which is to hold the numeric type
*     of an NDF axis array. This constant is defined in the include
*     file NDF_PAR.

*  Algorithm:
*     -  Import the NDF identifier.
*     -  Determine the integer type code of the result.
*     -  Translate the type code into a string and return it.
*     -  If an error occurred, then report context information.

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
*     11-OCT-1990 (RFWS):
*        Original version, derived from the NDF_TYPE routine.
*     15-OCT-1990 (RFWS):
*        Changed argument order in call to NDF1_ATYP.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants

*  Arguments Given:
      INTEGER INDF
      CHARACTER * ( * ) COMP
      INTEGER IAXIS

*  Arguments Returned:
      CHARACTER * ( * ) TYPE

*  Status:
      INTEGER STATUS             ! Global status

*  Local variables:
      CHARACTER * ( NDF__SZTYP ) DATYP( NDF__TYPUB : NDF__MXTYP ) ! Data
      INTEGER IACB               ! Index to NDF entry in the ACB
      INTEGER ITYPE              ! Integer type code of result

*  Local Data:
      DATA DATYP( NDF__TYPB ) / '_BYTE' / ! Data type code translations
      DATA DATYP( NDF__TYPD ) / '_DOUBLE' /
      DATA DATYP( NDF__TYPI ) / '_INTEGER' /
      DATA DATYP( NDF__TYPK ) / '_INT64' /
      DATA DATYP( NDF__TYPR ) / '_REAL' /
      DATA DATYP( NDF__TYPUB ) / '_UBYTE' /
      DATA DATYP( NDF__TYPUW ) / '_UWORD' /
      DATA DATYP( NDF__TYPW ) / '_WORD' /

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the NDF identifier.
      CALL NDF1_IMPID( INDF, IACB, STATUS )

*  Determine the integer type code of the result.
      CALL NDF1_ATYP( IAXIS, IACB, COMP, ITYPE, STATUS )

*  Translate the type code into a string and return it.
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL NDF1_CCPY( DATYP( ITYPE ), TYPE, STATUS )
      END IF

*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDF_ATYPE_ERR',
     :   'NDF_ATYPE: Error obtaining the numeric type of an NDF ' //
     :   'axis array.', STATUS )
         CALL NDF1_TRACE( 'NDF_ATYPE', STATUS )
      END IF

      END
