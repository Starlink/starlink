      SUBROUTINE NDF_SCTYP( INDF, COMP, TYPE, STATUS )
*+
*  Name:
*     NDF_SCTYP

*  Purpose:
*     Obtain the numeric type of a scaled NDF array component.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_SCTYP( INDF, COMP, TYPE, STATUS )

*  Description:
*     The routine returns the numeric type of a scaled NDF array component
*     as an upper-case character string (e.g. '_REAL'). The returned type
*     describes the values stored in the array, before they are unscaled
*     using the associated scale and zero values. Use NDF_TYPE if you
*     need the data type of the array after it has been unscaled.

*  Arguments:
*     INDF = INTEGER (Given)
*        NDF identifier.
*     COMP = CHARACTER * ( * ) (Given)
*        Name of the NDF array component whose type is required:
*        'DATA' or 'VARIANCE'.
*     TYPE = CHARACTER * ( * ) (Returned)
*        Numeric type of the component.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If the array is not stored in SCALED form, then this routine
*     returns the same type as the ARY_TYPE routine.
*     -  A comma-separated list of component names may also be supplied
*     to this routine. In this case the result returned will be the
*     lowest precision numeric type to which all the specified
*     components can be converted without unnecessary loss of
*     information.
*     -  The symbolic constant NDF__SZTYP may be used for declaring the
*     length of a character variable which is to hold the numeric type
*     of an NDF array component. This constant is defined in the
*     include file NDF_PAR.

*  Copyright:
*     Copyright (C) 2006 Particle Physics and Astronomy Research
*     Council. All Rights Reserved.

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
*     DSB: David S Berry (JAC)
*     {enter_new_authors_here}

*  History:
*     12-JUL-2006 (DSB):
*        Original version.
*     {enter_changes_here}

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
      CALL NDF1_SCTYP( IACB, COMP, ITYPE, STATUS )

*  Translate the type code into a string and return it.
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL NDF1_CCPY( DATYP( ITYPE ), TYPE, STATUS )
      END IF

*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDF_SCTYP_ERR', 'NDF_SCTYP: Error obtaining '//
     :                 'the numeric type of a scaled NDF array '//
     :                 'component.', STATUS )
         CALL NDF1_TRACE( 'NDF_SCTYP', STATUS )
      END IF

      END
