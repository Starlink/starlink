      SUBROUTINE NDF_MAPZ( INDF, COMP, TYPE, MMOD, RPNTR, IPNTR, EL,
     :                     STATUS )
*+
*  Name:
*     NDF_MAPZ

*  Purpose:
*     Obtain complex mapped access to an array component of an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_MAPZ( INDF, COMP, TYPE, MMOD, RPNTR, IPNTR, EL,
*     STATUS )

*  Description:
*     The routine obtains complex mapped access to an array component
*     of an NDF, returning pointers to the mapped real and imaginary
*     values and a count of the number of elements mapped.

*  Arguments:
*     INDF = INTEGER (Given)
*        NDF identifier.
*     COMP = CHARACTER * ( * ) (Given)
*        Name of the NDF array component to be mapped: 'DATA' or
*        'VARIANCE' (or 'ERROR').
*     TYPE = CHARACTER * ( * ) (Given)
*        Numeric type to be used for access (e.g.  '_REAL').
*     MMOD = CHARACTER * ( * ) (Given)
*        Mapping mode for access to the array: 'READ', 'UPDATE' or
*        'WRITE', with an optional initialisation mode '/ZERO' or
*        '/BAD' appended.
*     RPNTR( * ) = INTEGER (Returned)
*        Pointer(s) to the mapped real (i.e. non-imaginary) values (see
*        the Notes section).
*     IPNTR( * ) = INTEGER (Returned)
*        Pointer(s) to the mapped imaginary values (see the Notes
*        section).
*     EL = INTEGER (Returned)
*        Number of elements mapped.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  A comma-separated list of component names may also be given,
*     in which case the routine will map all the requested components
*     using the same data type and mapping mode. Pointers to the values
*     of these mapped components will be returned (in the specified
*     order) in the elements of the arrays RPNTR and IPNTR, which must
*     be of sufficient size to accommodate them.
*     -  Access to an NDF's QUALITY component is not available using
*     this routine.
*     -  If this routine is called with STATUS set, then a value of 1
*     will be returned for the EL argument, although no further
*     processing will occur.  The same value will also be returned if
*     the routine should fail for any reason.

*  Algorithm:
*     -  Import the NDF identifier.
*     -  Map the array component(s).
*     -  Obtain the number of mapped data elements.
*     -  If an error occurred, then report context information.
*     -  Under error conditions, return a "safe" value of EL.


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
*     12-OCT-1989 (RFWS):
*        Original, derived from the equivalent ARY_ system routine.
*     15-JAN-1990 (RFWS):
*        Added pointer arrays for use when mapping several NDF
*        components at once. Changed ACOMP argument to COMP.
*     16-JAN-1990 (RFWS):
*        Changed DPNTR to RPNTR.
*     23-JAN-1990 (RFWS):
*        Renamed from NDF_MAPZV to NDF_MAPZ.
*     4-DEC-1990 (RFWS):
*        Changed to return a "safe" value of EL under error conditions.
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

*  Global Variables:
      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_DID( NDF__MXACB ) = INTEGER (Read)
*           ARY_ system identifier for the NDF's data array.

*  Arguments Given:
      INTEGER INDF
      CHARACTER * ( * ) COMP
      CHARACTER * ( * ) TYPE
      CHARACTER * ( * ) MMOD

*  Arguments Returned:
      INTEGER RPNTR( * )
      INTEGER IPNTR( * )
      INTEGER EL

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IACB               ! Index to NDF entry in the ACB

*.

*  Check inherited global status.
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Import the NDF identifier.
         CALL NDF1_IMPID( INDF, IACB, STATUS )

*  Map the array component(s).
         CALL NDF1_MAP( IACB, COMP, TYPE, .TRUE., MMOD, RPNTR, IPNTR,
     :                  STATUS )

*  Obtain the number of mapped data elements.
         IF ( STATUS .EQ. SAI__OK ) THEN
            CALL ARY_SIZE( ACB_DID( IACB ), EL, STATUS )
         END IF

*  If an error occurred, then report context information and call the
*  error tracing routine.
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'NDF_MAPZ_ERR',
     :      'NDF_MAPZ: Error obtaining complex mapped access to an ' //
     :      'array component of an NDF.', STATUS )
            CALL NDF1_TRACE( 'NDF_MAPZ', STATUS )
         END IF
      END IF

*  Under error conditions, return a "safe" value of EL.
      IF ( STATUS .NE. SAI__OK ) THEN
         EL = 1
      END IF

      END
