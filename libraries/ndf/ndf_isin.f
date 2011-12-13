      SUBROUTINE NDF_ISIN( INDF1, INDF2, ISIN, STATUS )
*+
*  Name:
*     NDF_ISIN

*  Purpose:
*     See if one NDF is contained within another NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_ISIN( INDF1, INDF2, ISIN, STATUS )

*  Description:
*     The routine returns a logical flag indicating if the first supplied
*     NDF is contained within an extension of the second supplied NDF.
*     The search is recursive, so for instance a true value will be
*     returned if the second supplied NDF is contained within an extension
*     of an intermediate NDF that is contained within an extension of the
*     first supplied NDF.

*  Arguments:
*     INDF1 = INTEGER (Given)
*        The first NDF.
*     INDF2 = INTEGER (Given)
*        The second NDF.
*     ISIN = LOGICAL (Returned)
*        .TRUE. of the first NDF is contained within an extension of the
*        second NDF, and .FALSE. otherwise.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  A .TRUE. value is returned if the two supplied NDF identifiers
*     refer to the same base NDF.
*     -  If an identifier for an NDF section is supplied to this
*     routine, then the search will be applied to the associated
*     base NDF.
*     -  If this routine is called with STATUS set, then a value of
*     .FALSE. will be returned for the ISIN argument, although no
*     further processing will occur. The same value will also be
*     returned if the routine should fail for any reason.

*  Copyright:
*     Copyright (C) 2010 Science & Technology Facilities Council.
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
*     DSB: David S Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     5-JAN-2010 (DSB):
*        Original version.
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
      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_LOC( NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Data object locator.

      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_IDCB( NDF__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.

*  Arguments Given:
      INTEGER INDF1
      INTEGER INDF2

*  Arguments Returned:
      LOGICAL ISIN

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string

*  Local Variables:
      CHARACTER FILE1*( NDF__SZFIL )! Path of 1st NDF container file
      CHARACTER FILE2*( NDF__SZFIL )! Path of 2nd NDF container file
      CHARACTER PATH1*( NDF__SZPTH )! Path of 1st NDF within container file
      CHARACTER PATH2*( NDF__SZPTH )! Path of 2nd NDF within container file
      INTEGER IACB1              ! Index to 1st NDF entry in the ACB
      INTEGER IACB2              ! Index to 2nd NDF entry in the ACB
      INTEGER NLEV1              ! Depth of 1st NDF within container file
      INTEGER NLEV2              ! Depth of 2nd NDF within container file
      INTEGER PLEN2              ! Used length of PATH2
*.

*  Set an initial value for the returned ISIN flag.
      ISIN = .FALSE.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the NDF identifiers.
      CALL NDF1_IMPID( INDF1, IACB1, STATUS )
      CALL NDF1_IMPID( INDF2, IACB2, STATUS )

*  For each NDF, obtain an index to the data object entry in the DCB
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Obtain HDS locators for the two data object entries in the DCB, and
*  get the paths to the container files, and the paths to the objects
*  within the container files.
         CALL HDS_TRACE( DCB_LOC( ACB_IDCB( IACB1 ) ), NLEV1, PATH1,
     :                   FILE1, STATUS )
         CALL HDS_TRACE( DCB_LOC( ACB_IDCB( IACB2 ) ), NLEV2, PATH2,
     :                   FILE2, STATUS )

*  Check that the container files are the same, and that the depth of the
*  second NDF is less than or equal to the depth of the first NDF.
         IF( FILE1 .EQ. FILE2 .AND. NLEV1 .GE. NLEV2 ) THEN

*  See if the HDS path for the first NDF starts with the HDS path for
*  the second NDF. If so, the NDFs are equal or the first is a child of
*  the second.
            PLEN2 = CHR_LEN( PATH2 )
            ISIN = ( PATH1( : PLEN2 ) .EQ. PATH2( : PLEN2 ) )

         END IF
      END IF

*  If an error occurred, then return .FALSE.
      IF ( STATUS .NE. SAI__OK ) THEN
         ISIN = .FALSE.

*  Report context information and call the error tracing routine.
         CALL ERR_REP( 'NDF_ISIN_ERR', 'NDF_ISIN: Error checking if '//
     :                 'an NDF is contained within another NDF.',
     :                 STATUS )
         CALL NDF1_TRACE( 'NDF_ISIN', STATUS )
      END IF

      END
