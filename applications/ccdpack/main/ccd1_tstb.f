      SUBROUTINE CCD1_TSTB( TYPE, IPDATA, NEL, ALLBAD, STATUS )
*+
*  Name:
*     CCD1_TSTB

*  Purpose:
*     To test if all the elements of an array of a given type
*     are set BAD.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_TSTB( TYPE, ARRAY, NEL, ALLBAD, STATUS )

*  Description:
*     The routines tests all elements of the array ARRAY to see if they
*     are all BAD. If they are all BAD then the ALLBAD flag is set TRUE
*     , otherwise the flag is set FALSE.

*  Arguments:
*     TYPE = CHARACTER * ( * ) (Given)
*        The type of the input data - one of the HDS non-complex
*        numeric types.
*     IPDATA = INTEGER (Given)
*        Pointer to data to be tested for BADness.
*     NEL = INTEGER (Given)
*        Number of elements of data pointed to by IPDATA
*     ALLBAD = LOGICAL (Returned)
*        Set true if all elements are bad otherwise false.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council. All
*     Rights Reserved.

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
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     10-JAN-1992 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      CHARACTER * ( * ) TYPE
      INTEGER IPDATA
      INTEGER NEL

*  Arguments Returned:
      LOGICAL ALLBAD

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Call the appropriate subroutine for the given type to perform the
*  test.
      IF ( TYPE .EQ. '_UBYTE' ) THEN
         CALL CCG1_TBUB( %VAL( CNF_PVAL( IPDATA ) ),
     :                   NEL, ALLBAD, STATUS )
      ELSE IF ( TYPE .EQ. '_BYTE' ) THEN
         CALL CCG1_TBB( %VAL( CNF_PVAL( IPDATA ) ),
     :                  NEL, ALLBAD, STATUS )
      ELSE IF ( TYPE .EQ. '_UWORD' ) THEN
         CALL CCG1_TBUW( %VAL( CNF_PVAL( IPDATA ) ),
     :                   NEL, ALLBAD, STATUS )
      ELSE IF ( TYPE .EQ. '_WORD' ) THEN
         CALL CCG1_TBW( %VAL( CNF_PVAL( IPDATA ) ),
     :                  NEL, ALLBAD, STATUS )
      ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN
         CALL CCG1_TBI( %VAL( CNF_PVAL( IPDATA ) ),
     :                  NEL, ALLBAD, STATUS )
      ELSE IF ( TYPE .EQ. '_REAL' ) THEN
         CALL CCG1_TBR( %VAL( CNF_PVAL( IPDATA ) ),
     :                  NEL, ALLBAD, STATUS )
      ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
         CALL CCG1_TBD( %VAL( CNF_PVAL( IPDATA ) ),
     :                  NEL, ALLBAD, STATUS )
      ELSE IF ( TYPE .EQ. '_INT64' ) THEN
         CALL CCG1_TBK( %VAL( CNF_PVAL( IPDATA ) ),
     :                  NEL, ALLBAD, STATUS )
      ELSE

*  Unsupported numeric type, issue error.
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'TYPE', TYPE )
         CALL ERR_REP( 'CCD1_TSTB1',
     :   '  CCD1_TSTB: Unsupported numeric type ^TYPE', STATUS )
      END IF

      END
* $Id$
