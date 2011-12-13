      SUBROUTINE CCD1_STVB( ITYPE, EL, IPOINT, STATUS )
*+
*  Name:
*     CCD1_STVB

*  Purpose:
*     To set an array of a given type to BAD.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_STVB( ITYPE, EL, IPOINT, STATUS )

*  Description:
*     This routine just defers typing by one level calling the
*     appropriate version of CCG1_STVx for the given type with the
*     appropriate BAD flag.

*  Arguments:
*     ITYPE = CHARACTER * ( * ) (Given)
*        The type of the data pointed to by IPOINT.
*     EL = INTEGER (Given)
*        The number of elements in to pointed to array.
*     IPOINT = INTEGER (Given and Returned)
*        Pointer to the data to be set to BAD.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  Uses pointers to arrays.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council. All
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
*     28-AUG-1991 (PDRAPER):
*        Original Version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! BAD values constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      CHARACTER * ( * ) ITYPE
      INTEGER EL

*  Arguments Given and Returned:
      INTEGER IPOINT

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Call the appropriate version of CCG1_STV to set the pointed to array
*  to BAD.
      IF ( ITYPE .EQ. '_BYTE' ) THEN
         CALL CCG1_STVB( VAL__BADB, EL, %VAL( CNF_PVAL( IPOINT ) ) ,
     :                   STATUS )
      ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
         CALL CCG1_STVUB( VAL__BADUB, EL, %VAL( CNF_PVAL( IPOINT ) ),
     :                    STATUS )
      ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
         CALL CCG1_STVW( VAL__BADW, EL, %VAL( CNF_PVAL( IPOINT ) ),
     :                   STATUS )
      ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
         CALL CCG1_STVUW( VAL__BADUW, EL, %VAL( CNF_PVAL( IPOINT ) ),
     :                    STATUS )
      ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
         CALL CCG1_STVI( VAL__BADI, EL, %VAL( CNF_PVAL( IPOINT ) ),
     :                   STATUS )
      ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
         CALL CCG1_STVR( VAL__BADR, EL, %VAL( CNF_PVAL( IPOINT ) ),
     :                   STATUS )
      ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
         CALL CCG1_STVD( VAL__BADD, EL, %VAL( CNF_PVAL( IPOINT ) ),
     :                   STATUS )
      ELSE

*  Bad ITYPE.
         STATUS = SAI__ERROR
         CALL ERR_REP( 'CCD1_STVB',
     :   '  Error setting array, bad numeric type '//
     :   '(possible programming error)', STATUS )
      END IF

      END
* $Id$
