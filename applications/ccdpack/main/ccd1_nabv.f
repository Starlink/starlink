      SUBROUTINE CCD1_NABV( ITYPE, VEC, EL, BAD, VAL, NUM, STATUS )
*+
*  Name:
*     CDC1_NABV

*  Purpose:
*     Counts the number of values above a threshold value.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CDC1_NABV( ITYPE, VEC, EL, VAL, NUM, STATUS )

*  Description:
*     This routine looks at all the values in the vectorised array
*     VEC and counts the number that have value above VAL.

*  Arguments:
*     ITYPE = CHARACTER * ( * ) (Given)
*        The HDS type of the input data. Must be one of the non-complex
*        numeric types.
*     VEC = INTEGER (Given)
*        Pointer to the vectorised array for which a count of the
*        number of values above a threshold is required.
*     EL = INTEGER (Given)
*        The number of values in array pointed to by VEC.
*     BAD = LOGICAL (Given)
*        True if BAD values are present in the input data.
*     VAL = DOUBLE PRECISION (Given)
*        The threshold value.
*     NUM = INTEGER (Returned)
*        The number of elements whose value lies above the threshold
*        value.
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
*     22-OCT-1992 (PDRAPER):
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
      CHARACTER * ( * ) ITYPE
      INTEGER EL
      INTEGER VEC
      LOGICAL BAD
      DOUBLE PRECISION VAL

*  Arguments Returned:
      INTEGER NUM

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Call the appropriate version of CCG1_NAB to count the values.
      IF ( ITYPE .EQ. '_BYTE' ) THEN
         CALL CCG1_NABB( %VAL( CNF_PVAL( VEC ) ),
     :                   EL, BAD, VAL, NUM, STATUS )
      ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
         CALL CCG1_NABUB( %VAL( CNF_PVAL( VEC ) ),
     :                    EL, BAD, VAL, NUM, STATUS )
      ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
         CALL CCG1_NABW( %VAL( CNF_PVAL( VEC ) ),
     :                   EL, BAD, VAL, NUM, STATUS )
      ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
         CALL CCG1_NABUW( %VAL( CNF_PVAL( VEC ) ),
     :                    EL, BAD, VAL, NUM, STATUS )
      ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
         CALL CCG1_NABI( %VAL( CNF_PVAL( VEC ) ),
     :                   EL, BAD, VAL, NUM, STATUS )
      ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
         CALL CCG1_NABR( %VAL( CNF_PVAL( VEC ) ),
     :                   EL, BAD, VAL, NUM, STATUS )
      ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
         CALL CCG1_NABD( %VAL( CNF_PVAL( VEC ) ),
     :                   EL, BAD, VAL, NUM, STATUS )
      ELSE

*  Unsupported data type.
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'TYPE', ITYPE )
         CALL ERR_REP( 'CCD1_NABV',
     :   '  CCD1_NABV: Unsupported data type (^TYPE).', STATUS )

      END IF
      END
* $Id$
