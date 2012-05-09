      SUBROUTINE CCD1_CMUL( BAD, ITYPE, IPIN, EL, CVAL, IPOUT, STATUS )
*+
*  Name:
*     CCD1_CMUL

*  Purpose:
*     To multiply a data array of a given type by a constant.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_CMUL( BAD, ITYPE, IPIN, EL, CVAL, IPOUT, STATUS )

*  Description:
*     This routine just dummys to the appropriate CCG1_CMLT routine.

*  Arguments:
*     ITYPE = CHARACTER * ( * ) (Given)
*        The type of the data pointed to by IPOINT.
*     BAD = LOGICAL (Given and Returned)
*        Flag for BAD pixels present.
*     IPIN = INTEGER (Given and Returned)
*        Pointer to input data array.
*     EL = INTEGER (Given)
*        Number of elements in array.
*     CVAL = DOUBLE PRECISION (Given)
*        Constant to multiply.
*     IPOUT = INTEGER (Given and Returned)
*        Pointer to output array containing the modified values.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  Uses array pointers

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
*     30-APR-1991 (PDRAPER):
*        Original version.
*     29-MAY-1991 (PDRAPER):
*        Changed to return data in different array.
*     {enter_further_changes_here}

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
      DOUBLE PRECISION CVAL
      INTEGER EL

*  Arguments Given and Returned:
      INTEGER IPIN
      INTEGER IPOUT
      LOGICAL BAD

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER NERR               ! Numeric error count

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Call the appropriate subtraction routine by type.
      IF ( ITYPE .EQ. '_UBYTE' ) THEN
         CALL CCG1_CMLTUB( BAD, EL, %VAL( CNF_PVAL( IPIN ) ), CVAL,
     :                     %VAL( CNF_PVAL( IPOUT ) ),
     :                     NERR, STATUS )
      ELSE IF ( ITYPE .EQ. '_BYTE' ) THEN
         CALL CCG1_CMLTB( BAD, EL, %VAL( CNF_PVAL( IPIN ) ), CVAL,
     :                    %VAL( CNF_PVAL( IPOUT ) ),
     :                    NERR, STATUS )
      ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
         CALL CCG1_CMLTUW( BAD, EL, %VAL( CNF_PVAL( IPIN ) ), CVAL,
     :                     %VAL( CNF_PVAL( IPOUT ) ),
     :                     NERR, STATUS )
      ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
         CALL CCG1_CMLTW( BAD, EL, %VAL( CNF_PVAL( IPIN ) ), CVAL,
     :                    %VAL( CNF_PVAL( IPOUT ) ),
     :                    NERR, STATUS )
      ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
         CALL CCG1_CMLTI( BAD, EL, %VAL( CNF_PVAL( IPIN ) ), CVAL,
     :                    %VAL( CNF_PVAL( IPOUT ) ),
     :                    NERR, STATUS )
      ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
         CALL CCG1_CMLTR( BAD, EL, %VAL( CNF_PVAL( IPIN ) ), CVAL,
     :                    %VAL( CNF_PVAL( IPOUT ) ),
     :                    NERR, STATUS )
      ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
         CALL CCG1_CMLTD( BAD, EL, %VAL( CNF_PVAL( IPIN ) ), CVAL,
     :                    %VAL( CNF_PVAL( IPOUT ) ),
     :                    NERR, STATUS )
      ELSE IF ( ITYPE .EQ. '_INT64' ) THEN
         CALL CCG1_CMLTK( BAD, EL, %VAL( CNF_PVAL( IPIN ) ), CVAL,
     :                    %VAL( CNF_PVAL( IPOUT ) ),
     :                    NERR, STATUS )
      ELSE

*  Unsupported numeric type, issue error.
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'TYPE', ITYPE )
         CALL ERR_REP( 'CCD1_CMUL1',
     :   '  CCD1_CMUL: Unsupported numeric type ^TYPE', STATUS )
      END IF

*  Update BAD flag.
      BAD = BAD .OR. ( NERR .NE. 0 )

      END
* $Id$
