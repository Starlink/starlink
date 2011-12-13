      SUBROUTINE CCD1_COSUB( ITYPE, BAD, IPOINT, EL, CVAL, IPWORK,
     :                       STATUS )
*+
*  Name:
*     CCD1_COSUB

*  Purpose:
*     To subtract a constant from a data array returning the result in
*     the same array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_COSUB( ITYPE, BAD, IPOINT, EL, CVAL, IPWORK, STATUS )

*  Description:
*     This routine just dummys to the appropriate KPG1_CSUB routine,
*     and copies the result back into the IPOINT array.

*  Arguments:
*     ITYPE = CHARACTER * ( * ) (Given)
*        The type of the data pointed to by IPOINT.
*     BAD = LOGICAL (Given and Returned)
*        Flag for BAD pixels present.
*     IPOINT = INTEGER (Given and Returned)
*        Pointer to data array.
*     EL = INTEGER (Given)
*        Number of elements in array.
*     CVAL = DOUBLE PRECISION (Given)
*        Constant to subtract.
*     IPWORK = INTEGER (Given and Returned)
*        Pointer to workspace (same size as input array).
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
      DOUBLE PRECISION CVAL
      INTEGER EL

*  Arguments Given and Returned:
      INTEGER IPOINT
      INTEGER IPWORK
      LOGICAL BAD

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER NERR               ! Numeric error count

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Call the appropriate subtraction routine by type. Copy the result to
*  the  first array.
      IF ( ITYPE .EQ. '_UBYTE' ) THEN
         CALL KPG1_CSUBUB( BAD, EL, %VAL( CNF_PVAL( IPOINT ) ), CVAL,
     :                     %VAL( CNF_PVAL( IPWORK ) ), NERR, STATUS )
         CALL CCG1_COPAUB( EL, %VAL( CNF_PVAL( IPWORK ) ),
     :                     %VAL( CNF_PVAL( IPOINT ) ), STATUS )
      ELSE IF ( ITYPE .EQ. '_BYTE' ) THEN
         CALL KPG1_CSUBB( BAD, EL, %VAL( CNF_PVAL( IPOINT ) ), CVAL,
     :                    %VAL( CNF_PVAL( IPWORK ) ),
     :                    NERR, STATUS )
         CALL CCG1_COPAB( EL, %VAL( CNF_PVAL( IPWORK ) ),
     :                    %VAL( CNF_PVAL( IPOINT ) ), STATUS )
      ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
         CALL KPG1_CSUBUW( BAD, EL, %VAL( CNF_PVAL( IPOINT ) ), CVAL,
     :                     %VAL( CNF_PVAL( IPWORK ) ), NERR, STATUS )
         CALL CCG1_COPAUW( EL, %VAL( CNF_PVAL( IPWORK ) ),
     :                     %VAL( CNF_PVAL( IPOINT ) ), STATUS )
      ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
         CALL KPG1_CSUBW( BAD, EL, %VAL( CNF_PVAL( IPOINT ) ), CVAL,
     :                    %VAL( CNF_PVAL( IPWORK ) ),
     :                    NERR, STATUS )
         CALL CCG1_COPAW( EL, %VAL( CNF_PVAL( IPWORK ) ),
     :                    %VAL( CNF_PVAL( IPOINT ) ), STATUS )
      ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
         CALL KPG1_CSUBI( BAD, EL, %VAL( CNF_PVAL( IPOINT ) ), CVAL,
     :                    %VAL( CNF_PVAL( IPWORK ) ),
     :                    NERR, STATUS )
         CALL CCG1_COPAI( EL, %VAL( CNF_PVAL( IPWORK ) ),
     :                    %VAL( CNF_PVAL( IPOINT ) ), STATUS )
      ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
         CALL KPG1_CSUBR( BAD, EL, %VAL( CNF_PVAL( IPOINT ) ), CVAL,
     :                    %VAL( CNF_PVAL( IPWORK ) ),
     :                    NERR, STATUS )
         CALL CCG1_COPAR( EL, %VAL( CNF_PVAL( IPWORK ) ),
     :                    %VAL( CNF_PVAL( IPOINT ) ), STATUS )
      ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
         CALL KPG1_CSUBD( BAD, EL, %VAL( CNF_PVAL( IPOINT ) ), CVAL,
     :                    %VAL( CNF_PVAL( IPWORK ) ),
     :                    NERR, STATUS )
         CALL CCG1_COPAD( EL, %VAL( CNF_PVAL( IPWORK ) ),
     :                    %VAL( CNF_PVAL( IPOINT ) ), STATUS )
      ELSE

*  Unsupported numeric type, issue error.
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'TYPE', ITYPE )
         CALL ERR_REP( 'CCD1_COSUB1',
     :   '  CCD1_COSUB: Unsupported numeric type ^TYPE', STATUS )
      END IF
      BAD = BAD .OR. ( NERR .NE. 0 )

      END
* $Id$
