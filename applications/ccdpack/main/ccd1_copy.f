      SUBROUTINE CCD1_COPY( PTYPE, DIM, IPIN, IPOUT, STATUS )
*+
*  Name:
*     CCD1_COPY

*  Purpose:
*     To copy the data of type PTYPE from IPIN to IPOUT.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_COPY( PTYPE, DIM, IPIN, IPOUT, STATUS )

*  Description:
*     This routine calls the appropriate version of CCG1_COPA
*     to copy the data pointed to by IPIN to the array pointed to by
*     IPOUT.

*  Arguments:
*     PTYPE = CHARACTER * ( * ) (Given)
*        The data type to be copied; must be one of the HDS numeric
*        types excluding Complex types.
*     DIM = INTEGER (Given)
*        The size of the (vectorised) array pointed to by IPIN and
*        IPOUT.
*     IPIN = INTEGER (Given)
*        Pointer to the data to be copied.
*     IPOUT = INTEGER (Given and Returned)
*        Pointer to the array to contain the copied data.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  Uses array pointers.

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
*     8-MAY-1991 (PDRAPER):
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
      CHARACTER * ( * ) PTYPE
      INTEGER DIM
      INTEGER IPIN

*  Arguments Given and Returned:
      INTEGER IPOUT

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Call the appropriate routine, and copy the data.
      IF (  PTYPE .EQ. '_BYTE' ) THEN
         CALL CCG1_COPAB( DIM, %VAL( CNF_PVAL( IPIN ) ) ,
     :                    %VAL( CNF_PVAL( IPOUT ) ), STATUS )
      ELSE IF (  PTYPE .EQ. '_UBYTE' ) THEN
         CALL CCG1_COPAUB( DIM, %VAL( CNF_PVAL( IPIN ) ) ,
     :                     %VAL( CNF_PVAL( IPOUT ) ), STATUS )
      ELSE IF (  PTYPE .EQ. '_WORD' ) THEN
         CALL CCG1_COPAW( DIM, %VAL( CNF_PVAL( IPIN ) ) ,
     :                    %VAL( CNF_PVAL( IPOUT ) ), STATUS )
      ELSE IF (  PTYPE .EQ. '_UWORD' ) THEN
         CALL CCG1_COPAUW( DIM, %VAL( CNF_PVAL( IPIN ) ) ,
     :                     %VAL( CNF_PVAL( IPOUT ) ), STATUS )
      ELSE IF (  PTYPE .EQ. '_INTEGER' ) THEN
         CALL CCG1_COPAI( DIM, %VAL( CNF_PVAL( IPIN ) ) ,
     :                    %VAL( CNF_PVAL( IPOUT ) ), STATUS )
      ELSE IF (  PTYPE .EQ. '_REAL' ) THEN
         CALL CCG1_COPAR( DIM, %VAL( CNF_PVAL( IPIN ) ) ,
     :                    %VAL( CNF_PVAL( IPOUT ) ), STATUS )
      ELSE IF (  PTYPE .EQ. '_DOUBLE' ) THEN
         CALL CCG1_COPAD( DIM, %VAL( CNF_PVAL( IPIN ) ) ,
     :                    %VAL( CNF_PVAL( IPOUT ) ), STATUS )
      ELSE

*  Bad PTYPE.
         STATUS = SAI__ERROR
         CALL ERR_REP( 'CCD1_COPY',
     :   'Error copying array, bad numeric type (possible programming'//
     :   ' error)', STATUS )
      END IF

      END
* $Id$
