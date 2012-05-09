
      SUBROUTINE CCD1_MASKD( PTYPE, NOQUAL, IPMASK, IPOINT, EL, BBYTE1,
     :                       STATUS )
*+
*  Name:
*     CCD1_MASKD

*  Purpose:
*     To do the actual application of a mask of BAD pixels, or to
*     transfer the BAD value information to a quality array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_MASKD( PTYPE, NOQUAL, IPMASK, IPOINT, EL, BBYTE1,
*                      STATUS )

*  Description:
*     This routine is a convient dummy to conceal the typing of the
*     MASK A-task functionality

*  Arguments:
*     PTYPE = CHARACTER * ( * ) (Given)
*        The type at which the data will be processed.
*     NOQUAL = LOGICAL (Given)
*        Whether quality is to be used or not.
*     IPMASK = INTEGER (Given)
*        Pointer to mask frame.
*     IPOINT = INTEGER (Given and Returned)
*        Pointer to data which will have its values set BAD (only if
*        NOQUAL is true).
*     EL = INTEGER (Given)
*        Size of the data arrays.
*     BBYTE1 = BYTE (Given)
*        BADBITS value for quality component.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  Uses BYTE values.
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
      CHARACTER * ( * ) PTYPE
      LOGICAL NOQUAL
      INTEGER IPMASK
      INTEGER EL
      BYTE BBYTE1

*  Arguments Given and Returned:
      INTEGER IPOINT

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Right now transfer the 'BAD' pixel data to from the mask NDF to the
*  new NDF. Have two basic situations, setting the quality component to
*  include the BBYTE1 value, or setting pixels BAD if the mask pixels
*  are BAD.
      IF ( NOQUAL ) THEN

*  Have both data components.
         IF ( PTYPE .EQ. '_UBYTE') THEN
             CALL CCG1_CPBUB( %VAL( CNF_PVAL( IPMASK ) ),
     :                        %VAL( CNF_PVAL( IPOINT ) ), EL,
     :                        STATUS )
         ELSE IF ( PTYPE .EQ. '_BYTE' ) THEN
             CALL CCG1_CPBB( %VAL( CNF_PVAL( IPMASK ) ),
     :                       %VAL( CNF_PVAL( IPOINT ) ), EL,
     :                       STATUS )
         ELSE IF ( PTYPE .EQ. '_WORD' ) THEN
             CALL CCG1_CPBW( %VAL( CNF_PVAL( IPMASK )),
     :                       %VAL( CNF_PVAL( IPOINT ) ), EL,
     :                       STATUS )
         ELSE IF ( PTYPE .EQ. '_UWORD' ) THEN
             CALL CCG1_CPBUW( %VAL( CNF_PVAL( IPMASK ) ),
     :                        %VAL( CNF_PVAL( IPOINT ) ), EL,
     :                        STATUS )
         ELSE IF ( PTYPE .EQ. '_INTEGER' ) THEN
             CALL CCG1_CPBI( %VAL( CNF_PVAL( IPMASK ) ),
     :                       %VAL( CNF_PVAL( IPOINT ) ), EL,
     :                       STATUS )
         ELSE IF ( PTYPE .EQ. '_REAL' ) THEN
             CALL CCG1_CPBR( %VAL( CNF_PVAL( IPMASK ) ),
     :                       %VAL( CNF_PVAL( IPOINT ) ), EL,
     :                       STATUS )
         ELSE IF ( PTYPE .EQ. '_DOUBLE' ) THEN
             CALL CCG1_CPBD( %VAL( CNF_PVAL( IPMASK ) ),
     :                       %VAL( CNF_PVAL( IPOINT )), EL,
     :                       STATUS )
         ELSE IF ( PTYPE .EQ. '_INT64' ) THEN
             CALL CCG1_CPBK( %VAL( CNF_PVAL( IPMASK ) ),
     :                       %VAL( CNF_PVAL( IPOINT )), EL,
     :                       STATUS )
         END IF
      ELSE

*  Have a quality array to set BAD.
         IF ( PTYPE .EQ. '_UBYTE') THEN
             CALL CCG1_SQBUB( %VAL( CNF_PVAL( IPMASK ) ),
     :                        %VAL( CNF_PVAL( IPOINT ) ), EL ,
     :                        BBYTE1, STATUS )
         ELSE IF ( PTYPE .EQ. '_BYTE' ) THEN
             CALL CCG1_SQBB( %VAL( CNF_PVAL( IPMASK )),
     :                       %VAL( CNF_PVAL( IPOINT ) ), EL ,
     :                       BBYTE1, STATUS )
         ELSE IF ( PTYPE .EQ. '_WORD' ) THEN
             CALL CCG1_SQBW( %VAL( CNF_PVAL( IPMASK ) ),
     :                       %VAL( CNF_PVAL( IPOINT ) ), EL ,
     :                       BBYTE1, STATUS )
         ELSE IF ( PTYPE .EQ. '_UWORD' ) THEN
             CALL CCG1_SQBUW( %VAL( CNF_PVAL( IPMASK ) ),
     :                        %VAL( CNF_PVAL( IPOINT ) ), EL ,
     :                        BBYTE1, STATUS )
         ELSE IF ( PTYPE .EQ. '_INTEGER' ) THEN
             CALL CCG1_SQBI( %VAL( CNF_PVAL( IPMASK ) ),
     :                       %VAL( CNF_PVAL( IPOINT ) ), EL ,
     :                       BBYTE1, STATUS )
         ELSE IF ( PTYPE .EQ. '_REAL' ) THEN
             CALL CCG1_SQBR( %VAL( CNF_PVAL( IPMASK ) ),
     :                       %VAL( CNF_PVAL( IPOINT ) ), EL ,
     :                       BBYTE1, STATUS )
         ELSE IF ( PTYPE .EQ. '_DOUBLE' ) THEN
             CALL CCG1_SQBD( %VAL( CNF_PVAL( IPMASK ) ),
     :                       %VAL( CNF_PVAL( IPOINT ) ), EL ,
     :                       BBYTE1, STATUS )
         ELSE IF ( PTYPE .EQ. '_INT64' ) THEN
             CALL CCG1_SQBK( %VAL( CNF_PVAL( IPMASK ) ),
     :                       %VAL( CNF_PVAL( IPOINT ) ), EL ,
     :                       BBYTE1, STATUS )
         END IF
      END IF

      END
* $Id$
