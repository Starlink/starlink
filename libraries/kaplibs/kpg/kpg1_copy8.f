      SUBROUTINE KPG1_COPY8( TYPE, NEL, IPIN, IPOUT, STATUS )
*+
*  Name:
*     KPG1_COPY8

*  Purpose:
*     Copies an array of a given type to another array of the same type.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_COPY8( TYPE, NEL, IPIN, IPOUT, STATUS )

*  Description:
*     This routine copies a one-dimensional array of numerical or
*     character values from an input array to an output array.

*  Arguments:
*     TYPE = CHARACTER * ( * ) (Given)
*        The data type to be copied. Must be one of the HDS numeric
*        types, _BYTE, _UBYTE, _WORD, _UWORD, _INTEGER, _INT64, _REAL or
*         _DOUBLE, or "_CHAR*<N>)" where "<N>" is an integer giving the
*        length of the character strings.
*     NEL = INTEGER*8 (Given)
*        The number of elements in the vectorised arrays pointed to by
*        IPIN and IPOUT.
*     IPIN = INTEGER (Given)
*        Pointer to the data to be copied.
*     IPOUT = INTEGER (Given and Returned)
*        Pointer to the array to contain the copied data.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  Uses array pointers.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
*     Copyright (C) 2005 Particle Physics & Astronomy Research Council.
*     Copyright (C) 2008, 2012 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     DSB: David S. Berry (JAC, UCLan)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     8-MAY-1991 (PDRAPER):
*        Original version.
*     29-SEP-2005 (PDRAPER):
*        Converted into KAPLIBS routine.
*     21-JAN-2008 (DSB):
*        Added support for copying character arrays.
*     2012-05-09 (TIMJ):
*        Add _INT64
*     20-FEB-2020 (DSB):
*        Add support for huge arrays.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'CNF_PAR'         ! For CNF_PVAL function

*  Arguments Given:
      CHARACTER * ( * ) TYPE
      INTEGER*8 NEL
      INTEGER IPIN

*  Arguments Given and Returned:
      INTEGER IPOUT

*  Status:
      INTEGER STATUS            ! Global status

*  Local Variable:
      INTEGER*8 IERR            ! Not used
      INTEGER*8 NERR            ! Not used
      INTEGER CLEN              ! Length of character strings
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Copy data. Note BAD can be set .FALSE. or .TRUE. as that doesn't
*  matter for data of the same type. IERR and NERR can be ignored as
*  "conversion" is guaranteed.
      IF( TYPE .EQ. '_DOUBLE' ) THEN
         CALL VEC8_DTOD( .FALSE., NEL, %VAL( CNF_PVAL( IPIN ) ),
     :                  %VAL( CNF_PVAL( IPOUT ) ), IERR, NERR,
     :                  STATUS )

      ELSE IF( TYPE .EQ. '_REAL' ) THEN
         CALL VEC8_RTOR( .FALSE., NEL, %VAL( CNF_PVAL( IPIN ) ),
     :                  %VAL( CNF_PVAL( IPOUT ) ), IERR, NERR,
     :                  STATUS )

      ELSE IF( TYPE .EQ. '_INTEGER' ) THEN
         CALL VEC8_ITOI( .FALSE., NEL, %VAL( CNF_PVAL( IPIN ) ),
     :                  %VAL( CNF_PVAL( IPOUT ) ), IERR, NERR,
     :                  STATUS )

      ELSE IF( TYPE .EQ. '_INT64' ) THEN
         CALL VEC8_KTOK( .FALSE., NEL, %VAL( CNF_PVAL( IPIN ) ),
     :                  %VAL( CNF_PVAL( IPOUT ) ), IERR, NERR,
     :                  STATUS )

      ELSE IF( TYPE .EQ. '_WORD' ) THEN
         CALL VEC8_WTOW( .FALSE., NEL, %VAL( CNF_PVAL( IPIN ) ),
     :                  %VAL( CNF_PVAL( IPOUT ) ), IERR, NERR,
     :                  STATUS )

      ELSE IF( TYPE .EQ. '_UWORD' ) THEN
         CALL VEC8_UWTOUW( .FALSE., NEL, %VAL( CNF_PVAL( IPIN ) ),
     :                    %VAL( CNF_PVAL( IPOUT ) ), IERR, NERR,
     :                    STATUS )

      ELSE IF( TYPE .EQ. '_BYTE' ) THEN
         CALL VEC8_BTOB( .FALSE., NEL, %VAL( CNF_PVAL( IPIN ) ),
     :                  %VAL( CNF_PVAL( IPOUT ) ), IERR, NERR,
     :                  STATUS )

      ELSE IF( TYPE .EQ. '_UBYTE' ) THEN
         CALL VEC8_UBTOUB( .FALSE., NEL, %VAL( CNF_PVAL( IPIN ) ),
     :                    %VAL( CNF_PVAL( IPOUT ) ), IERR, NERR,
     :                    STATUS )

      ELSE IF( TYPE( : 6 ) .EQ. '_CHAR*' ) THEN
         CALL CHR_CTOI( TYPE( 7 : ), CLEN, STATUS )
         CALL KPG1_COPYC8( NEL, %VAL( CNF_PVAL( IPIN ) ),
     :                    %VAL( CNF_PVAL( IPOUT ) ), STATUS,
     :                    %VAL( CNF_CVAL( CLEN ) ),
     :                    %VAL( CNF_CVAL( CLEN ) ) )

      ELSE

*  Bad TYPE.
         STATUS = SAI__ERROR
         CALL ERR_REP( 'KPG1_COPY8',
     :   'Error copying array, bad data type (possible programming'//
     :   ' error)', STATUS )
      END IF
      END
