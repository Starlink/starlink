      SUBROUTINE IMG1_CPY( IPIN, TYPEI, N, TYPEO, IPOUT, STATUS )
*+
* Name:
*    IMG1_CPY

*  Purpose:
*     Copys an array of data of a given type into another array of given
*     type.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IMG1_CPY( IPIN, TYPEI, N, TYPEO, IPOUT, STATUS )

*  Description:
*     This routine copies a vectorised array of data of one type into
*     an array of data of another type. It uses the VEC facilities to
*     perform appropriate format conversion.

*  Arguments:
*     IPIN = INTEGER (Given)
*        Pointer to data to be copied.
*     TYPEI = CHARACTER * ( * ) (Given)
*        The data type of the input data.
*     N = INTEGER (Given)
*        Size of the data to be copied.
*     TYPEO = CHARACTER * ( * ) (Given)
*        The data type of the output array.
*     IPOUT = INTEGER (Given)
*        Pointer to memory to copy input data to.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
*     Copyright (C) 1999 Central Laboratory of the Research Councils.
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
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     16-NOV-1994 (PDRAPER):
*        Original version.
*     20-APR-1999 (PDRAPER):
*        Modified to use CNF_PVAL to deference C memory pointers.
*     {enter_changes_here}

*  Bugs:
*     -  {description_of_bug}
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'IMG_ERR'         ! IMG_ error codes
      INCLUDE 'CNF_PAR'         ! CNF parameters

*  Arguments Given:
      INTEGER IPIN
      CHARACTER * ( * ) TYPEI
      INTEGER N
      CHARACTER * ( * ) TYPEO
      INTEGER IPOUT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      LOGICAL BAD               ! Whether to check for BAD pixels
      PARAMETER ( BAD = .TRUE. )

*  Local Variables:
      INTEGER IERR              ! Position of first conversion error
      INTEGER NERR              ! Number of conversion errors
      LOGICAL TYPEOK            ! Whether the TYPE argument is valid

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Trap any problems with the data types.
      TYPEOK = .TRUE.

*  Set a mark on the error stack to trap any conversion errors.
      CALL ERR_MARK

*  Test for each valid input data type in turn and call the appropriate
*  conversion routine.
      IF ( TYPEI .EQ. '_BYTE' ) THEN
         IF ( TYPEO .EQ. '_BYTE' ) THEN
            CALL VEC_BTOB( BAD, N, %VAL( CNF_PVAL( IPIN ) ),
     :                     %VAL( CNF_PVAL( IPOUT ) ), IERR,
     :                     NERR, STATUS )

         ELSE IF ( TYPEO .EQ. '_UBYTE' ) THEN
            CALL VEC_BTOUB( BAD, N, %VAL( CNF_PVAL( IPIN ) ),
     :                      %VAL( CNF_PVAL( IPOUT ) ), IERR,
     :                      NERR, STATUS )

         ELSE IF ( TYPEO .EQ. '_DOUBLE' ) THEN
            CALL VEC_BTOD( BAD, N, %VAL( CNF_PVAL( IPIN ) ),
     :                     %VAL( CNF_PVAL( IPOUT ) ), IERR,
     :                     NERR, STATUS )

         ELSE IF ( TYPEO .EQ. '_INTEGER' ) THEN
            CALL VEC_BTOI( BAD, N, %VAL( CNF_PVAL( IPIN ) ),
     :                     %VAL( CNF_PVAL( IPOUT ) ), IERR,
     :                     NERR, STATUS )

         ELSE IF ( TYPEO .EQ. '_REAL' ) THEN
            CALL VEC_BTOR( BAD, N, %VAL( CNF_PVAL( IPIN ) ),
     :                     %VAL( CNF_PVAL( IPOUT ) ), IERR,
     :                     NERR, STATUS )

         ELSE IF ( TYPEO .EQ. '_WORD' ) THEN
            CALL VEC_BTOW( BAD, N, %VAL( CNF_PVAL( IPIN ) ),
     :                     %VAL( CNF_PVAL( IPOUT ) ), IERR,
     :                     NERR, STATUS )

         ELSE IF ( TYPEO .EQ. '_UWORD' ) THEN
            CALL VEC_BTOUW( BAD, N, %VAL( CNF_PVAL( IPIN ) ),
     :                      %VAL( CNF_PVAL( IPOUT ) ), IERR,
     :                      NERR, STATUS )

*  Note if the data type specified is not valid.
         ELSE
            TYPEOK = .FALSE.
            TYPEOK = .FALSE.
         END IF

      ELSE IF ( TYPEI .EQ. '_UBYTE' ) THEN
         IF ( TYPEO .EQ. '_BYTE' ) THEN
            CALL VEC_UBTOB( BAD, N, %VAL( CNF_PVAL( IPIN ) ),
     :                      %VAL( CNF_PVAL( IPOUT ) ), IERR,
     :                      NERR, STATUS )

         ELSE IF ( TYPEO .EQ. '_UBYTE' ) THEN
            CALL VEC_UBTOUB( BAD, N, %VAL( CNF_PVAL( IPIN ) ),
     :                       %VAL( CNF_PVAL( IPOUT ) ), IERR,
     :                       NERR, STATUS )

         ELSE IF ( TYPEO .EQ. '_DOUBLE' ) THEN
            CALL VEC_UBTOD( BAD, N, %VAL( CNF_PVAL( IPIN ) ),
     :                      %VAL( CNF_PVAL( IPOUT ) ), IERR,
     :                      NERR, STATUS )

         ELSE IF ( TYPEO .EQ. '_INTEGER' ) THEN
            CALL VEC_UBTOI( BAD, N, %VAL( CNF_PVAL( IPIN ) ),
     :                      %VAL( CNF_PVAL( IPOUT ) ), IERR,
     :                      NERR, STATUS )

         ELSE IF ( TYPEO .EQ. '_REAL' ) THEN
            CALL VEC_UBTOR( BAD, N, %VAL( CNF_PVAL( IPIN ) ),
     :                      %VAL( CNF_PVAL( IPOUT ) ), IERR,
     :                      NERR, STATUS )

         ELSE IF ( TYPEO .EQ. '_WORD' ) THEN
            CALL VEC_UBTOW( BAD, N, %VAL( CNF_PVAL( IPIN ) ),
     :                      %VAL( CNF_PVAL( IPOUT ) ), IERR,
     :                      NERR, STATUS )

         ELSE IF ( TYPEO .EQ. '_UWORD' ) THEN
            CALL VEC_UBTOUW( BAD, N, %VAL( CNF_PVAL( IPIN ) ),
     :                       %VAL( CNF_PVAL( IPOUT ) ), IERR,
     :                       NERR, STATUS )

*  Note if the data type specified is not valid.
         ELSE
            TYPEOK = .FALSE.
            TYPEOK = .FALSE.
         END IF

      ELSE IF ( TYPEI .EQ. '_DOUBLE' ) THEN
         IF ( TYPEO .EQ. '_BYTE' ) THEN
            CALL VEC_DTOB( BAD, N, %VAL( CNF_PVAL( IPIN ) ),
     :                     %VAL( CNF_PVAL( IPOUT ) ), IERR,
     :                     NERR, STATUS )

         ELSE IF ( TYPEO .EQ. '_UBYTE' ) THEN
            CALL VEC_DTOUB( BAD, N, %VAL( CNF_PVAL( IPIN ) ),
     :                      %VAL( CNF_PVAL( IPOUT ) ), IERR,
     :                      NERR, STATUS )

         ELSE IF ( TYPEO .EQ. '_DOUBLE' ) THEN
            CALL VEC_DTOD( BAD, N, %VAL( CNF_PVAL( IPIN ) ),
     :                     %VAL( CNF_PVAL( IPOUT ) ), IERR,
     :                     NERR, STATUS )

         ELSE IF ( TYPEO .EQ. '_INTEGER' ) THEN
            CALL VEC_DTOI( BAD, N, %VAL( CNF_PVAL( IPIN ) ),
     :                     %VAL( CNF_PVAL( IPOUT ) ), IERR,
     :                     NERR, STATUS )

         ELSE IF ( TYPEO .EQ. '_REAL' ) THEN
            CALL VEC_DTOR( BAD, N, %VAL( CNF_PVAL( IPIN ) ),
     :                     %VAL( CNF_PVAL( IPOUT ) ), IERR,
     :                     NERR, STATUS )

         ELSE IF ( TYPEO .EQ. '_WORD' ) THEN
            CALL VEC_DTOW( BAD, N, %VAL( CNF_PVAL( IPIN ) ),
     :                     %VAL( CNF_PVAL( IPOUT ) ), IERR,
     :                     NERR, STATUS )

         ELSE IF ( TYPEO .EQ. '_UWORD' ) THEN
            CALL VEC_DTOUW( BAD, N, %VAL( CNF_PVAL( IPIN ) ),
     :                      %VAL( CNF_PVAL( IPOUT ) ), IERR,
     :                      NERR, STATUS )

*  Note if the data type specified is not valid.
         ELSE
            TYPEOK = .FALSE.
            TYPEOK = .FALSE.
         END IF

      ELSE IF ( TYPEI .EQ. '_INTEGER' ) THEN
         IF ( TYPEO .EQ. '_BYTE' ) THEN
            CALL VEC_ITOB( BAD, N, %VAL( CNF_PVAL( IPIN ) ),
     :                     %VAL( CNF_PVAL( IPOUT ) ), IERR,
     :                     NERR, STATUS )

         ELSE IF ( TYPEO .EQ. '_UBYTE' ) THEN
            CALL VEC_ITOUB( BAD, N, %VAL( CNF_PVAL( IPIN ) ),
     :                      %VAL( CNF_PVAL( IPOUT ) ), IERR,
     :                      NERR, STATUS )

         ELSE IF ( TYPEO .EQ. '_DOUBLE' ) THEN
            CALL VEC_ITOD( BAD, N, %VAL( CNF_PVAL( IPIN ) ),
     :                     %VAL( CNF_PVAL( IPOUT ) ), IERR,
     :                     NERR, STATUS )

         ELSE IF ( TYPEO .EQ. '_INTEGER' ) THEN
            CALL VEC_ITOI( BAD, N, %VAL( CNF_PVAL( IPIN ) ),
     :                     %VAL( CNF_PVAL( IPOUT ) ), IERR,
     :                     NERR, STATUS )

         ELSE IF ( TYPEO .EQ. '_REAL' ) THEN
            CALL VEC_ITOR( BAD, N, %VAL( CNF_PVAL( IPIN ) ),
     :                     %VAL( CNF_PVAL( IPOUT ) ), IERR,
     :                     NERR, STATUS )

         ELSE IF ( TYPEO .EQ. '_WORD' ) THEN
            CALL VEC_ITOW( BAD, N, %VAL( CNF_PVAL( IPIN ) ),
     :                     %VAL( CNF_PVAL( IPOUT ) ), IERR,
     :                     NERR, STATUS )

         ELSE IF ( TYPEO .EQ. '_UWORD' ) THEN
            CALL VEC_ITOUW( BAD, N, %VAL( CNF_PVAL( IPIN ) ),
     :                      %VAL( CNF_PVAL( IPOUT ) ), IERR,
     :                      NERR, STATUS )

*  Note if the data type specified is not valid.
         ELSE
            TYPEOK = .FALSE.
            TYPEOK = .FALSE.
         END IF

      ELSE IF ( TYPEI .EQ. '_REAL' ) THEN
         IF ( TYPEO .EQ. '_BYTE' ) THEN
            CALL VEC_RTOB( BAD, N, %VAL( CNF_PVAL( IPIN ) ),
     :                     %VAL( CNF_PVAL( IPOUT ) ), IERR,
     :                     NERR, STATUS )

         ELSE IF ( TYPEO .EQ. '_UBYTE' ) THEN
            CALL VEC_RTOUB( BAD, N, %VAL( CNF_PVAL( IPIN ) ),
     :                      %VAL( CNF_PVAL( IPOUT ) ), IERR,
     :                      NERR, STATUS )

         ELSE IF ( TYPEO .EQ. '_DOUBLE' ) THEN
            CALL VEC_RTOD( BAD, N, %VAL( CNF_PVAL( IPIN ) ),
     :                     %VAL( CNF_PVAL( IPOUT ) ), IERR,
     :                     NERR, STATUS )

         ELSE IF ( TYPEO .EQ. '_INTEGER' ) THEN
            CALL VEC_RTOI( BAD, N, %VAL( CNF_PVAL( IPIN ) ),
     :                     %VAL( CNF_PVAL( IPOUT ) ), IERR,
     :                     NERR, STATUS )

         ELSE IF ( TYPEO .EQ. '_REAL' ) THEN
            CALL VEC_RTOR( BAD, N, %VAL( CNF_PVAL( IPIN ) ),
     :                     %VAL( CNF_PVAL( IPOUT ) ), IERR,
     :                     NERR, STATUS )

         ELSE IF ( TYPEO .EQ. '_WORD' ) THEN
            CALL VEC_RTOW( BAD, N, %VAL( CNF_PVAL( IPIN ) ),
     :                     %VAL( CNF_PVAL( IPOUT ) ), IERR,
     :                     NERR, STATUS )

         ELSE IF ( TYPEO .EQ. '_UWORD' ) THEN
            CALL VEC_RTOUW( BAD, N, %VAL( CNF_PVAL( IPIN ) ),
     :                      %VAL( CNF_PVAL( IPOUT ) ), IERR,
     :                      NERR, STATUS )

*  Note if the data type specified is not valid.
         ELSE
            TYPEOK = .FALSE.
            TYPEOK = .FALSE.
         END IF

      ELSE IF ( TYPEI .EQ. '_WORD' ) THEN
         IF ( TYPEO .EQ. '_BYTE' ) THEN
            CALL VEC_WTOB( BAD, N, %VAL( CNF_PVAL( IPIN ) ),
     :                     %VAL( CNF_PVAL( IPOUT ) ), IERR,
     :                     NERR, STATUS )

         ELSE IF ( TYPEO .EQ. '_UBYTE' ) THEN
            CALL VEC_WTOUB( BAD, N, %VAL( CNF_PVAL( IPIN ) ),
     :                      %VAL( CNF_PVAL( IPOUT ) ), IERR,
     :                      NERR, STATUS )

         ELSE IF ( TYPEO .EQ. '_DOUBLE' ) THEN
            CALL VEC_WTOD( BAD, N, %VAL( CNF_PVAL( IPIN ) ),
     :                     %VAL( CNF_PVAL( IPOUT ) ), IERR,
     :                     NERR, STATUS )

         ELSE IF ( TYPEO .EQ. '_INTEGER' ) THEN
            CALL VEC_WTOI( BAD, N, %VAL( CNF_PVAL( IPIN ) ),
     :                     %VAL( CNF_PVAL( IPOUT ) ), IERR,
     :                     NERR, STATUS )

         ELSE IF ( TYPEO .EQ. '_REAL' ) THEN
            CALL VEC_WTOR( BAD, N, %VAL( CNF_PVAL( IPIN ) ),
     :                     %VAL( CNF_PVAL( IPOUT ) ), IERR,
     :                     NERR, STATUS )

         ELSE IF ( TYPEO .EQ. '_WORD' ) THEN
            CALL VEC_WTOW( BAD, N, %VAL( CNF_PVAL( IPIN ) ),
     :                     %VAL( CNF_PVAL( IPOUT ) ), IERR,
     :                     NERR, STATUS )

         ELSE IF ( TYPEO .EQ. '_UWORD' ) THEN
            CALL VEC_WTOUW( BAD, N, %VAL( CNF_PVAL( IPIN ) ),
     :                      %VAL( CNF_PVAL( IPOUT ) ), IERR,
     :                      NERR, STATUS )

*  Note if the data type specified is not valid.
         ELSE
            TYPEOK = .FALSE.
            TYPEOK = .FALSE.
         END IF

      ELSE IF ( TYPEI .EQ. '_UWORD' ) THEN
         IF ( TYPEO .EQ. '_BYTE' ) THEN
            CALL VEC_UWTOB( BAD, N, %VAL( CNF_PVAL( IPIN ) ),
     :                      %VAL( CNF_PVAL( IPOUT ) ), IERR,
     :                      NERR, STATUS )

         ELSE IF ( TYPEO .EQ. '_UBYTE' ) THEN
            CALL VEC_UWTOUB( BAD, N, %VAL( CNF_PVAL( IPIN ) ),
     :                       %VAL( CNF_PVAL( IPOUT ) ), IERR,
     :                       NERR, STATUS )

         ELSE IF ( TYPEO .EQ. '_DOUBLE' ) THEN
            CALL VEC_UWTOD( BAD, N, %VAL( CNF_PVAL( IPIN ) ),
     :                      %VAL( CNF_PVAL( IPOUT ) ), IERR,
     :                      NERR, STATUS )

         ELSE IF ( TYPEO .EQ. '_INTEGER' ) THEN
            CALL VEC_UWTOI( BAD, N, %VAL( CNF_PVAL( IPIN ) ),
     :                      %VAL( CNF_PVAL( IPOUT ) ), IERR,
     :                      NERR, STATUS )

         ELSE IF ( TYPEO .EQ. '_REAL' ) THEN
            CALL VEC_UWTOR( BAD, N, %VAL( CNF_PVAL( IPIN ) ),
     :                      %VAL( CNF_PVAL( IPOUT ) ), IERR,
     :                      NERR, STATUS )

         ELSE IF ( TYPEO .EQ. '_WORD' ) THEN
            CALL VEC_UWTOW( BAD, N, %VAL( CNF_PVAL( IPIN ) ),
     :                      %VAL( CNF_PVAL( IPOUT ) ), IERR,
     :                      NERR, STATUS )

         ELSE IF ( TYPEO .EQ. '_UWORD' ) THEN
            CALL VEC_UWTOUW( BAD, N, %VAL( CNF_PVAL( IPIN ) ),
     :                       %VAL( CNF_PVAL( IPOUT ) ), IERR,
     :                       NERR, STATUS )

*  Note if the data type specified is not valid.
         ELSE
            TYPEOK = .FALSE.
            TYPEOK = .FALSE.
         END IF

*  Note if the input data type specified is not valid.
      ELSE
         TYPEOK = .FALSE.
         TYPEOK = .FALSE.
      END IF

*  If a data conversion error occurred, then annul any error reports.
      IF ( NERR .NE. 0 ) CALL ERR_ANNUL( STATUS )
      CALL ERR_RLSE

*  Report an error if the input data type specified was not valid.
      IF ( STATUS .EQ. SAI__OK ) THEN
         IF( .NOT. TYPEOK ) THEN
            STATUS = IMG__FATIN
            CALL ERR_REP( 'IMG1_CPY_BAD',
     :                    'IMG1_CPY: Invalid TYPE argument ' //
     :                    '(internal programming error).', STATUS )
         END IF
      END IF

*  Check the value of the input type until a match is found.

      END
* $Id$
