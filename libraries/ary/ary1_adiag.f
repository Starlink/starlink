      SUBROUTINE ARY1_ADIAG( IACB )
*+
*  Name:
*     ARY1_ADIAG

*  Purpose:
*     Diagnostic routine for ACB entries.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_ADIAG( IACB )

*  Description:
*     The routine displays diagnostic information about the specified
*     ACB entry through the ADAM parameter system.

*  Arguments:
*     IACB = INTEGER (Given)
*        The ACB entry to be inspected.

*  Notes:
*     -  This routine does not perform any error checking or reporting.

*  Algorithm:
*     -  Format and display information about each ACB item in turn.

*  Copyright:
*     Copyright (C) 1989 Science & Engineering Research Council.
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1-SEP-1989 (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'ARY_PAR'          ! ARY_ public constants
      INCLUDE 'ARY_CONST'        ! ARY_ private constants

*  Global Variables:
      INCLUDE 'ARY_ACB'          ! ARY_ Access Control Block
*        The routine access all the global variables in the ACB using
*        READ access.


*  Arguments Given:
      INTEGER IACB

*  External References:
      EXTERNAL ARY1_INIT         ! Initialise common blocks

*  Local Variables:
      INTEGER STATUS             ! Local status variable
      INTEGER IACC               ! Loop counter for access control flags
      INTEGER I                  ! Loop counter for dimensions
      CHARACTER * ( 60 ) BUF     ! Buffer for formatter output

*.

      STATUS = SAI__OK

*  Show which ACB entry the following information is for.
      CALL MSG_SETI( 'IACB', IACB )
      CALL MSG_OUT( ' ', 'Access Control Block entry number ^IACB',
     :              STATUS )

*  Indicate if the entry number is not valid.
      IF ( ( IACB .LT. 1 ) .OR. ( IACB .GT. ARY__MXACB ) ) THEN
         CALL MSG_OUT( ' ', 'This ACB entry number is invalid.',
     :                 STATUS )

*  Indicate if the ACB entry is not marked as in use. If this is so,
*  then there is nothing more to do.
      ELSE IF ( .NOT. ACB_USED( IACB ) ) THEN
         CALL MSG_OUT( ' ', 'Entry is not in use.', STATUS )

*  Note whether the ACB entry describes a base array.
      ELSE
         IF ( .NOT. ACB_CUT( IACB ) ) THEN
            CALL MSG_OUT( ' ', 'This entry describes a base array.',
     :                    STATUS )
         ELSE
            CALL MSG_OUT( ' ', 'This entry describes a non-base ' //
     :      'array.', STATUS )
         END IF

*  Format and display the access control flags.
         WRITE( BUF, 91 ) ( ACB_ACC( IACC, IACB ),
     :                       IACC = 1, ARY__MXACC )
         CALL MSG_SETC( 'ACCFLAGS', BUF )
         CALL MSG_OUT( ' ', 'Access control flags: ^ACCFLAGS', STATUS )
91    FORMAT ( 10( L1, 1X ) )

*  Display the data object index in the DCB.
         CALL MSG_SETI( 'IDCB', ACB_IDCB( IACB ) )
         CALL MSG_OUT( ' ', 'Data object DCB index = ^IDCB', STATUS )

*  Display the mapping index in the MCB.
         CALL MSG_SETI( 'IMCB', ACB_IMCB( IACB ) )
         CALL MSG_OUT( ' ', 'Mapping index in the MCB = ^IMCB', STATUS )

*  Display the check count for the ACB entry.
         CALL MSG_SETI( 'CHK', ACB_CHK( IACB ) )
         CALL MSG_OUT( ' ', 'Array check count = ^CHK', STATUS )

*  Display the ACB bad pixel flag.
         CALL MSG_SETL( 'BAD', ACB_BAD( IACB ) )
         CALL MSG_OUT( ' ', 'ACB bad pixel flag = ^BAD', STATUS )

*  Display the number of array dimensions and its bounds and accumulated
*  pixel index shifts.
         CALL MSG_SETI( 'NDIM', ACB_NDIM( IACB ) )
         CALL MSG_OUT( ' ', 'Array has ^NDIM dimension(s), with ' //
     :   'bounds and pixel shifts:', STATUS )
         DO 1 I = 1, ACB_NDIM( IACB )
            WRITE( BUF, 92 ) ACB_LBND( I, IACB ),
     :                        ACB_UBND( I, IACB ),
     :                        ACB_SFT( I, IACB )
            CALL MSG_OUT( ' ', BUF, STATUS )
1        CONTINUE
         DO 2 I = ACB_NDIM( IACB ) + 1, ARY__MXDIM
            WRITE( BUF, 93 ) ACB_LBND( I, IACB ),
     :                        ACB_UBND( I, IACB ),
     :                        ACB_SFT( I, IACB )
            CALL MSG_OUT( ' ', BUF, STATUS )
2        CONTINUE

92    FORMAT ( 10X, I10, ':', I10, 10X, I10 )
93    FORMAT ( 9X, '(', I10, ':', I10, ')', 8X, '(', I10, ')' )

*  Indicate if the data transfer window does not exist.
         IF ( .NOT. ACB_DTWEX( IACB ) ) THEN
            CALL MSG_OUT( ' ', 'Data transfer window does not exist.',
     :                    STATUS )

*  Display the data transfer window bounds.
         ELSE
            CALL MSG_OUT( ' ', 'Data transfer window bounds:', STATUS )
            DO 3 I = 1, ACB_NDIM( IACB )
               WRITE( BUF, 94 ) ACB_LDTW( I, IACB ),
     :                          ACB_UDTW( I, IACB )
               CALL MSG_OUT( ' ', BUF, STATUS )
3           CONTINUE
            DO 4 I = ACB_NDIM( IACB ) + 1, ARY__MXDIM
               WRITE( BUF, 95 ) ACB_LDTW( I, IACB ),
     :                          ACB_UDTW( I, IACB )
               CALL MSG_OUT( ' ', BUF, STATUS )
4           CONTINUE

94    FORMAT ( 10X, I11, ':', I11 )
95    FORMAT ( 9X, '(', I11, ':', I11, ')' )

         END IF
      END IF

      END
