      SUBROUTINE ARY1_MDIAG( IMCB )
*+
*  Name:
*     ARY1_MDIAG

*  Purpose:
*     Diagnostic routine for MCB entries.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_MDIAG( IMCB )

*  Description:
*     The routine displays diagnostic information about the specified
*     MCB entry through the ADAM parameter system.

*  Arguments:
*     IMCB = INTEGER (Given)
*        The MCB entry to be inspected.

*  Notes:
*     -  This routine doses not perform any error checking or reporting.

*  Algorithm:
*     -  Format and display information about each MCB item in turn.

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
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     11-SEP-1989 (RFWS):
*        Original version.
*     9-NOV-1989 (RFWS):
*        Corrected error where DPNTR message token was used but IPNTR
*        intended.
*     {enter_further_changes_here}

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
      INCLUDE 'ARY_MCB'          ! ARY_ Mapping Control Block
*        This routine accesses all the global variables in the MCB using
*        READ access.

*  Arguments Given:
      INTEGER IMCB

*  External References:
      EXTERNAL ARY1_INIT         ! Initialise common blocks

*  Local Variables:
      CHARACTER * ( 60 ) BUF     ! Output buffer for bounds information
      INTEGER I                  ! Loop counter for dimensions
      INTEGER STATUS             ! Local status variable
      LOGICAL VALID              ! Whether locator is valid

*.

      STATUS = SAI__OK

*  Show which MCB entry the following information is for.
      CALL MSG_SETI( 'IMCB', IMCB )
      CALL MSG_OUT( ' ', 'Mapping Control Block entry number ^IMCB',
     :              STATUS )

*  Indicate if the entry number is not valid.
      IF ( ( IMCB . LT. 1 ) .OR. ( IMCB .GT. ARY__MXMCB ) ) THEN
         CALL MSG_OUT( ' ', 'This MCB entry number is invalid.',
     :                 STATUS )

*  Indicate if the MCB slot is not marked as in use. If this is so, then
*  there is nothing more to do.
      ELSE IF ( .NOT. MCB_USED( IMCB ) ) THEN
         CALL MSG_OUT( ' ', 'Entry is not in use.', STATUS )

*  Display the active mapping mode and the data type used for access.
      ELSE
         CALL MSG_SETC( 'AMM', MCB_AMM( IMCB ) )
         CALL MSG_OUT( ' ', 'Active mapping mode is ''^AMM''', STATUS )
         CALL MSG_SETC( 'TYP', MCB_TYP( IMCB ) )
         CALL MSG_OUT( ' ', 'Data type for access is ''^TYP''', STATUS )

*  Indicate whether non-complex or complex data access has been
*  obtained.
         IF ( MCB_CPX( IMCB ) ) THEN
            CALL MSG_OUT( ' ', 'Access is to complex data.', STATUS )
         ELSE
            CALL MSG_OUT( ' ', 'Access is to non-complex data.',
     :                    STATUS )
         END IF

*  Display the mapping region bounds.
         CALL MSG_OUT( ' ', 'Mapping region bounds:', STATUS )
         DO 1 I = 1, ARY__MXDIM
            WRITE( BUF, 91 ) MCB_LMRB( I, IMCB ), MCB_UMRB( I, IMCB )
            CALL MSG_OUT( ' ', BUF, STATUS )
1        CONTINUE

91    FORMAT ( 10X, I10, ':', I10 )

*  If the mapping transfer region exists, then display its bounds.
         IF ( MCB_MTREX( IMCB ) ) THEN
            CALL MSG_OUT( ' ', 'Mapping transfer region exists, ' //
     :                    'with the following bounds:', STATUS )
            DO 2 I = 1, ARY__MXDIM
               WRITE( BUF, 91 ) MCB_LMTR( I, IMCB ), MCB_UMTR( I, IMCB )
               CALL MSG_OUT( ' ', BUF, STATUS )
2           CONTINUE

*  Indicate whether the mapping transfer region completely fills the
*  mapping region.
            CALL MSG_SETL( 'MRFUL', MCB_MRFUL( IMCB ) )
            CALL MSG_OUT( ' ', 'Mapping transfer region fills ' //
     :                    'mapping region = ^MRFUL.', STATUS )

*  Indicate whether the mapping region comprises the whole data object.
            CALL MSG_SETL( 'WHOLE', MCB_WHOLE( IMCB ) )
            CALL MSG_OUT( ' ', 'Whole data object may be mapped = ' //
     :                    '^WHOLE.', STATUS )

*  Indicate if no mapping transfer region exists.

         ELSE
            CALL MSG_OUT( ' ', 'Mapping transfer region does not ' //
     :                    'exist.', STATUS )
         END IF

*  See if the mapped non-imaginary data object locator is valid. If so,
*  then display the associated object's name. Otherwise show the
*  character value of the locator.
         CALL DAT_VALID( MCB_DLOC( IMCB ), VALID, STATUS )
         IF ( VALID ) THEN
            CALL DAT_MSG( 'DOBJ', MCB_DLOC( IMCB ) )
            CALL MSG_OUT( ' ', 'Mapped non-imaginary data object ' //
     :                    'is ^DOBJ.', STATUS )
         ELSE
            CALL MSG_SETC( 'DLOC', MCB_DLOC( IMCB ) )
            CALL MSG_OUT( ' ', 'Mapped non-imaginary data locator ' //
     :                    'is invalid: ''^DLOC''.', STATUS )
         END IF

*  Indicate whether a copy of the actual data has been mapped.
         IF ( MCB_DCOPY( IMCB ) ) THEN
            CALL MSG_OUT( ' ', 'A copy of the non-imaginary data ' //
     :                    'component has been mapped.', STATUS )
         ELSE
            CALL MSG_OUT( ' ', 'The non-imaginary data component ' //
     :                    'has been mapped directly via HDS.', STATUS )
         END IF

*  Show the value of the mapped data pointer.
         CALL MSG_SETI( 'DPNTR', MCB_DPNTR( IMCB ) )
         CALL MSG_OUT( ' ', 'Non-imaginary data pointer = ^DPNTR',
     :                 STATUS )

*  If access to complex data has been obtained, then display similar
*  information for the imaginary component.
         IF ( MCB_CPX( IMCB ) ) THEN

*  ...Object locator.
            CALL DAT_VALID( MCB_ILOC( IMCB ), VALID, STATUS )
            IF ( VALID ) THEN
               CALL DAT_MSG( 'IOBJ', MCB_ILOC( IMCB ) )
               CALL MSG_OUT( ' ', 'Mapped imaginary data object ' //
     :                       'is ^IOBJ.', STATUS )
            ELSE
               CALL MSG_SETC( 'ILOC', MCB_ILOC( IMCB ) )
               CALL MSG_OUT( ' ', 'Mapped imaginary data locator ' //
     :                       'is invalid: ''^ILOC''.', STATUS )
            END IF

*  ...Whether a copy of the data has been made.
            IF ( MCB_ICOPY( IMCB ) ) THEN
               CALL MSG_OUT( ' ', 'A copy of the imaginary data ' //
     :                       'component has been mapped.', STATUS )
            ELSE
               CALL MSG_OUT( ' ', 'The imaginary data component ' //
     :                       'has been mapped directly via HDS.',
     :                       STATUS )
            END IF

*  ...Pointer to mapped data.
            CALL MSG_SETI( 'IPNTR', MCB_IPNTR( IMCB ) )
            CALL MSG_OUT( ' ', 'Imaginary data pointer = ^IPNTR',
     :                    STATUS )
         END IF

*  Display the bad pixel flag values.
         CALL MSG_SETL( 'BAD', MCB_BAD( IMCB ) )
         CALL MSG_OUT( ' ', 'Bad pixel flag (transfer region) = ' //
     :                 '^BAD.', STATUS )
         CALL MSG_SETL( 'PBAD', MCB_PBAD( IMCB ) )
         CALL MSG_OUT( ' ', 'Bad pixel flag (padding region) = ' //
     :                 '^PBAD.', STATUS )
      END IF

      END
