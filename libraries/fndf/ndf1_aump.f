      SUBROUTINE NDF1_AUMP( IAXIS, IACB, COMP, STATUS )
*+
*  Name:
*     NDF1_AUMP

*  Purpose:
*     Unmap an axis array for an ACB entry.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_AUMP( IAXIS, IACB, COMP, STATUS )

*  Description:
*     The routine unmaps an NDF axis array which has previously been
*     mapped for access. The NDF is identified by its ACB entry.  The
*     routine will unmap a list of arrays for a specified axis if
*     required (in which case an error will be reported if any of those
*     arrays has not previously been mapped). Alternatively, a "wild
*     card" unmapping operation may be performed on all those axis
*     arrays which have been mapped (which could be none). A similar
*     "wild card" operation can also be used to unmap arrays for all
*     the NDF's axes, by specifying an axis number of zero.

*  Arguments:
*     IAXIS = INTEGER (Given)
*        Number of the axis whose array is to be unmapped.  A value of
*        zero may be specified to indicate that mapped arrays on all
*        the NDF's axes are to be unmapped.
*     IACB = INTEGER (Given)
*        Index to the NDF's entry in the ACB.
*     COMP = CHARACTER * ( * ) (Given)
*        Name of the NDF axis array to be unmapped: 'CENTRE',
*        'VARIANCE', 'WIDTH' or '*' (case insensitive).  The last value
*        acts as a wild card, causing all mapped axis arrays to be
*        unmapped.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine attempts to execute even if STATUS is set on
*     entry, although no further error report will be made if it
*     subsequently fails under these circumstances.

*  Algorithm:
*     -  Save the error context on entry.
*     -  Check the axis number for validity, obtaining the range of
*     axes to be processed.
*     -  Initialise the array name count.
*     -  Initialise the character pointer to the start of the array
*     name list.  Then loop to extract each element from the name list.
*     -  Find the final character of the next element in the array name
*     list (the last character before a comma or end of string).
*     -  Locate the first and last non-blank characters in the element,
*     checking that it is not entirely blank.
*     -  Increment the array name count.
*     -  If a wild card array name was given, then loop to process all
*     the specified axes.
*     -  Consider all the arrays on each axis to see if they are
*     mapped. Call the appropriate routine to unmap each one.
*     -  Otherwise, test the axis array name against each permitted
*     value in turn, calling the appropriate unmapping routines. In
*     each case, test if the array is currently mapped only if a "wild
*     card" axis specification (IAXIS=0) was given.
*     -  If the array name is not recognised, then report an error.
*     -  Increment the character pointer to the start of the next
*     element in the array name list and return to process the next
*     element.
*     -  If no error has occurred, but no non-blank array names have
*     been processed, then report an error.
*     -  Restore the error context.

*  Copyright:
*     Copyright (C) 1990 Science & Engineering Research Council.
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
*     9-OCT-1990 (RFWS):
*        Original version.
*     16-OCT-1990 (RFWS):
*        Installed unmapping of axis variance arrays.
*     18-OCT-1990 (RFWS):
*        Installed unmapping of axis width arrays.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Global Variables:
      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_ADMAP( NDF__MXDIM, NDF__MXACB ) = LOGICAL (Read)
*           Whether NDF axis data arrays are currently mapped for
*           access.
*        ACB_AVMAP( NDF__MXDIM, NDF__MXACB ) = LOGICAL (Read)
*           Whether NDF axis variance arrays are currently mapped for
*           access.
*        ACB_AWMAP( NDF__MXDIM, NDF__MXACB ) = LOGICAL (Read)
*           Whether NDF axis width arrays are currently mapped for
*           access.

*  Arguments Given:
      INTEGER IAXIS
      INTEGER IACB
      CHARACTER * ( * ) COMP

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL NDF1_SIMLR         ! String compare with abbreviation

*  Local Variables:
      INTEGER F                  ! Position of first non-blank character
      INTEGER I1                 ! Position of first name character
      INTEGER I2                 ! Position of last name character
      INTEGER IAX                ! Loop counter for axes
      INTEGER IAX1               ! First axis to process
      INTEGER IAX2               ! Last axis to process
      INTEGER L                  ! Position of last non-blank character
      INTEGER NCOMP              ! Number non-blank names specified
      INTEGER TSTAT              ! Temporary status variable

*.

*  Save the STATUS value and mark the error stack.
      TSTAT = STATUS
      CALL ERR_MARK

*  Check the axis number for validity, obtaining the range of axes to be
*  processed.
      STATUS = SAI__OK
      CALL NDF1_VAN( IACB, IAXIS, .TRUE., IAX1, IAX2, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Initialise the array name count.
         NCOMP = 0

*  Initialise the character pointer to the start of the array name
*  list.  Then loop to extract each element from the name list.
         I1 = 1
 1       CONTINUE                ! Start of 'DO WHILE' loop
         IF ( ( STATUS .EQ. SAI__OK ) .AND.
     :        ( I1 .LE. LEN( COMP ) ) ) THEN

*  Find the final character of the next element in the array name list
*  (the last character before a comma or end of string).
            I2 = INDEX( COMP( I1 : ), ',' )
            IF ( I2 .EQ. 0 ) THEN
               I2 = LEN( COMP )
            ELSE
               I2 = I2 + I1 - 2
            END IF
            IF ( I2 .GE. I1 ) THEN

*  Locate the first and last non-blank characters in the element,
*  checking that it is not entirely blank.
               CALL CHR_FANDL( COMP( I1 : I2 ), F, L )
               IF ( L .GE. F ) THEN
                  F = F + I1 - 1
                  L = L + I1 - 1

*  Increment the array name count.
                  NCOMP = NCOMP + 1

*  If a wild card array name was given, then loop to process all the
*  specified axes.
                  IF ( COMP .EQ. '*' ) THEN
                     DO 2 IAX = IAX1, IAX2

*  Consider all the arrays on each axis to see if they are mapped. Call
*  the appropriate routine to unmap each one.

*  ...unmap the axis data array.
                        IF ( ACB_ADMAP( IAX, IACB ) ) THEN
                           CALL NDF1_ADUMP( IAX, IACB, STATUS )
                        END IF

*  ...unmap the axis variance array.
                        IF ( ACB_AVMAP( IAX, IACB ) ) THEN
                           CALL NDF1_AVUMP( IAX, IACB, STATUS )
                        END IF

*  ...unmap the axis width array.
                        IF ( ACB_AWMAP( IAX, IACB ) ) THEN
                           CALL NDF1_AWUMP( IAX, IACB, STATUS )
                        END IF
 2                   CONTINUE

*  Otherwise, test the axis array name against each permitted value in
*  turn, calling the appropriate unmapping routines. In each case, take
*  test if the array is currently mapped only if a "wild card" axis
*  specification (IAXIS=0) was given.

*  CENTRE array:
*  ============
*  Unmap the axis data array.
                  ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'CENTRE',
     :                                  NDF__MINAB ) .OR.
     :                      NDF1_SIMLR( COMP( F : L ), 'CENTER',
     :                                  NDF__MINAB ) ) THEN
                     DO 3 IAX = IAX1, IAX2
                        IF ( ( IAXIS .NE. 0 ) .OR.
     :                       ACB_ADMAP( IAX, IACB ) ) THEN
                           CALL NDF1_ADUMP( IAX, IACB, STATUS )
                        END IF
 3                   CONTINUE

*  VARIANCE array:
*  ==============
*  Unmap the axis variance array.
                  ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'VARIANCE',
     :                                  NDF__MINAB ) ) THEN
                     DO 4 IAX = IAX1, IAX2
                        IF ( ( IAXIS .NE. 0 ) .OR.
     :                       ACB_AVMAP( IAX, IACB ) ) THEN
                           CALL NDF1_AVUMP( IAX, IACB, STATUS )
                        END IF
 4                   CONTINUE

*  WIDTH array:
*  ===========
*  Unmap the axis width array.
                  ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'WIDTH',
     :                                  NDF__MINAB ) ) THEN
                     DO 5 IAX = IAX1, IAX2
                        IF ( ( IAXIS .NE. 0 ) .OR.
     :                       ACB_AWMAP( IAX, IACB ) ) THEN
                           CALL NDF1_AWUMP( IAX, IACB, STATUS )
                        END IF
 5                   CONTINUE

*  If the array name is not recognised, then report an error.
                  ELSE
                     STATUS = NDF__CNMIN
                     CALL MSG_SETC( 'BADCOMP', COMP( F : L ) )
                     CALL ERR_REP( 'NDF1_AUMP_COMP',
     :                             'Invalid axis array component ' //
     :                             'name ''^BADCOMP'' specified ' //
     :                             '(possible programming error).',
     :                             STATUS )
                  END IF
               END IF
            END IF

*  Increment the character pointer to the start of the next element in
*  the array name list and return to process the next element.
            I1 = I2 + 2
            GO TO 1
         END IF

*  If no error has occurred, but no non-blank array names have been
*  processed, then report an error.
         IF ( ( STATUS .EQ. SAI__OK ) .AND. ( NCOMP .EQ. 0 ) ) THEN
            STATUS = NDF__NOCMP
            CALL ERR_REP( 'NDF1_AUMP_NONE',
     :                    'No axis array component name specified ' //
     :                    '(possible programming error).', STATUS )
         END IF
      END IF

*  Annul any error if STATUS was previously bad, otherwise let the new
*  error report stand.
      IF ( STATUS .NE. SAI__OK ) THEN
         IF ( TSTAT .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )
            STATUS = TSTAT

*  Call the error tracing routine if appropriate.
         ELSE
            CALL NDF1_TRACE( 'NDF1_AUMP', STATUS )
         END IF
      ELSE
         STATUS = TSTAT
      END IF

*  Release error stack.
      CALL ERR_RLSE

      END
