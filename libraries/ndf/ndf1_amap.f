      SUBROUTINE NDF1_AMAP( IAXIS, IACB, COMP, TYPE, MMOD, PNTR, EL,
     :                      STATUS )
*+
*  Name:
*     NDF1_AMAP

*  Purpose:
*     Map an NDF axis array for access.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_AMAP( IAXIS, IACB, COMP, TYPE, MMOD, PNTR, EL, STATUS )

*  Description:
*     The routine obtains mapped access to an NDF axis array, specified
*     by the axis number, the NDF's ACB index and the name of the axis
*     array.

*  Arguments:
*     IAXIS = INTEGER (Given)
*        Number of the axis to be accessed.
*     IACB = INTEGER (Given)
*        Index to the NDF entry in the ACB.
*     COMP = CHARACTER * ( * ) (Given)
*        Name of the axis array: 'CENTRE', 'VARIANCE' (or 'ERROR') or
*        'WIDTH' (case insensitive).
*     TYPE = CHARACTER * ( * ) (Given)
*        Numeric type to be used for accessing the array (case
*        insensitive).
*     MMOD = CHARACTER * ( * ) (Given)
*        Mapping mode for access: 'READ', 'UPDATE' or 'WRITE' (case
*        insensitive).
*     PNTR( * ) = INTEGER (Returned)
*        Pointer(s) to the mapped axis array(s).
*     EL = INTEGER (Returned)
*        Number of array elements mapped.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  A comma-separated list of axis array names may also be given,
*     in which case each will be mapped in turn. The PNTR array should
*     have sufficient elements to accommodate the returned pointers.

*  Algorithm:
*     -  Check the axis number for validity.
*     -  Check the mapping mode for validity.
*     -  If an initialisation option was specified, then report an
*     error, as these are not permitted when mapping axis arrays.
*     -  Check that the requested mode of NDF access is available.
*     -  Initialise the array name count.
*     -  Initialise the character pointer to the start of the array
*     name list.  Then loop to extract each element from the array name
*     list.
*     -  Find the final character of the next element in the array name
*     list (the last character before a comma or end of string).
*     -  Locate the first and last non-blank characters in the element,
*     checking that it is not entirely blank.
*     -  Increment the array name count.
*     -  Compare the array name with each valid value in turn, calling
*     the appropriate routine to map the array.
*     -  If the axis array name was not recognised, then report an
*     error.
*     -  Increment the character pointer to the start of the next
*     element in the array name list and return to process the next
*     element.
*     -  If no error has occurred, but no non-blank array names have
*     been processed, then report an error.

*  Copyright:
*     Copyright (C) 1990, 1991 Science & Engineering Research Council.
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
*     10-OCT-1990 (RFWS):
*        Fixed error in constructing error message.
*     10-OCT-1990 (RFWS):
*        Upgraded to handle a list of axis array names.
*     16-OCT-1990 (RFWS):
*        Installed mapping of the axis variance and errors.
*     18-OCT-1990 (RFWS):
*        Installed mapping of the axis width array.
*     2-JAN-1991 (RFWS):
*        Removed unnecessary use of NDF_PAR include file.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Arguments Given:
      INTEGER IAXIS
      INTEGER IACB
      CHARACTER * ( * ) COMP
      CHARACTER * ( * ) TYPE
      CHARACTER * ( * ) MMOD

*  Arguments Returned:
      INTEGER PNTR( * )
      INTEGER EL

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL NDF1_SIMLR         ! String compare with abbreviation

*  Local Variables:
      CHARACTER * ( NDF__SZIOP ) INOPT ! Initialisation option
      CHARACTER * ( NDF__SZMOD ) MODE ! Mapping access mode
      INTEGER F                  ! First non-blank character position
      INTEGER I1                 ! Position of first name character
      INTEGER I2                 ! Position of last name character
      INTEGER IAX1               ! First axis number to process
      INTEGER IAX2               ! Last axis number to process (junk)
      INTEGER L                  ! Last non-blank character position
      INTEGER NCOMP              ! Axis array name count

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check the axis number for validity.
      CALL NDF1_VAN( IACB, IAXIS, .FALSE., IAX1, IAX2, STATUS )

*  Check the mapping mode for validity.
      CALL NDF1_VMMD( MMOD, MODE, INOPT, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  If an initialisation option was specified, then report an error, as
*  these are not permitted when mapping axis arrays.
         IF ( INOPT .NE. ' ' ) THEN
            STATUS = NDF__MMDIN
            CALL MSG_SETC( 'OPTION', INOPT )
            CALL ERR_REP( 'NDF1_AMAP_MMOD',
     :                    'The mapping mode initialisation option ' //
     :                    '''/^OPTION'' is not permitted when ' //
     :                    'mapping axis arrays (possible ' //
     :                    'programming error).', STATUS )
         END IF
      END IF

*  Check that the requested mode of NDF access is available.
      CALL NDF1_CHMOD( IACB, MODE, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Initialise the array name count.
         NCOMP = 0

*  Initialise the character pointer to the start of the array name
*  list.  Then loop to extract each element from the array name list.
         I1 = 1
1        CONTINUE                ! Start of "DO WHILE" loop
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

*  Compare the array name with each valid value in turn, calling the
*  appropriate routine to map the array.

*  CENTRE array:
*  ============
*  Map the axis data array.
                  IF ( NDF1_SIMLR( COMP( F : L ), 'CENTRE',
     :                             NDF__MINAB ) .OR.
     :                 NDF1_SIMLR( COMP( F : L ), 'CENTER',
     :                             NDF__MINAB ) ) THEN
                     CALL NDF1_ADMAP( IAX1, IACB, TYPE, MODE,
     :                                PNTR( NCOMP ), EL, STATUS )

*  ERROR array:
*  ===========
*  Map the axis variance array with conversion to standard deviations.
                  ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'ERRORS',
     :                                  NDF__MINAB ) ) THEN
                     CALL NDF1_AVMAP( IAX1, IACB, TYPE, MODE, .TRUE.,
     :                                PNTR( NCOMP ), EL, STATUS )

*  VARIANCE array:
*  ==============
*  Map the axis variance array directly.
                  ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'VARIANCE',
     :                                  NDF__MINAB ) ) THEN
                     CALL NDF1_AVMAP( IAX1, IACB, TYPE, MODE, .FALSE.,
     :                                PNTR( NCOMP ), EL, STATUS )

*  WIDTH array:
*  ===========
*  Map the axis width array.
                  ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'WIDTH',
     :                                  NDF__MINAB ) ) THEN
                     CALL NDF1_AWMAP( IAX1, IACB, TYPE, MODE,
     :                                PNTR( NCOMP ), EL, STATUS )

*  If the axis array name was not recognised, then report an error.
                  ELSE
                     STATUS = NDF__CNMIN
                     CALL MSG_SETC( 'BADCOMP', COMP( F : L ) )
                     CALL ERR_REP( 'NDF1_AMAP_COMP',
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
            CALL ERR_REP( 'NDF1_AMAP_NONE',
     :                    'No axis array component name specified ' //
     :                    '(possible programming error).', STATUS )
         END IF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_AMAP', STATUS )

      END
