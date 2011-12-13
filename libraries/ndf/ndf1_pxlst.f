      SUBROUTINE NDF1_PXLST( INCLUD, STR, KEYMAP, STATUS )
*+
*  Name:
*     NDF1_PXLST

*  Purpose:
*     Parse an extension name list.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_PXLST( INCLUD, STR, KEYMAP, STATUS )

*  Description:
*     The routine parses a list of NDF extension names, extracting each
*     name from a comma separated list supplied and adding the name to an
*     AST KeyMap. Each entry in the KeyMap has a key that is the
*     extension name and a value which is zero if INCLUD is FALSE and one
*     if INCLUD is TRUE. The comma separated list may specify names for
*     EXCLUSION (i.e. extensions not to be copied) or INCLUSION (i.e.
*     extensions to be copied, over-riding a previous inclusion).
*
*     If a name equal to "*" is encountered, all entries currently in
*     the KeyMap are set to 1 (if INCLUD is .TRUE.) or 0 (if INCLUD is
*     .FALSE.).

*  Arguments:
*     INCLUD = LOGICAL (Given)
*        Whether the extensions specified in the list supplied are to
*        be included (as opposed to excluded) from an NDF copying
*        operation.
*     STR = CHARACTER * ( * ) (Given)
*        The comma separated list of extension names.
*     KEYMAP = INTEGER (Given)
*        A pointer to the AST KeyMap.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1989, 1991 Science & Engineering Research Council.
*     Copyright (C) 2007-2010 Science & Technology Facilities Council.
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
*     DSB: David S. Berry (JACH, UCLan)
*     {enter_new_authors_here}

*  History:
*     10-OCT-1989 (RFWS):
*        Original version.
*     2-JAN-1991 (RFWS):
*        Fixed illegal string concatenation problem.
*     1-NOV-2007 (DSB):
*        Use an AST KeyMap to hold the results rather than an array of
*        characters.
*     22-FEB-2010 (DSB):
*        Allow an asterisk to be used as a wild card to match all
*        extension names.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'AST_PAR'          ! AST_ public constants and functions
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Arguments Given:
      LOGICAL INCLUD
      CHARACTER * ( * ) STR
      INTEGER KEYMAP

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( DAT__SZNAM ) NAME ! Extension name
      INTEGER F                  ! First character position of name
      INTEGER I                  ! Loop counter for list entries
      INTEGER I1                 ! Position of start of list element
      INTEGER I2                 ! Position of end of list element
      INTEGER IKEY               ! KeyMap entry index
      INTEGER KEYVAL             ! Value to assign to KeyMap entries
      INTEGER L                  ! Last character position of name
      INTEGER NKEY               ! Number of entries in the keymap

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the value to assign to entries in the keymap.
      IF( INCLUD ) THEN
         KEYVAL = 1
      ELSE
         KEYVAL = 0
      END IF

*  Initialise a pointer to the character position of the start of the
*  "current" extension name.
      I1 = 1

*  Loop to identify each element in the extension name list.
1     CONTINUE                   ! Start of 'DO WHILE' loop
      IF ( ( I1 .LE. LEN( STR ) ) .AND. ( STATUS .EQ. SAI__OK ) ) THEN

*  Find the end of the next extension name (the character before the
*  next comma or end of string).
         I2 = INDEX( STR( I1 : ), ',' )
         IF ( I2 .EQ. 0 ) THEN
            I2 = LEN( STR )
         ELSE
            I2 = I2 + I1 - 2
         END IF

*  If the next name was found, then find the first and last characters
*  in the name (excluding surrounding spaces).
         IF ( I1 .LE. I2 ) THEN
            CALL CHR_FANDL( STR( I1 : I2 ), F, L )

*  Check that the name is not all blank.
            IF ( F .LE. L ) THEN
               F = F + I1 - 1
               L = L + I1 - 1

*  If the name is just an asterisk, assign 1 or 0 to all entries
*  currently in the KeyMap.
               IF( STR( F : L ) .EQ. '*' ) THEN
                  NKEY = AST_MAPSIZE( KEYMAP, STATUS )
                  DO IKEY = 1, NKEY
                     CALL AST_MAPPUT0I( KEYMAP, AST_MAPKEY( KEYMAP,
     :                                                   IKEY, STATUS ),
     :                                  KEYVAL, ' ', STATUS )
                  END DO

*  Otherwise, check the name for validity.
               ELSE
                  CALL NDF1_CHXNM( STR( F : L ), STATUS )
                  IF ( STATUS .EQ. SAI__OK ) THEN

*  Extract the name and convert to upper case.
                     NAME = STR( F : L )
                     CALL CHR_UCASE( NAME )

*  Add an entry to the KeyMap. This will over-write any existing entry
*  for this extension name.
                     CALL AST_MAPPUT0I( KEYMAP, NAME, KEYVAL, ' ',
     :                                  STATUS )

                  END IF
               END IF
            END IF
         END IF

*  Increment the pointer to the start of the next element in the input
*  string and return to process it.
         I1 = I2 + 2
         GO TO 1
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_PXLST', STATUS )

      END
