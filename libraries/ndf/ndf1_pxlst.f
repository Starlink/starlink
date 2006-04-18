      SUBROUTINE NDF1_PXLST( INCLUD, STR, MXEXTN, EXTN, NEXTN, STATUS )
*+
*  Name:
*     NDF1_PXLST

*  Purpose:
*     Parse an extension name list.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_PXLST( INCLUD, STR, MXEXTN, EXTN, NEXTN, STATUS )

*  Description:
*     The routine parses a list of NDF extension names, extracting each
*     name from a comma separated list supplied and filling a character
*     array with validated extension names which specify which
*     extensions to exclude from an NDF copying operation. The comma
*     separated list may specify names for EXCLUSION (i.e. extensions
*     not to be copied) or INCLUSION (i.e. extensions to be copied,
*     over-riding a previous inclusion).

*  Arguments:
*     INCLUD = LOGICAL (Given)
*        Whether the extensions specified in the list supplied are to
*        be included (as opposed to excluded) from an NDF copying
*        operation.
*     STR = CHARACTER * ( * ) (Given)
*        The comma separated list of extension names.
*     MXEXTN = INTEGER (Given)
*        The maximum number of extensions which can be excluded (the
*        declared size of the EXTN array).
*     EXTN( MXEXTN ) = CHARACTER * ( DAT__SZNAM ) (Given and Returned)
*        The list of extensions to be excluded from an NDF copying
*        operation. This list may contain initial entries and will be
*        updated.
*     NEXTN = INTEGER (Given and Returned)
*        The number of significant elements in the EXTN array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Initialise pointers to the position of the "current" list
*     element.
*     -  Loop to identify each element in the list.
*     -  Find the next element and check it is not blank.
*     -  Extract the extension name, validate it, and convert to upper
*     case.
*     -  Search the existing excluded extension list to see if the name
*     is already there.
*     -  If it is there and extensions are being INCLUDED, then remove
*     the name from the list. Move following names down to close the
*     gap and decrement the count of excluded extensions.
*     -  If it is not there and extensions are being EXCLUDED, then add
*     the name to the excluded extension list, first checking that the
*     list will not overflow and reporting an error if it will.
*     -  Increment the pointer to the start of the next input list
*     element and return to process it.

*  Copyright:
*     Copyright (C) 1989, 1991 Science & Engineering Research Council.
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
*     10-OCT-1989 (RFWS):
*        Original version.
*     2-JAN-1991 (RFWS):
*        Fixed illegal string concatenation problem.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Arguments Given:
      LOGICAL INCLUD
      CHARACTER * ( * ) STR
      INTEGER MXEXTN

*  Arguments Given and Returned:
      CHARACTER * ( DAT__SZNAM ) EXTN( MXEXTN )
      INTEGER NEXTN

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( DAT__SZNAM ) NAME ! Extension name
      INTEGER F                  ! First character position of name
      INTEGER I                  ! Loop counter for list entries
      INTEGER I1                 ! Position of start of list element
      INTEGER I2                 ! Position of end of list element
      INTEGER L                  ! Last character position of name
      INTEGER POSN               ! List position of pre-existing name
      LOGICAL THERE              ! Whether a name is in the list

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

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

*  Check the name for validity.
               CALL NDF1_CHXNM( STR( F : L ), STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN

*  Extract the name and convert to upper case.
                  NAME = STR( F : L )
                  CALL CHR_UCASE( NAME )

*  Search the existing list of excluded extensions to see if this name
*  is already there.
                  THERE = .FALSE.
                  DO 2 I = 1, NEXTN
                     IF ( EXTN( I ) .EQ. NAME ) THEN
                        THERE = .TRUE.
                        POSN = I
                        GO TO 3
                     END IF
2                 CONTINUE
3                 CONTINUE

*  If present, and the name specifies an extension to be included, then
*  the name must be removed from the list.
                  IF ( THERE ) THEN
                     IF ( INCLUD ) THEN

*  Move following names in the list down to close the gap which this
*  name leaves and decrement the excluded extension count.
                        DO 4 I = POSN, NEXTN - 1
                           EXTN( I ) = EXTN( I + 1 )
4                       CONTINUE
                        NEXTN = NEXTN - 1
                     END IF

*  If the name is not in the list and it specifies an extension to be
*  excluded, then check that the list will not overflow. Report an error
*  if it will.
                  ELSE
                     IF ( .NOT. INCLUD ) THEN
                        IF ( NEXTN .GE. MXEXTN ) THEN
                           STATUS = NDF__XSEXT
                           CALL MSG_SETI( 'MXEXTN', MXEXTN )
                           CALL ERR_REP( 'NDF1_PXLST_XS',
     :                     'The maximum number of extension names ' //
     :                     '(^MXEXTN) has been exceeded.', STATUS )

*  Increment the excluded extension name count and store the new name in
*  the excluded extension list.
                        ELSE
                           NEXTN = NEXTN + 1
                           EXTN( NEXTN ) = NAME
                        END IF
                     END IF
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
