      SUBROUTINE NDF1_ATYP( IAXIS, IACB, COMP, ITYPE, STATUS )
*+
*  Name:
*     NDF1_ATYP

*  Purpose:
*     Obtain the numeric type of an NDF axis array identified by its
*     ACB entry.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_ATYP( IAXIS, IACB, COMP, ITYPE, STATUS )

*  Description:
*     The routine returns the numeric type of an NDF axis array as a
*     symbolic integer type code. The NDF is identified by its entry in
*     the ACB.

*  Arguments:
*     IAXIS = INTEGER (Given)
*        Number of the NDF axis for which information is required.
*     IACB = INTEGER (Given)
*        Index to the NDF's entry in the ACB.
*     COMP = CHARACTER * ( * ) (Given)
*        Name of the NDF axis array: 'CENTRE', 'VARIANCE' or 'WIDTH'.
*     ITYPE = INTEGER (Returned)
*        Numeric data type code of the axis array; a symbolic constant
*        with a name of the form NDF__TYPx, where x identifies the data
*        type (these constants are defined in the include file
*        NDF_CONST).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  A comma-separated list of axis array names may also be
*     supplied to this routine. In this case the result returned will
*     be the integer code for the data type with the lowest precision
*     such that the values held in any of the specified arrays can be
*     converted to that type without unnecessary loss of information.
*     -  A value of zero may be given for the IAXIS argument. In this
*     case, the results for all the NDF's axes will be combined in the
*     same way as described above.
*

*  Algorithm:
*     -  Check the axis number for validity.
*     -  Initialise the count of array names and the first type flag.
*     -  Initialise the character pointer to the start of the array
*     name list.  Then loop to extract each element from the name list.
*     -  Find the final character of the next element in the array name
*     list (the last character before a comma or end of string).
*     -  Locate the first and last non-blank characters in the element,
*     checking that it is not entirely blank.
*     -  Increment the array name count.
*     -  Compare the axis array name with each value in turn (allowing
*     abbreviation), and take the appropriate action.
*     -  For the CENTRE array, loop through each relevant axis,
*     obtaining the numeric type of its data array.
*     -  Convert the numeric type string into the corresponding integer
*     type code.
*     -  ITYPE accumulates the type code for the numeric type of the
*     result.  For the first array, simply set its value.  For
*     subsequent arrays, "maximise" the type code obtained for the
*     latest array with that obtained for previous ones.
*     -  Perform the same process for the variance and width arrays.
*     -  If the array name was not recognised, then report an error.
*     -  Increment the character pointer to the start of the next
*     element in the array name list and return to process the next
*     element.
*     -  If no error has occurred, but no non-blank array names names
*     have been processed, then report an error.

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
*     11-OCT-1990 (RFWS):
*        Original version, derived from the NDF1_TYP routine.
*     15-OCT-1990 (RFWS):
*        Changed argument order.
*     15-OCT-1990 (RFWS):
*        Added support for the axis variance and width arrays.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Arguments Given:
      INTEGER IAXIS
      INTEGER IACB
      CHARACTER * ( * ) COMP

*  Arguments Returned:
      INTEGER ITYPE

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL NDF1_SIMLR         ! String compare with abbreviation

*  Local variables:
      CHARACTER * ( NDF__SZTYP ) TYPEA ! Axis array type string
      INTEGER F                  ! Position of first non-blank character
      INTEGER I1                 ! Position of first name character
      INTEGER I2                 ! Position of last name character
      INTEGER IAX                ! Loop counter for axes
      INTEGER IAX1               ! First axis to process
      INTEGER IAX2               ! Last axis to process
      INTEGER ITYPEA             ! Integer type code of axis array
      INTEGER L                  ! Position of last non-blank character
      INTEGER MXTYPE             ! "Maximised" numeric type code
      INTEGER NCOMP              ! Number non-blank names specified
      LOGICAL FIRST              ! First array being processed?

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check the axis number for validity.
      CALL NDF1_VAN( IACB, IAXIS, .TRUE., IAX1, IAX2, STATUS )

*  Initialise the count of array names and the first array flag.
      NCOMP = 0
      FIRST = .TRUE.

*  Initialise the character pointer to the start of the array name
*  list.  Then loop to extract each element from the name list.
      I1 = 1
1     CONTINUE                ! Start of "DO WHILE" loop
      IF ( ( STATUS .EQ. SAI__OK ) .AND.
     :     ( I1 .LE. LEN( COMP ) ) ) THEN

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

*  Compare the axis array name with each value in turn (allowing
*  abbreviation), and take the appropriate action.

*  CENTRE array:
*  ============
*  Loop through each relevant axis, obtaining the numeric type of its
*  data array.
               IF ( NDF1_SIMLR( COMP( F : L ), 'CENTRE',
     :                          NDF__MINAB ) .OR.
     :              NDF1_SIMLR( COMP( F : L ), 'CENTER',
     :                          NDF__MINAB ) ) THEN
                  DO 2 IAX = IAX1, IAX2
                     CALL NDF1_ADTYP( IAX, IACB, TYPEA, STATUS )

*  Convert the numeric type string into the corresponding integer type
*  code.
                     CALL NDF1_PSTYP( TYPEA, ITYPEA, STATUS )
                     IF ( STATUS .EQ. SAI__OK ) THEN

*  ITYPE accumulates the type code for the numeric type of the result.
*  For the first array, simply set its value.
                        IF ( FIRST ) THEN
                           ITYPE = ITYPEA
                           FIRST = .FALSE.

*  For subsequent arrays, "maximise" the type code obtained for the
*  latest array with that obtained for previous ones.
                        ELSE
                           CALL NDF1_MXTYP( ITYPE, ITYPEA, MXTYPE,
     :                                      STATUS )
                           ITYPE = MXTYPE
                        END IF
                     END IF
 2                CONTINUE

*  VARIANCE array:
*  ==============
*  Loop through each relevant axis, obtaining the numeric type of its
*  variance array.
               ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'VARIANCE',
     :                               NDF__MINAB ) ) THEN
                  DO 3 IAX = IAX1, IAX2
                     CALL NDF1_AVTYP( IAX, IACB, TYPEA, STATUS )

*  Convert the numeric type string into the corresponding integer type
*  code.
                     CALL NDF1_PSTYP( TYPEA, ITYPEA, STATUS )
                     IF ( STATUS .EQ. SAI__OK ) THEN

*  ITYPE accumulates the type code for the numeric type of the result.
*  For the first array, simply set its value.
                        IF ( FIRST ) THEN
                           ITYPE = ITYPEA
                           FIRST = .FALSE.

*  For subsequent arrays, "maximise" the type code obtained for the
*  latest array with that obtained for previous ones.
                        ELSE
                           CALL NDF1_MXTYP( ITYPE, ITYPEA, MXTYPE,
     :                                      STATUS )
                           ITYPE = MXTYPE
                        END IF
                     END IF
 3                CONTINUE

*  WIDTH array:
*  ===========
*  Loop through each relevant axis, obtaining the numeric type of its
*  width array.
               ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'WIDTH',
     :                               NDF__MINAB ) ) THEN
                  DO 4 IAX = IAX1, IAX2
                     CALL NDF1_AWTYP( IAX, IACB, TYPEA, STATUS )

*  Convert the numeric type string into the corresponding integer type
*  code.
                     CALL NDF1_PSTYP( TYPEA, ITYPEA, STATUS )
                     IF ( STATUS .EQ. SAI__OK ) THEN

*  ITYPE accumulates the type code for the numeric type of the result.
*  For the first array, simply set its value.
                        IF ( FIRST ) THEN
                           ITYPE = ITYPEA
                           FIRST = .FALSE.

*  For subsequent arrays, "maximise" the type code obtained for the
*  latest array with that obtained for previous ones.
                        ELSE
                           CALL NDF1_MXTYP( ITYPE, ITYPEA, MXTYPE,
     :                                      STATUS )
                           ITYPE = MXTYPE
                        END IF
                     END IF
 4                CONTINUE

*  If the array name was not recognised, then report an error.
               ELSE
                  STATUS = NDF__CNMIN
                  CALL MSG_SETC( 'BADCOMP', COMP( F : L ) )
                  CALL ERR_REP( 'NDF1_ATYP_COMP',
     :                          'Invalid axis array component name ' //
     :                          '''^BADCOMP'' specified (possible ' //
     :                          'programming error).', STATUS )
               END IF
            END IF
         END IF

*  Increment the character pointer to the start of the next element in
*  the array name list and return to process the next element.
         I1 = I2 + 2
         GO TO 1
      END IF

*  If no error has occurred, but no non-blank array names names have
*  been processed, then report an error.
      IF ( ( STATUS .EQ. SAI__OK ) .AND. ( NCOMP .EQ. 0 ) ) THEN
         STATUS = NDF__NOCMP
         CALL ERR_REP( 'NDF1_ATYP_NONE',
     :                 'No axis array component name specified ' //
     :                 '(possible programming error).', STATUS )
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_ATYP', STATUS )

      END
