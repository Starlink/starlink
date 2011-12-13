      SUBROUTINE NDF_ASTYP( TYPE, INDF, COMP, IAXIS, STATUS )
*+
*  Name:
*     NDF_ASTYP

*  Purpose:
*     Set a new numeric type for an NDF axis array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_ASTYP( TYPE, INDF, COMP, IAXIS, STATUS )

*  Description:
*     The routine sets a new numeric type for an NDF axis array,
*     causing its data storage type to be changed. If the array's
*     values are defined, they will be converted from from the old type
*     to the new one. If they are undefined, then no conversion will be
*     necessary. Subsequent enquiries will reflect the new numeric
*     type. Conversion may be performed between any numeric types
*     supported by the NDF_ routines.

*  Arguments:
*     TYPE = CHARACTER * ( * ) (Given)
*        New numeric type for the axis array (e.g. '_DOUBLE').
*     INDF = INTEGER (Given)
*        NDF identifier.
*     COMP = CHARACTER * ( * ) (Given)
*        Name of the axis array component whose numeric type is to be
*        set: 'CENTRE', 'VARIANCE' or 'WIDTH'.
*     IAXIS = INTEGER (Given)
*        Number of the NDF axis whose array is to be modified.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  A comma-separated list of axis array component names may also
*     be supplied, in which case the numeric type of each array will be
*     set to the same value in turn.
*     -  A value of zero may be supplied for the IAXIS argument, in
*     which case the routine will set a new numeric type for the
*     specified component(s) of all the NDF's axes.
*     -  This routine may only be used to change the numeric type of an
*     axis array via a base NDF. If an NDF section is supplied, then it
*     will return without action. No error will result.
*     -  The numeric type of an axis array component cannot be changed
*     while it, or any part of it, is mapped for access (e.g. via
*     another NDF identifier). This routine will fail, and set a STATUS
*     value, if this is the case.
*     -  If the numeric type of an axis array component is to be
*     changed without its values being retained, then a call to
*     NDF_AREST should be made beforehand.  This will avoid the cost of
*     converting all the values.

*  Algorithm:
*     -  Check the data type specification for validity.
*     -  Check that a complex type has not been specified. Report an
*     error if it has.
*     -  Import the NDF identifier.
*     -  Check the axis number for validity.
*     -  Check that TYPE access to the NDF is available.
*     -  Initialise the array name count.
*     -  Initialise the character pointer to the start of the array
*     list.
*     -  Then loop to extract each element from the array list.
*     -  Find the final character of the next element in the array list
*     (the last character before a comma or end of string).
*     -  Locate the first and last non-blank characters in the element,
*     checking that it is not entirely blank.
*     -  Increment the array name count.
*     -  Compare the array name with each value in turn (allowing
*     abbreviation), and take the appropriate action.
*     -  If the array name is not recognised, then report an error.
*     -  Increment the character pointer to the start of the next
*     element in the array list and return to process the next element.
*     -  If no error has occurred, but no non-blank array names have
*     been processed, then report an error.
*     -  If an error occurred, then report context information.

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
*        Original version, derived from the NDF_STYPE routine.
*     15-OCT-1990 (RFWS):
*        Added support for the axis variance and width arrays.
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

*  Arguments Given:
      CHARACTER * ( * ) TYPE
      INTEGER INDF
      CHARACTER * ( * ) COMP
      INTEGER IAXIS

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL NDF1_SIMLR         ! String compare with abbreviation

*  Local Variables:
      CHARACTER * ( NDF__SZTYP ) VTYPE ! Validated numeric data type
      INTEGER F                  ! Position of first non-blank character
      INTEGER I1                 ! Position of first name character
      INTEGER I2                 ! Position of last name character
      INTEGER IACB               ! Index to NDF entry in the ACB
      INTEGER IAX                ! Loop counter for axes
      INTEGER IAX1               ! First axis to process
      INTEGER IAX2               ! Last axis to process
      INTEGER L                  ! Position of last non-blank character
      INTEGER NCOMP              ! Number non-blank names specified
      LOGICAL CMPLX              ! Whether data type is complex

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check the data type specification for validity.
      CALL NDF1_CHFTP( TYPE, VTYPE, CMPLX, STATUS )

*  Check that a complex type has not been specified. Report an error if
*  it has.
      IF ( STATUS .EQ. SAI__OK ) THEN
         IF ( CMPLX ) THEN
            STATUS = NDF__TYPIN
            CALL MSG_SETC( 'BADTYPE', VTYPE )
            CALL ERR_REP( 'NDF_ASTYP_TYPE',
     :                    'Invalid numeric type ''^BADTYPE'' ' //
     :                    'specified; complex types are not ' //
     :                    'permitted for axis arrays (possible ' //
     :                    'programming error).', STATUS )
         END IF
      END IF

*  Import the NDF identifier.
      CALL NDF1_IMPID( INDF, IACB, STATUS )

*  Check the axis number for validity.
      CALL NDF1_VAN( IACB, IAXIS, .TRUE., IAX1, IAX2, STATUS )

*  Check that TYPE access to the NDF is available.
      CALL NDF1_CHACC( IACB, 'TYPE', STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Initialise the array name count.
         NCOMP = 0

*  Initialise the character pointer to the start of the array list.
*  Then loop to extract each element from the array list.
         I1 = 1
1        CONTINUE
         IF ( ( STATUS .EQ. SAI__OK ) .AND.
     :        ( I1 .LE. LEN( COMP ) ) ) THEN

*  Find the final character of the next element in the array list (the
*  last character before a comma or end of string).
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

*  Compare the array name with each value in turn (allowing
*  abbreviation), and take the appropriate action.

*  CENTRE array:
*  ============
*  Set a new type for the axis data array(s).
                  IF ( NDF1_SIMLR( COMP( F : L ), 'CENTRE',
     :                             NDF__MINAB ) .OR.
     :                 NDF1_SIMLR( COMP( F : L ), 'CENTER',
     :                             NDF__MINAB ) ) THEN
                     DO 2 IAX = IAX1, IAX2
                        CALL NDF1_ADSTP( VTYPE, IAX, IACB, STATUS )
 2                   CONTINUE

*  VARIANCE array:
*  ==============
*  Set a new type for the axis variance array(s).
                  ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'VARIANCE',
     :                                   NDF__MINAB ) ) THEN
                     DO 3 IAX = IAX1, IAX2
                        CALL NDF1_AVSTP( VTYPE, IAX, IACB, STATUS )
 3                   CONTINUE

*  WIDTH array:
*  ===========
*  Set a new type for the axis width array(s).
                  ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'WIDTH',
     :                                   NDF__MINAB ) ) THEN
                     DO 4 IAX = IAX1, IAX2
                        CALL NDF1_AWSTP( VTYPE, IAX, IACB, STATUS )
 4                   CONTINUE

*  If the array name is not recognised, then report an error.
                  ELSE
                     STATUS = NDF__CNMIN
                     CALL MSG_SETC( 'BADCOMP', COMP( F : L ) )
                     CALL ERR_REP( 'NDF_ASTYP_COMP',
     :                             'Invalid axis array component ' //
     :                             'name ''^BADCOMP'' specified ' //
     :                             '(possible programming error).',
     :                             STATUS )
                  END IF
               END IF
            END IF

*  Increment the character pointer to the start of the next element in
*  the array list and return to process the next element.
            I1 = I2 + 2
            GO TO 1
         END IF

*  If no error has occurred, but no non-blank array names have been
*  processed, then report an error.
         IF ( ( STATUS .EQ. SAI__OK ) .AND. ( NCOMP .EQ. 0 ) ) THEN
            STATUS = NDF__NOCMP
            CALL ERR_REP( 'NDF_ASTYP_NONE',
     :                    'No axis array component name specified ' //
     :                    '(possible programming error).', STATUS )
         END IF
      END IF

*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDF_ASTYP_ERR',
     :   'NDF_ASTYP: Error setting a new numeric type for an ' //
     :   'NDF axis array.', STATUS )
         CALL NDF1_TRACE( 'NDF_ASTYP', STATUS )
      END IF

      END
