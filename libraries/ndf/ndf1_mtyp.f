      SUBROUTINE NDF1_MTYP( TYPLST, N, NDFS, COMP, ITYPE, DTYPE,
     :                      STATUS )
*+
*  Name:
*     NDF1_MTYP

*  Purpose:
*     Match the data type of an array component of a sequence of NDFs.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_MTYP( TYPLST, N, NDFS, COMP, ITYPE, DTYPE, STATUS )

*  Description:
*     The routine determines the numeric data type of the specified
*     array components of a series of NDFs and checks a comma-separated
*     list of the numeric types supported explicitly by an application,
*     returning the first type from this list which can be used to
*     process the data without loss of information. If no suitable type
*     appears in the list, then an error is reported to this effect.
*     The routine also returns the "maximised" numeric type (i.e. the
*     lowest precision type to which all the NDFs may be converted
*     without losing information).

*  Arguments:
*     TYPLST = CHARACTER * ( * ) (Given)
*        A comma-separated list of the numeric types supported by an
*        application (normally in increasing order of computational
*        cost).
*     N = INTEGER (Given)
*        The number of NDFs whose numeric data types are to be matched.
*     NDFS( N ) = INTEGER (Given)
*        Identifiers for the NDFs to be matched.
*     COMP = CHARACTER * ( * ) (Given)
*        Name of the NDF array component whose type is to be
*        considered.
*     ITYPE = CHARACTER * ( * ) (Returned)
*        The numeric data type (selected from the TYPLST argument) to
*        which the NDF components should be converted for processing by
*        the application. This value is returned as an upper case
*        character string of maximum length NDF__SZTYP.
*     DTYPE = CHARACTER * ( * ) (Returned)
*        The lowest precision numeric data type to which all the
*        specified NDF components may be converted without loss of
*        information. This value is returned as an upper case character
*        string of maximum length NDF__SZFTP.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  A comma-separated list of component names may also be given, in
*     which case all the specified NDF components will be considered
*     when calculating the "maximised" data type.
*     -  If insufficient precision is available using any of the data
*     types specified in TYPLST, then ITYPE will return the data type
*     with the highest precision in this list as the "best compromise"
*     result. An error will result, however, and a STATUS value of
*     NDF__TYPNI (type not implemented) will be returned. This error
*     may simply be annulled if the "best compromise" result is to be
*     used.
*     -  The value of DTYPE returned by this routine is intended as
*     input to the NDF_STYPE routine, to set the data type of an
*     application's output NDF appropriately to hold the result without
*     loss of information.

*  Algorithm:
*     -  Loop to consider each NDF identifier supplied.
*     -  Import the identifier and determine the component data type
*     code.
*     -  If this is the first NDF, then initialise the maximised data
*     type. For subsequent NDFs, form the new maximised data type.
*     -  Convert the maximised data type code into a character string
*     and return it.
*     -  Initialise the character pointer to the start of the
*     implemented type list and zero the list element count. Then loop
*     to extract each element from the list.
*     -  Find the final character of the next element (the last
*     character before a comma or end of string). Locate the first and
*     last non-blank characters in the element, checking that it is not
*     entirely blank. Increment the element count.
*     -  Convert the string into the element's implemented type code
*     and check if it has sufficient precision to process the maximised
*     data type without loss of information.
*     -  If the current element's precision is adequate, then return
*     the appropriate character string and quit searching the
*     implemented type list.
*     -  If this is the first element, then initialise the maximised
*     implemented type code. Subsequently, accumulate the maximum
*     precision type code which occurs in the implemented type list.
*     This will be used as the "best compromise" result if a suitable
*     implemented data type cannot be found.
*     -  Increment the character pointer to the start of the next
*     element in the component list and return to process the next
*     element.
*     -  If no adequate type was found in the implemented type list
*     because no non-blank component names have been processed, then
*     report an error.
*     -  If it was because no implemented type had adequate precision,
*     then return the type with maximum precision as the "best
*     compromise" solution and report an error.

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
*     18-JAN-1990 (RFWS):
*        Original version.
*     1-MAR-1990 (RFWS):
*        Fixed illegal character string concatenation.
*     16-NOV-1990 (RFWS):
*        Added a further status check.
*     27-NOV-1990 (RFWS):
*        Improved error messages.
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
      CHARACTER * ( * ) TYPLST
      INTEGER N
      INTEGER NDFS( N )
      CHARACTER * ( * ) COMP

*  Arguments Returned:
      CHARACTER * ( * ) ITYPE
      CHARACTER * ( * ) DTYPE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( NDF__SZTYP ) DATYP( NDF__TYPUB : NDF__MXTYP ) ! Data
      INTEGER CDTYPE             ! Component data type code
      INTEGER EITYPE             ! Element implemented type code
      INTEGER F                  ! Position of first non-blank character
      INTEGER I                  ! Loop counter for NDFs
      INTEGER I1                 ! Position of first component character
      INTEGER I2                 ! Position of last component character
      INTEGER IACB               ! Index to ACB entry
      INTEGER L                  ! Position of last non-blank character
      INTEGER MXDTYP             ! Max. data type code
      INTEGER MXITYP             ! Max. implemented type code
      INTEGER NELE               ! Number non-blank type list elements
      INTEGER TTYPE              ! Temporary type code variable
      LOGICAL OK                 ! Whether precision is sufficient

*  Local Data:
      DATA DATYP( NDF__TYPB ) / '_BYTE' / ! Data type code translations
      DATA DATYP( NDF__TYPD ) / '_DOUBLE' /
      DATA DATYP( NDF__TYPI ) / '_INTEGER' /
      DATA DATYP( NDF__TYPK ) / '_INT64' /
      DATA DATYP( NDF__TYPR ) / '_REAL' /
      DATA DATYP( NDF__TYPUB ) / '_UBYTE' /
      DATA DATYP( NDF__TYPUW ) / '_UWORD' /
      DATA DATYP( NDF__TYPW ) / '_WORD' /

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop to consider each NDF identifier supplied.
      DO 1 I = 1, N

*  Import the identifier and determine the component data type code.
         CALL NDF1_IMPID( NDFS( I ), IACB, STATUS )
         CALL NDF1_TYP( IACB, COMP, CDTYPE, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  If this is the first NDF, then initialise the maximised data type.
            IF ( I .EQ. 1 ) THEN
               MXDTYP = CDTYPE

*  For subsequent NDFs, form the new maximised data type.
            ELSE
               CALL NDF1_MXTYP( MXDTYP, CDTYPE, TTYPE, STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN
                  MXDTYP = TTYPE
               END IF
            END IF
         END IF
1     CONTINUE

*  Convert the maximised data type code into a character string and
*  return it.
      CALL NDF1_CCPY( DATYP( MXDTYP ), DTYPE, STATUS )

*  Initialise the character pointer to the start of the implemented
*  type list and zero the list element count. Then loop to extract each
*  element from the list.
      I1 = 1
      NELE = 0
2     CONTINUE                   ! Start of "DO WHILE" loop
      IF ( ( STATUS .EQ. SAI__OK ) .AND.
     :     ( I1 .LE. LEN( TYPLST ) ) ) THEN

*  Find the final character of the next element (the last character
*  before a comma or end of string).
         I2 = INDEX( TYPLST( I1 : ), ',' )
         IF ( I2 .EQ. 0 ) THEN
            I2 = LEN( TYPLST )
         ELSE
            I2 = I2 + I1 - 2
         END IF
         IF ( I2 .GE. I1 ) THEN

*  Locate the first and last non-blank characters in the element,
*  checking that it is not entirely blank.
            CALL CHR_FANDL( TYPLST( I1 : I2 ), F, L )
            IF ( L .GE. F ) THEN
               F = F + I1 - 1
               L = L + I1 - 1

*  Increment the element count.
               NELE = NELE + 1

*  Convert the string into the element's implemented type code and
*  check if it has sufficient precision to process the maximised data
*  type without loss of information.
               CALL NDF1_PSTYP( TYPLST( F : L ), EITYPE, STATUS )
               CALL NDF1_QITYP( MXDTYP, EITYPE, OK, STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN

*  If the current element's precision is adequate, then return the
*  appropriate character string and quit searching the implemented type
*  list.
                  IF ( OK ) THEN
                     CALL NDF1_CCPY( DATYP( EITYPE ), ITYPE, STATUS )
                     GO TO 3
                  END IF

*  If this is the first element, then initialise the maximised
*  implemented type code.
                  IF ( NELE .EQ. 1 ) THEN
                     MXITYP = EITYPE

*  Subsequently, accumulate the maximum precision type code which occurs
*  in the implemented type list. This will be used as the "best
*  compromise" result if a suitable implemented data type cannot be
*  found.
                  ELSE
                     CALL NDF1_MXTYP( MXITYP, EITYPE, TTYPE, STATUS )
                     IF ( STATUS .EQ. SAI__OK ) THEN
                        MXITYP = TTYPE
                     END IF
                  END IF

               END IF
            END IF
         END IF

*  Increment the character pointer to the start of the next element in
*  the component list and return to process the next element.
         I1 = I2 + 2
         GO TO 2
      END IF

*  If this point is reached without STATUS being set, then no adequate
*  type was found in the implemented type list.  If this is because no
*  non-blank component names have been processed, then report an error.
      IF ( STATUS .EQ. SAI__OK ) THEN
         IF ( NELE .EQ. 0 ) THEN
            STATUS = NDF__NOTYP
            CALL ERR_REP( 'NDF1_MTYP_NOTYP',
     :      'No implemented type(s) specified (possible programming ' //
     :      'error).', STATUS )

*  If it is because no implemented type had adequate precision, then
*  return the type with maximum precision as the "best compromise"
*  solution and report an error.
          ELSE
            CALL NDF1_CCPY( DATYP( MXITYP ), ITYPE, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN
               STATUS = NDF__TYPNI
               CALL MSG_SETC( 'DTYPE', DTYPE )
               CALL MSG_SETC( 'ITYPE', ITYPE )
               CALL ERR_REP( 'NDF1_MTYP_NI',
     :         'NDF array components of type ''^DTYPE'' cannot ' //
     :         'be processed by this application without loss of ' //
     :         'precision (the best available precision is ' //
     :         '''^ITYPE'').', STATUS )
            END IF
         END IF
      END IF
3     CONTINUE

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_MTYP', STATUS )

      END
