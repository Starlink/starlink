      SUBROUTINE NDF1_FSPLT( FNAME, D1, D2, N1, N2, T1, T2, V1, V2,
     :                       STATUS )
*+
*  Name:
*     NDF1_FSPLT

*  Purpose:
*     Split a file name into directory, name, type and version fields.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_FSPLT( FNAME, D1, D2, N1, N2, T1, T2, V1, V2, STATUS )

*  Description:
*     The routine splits a full file name into a directory field, a
*     name field, a type field (which contains a leading '.') and a
*     version field and returns the character positions of the start and
*     end of each field.

*  Arguments:
*     FNAME = CHARACTER * ( * ) (Given)
*        The file specification.
*     D1 = INTEGER (Returned)
*        The position of the first character in the directory field.
*     D2 = INTEGER (Returned)
*        The position of the last character in the directory field.
*     N1 = INTEGER (Returned)
*        The position of the first character in the file name field.
*     N2 = INTEGER (Returned)
*        The position of the last character in the file name field.
*     T1 = INTEGER (Returned)
*        The position of the first character in the type field.
*     T2 = INTEGER (Returned)
*        The position of the last character in the type field.
*     V1 = INTEGER (Returned)
*        The position of the first character in the version field.
*     V2 = INTEGER (Returned)
*        The position of the last character in the version field.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If the supplied file name contains no directory field, then D2
*     is returned less than D1, if it contains no name field, then N2
*     is returned less than N1, if it contains no type field, then
*     T2 is returned less than T1, and if it contains no version field,
*     then V2 is returned less than V1.
*     -  On VMS, the directory field is considered to end at the first
*     occurence of the first of the following characters to be
*     identified as present: "]", ">" or ":".
*     -  On POSIX, the directory field ends at the last "/" and the type
*     field begins at the last "." (if any) which follows this. As there
*     is no version field, V2 will always be returned less than V1.

*  Machine-specific features used:
*     This routine unavoidably has to make assumptions about the form
*     of VMS and POSIX file names.

*  Implementation Deficiencies:
*     The file name supplied should be in a fully expanded form. If it
*     contains logical names (VMS) or environment variables (POSIX),
*     then it is not guaranteed that the file name syntax can be
*     correctly analysed. It will, however, work correctly on VMS file
*     names where the directory field is specified by a logical name
*     followed by a colon.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council

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
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     23-JUN-1993 (RFWS):
*        Original version.
*     12-OCT-1993 (RFWS):
*        Return the type field as well as the directory and name fields
*        and eliminate the VMS version field.
*     13-OCT-1993 (RFWS):
*        Changed to use the TCB_FNFMT flag to distinguish between
*        different formats of file name.
*     4-MAY-1994 (RFWS):
*        Return version field information as well.
*     4-MAY-1994 (RFWS):
*        Fixed bug: VMS null type field position was incorrect if name
*        field was also absent.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Global Variables:
      INCLUDE 'NDF_TCB'          ! NDF_ Tuning Control Block
*        TCB_FNFMT = INTEGER (Read)
*           Code specifying the file name format of the host machine.

*  Arguments Given:
      CHARACTER * ( * ) FNAME

*  Arguments Returned:
      INTEGER D1
      INTEGER D2
      INTEGER N1
      INTEGER N2
      INTEGER T1
      INTEGER T2
      INTEGER V1
      INTEGER V2

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IEND               ! Last non-blank character

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Ensure that the TCB is initialised.
      CALL NDF1_INTCB( STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Find the first and last non-blank characters in the file name.
         CALL CHR_FANDL( FNAME, D1, IEND )

*  If the file name is blank, then report an error.
         IF ( D1 .GT. IEND ) THEN
            STATUS = NDF__NAMIN
            CALL ERR_REP( 'NDF1_FSPLT_BLNK',
     :                    'Blank file specification supplied.',
     :                    STATUS )

*  VMS.
*  ===
*  If we are on a VMS system, then search for a ']' or '>' character,
*  which may mark the end of an explicit directory specification (note
*  the latter may result if a file name is entered on a DCL command
*  line). If neither of these is found, then search for a ':' which may
*  mark the end of a logical name representing the directory.
         ELSE IF ( TCB_FNFMT .EQ. NDF__VMS ) THEN
            D2 = INDEX( FNAME( D1 : IEND ), ']' )
            IF ( D2 .EQ. 0 ) D2 = INDEX( FNAME( D1 : IEND ), '>' )
            IF ( D2 .EQ. 0 ) D2 = INDEX( FNAME( D1 : IEND ), ':' )

*  Correct for the starting position of the search. D2 will be changed
*  from zero to one less than D1 if no directory specification was
*  found.
            D2 = D2 + D1 - 1

*  Return the position of the first character in the name field (equal
*  to D1 if no directory specification was found).
            N1 = D2 + 1

*  If there is nothing left, then there cannot be a name, type or
*  version field, so return null values.
            IF ( N1 .GT. IEND ) THEN
               N2 = N1 - 1
               T1 = 1
               T2 = 0
               V1 = 1
               V2 = 0

*  Otherwise, search for a ';' which introduces a version number. If
*  not found, return the last character position of the type field and
*  a null version field position.
            ELSE
               T2 = INDEX( FNAME( N1 : IEND ), ';' )
               IF ( T2 .EQ. 0 ) THEN
                  T2 = IEND
                  V2 = 0
                  V1 = 1

*  If a version number is present, then adjust the last character
*  position of the type field and return the bounds of the version
*  field.
               ELSE
                  T2 = T2 + N1 - 2
                  V1 = T2 + 1
                  V2 = IEND
               END IF

*  If there is nothing left, then return null name and type field
*  limits.
               IF ( T2 .LT. N1 ) THEN
                  N2 = N1 - 1
                  T1 = T2 + 1

*  Otherwise, search for the '.' which marks the start of the type
*  field. If not found, then there is no type field. Otherwise, return
*  the final character position of the name field.
               ELSE
                  T1 = INDEX( FNAME( N1 : T2 ), '.' )
                  IF ( T1 .EQ. 0 ) THEN
                     N2 = T2
                     T1 = T2 + 1
                  ELSE
                     T1 = T1 + N1 - 1
                     N2 = T1 - 1
                  END IF
               END IF
            END IF

*  POSIX.
*  =====
*  If we are on a POSIX system, search for the last '/' character which
*  delimits the final field of the file's path name.
         ELSE IF ( TCB_FNFMT .EQ. NDF__POSIX ) THEN
            DO 1 D2 = IEND, D1, -1
               IF ( FNAME( D2 : D2 ) .EQ. '/' ) GO TO 2
 1          CONTINUE
 2          CONTINUE

*  Return the position of the first character in the name field (equal
*  to D1 if no directory specification was found).
            N1 = D2 + 1

*  If there is nothing left, then there cannot be a name field or a
*  type field, so return null values.
            IF ( N1 .GT. IEND ) THEN
               N2 = N1 - 1
               T1 = 1
               T2 = 0

*  Otherwise, return the last character position of the type field.
            ELSE
               T2 = IEND

*  Search backwards for the last '.' which marks the start of the type
*  field.
               DO 3 T1 = T2, N1, -1
                  IF ( FNAME( T1 : T1 ) .EQ. '.' ) GO TO 4
 3             CONTINUE
 4             CONTINUE

*  If not found, then there is no type field. Otherwise, return
*  the final character position of the name field.
               IF ( T1 .LT. N1 ) THEN
                  N2 = T2
                  T1 = T2 + 1
               ELSE
                  N2 = T1 - 1
               END IF
            END IF

*  Return a null version field.
            V1 = 1
            V2 = 0

*  If the file name format code is not recognied, then report an error.
         ELSE
            STATUS = NDF__FATIN
            CALL MSG_SETI( 'FNFMT', TCB_FNFMT )
            CALL ERR_REP( 'NDF1_FSPLT_FMT',
     : 'Invalid file name format code (value = ^FNFMT) encountered ' //
     : 'in the NDF_ system Tuning Control Block (internal ' //
     : 'programming error).',
     :                    STATUS )
         END IF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_FSPLT', STATUS )

      END
