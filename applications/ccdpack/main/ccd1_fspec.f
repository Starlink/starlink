      SUBROUTINE CCD1_FSPEC( FSPEC, DFSPEC, FLDNAM, FIELD, STATUS )
*+
*  Name:
*     CCD1_FSPEC

*  Purpose:
*     Produces a new file specification by modification of an old file
*     specification.

*  Language:
*     Fortran-77.

*  Invocation:
*     CALL CCD1_FSPEC( FSPEC, DFSPEC, FLDNAM, FIELD, STATUS )

*  Description:
*     This is a UNIX specific routine.
*
*     The supplied file specification (FSPEC) is parsed, and the
*     requested fields returned. Any fields missing from FSPEC are
*     defaulted to the corresponding field values from the default file
*     specification (DFSPEC). The full file specification can be
*     returned by supplying a blank value for FLDNAM.

*  Arguments:
*     FSPEC = CHARACTER (Given)
*        The original file specification.
*     DFSPEC = CHARACTER (Given)
*        The default file specification.
*     FLDNAM = CHARACTER (Given)
*        A string specifying which fields are to be returned in FIELD.
*        FLDNAM can take any of the values: DEVICE, DIRECTORY, NAME,
*        TYPE, VERSION. In addition, a blank value will return the full
*        file specification in FIELD. Unambiguous abbreviations may be
*        given.
*        The DEVICE and VERSION fields return ' ' in this version.
*     FIELD = CHARACTER (Returned)
*        The requested field from the full file specification.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - This is a simple minded look-a-like to the VMS version its
*     capabilities are limited and should not be exceeded.

*  Copyright:
*     Copyright (C) 1992, 2000 Central Laboratory of the Research Councils
*     Copyright (C) 2005 Particle Physics and Astronomy Research Council.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     MBT: Mark Taylor (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     27-FEB-1992 (PDRAPER):
*        Original Unix version - based on VMS version of same name.
*     29-JUN-2000 (MBT):
*        Renamed from IRG1_FSPEC to CCD1_FSPEC.
*     27-DEC-2005 (TIMJ):
*        Use CHR_LASTO rather than CCD1_LASTO
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! Standard GRP constants

*  Arguments Given:
      CHARACTER * ( * ) FSPEC
      CHARACTER * ( * ) DFSPEC
      CHARACTER * ( * ) FLDNAM

*  Arguments Returned:
      CHARACTER * ( * ) FIELD

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Function giving used length of a
                                 ! string.

*  Local Variables:
      CHARACTER * ( GRP__SZNAM ) DIR1 ! Directory specification in FSPEC
      CHARACTER * ( GRP__SZNAM ) DIR2 ! Directory specification in
                                      ! DFSPEC
      CHARACTER * ( GRP__SZNAM ) NAM1 ! Name specification in FSPEC
      CHARACTER * ( GRP__SZNAM ) NAM2 ! Name specification in DFSPEC
      CHARACTER * ( GRP__SZNAM ) TYP1 ! Type specification in FSPEC
      CHARACTER * ( GRP__SZNAM ) TYP2 ! Type specification in DFSPEC
      CHARACTER UPFLD * ( 2 )    ! First two characters of FLDNAM
                                 ! converted to upper case.
      INTEGER DIREND             ! Position of last directory character
      INTEGER NAMEND             ! Position of last name character
      INTEGER IAT                ! Temporary string position
      INTEGER STRLEN             ! Length of input strings excluding
                                 ! trailing blanks
      INTEGER TYPEND             ! Position of last type character
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Convert input type to upper case.
      UPFLD = FLDNAM( 1 : MIN( LEN( FLDNAM ), 2 ) )
      CALL CHR_UCASE( UPFLD )

*=======================================================================
*  Current name parsing.
*=======================================================================
*  Get length of string.
      STRLEN = MAX( CHR_LEN( FSPEC ), 1 )

*  Parse input file name into directory, name and type.
*  Look for directory terminator first -- last '/'
      CALL CHR_LASTO( FSPEC, '/', DIREND )
      IF ( DIREND .EQ. 0 ) THEN

*  No directory information just supply a blank.
         DIR1 = ' '
      ELSE

*  Extract the value.
         DIR1 = FSPEC( 1 : DIREND )
      END IF

*  Look for name value, this should be indicated by the . terminator or
*  end of string (excluding trailing blanks) after the directory
*  specification.
      IF ( DIREND .EQ. 0 ) THEN
         IAT = 1
      ELSE
         IAT = DIREND + 1
      END IF
      NAMEND = INDEX( FSPEC( IAT : ), '.' )
      IF ( NAMEND .EQ. 0 ) THEN

*  May be no name, are there any characters after the directory?
         IF ( DIREND .EQ. STRLEN ) THEN

*  No there are none. Just return a ' '
            NAM1 = ' '
         ELSE

*  Set NAMEND to end of string.
            NAMEND = STRLEN
         END IF
      ELSE

*  NAMEND non-zero, adjust for IAT offset.
         NAMEND = NAMEND + IAT - 2
      END IF

*  Extract the NAME
      IF ( NAMEND .NE. 0 ) THEN
         NAM1 = FSPEC( DIREND + 1 : NAMEND )

*  Look for file type - any characters after the name.
         IF ( NAMEND .NE. STRLEN ) THEN
            TYP1 = FSPEC( NAMEND + 1 : STRLEN )
         ELSE

*  No type supply a null.
            TYP1 = ' '
         END IF
      ELSE

*  NAMEND zero - may be just a type here, is there a period?
         IF( INDEX( FSPEC( IAT : ), '.' ) .NE. 0 ) THEN
            TYP1 = FSPEC
         ELSE
            TYP1 = ' '
         END IF
      END IF

*=======================================================================
*  Default string parsing
*=======================================================================
*  Get length of string.
      STRLEN = MAX( 1, CHR_LEN( DFSPEC ) )

*  Parse input file name into directory, name and type.
*  Look for directory terminator first -- last '/'
      CALL CHR_LASTO( DFSPEC, '/', DIREND )
      IF ( DIREND .EQ. 0 ) THEN

*  No directory information just supply a blank.
         DIR2 = ' '
      ELSE

*  Extract the value.
         DIR2 = DFSPEC( 1 : DIREND )
      END IF

*  Look for name value, this should be indicated by the . terminator or
*  end of string (excluding trailing blanks) after the directory
*  specification.
      IF ( DIREND .EQ. 0 ) THEN
         IAT = 1
      ELSE
         IAT = DIREND + 1
      END IF
      NAMEND = INDEX( DFSPEC( IAT : ), '.' )
      IF ( NAMEND .EQ. 0 ) THEN

*  May be no name, are there any characters after the directory?
         IF ( DIREND .EQ. STRLEN ) THEN

*  No there are none. Just return a ' '
            NAM2 = ' '
         ELSE

*  Set NAMEND to end of string.
            NAMEND = STRLEN
         END IF
      ELSE

*  NAMEND non-zero, adjust for IAT offset.
         NAMEND = NAMEND + IAT - 2
      END IF

*  Extract the NAME
      IF ( NAMEND .NE. 0 ) THEN
         NAM2 = DFSPEC( DIREND + 1 : NAMEND )

*  Look for file type - any characters after the name.
         IF ( NAMEND .NE. STRLEN ) THEN
            TYP2 = DFSPEC( NAMEND + 1 : STRLEN )
         ELSE

*  No type supply a null.
            TYP2 = ' '
         END IF
      ELSE

*  NAMEND zero - may be just a type here, is there a period?
         IF( INDEX( DFSPEC( IAT : ), '.' ) .NE. 0 ) THEN
            TYP2 = DFSPEC
         ELSE
            TYP2 = ' '
         END IF
      END IF

*=======================================================================
*  If a specific field is required.
*=======================================================================
      IF( FLDNAM .NE. ' ' ) THEN

*  Copy the relevant field to the FIELD argument.
         IF( INDEX( UPFLD, 'DE') .EQ. 1 ) THEN

*  Not activated in Unix supply a blank.
            FIELD = ' '
         ELSE IF( INDEX( UPFLD, 'DI') .EQ. 1 ) THEN

*  Supply the directory name
            IF ( DIR1 .NE. ' ' ) THEN
               FIELD = DIR1
            ELSE
               FIELD = DIR2
            END IF
         ELSE IF( INDEX( UPFLD, 'N') .EQ. 1 ) THEN

*  Supply the name
            IF ( NAM1 .NE. ' ' ) THEN
               FIELD = NAM1
            ELSE
               FIELD = NAM2
            END IF

         ELSE IF( INDEX( UPFLD, 'T') .EQ. 1 ) THEN

*  Supply the type
            IF ( TYP1 .NE. ' ' ) THEN
               FIELD = TYP1
            ELSE
               FIELD = TYP2
            END IF

         ELSE IF( INDEX( UPFLD, 'V') .EQ. 1 ) THEN

*  No versions in unix.
            FIELD = ' '

*  If the field name was unrecognised, report an error.
         ELSE
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'NAM', FLDNAM )
            CALL ERR_REP( 'CCD1_FSPEC_ERR1',
     :              'CCD1_FSPEC: Unrecognised field requested - "^NAM"',
     :               STATUS )
            GO TO 999
         END IF


      ELSE

*  Full name required use strings that we've got.
         IF ( DIR1 .EQ. ' ' ) DIR1 = DIR2
         DIREND = MAX(1, CHR_LEN( DIR1 ) )
         IF ( NAM1 .EQ. ' ' ) NAM1 = NAM2
         NAMEND = MAX(1, CHR_LEN( NAM1 ) )
         IF ( TYP1 .EQ. ' ' ) TYP1 = TYP2
         TYPEND = MAX(1, CHR_LEN( TYP1 ) )

         FIELD = DIR1( : DIREND ) //
     :           NAM1( : NAMEND ) //
     :           TYP1( : TYPEND )
      END IF

 999  CONTINUE

      END
* $Id$
