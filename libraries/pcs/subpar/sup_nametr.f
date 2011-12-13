    	SUBROUTINE SUBPAR_NAMETR( COMMAND, INNAME, OUTNM, HSTAT )
*+
*  Name:
*     SUBPAR_NAMETR

*  Purpose:
*     Translates names for the portable help system used within adam.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_NAMETR(  COMMAND, INNAME, OUTNM, HSTAT )

*  Description:
*     The routine expands the given filename according to the ADAM rules,
*     translating environment variables etc. (see SUBPAR_HLPEX).
*     If no filetype is specified .shl is assumed.
*     If the expansion is a full pathname, that is used
*     If not, an attempt is made to find the file along the path defined by the
*     environment variable ADAM_HELP.
*     If that fails, an attempt is made to find the file in /star/help.
*     If that fails, bad status is returned.
*
*     The routine will do nothing if COMMAND is not 0
*
*  Deficiencies:
*     Assumptions about syntax of filenames are made - VMS and Unix forms
*     are catered for.
*
*  Arguments:
*     COMMAND = INTEGER (Given)
*        Not used
*     INNAME = CHARACTER*(*) (Given)
*        The given name
*     OUTNM = CHARACTER*(*) (Returned)
*        The translated name
*     HSTAT = INTEGER (Given)
*        The HLP system status.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council.
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
*     AJC: A J Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     20-AUG-1992 (AJC):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER COMMAND
      CHARACTER*(*) INNAME

*  Arguments Returned:
      CHARACTER*(*) OUTNM

*  Status:
      INTEGER HSTAT              ! HLP system status

*  External References:
      INTEGER STRING_IANYR
      EXTERNAL STRING_IANYR      ! Find character in string

*  Local Variables:
      INTEGER STATUS             ! ADAM status
      INTEGER STNM               ! Pointer to start of file name
      INTEGER ENDNM              ! Pointer to end of file name
      INTEGER FLEN               ! Used length of OUTNM
      INTEGER IND                ! Indicator for FIFIL
      INTEGER ISTAT              ! INQUIRE status
      CHARACTER*200 FILE         ! Intermediate file name
      LOGICAL EXISTS             ! File existence
*.

      IF ( COMMAND .NE. 0 ) RETURN

*  Initialise ADAM STATUS
      STATUS = SAI__OK

*  Now expand the library name
      CALL SUBPAR_HLPEX( INNAME, OUTNM, FLEN, STATUS )

*  IF OK, check the file type
      IF ( STATUS .EQ. SAI__OK ) THEN
         STNM = STRING_IANYR( OUTNM(1:FLEN), ']/' ) + 1
         ENDNM = STRING_IANYR( OUTNM(STNM:FLEN), '.') - 1
         IF ( ENDNM .LE. 0 ) THEN
            ENDNM = FLEN
         ELSE
            ENDNM = STNM + ENDNM - 1
         ENDIF

*     If no extension or .hlb is specified, default to .shl
         IF ( ( ENDNM .EQ. FLEN )
     :   .OR. ( OUTNM(ENDNM+1:FLEN) .EQ. '.hlb' ) ) THEN

            OUTNM(ENDNM+1:) = '.shl'
            IF ( ENDNM .EQ. FLEN ) FLEN = FLEN + 4

         ENDIF

*     If there is no directory spec, look along search path ADAM_HELP
         IF ( STNM .EQ. 1 ) THEN

            CALL SUBPAR_FIFIL( 'ADAM_HELP', OUTNM(1:FLEN-4), '.shl',
     :      'r', FILE, IND, STATUS )

*        or, failing that, in /star/help
            IF ( STATUS .NE. SAI__OK ) THEN

*           First annul any errors
               CALL EMS_ANNUL( STATUS )

*           The next bit will fail on VMS but, as there is no equivalent,
*           in the interest of common code, we ignore the failure.
*
*           Inquire in /star/help
               FILE = '/star/help/'//OUTNM(1:FLEN-4)//'.shl'
               INQUIRE( FILE = FILE,
     :                  EXIST = EXISTS,
     :                  IOSTAT = ISTAT )
               IF ( ISTAT .EQ. 0 ) THEN
                  IF ( EXISTS ) OUTNM = FILE
               ENDIF

            ELSE
*           Use the name found in the search path
               OUTNM = FILE

            ENDIF

         ENDIF

      ENDIF

*  Set HLP-style STATUS (No translation)
      IF ( STATUS .EQ. SAI__OK ) THEN
         HSTAT = 0
      ELSE
         HSTAT = -17
      ENDIF

      END
