    	SUBROUTINE SUBPAR_WRHELP( TOPIC, LIBRARY, FLAG, STATUS )
*+
*  Name:
*     SUBPAR_WRHELP

*  Purpose:
*     Outputs help information to the terminal using either the portable
*     or VMS help systems, depending on which type of library is
*     available.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_WRHELP( TOPIC, LIBRARY, FLAG, STATUS )

*  Description:
*     The routine does any required expansion of logical names in the LIBRARY
*     specification. If the resultant name has no filetype extension or the
*     extension .hlb or .shl, the routine checks for the existence of a file
*     with that name and extension .shl. If it is found, SUBPAR_PWHLP is called
*     to use the portable help system; otherwise SUBPAR_VWHLP is called.
*     On VMS this will use the VMS help system using the library name supplied;
*     on Unix SUBPAR_VWHLP will report an error.
*
*     This option is available to allow a gradual changeover of application
*     package help libraries to the portable system.
*
*     LIBRARY name expansion is handled by SUBPAR_HLPEX which, in the interests
*     of allowing the same interface file to work for VMS or Unix, permits
*     LIBRARY to take the form $ENV/name or ENV:name,  where ENV is a
*     logical-name/environment-variable and name is a simple filename. Either
*     the environment variable or name part may be used alone.
*
*     Environment variables in the VMS form (ENV:) will be forced to upper
*     case for translation, and any associated 'name' to lower case; the
*     Unix form ($ENV) will be translated as given.
*     On VMS this makes no difference but on Unix means that the filename of
*     the library spec must be lower case with extension .shl. The specification in the
*     interface file may be either case and the extension may be omitted.
*
*     The routine will do nothing if STATUS is given as not SAI__OK. If STATUS
*     is OK on entry, the routine will always flush any error reports generated
*     and return  STATUS = SAI__OK.

*  Deficiencies:
*     1.Assumptions about syntax of filenames are made - VMS and Unix forms
*       are catered for.
*     2.Lower case logical names may not be used on VMS
*
*  Arguments:
*     TOPIC = CHARACTER*(*) (Given)
*        The topic string
*     LIBRARY = CHARACTER*(*) (Given)
*        The help library name
*        It may take the form: NAME,
*                              LOG:NAME,
*                          or  $LOG/NAME
*        where NAME is a filename optionally including directory spec and
*                   filetype.
*          and LOG  is a logical name
*        Note that the VMS system assumes SYSLIB if no directory is specified
*     FLAG = INTEGER (Given)
*        Non-zero if help library search is required
*     STATUS = INTEGER (Given)
*        The global status.

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
*     28-APR-1992 (AJC):
*        Original version.
*     20-AUG-1992 (AJC):
*        Strip library name expansion into SUBPAR_HLPEX.
*     24-FEB-1992 (AJC):
*        Update description and comments
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'SUBPAR_ERR'       ! SUBPAR error codes

*  Arguments Given:
      CHARACTER*(*) TOPIC
      CHARACTER*(*) LIBRARY
      INTEGER FLAG

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER STRING_IANYR
      EXTERNAL STRING_IANYR      ! Find character in string

*  Local Variables:
      INTEGER STNM               ! Pointer to start of file name
      INTEGER ENDNM              ! Pointer to end of file name
      INTEGER FLEN               ! Used length of FNAME
      INTEGER ISTAT              ! INQUIRE status
      CHARACTER*200 FNAME        ! Expanded filename
      LOGICAL EXISTS             ! File existence
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set an error context - this routine will flush any errors which occur
*  and reset STATUS to SAI__OK
      CALL EMS_MARK

*  Now expand the library name
*  We have to do this here to check the filetype even though it will be
*  done again in SUBPAR_NAMETR when it's called by HLP.
      CALL SUBPAR_HLPEX( LIBRARY, FNAME, FLEN, STATUS )

*  Now check the file type if any and decide which help system to use.
      STNM = STRING_IANYR( FNAME(1:FLEN), ']/' ) + 1
      ENDNM = STRING_IANYR( FNAME(STNM:FLEN), '.') - 1
      IF ( ENDNM .LE. 0 ) THEN
         ENDNM = FLEN
      ELSE
         ENDNM = STNM + ENDNM - 1
      ENDIF

      EXISTS = .FALSE.
*  If no extension or .hlb or .shl is specified, look for a .shl file with
*  the given name
      IF ( STATUS .EQ. SAI__OK ) THEN
         IF ( ( ENDNM .EQ. FLEN )
     :   .OR. ( FNAME(ENDNM+1:FLEN) .EQ. '.shl' )
     :   .OR. ( FNAME(ENDNM+1:FLEN) .EQ. '.hlb' ) ) THEN

            INQUIRE( FILE=FNAME(1:ENDNM)//'.shl',
     :               EXIST=EXISTS,
     :               IOSTAT = ISTAT )

*        If there is a .shl, set the filetype to .shl
            IF ( EXISTS ) THEN

               FNAME(ENDNM+1:) = '.shl'
               IF ( ENDNM .EQ. FLEN ) FLEN = FLEN + 4

            ENDIF

         ENDIF

*     If a .shl exists, use it with the portable system
         IF ( EXISTS ) THEN
            CALL SUBPAR_PWHLP( TOPIC, FNAME(1:FLEN), FLAG, STATUS )

*     Otherwise, if the extension WAS .shl - ERROR
         ELSE IF ( FNAME(ENDNM+1:ENDNM+5) .EQ. '.shl' ) THEN
            STATUS = SUBPAR__IFNF
            CALL EMS_SETC( 'LIB', FNAME )
            CALL EMS_REP( 'SUP_WRHELP2',
     :      'SUBPAR: Help library ^LIB - not found', STATUS )

*     Otherwise use SUBPAR_VWHLP -
*     It should check the file exists first
*     For Unix this is an error condition - it means that something other
*     than .shl, .hlb or no extension is specified in the interface file.
*     The Unix version of SUBPAR_VWHLP will report this.
         ELSE
            CALL SUBPAR_VWHLP( TOPIC, FNAME(1:FLEN), FLAG, STATUS )

         ENDIF

      ENDIF

*  Flush any errors and reset STATUS
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL SUBPAR_EFLSH ( STATUS )
      ENDIF

*  Release the error context
      CALL EMS_RLSE

      END
