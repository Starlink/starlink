      SUBROUTINE GRP1_RDIND( UNIT, INFILE, SLOT, INDX, EDEP, EIFILE,
     :                       EMODGP, EMODIN, NADDED, FLAG, STATUS )
*+
*  Name:
*     GRP1_RDIND

*  Purpose:
*     Read the contents of an indirection text file into a group.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRP1_RDIND( UNIT, INFILE, SLOT, INDX, EDEP, EIFILE, EMODGP,
*                      EMODIN, NADDED, FLAG, STATUS )

*  Description:
*     Each record in the specified file is stored in the supplied group,
*     starting at the index given by argument INDX. Each record is
*     stored as a single element (i.e. no expansion takes place). Each
*     element is enclosed between opening and closing kernel dilimiters.
*     The number of records added to the group is returned in argument
*     NADDED.

*  Arguments:
*     UNIT = INTEGER (Given)
*        The fortran unit number on which to open the file.
*     INFILE = CHARACTER * ( * ) (Given)
*        The name of the indirection file.
*     SLOT = INTEGER (Given)
*        The slot number for the group in which to store the records
*        read from the file.
*     INDX = INTEGER (Given)
*        The index within the group given by SLOT at which the first
*        new element is to be stored.
*     EDEP = INTEGER (Given)
*        The indirection depth at which the name was specified. Zero
*        should be given if the name was given directly, instead of by
*        an indirection element.
*     EIFILE = INTEGER (Given)
*        The index within the FILES array (see routine GRP1_PTIND) at
*        which the the name of the indirection file in which the name
*        was specified is given. A value of zero should be given if the
*        name was given directly, instead of by an indirection element.
*     EMODGP = INTEGER (Given)
*        The GRP identifier for the group used as a basis for the name
*        if it was created as a result of a modification element. A
*        value of GRP__NOID should be given if the name was not created
*        as a result of a modification element.
*     EMODIN = INTEGER (Given)
*        The index within the group specified by MODGP, of the name
*        used as a basis for the name given by argument NAME.  If MODGP
*        is given as GRP__NOID, then MODIN is ignored.
*     NADDED = INTEGER (Returned)
*        The number of elements added to the group given by SLOT.
*     FLAG = LOGICAL (Returned)
*        Returned true if the last element in the file is flagged. The
*        flag character is removed in this case.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2010-2011 Science & Technology Facilities Council.
*     Copyright (C) 1994 Science & Engineering Research Council.
*     Copyright (C) 1999, 2000, 2003, 2004 Central Laboratory of the Research Councils.
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
*     DSB: David Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     19-JAN-1994 (DSB):
*        Original version.
*     27-AUG-1999 (DSB):
*        Added control character escape facility.
*     27-OCT-2000 (DSB):
*        Modify loop exit condition so that the last line of the file is
*        read even if it is terminated with an EOF rather than a newline.
*     7-JAN-2003 (DSB):
*        Expand shell meta-characters using GRP1_WILD.
*     2-SEP-2004 (TIMJ):
*        Switch from private GRP1_WILD to ONE_FIND_FILE
*     2010-03-19 (TIMJ):
*        Use PSX_WORDEXP instead of ONE_FIND_FILE
*     2011-03-07 (TIMJ):
*        Use ONE_WORDEXP_FILE so that we can trap cases
*        where no files match.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants.
      INCLUDE 'GRP_PAR'          ! GRP public constants.
      INCLUDE 'GRP_CONST'        ! GRP private constants.
      INCLUDE 'GRP_ERR'          ! GRP error values.

*  Arguments Given:
      INTEGER UNIT
      CHARACTER INFILE*(*)
      INTEGER SLOT
      INTEGER INDX
      INTEGER EDEP
      INTEGER EIFILE
      INTEGER EMODGP
      INTEGER EMODIN

*  Arguments Returned:
      INTEGER NADDED
      LOGICAL FLAG

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string
      INTEGER GRP1_INDEX         ! Finds un-escaped control characters
      LOGICAL GRP1_CHKCC         ! See if a character is a control character

*  Local Variables:
      CHARACTER COMC*1           ! Groups current omment character.
      CHARACTER DUMMY*(GRP__SZFNM) ! Dummy buffer for PSX_WORDEXP
      CHARACTER ESCC*1           ! The escape character
      CHARACTER FILE*(GRP__SZFNM)! A line of text read from the file
      CHARACTER FLAGC*1          ! Current flag character
      CHARACTER GEXP*(GRP__SZNAM)! A group expression read from a file
      CHARACTER KCLCC*1          ! Closing kernel delimiter character
      CHARACTER KOPCC*1          ! Opening kernel delimiter character
      CHARACTER LGEXP*(GRP__SZNAM)! Last times group expression
      CHARACTER LINE*(GRP__SZNAM)! A line of text read from the file
      INTEGER COM                ! Index of the comment character
      INTEGER GF                 ! First free character in GEXP.
      INTEGER ICONTX             ! Context for grp1_wild
      INTEGER INDIND             ! Index within the FILES array
      INTEGER IOERR              ! Fortran IO status value
      INTEGER TLEN               ! Used length of the group expression
      LOGICAL COMOK              ! .TRUE. if COMC is not NULL.
      LOGICAL EOF                ! Has end of file has been reached ?
      LOGICAL ESCOK              ! Is the escape character defined?
      LOGICAL FLAGOK             ! Is a flag character defined?
      LOGICAL KCLOK              ! Is closing kernel delimiter defined?
      LOGICAL KOPOK              ! Is opening kernel delimiter defined?
      LOGICAL VERB               ! Are we in a verbatim section?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the context value used by GRP1_WILD so that a new file
*  searching context will be started.
      ICONTX = 0

*  Use PSX_WORDEXP to expand any shell meta-characters in the supplied file
*  name. If the file name includes any wild-cards, the first matching file
*  name is returned.
      FILE = ' '
      CALL ONE_WORDEXP_FILE( INFILE, ICONTX, FILE, STATUS )

*  Currently no way to free the memory if we are not interested in additional
*  results so we have to read them back anyhow.
      DO WHILE ( ICONTX .NE. 0 )
         CALL ONE_WORDEXP_FILE( INFILE, ICONTX, DUMMY, STATUS )
      END DO

*  Trap empty match
      IF ( STATUS .EQ. SAI__OK .AND. FILE .EQ. ' ' ) THEN
         STATUS = GRP__FIOER
         CALL MSG_SETC( 'FNAME', INFILE )
         CALL MSG_SETI( 'UNIT', UNIT )
         CALL ERR_REP( 'GRP1_RDIND_ERR1', 'GRP1_RDIND: Error '//
     :        'opening text file ^FNAME on Fortran '//
     :        'unit ^UNIT - "File not found".', STATUS )
      END IF

*  If a file was found which matches the name...
      IF( FILE .NE. ' ' .AND. STATUS .EQ. SAI__OK ) THEN

*  Open the text file specified after the first character.
         OPEN( UNIT = UNIT, FILE = FILE, STATUS = 'OLD',
     :         IOSTAT = IOERR )

*  Check for errors, setting a suitable STATUS value and reporting the
*  error.
         IF ( IOERR .NE. 0 ) THEN
            STATUS = GRP__FIOER
            CALL MSG_SETC( 'FNAME', FILE )
            CALL MSG_SETI( 'UNIT', UNIT )
            CALL ERR_FIOER( 'MESSAGE', IOERR )
            CALL ERR_REP( 'GRP1_RDIND_ERR1', 'GRP1_RDIND: Error '//
     :                    'opening text file ^FNAME on Fortran '//
     :                    'unit ^UNIT - "^MESSAGE".', STATUS )
            GO TO 999
         END IF

*  Get the group's current escape character.
         CALL GRP1_CONC( SLOT, GRP__PESCC, ESCC, ESCOK, STATUS )

*  Get the group's current comment and flag characters.
         CALL GRP1_CONC( SLOT, GRP__PCOMC, COMC, COMOK, STATUS )
         CALL GRP1_CONC( SLOT, GRP__PFLGC, FLAGC, FLAGOK, STATUS )

*  Store the file name in the FILES array for this group, extending
*  the size of the array by one to make room for it. The index at which
*  te file name is stored within the FILES array is returned in INDIND.
         CALL GRP1_PTIND( SLOT, INFILE, INDIND, STATUS )

*  Initialise the number of elements added.
         NADDED = 0

*  Initialise the first character of the group expression to hold an
*  opening kernel delimiter (if one is defined).
         CALL GRP1_CONC( SLOT, GRP__POPKC, KOPCC, KOPOK, STATUS )
         CALL GRP1_CONC( SLOT, GRP__PCLKC, KCLCC, KCLOK, STATUS )
         IF( KOPOK .AND. KCLOK ) THEN
            GEXP( 1 : 1 ) = KOPCC
            GF = 2
         ELSE
            GF = 1
         END IF

*  Read the first record from the file.
         CALL GRP1_READF( UNIT, LINE, EOF, STATUS )

*  Indicate that no flag character has yet been found.
         FLAG = .FALSE.

*  Indicate that we are currently not in a verbatim section.
         VERB = .FALSE.

*  Loop round while text can be read from the file, and no error occurs.
         DO WHILE( LINE .NE. " " .OR.  .NOT. EOF .AND.
     :             STATUS .EQ. SAI__OK )

*  Create a copy of the text read form the file in which any control
*  characters within verbatim sections (delimited by "<!!" and !!>"
*  strings) are preceeded by escape charaters. The copy is stored in
*  GEXP, following any opening kernel delimiter.
            CALL GRP1_VRBTM( SLOT, LINE, VERB, GEXP( GF : ), STATUS )

*  If a comment character is defined...
            IF( COMOK ) THEN

*  Search for the first occurrence of the comment character in the
*  group expression.
               COM = GRP1_INDEX( GEXP, COMC, ESCC, ESCOK )

*  If a comment character was found, set the rest of the group
*  expression blank (including the comment character itself).
               IF( COM .GT. 0 ) GEXP( COM : ) = ' '

            ELSE
               COM = 0

            END IF

*  If a comment character was found in the first column, ignore this
*  record.
            IF( COM .NE. GF ) THEN

*  Get the used length of the group expression.
               TLEN = CHR_LEN( GEXP )

*  See if the last character is a flag character.
               IF( TLEN .GT. 0 ) THEN
                  FLAG = GRP1_CHKCC( GEXP, TLEN, FLAGC, ESCC, ESCOK )
     :                   .AND. FLAGOK
               ELSE
                  FLAG = .FALSE.
               END IF

*  If kernel delimiters are defined, append a closing kernel delimiter
*  to the group expression. Report an error if there is no room for the
*  closing kernel delimiter.
               IF( GF .EQ. 2 ) THEN
                  IF( TLEN .EQ. LEN( GEXP ) ) THEN
                     STATUS = GRP__INVEL
                     CALL MSG_SETC( 'REC', GEXP( GF: ) )
                     CALL ERR_REP( 'GRP1_RDIND_ERR2', 'GRP1_RDIND: '//
     :                          ' Group expression too long - ''^REC''',
     :                          STATUS )
                     GO TO 999
                  END IF
                  CALL CHR_APPND( KCLCC, GEXP, TLEN )
               END IF

*  Store this record in the group, giving it an indirection depth of
*  one more than that of the supplied group expression, and storing the
*  index of the file given in the indirection element.
               CALL GRP1_PTELM( SLOT, INDX + NADDED, GEXP, EDEP + 1,
     :                          INDIND, EMODGP, EMODIN, STATUS )

*  Save a copy of the record.
               LGEXP = GEXP

*  Increment the number of elements added to the group.
               NADDED = NADDED + 1

            END IF

*  Read the next record from the file.
            CALL GRP1_READF( UNIT, LINE, EOF, STATUS )

         END DO

*  Close the text file.
         CLOSE( UNIT )

*  If the last record read from the file terminated with a flag
*  character, remove it.
         IF( FLAG ) THEN
            IF( GF .EQ. 2 ) THEN
               LGEXP( TLEN - 1 : TLEN - 1 ) = ' '
            ELSE
               LGEXP( TLEN : TLEN ) = ' '
            END IF

*  Save the new value.
            CALL GRP1_PTELM( SLOT, INDX + NADDED - 1, LGEXP, EDEP + 1,
     :                       INDIND, EMODGP, EMODIN, STATUS )

         END IF

      END IF

*  End the search context.
 999  CONTINUE

      END
