      SUBROUTINE NDFECHO( STATUS )
*+
*  Name:
*     NDFECHO

*  Purpose:
*     Displays a group of NDF names.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL NDFECHO( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application lists the names of the supplied NDFs to the
*     screen, optionally filtering them using a regular expression. Its
*     primary use is within scripts that need to process groups of NDFs.
*     Instead of the full name, a required component of the name may be
*     displayed instead (see Parameter SHOW).
*
*     Two modes are available.
*
*     - If the NDFs are specified via the NDF parameter, then the NDFs
*     must exist and be accessible (an error is reported otherwise). The
*     NDF names obtained can then be modified by supplying a suitable
*     GRP modification expression such as "*_A" for Parameter MOD.
*
*     - To list NDFs that may not exist, supply a null (!) value for
*     Parameter NDF and the main group expression to Parameter MOD.

*  Usage:
*     ndfecho ndf [mod] [first] [last] [show]

*  ADAM Parameters:
*     ABSPATH = _LOGICAL (Read)
*        If TRUE, any relative NDF paths are converted to absolute,
*        using the current working directory. [FALSE]
*     EXISTS = _LOGICAL (Read)
*        If TRUE, then only display paths for NDFs specified by Parameter
*        MOD that actually exist and are accessible. [FALSE]
*     FIRST = _INTEGER (Read)
*        The index of the first NDF to be tested. A null (!) value
*        causes the first NDF to be used (Index 1). [!]
*     LAST = _INTEGER (Read)
*        The index of the last NDF to be tested. If a non-null value is
*        supplied for FIRST, then the run-time default for LAST is equal
*        to the supplied FIRST value (so that only a single NDF will be
*        tested). If a null value is supplied for FIRST, then the
*        run-time default for LAST is the last NDF in the supplied
*        group. []
*     LOGFILE = FILENAME (Write)
*        The name of a text file in which to store the listed NDF names.
*        If a null (!) value is supplied, no log file is created. [!]
*     MOD = LITERAL (Read)
*        An optional GRP modification expression that will be used to
*        modify any names obtained via the Parameter NDF.  For instance,
*        if MOD is "*_A" then the supplied NDF names will be modified
*        by appending "_A" to them. No modification occurs if a null (!)
*        value is supplied.
*
*        If a null value is supplied for Parameter NDF then the value
*        supplied for Parameter MOD should not include an asterisk,
*        since there are no names to be modified. Instead, the MOD value
*        should specify an explicit group of NDF names that do not need
*        to exist.
*
*        The list can be filtered to remove any NDFs that do not exist
*        (see Parameter EXISTS). [!]
*     NDF = NDF (Read)
*        A group of existing NDFs, or null (!).  This should be given as
*        a comma-separated list, in which each list element can be one
*        of the following options:
*
*        - An NDF name, optionally containing wild-cards and/or regular
*        expressions ("*", "?", "[a-z]" etc.).
*
*        - The name of a text file, preceded by an up-arrow character
*        "^".  Each line in the text file should contain a
*        comma-separated list of elements, each of which can in turn be
*        an NDF name (with optional wild-cards, etc.), or another file
*        specification (preceded by an up-arrow).  Comments can be
*        included in the file by commencing lines with a hash character
*        "#".
*
*        If the value supplied for this parameter ends with a hyphen,
*        then you are re-prompted for further input until a value is
*        given which does not end with a hyphen.  All the NDFs given in
*        this way are concatenated into a single group.
*
*        If a null (!) value is supplied, then the displayed list of
*        NDFs is determined by the value supplied for the MOD parameter.
*     NMATCH = _INTEGER (Write)
*        An output parameter to which is written the number of NDFs
*        between FIRST and LAST that match the pattern supplied by
*        Parameter PATTERN.
*     PATTERN = LITERAL (Read)
*        Specifies a pattern matching template using the syntax
*        described below in "Pattern Matching Syntax".  Each NDF is
*        displayed only if a match is found between this pattern and the
*        item specified by Parameter SHOW.   A null (!) value causes
*        all NDFs to be displayed. [!]
*     SHOW = LITERAL (Read)
*        Specifies the information to be displayed about each NDF. The
*        options are as follows.
*
*        - "Base" -- The base file name.
*
*        - "Dir" -- The directory path (if any).
*
*        - "Fspec" -- The directory, base name and file type
*        concatenated to form a full file specification.
*
*        - "Ftype" -- The file type (usually ".sdf" but may not be if
*        any foreign NDFs are supplied).
*
*        - "HDSpath" -- The HDS path within the container file (if any).
*
*        - "Path" -- The full name of the NDF as supplied by the user.
*
*        - "Slice" -- The NDF slice specification (if any).
*
*        Items that do not match the pattern specified by Parameter
*        PATTERN are not displayed. ["Path"]
*     SIZE = _INTEGER (Write)
*        An output parameter to which is written the total number of
*        NDFs in the specified group.
*     VALUE = LITERAL (Write)
*        An output parameter to which is written information about the
*        first NDF that matches the pattern specified by Parameter
*        PATTERN. The information to write is specified by the SHOW
*        parameter.

*  Examples:
*     ndfecho mycont
*        Report the full path of all the NDFs within the HDS container
*        file "mycont.sdf". The NDFs must all exist.
*     ndfecho ^files.lis first=4 show=base
*        This reports the file base name for just the fourth NDF in the
*        list specified within the text file "files.lis". The NDFs must
*        all exist.
*     ndfecho ^files.lis *_a logfile=log.lis
*        This reports the names of the NDFs listed in text file
*        files.lis, but appending "_a" to the end of each name. The NDFs
*        must all exist. The listed NDF names are written to a new text
*        file called "log.lis".
*     ndfecho in=! mod={^base}|_a|_b|
*        This reports the names of the NDFs listed in text file "base",
*        but replacing "_a" with "_b" in their names. The NDFs need not
*        exist since they are completely specified by Parameter MOD and
*        not by Parameter NDF.

*  Pattern Matching Syntax:
*     The syntax for the PATTERN parameter value is a minimal form of
*     regular expression. The following atoms are allowed.
*
*     "[chars]" -- Matches any of the characters within the brackets.
*     "[^chars]" -- Matches any character that is not within the
*                   brackets (ignoring the initial "^" character).
*     "." -- Matches any single character.
*     "\d" -- Matches a single digit.
*     "\D" -- Matches anything but a single digit.
*     "\w" -- Matches any alphanumeric character, and "_".
*     "\W" -- Matches anything but alphanumeric characters, and "_".
*     "\s" -- Matches white space.
*     "\S" -- Matches anything but white space.
*
*     Any other character that has no special significance within a
*     regular expression matches itself.  Characters that have special
*     significance can be matched by preceding them with a backslash
*     (\) in which case their special significance is ignored (note,
*     this does not apply to the characters in the set dDsSwW).
*
*     Note, minus signs ("-") within brackets have no special
*     significance, so ranges of characters must be specified
*     explicitly.
*
*     The following quantifiers are allowed.
*
*     "*" -- Matches zero or more of the preceding atom, choosing the
*            largest possible number that gives a match.
*     "*?" -- Matches zero or more of the preceding atom, choosing the
*             smallest possible number that gives a match.
*     "+" -- Matches one or more of the preceding atom, choosing the
*            largest possible number that gives a match.
*     "+?" -- Matches one or more of the preceding atom, choosing the
*             smallest possible number that gives a match.
*     "?" -- Matches zero or one of the preceding atom.
*     "{n}" -- Matches exactly "n" occurrences of the preceding atom.
*
*     The following constraints are allowed.
*
*     "^" -- Matches the start of the test string.
*     "$" -- Matches the end of the test string.
*
*     Multiple templates can be concatenated, using the "|" character to
*     separate them.  The test string is compared against each one in
*     turn until a match is found.

*  Copyright:
*     Copyright (C) 2012 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
*     Boston, MA 02110-1301, USA.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     24-SEP-2012 (DSB):
*        Original version.
*     11-OCT-2012 (DSB):
*        Added Parameters MOD, LOGFILE and ABSPATH.
*     12-OCT-2012 (DSB):
*        Added Parameter PATTERN.
*     28-JUN-2013 (DSB):
*        Added Parameter EXISTS.
*     29-NOV-2017 (DSB):
*        Ensure output NDF paths displayed on the screen are not split
*        up into multiple lines.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! SSE global definitions
      INCLUDE 'GRP_PAR'          ! GRP constants
      INCLUDE 'PAR_ERR'          ! PAR error constants
      INCLUDE 'AST_PAR'          ! AST constants and functions
      INCLUDE 'DAT_PAR'          ! DAT constants

*  Status:
      INTEGER STATUS             ! Global inherited status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string

*  Local Variables:
      CHARACTER FIELDS( 6 )*(GRP__SZNAM)! Info about next NDF
      CHARACTER PAT*400      ! Pattern matching template
      CHARACTER RESULT*400   ! Result of substitutions
      CHARACTER SHOW*7       ! What to show
      CHARACTER TEXT*(GRP__SZNAM)! Info to display
      INTEGER FIRST          ! The index of the first NDF to display
      INTEGER I              ! Index of next NDF to display
      INTEGER IAT            ! Used length of string
      INTEGER IGRP0          ! GRP id. for group holding existing NDFs
      INTEGER IGRP1          ! GRP id. for group holding listed NDFs
      INTEGER IGRP2          ! GRP id. for log file group
      INTEGER ILEN           ! Length of the NDF info item
      INTEGER INDF           ! Identifier for existing NDF
      INTEGER ISHOW          ! What to show
      INTEGER LAST           ! The index of the last NDF to display
      INTEGER NMATCH         ! Number of matching NDFs
      INTEGER SIZE0          ! Size of group IGRP0
      INTEGER SIZE1          ! Size of group IGRP1
      LOGICAL ABSPTH         ! Convert to absolute paths?
      LOGICAL EXISTS         ! Only display existing NDFs?
      LOGICAL FLAG           ! Was group expression flagged?
      LOGICAL MATCH          ! DId the NDF match the pattern?
*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get a group containing the names of any existing NDFs to be listed.
*  Allow this group to contain zero NDFs.
      IGRP0 = GRP__NOID
      CALL KPG1_RGNDF( 'NDF', 0, 0, '  Give more NDFs...',
     :                 IGRP0, SIZE0, STATUS )

*  If no value was supplied for NDF, annul the error, and continue to
*  ue IGRP0 in place of IGRP1.
      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL GRP_DELET( IGRP0, STATUS )
         CALL ERR_ANNUL( STATUS )
         SIZE0 = 0
      ELSE IF( SIZE0 .EQ. 0 ) THEN
         CALL GRP_DELET( IGRP0, STATUS )
      END IF

*  Allow the user to modify the above group of existing NDFs, or to
*  specify a group of NDFs that may not exist. Loop until a group
*  expression is given which is not terminated by a flag character.
      IGRP1 = GRP__NOID
      FLAG = .TRUE.
      DO WHILE( FLAG .AND. STATUS .EQ. SAI__OK )
         CALL NDG_CREAT( 'MOD', IGRP0, IGRP1, SIZE1, FLAG, STATUS )
         IF( FLAG ) THEN
            CALL PAR_CANCL( 'MOD', STATUS )
            CALL MSG_OUT( ' ', 'Please supply more values for '//
     :                    'parameter MOD.', STATUS )
         END IF
      END DO

*  If no value was supplied for MOD, annul the error, and continue to
*  use IGRP0 in place of IGRP1.
      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL GRP_DELET( IGRP1, STATUS )
         CALL ERR_ANNUL( STATUS )
         IGRP1 = IGRP0
         IGRP0 = GRP__NOID
         SIZE1 = SIZE0
      END IF

*  Create a group in which to store the names to be logged.
      CALL GRP_NEW( ' ', IGRP2, STATUS )

*  Only proceed if some NDFs were specified.
      IF( SIZE1 .GT. 0 ) THEN

*  See what the user wants to display.
         CALL PAR_CHOIC( 'SHOW', 'PATH', 'SLICE,HDSPATH,FTYPE,BASE,'//
     :                   'DIR,PATH,FSPEC', .TRUE., SHOW, STATUS )
         IF( SHOW .EQ. 'SLICE' ) THEN
            ISHOW = 1
         ELSE IF( SHOW .EQ. 'HDSPATH' ) THEN
            ISHOW = 2
         ELSE IF( SHOW .EQ. 'FTYPE' ) THEN
            ISHOW = 3
         ELSE IF( SHOW .EQ. 'BASE' ) THEN
            ISHOW = 4
         ELSE IF( SHOW .EQ. 'DIR' ) THEN
            ISHOW = 5
         ELSE IF( SHOW .EQ. 'PATH' ) THEN
            ISHOW = 6
         ELSE
            ISHOW = 7
         END IF

*  Abort if an error has occurred.
         IF( STATUS .NE. SAI__OK ) GO TO 999

*  Get the index of the first NDF to test.
         CALL PAR_GDR0I( 'FIRST', 0, 1, SIZE1, .FALSE., FIRST, STATUS )

*  If a null value was supplied, annull the error and start from the
*  first NDF in the group (Index 1). Also set the dynamic default for
*  LAST to the last NDF in the group.
         IF( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            FIRST = 1
            LAST = SIZE1

*  If a value was supplied for FIRST, use the same value as the dynamic
*  default for LAST.
         ELSE
            LAST = FIRST
         END IF

*  Get the index of the last NDF to test, using the above dynamic
*  default.
         CALL PAR_GDR0I( 'LAST', LAST, FIRST, SIZE1, .TRUE., LAST,
     :                   STATUS )

*  If required, ensure all paths in the group are absolute.
         CALL PAR_GET0L( 'ABSPATH', ABSPTH, STATUS )
         IF( ABSPTH ) CALL NDG_ABPTH( IGRP1, STATUS )

*  Get the filter pattern
         IF( STATUS .NE. SAI__OK ) GO TO 999
         CALL PAR_GET0C( 'PATTERN', PAT, STATUS )
         IF( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            PAT = ' '
         END IF

*  See if only existing and accessible NDFs are to be displayed.
         CALL PAR_GET0L( 'EXISTS', EXISTS, STATUS )

*  Avoid line breaks in the screen output created by MSG_OUT.
         CALL MSG_TUNE( 'SZOUT', 0, STATUS )

*  Loop round testing the required NDFs.
         NMATCH = 0
         DO I = FIRST, LAST

*  Get all items of information about the NDF.
            CALL NDG_GTSUP( IGRP1, I, FIELDS, STATUS )

*  Form the full file spec if required.
            IF( ISHOW .EQ. 7 ) THEN
               TEXT = ' '
               IAT = 0
               CALL CHR_APPND( FIELDS( 5 ), TEXT, IAT )
               CALL CHR_APPND( FIELDS( 4 ), TEXT, IAT )
               CALL CHR_APPND( FIELDS( 3 ), TEXT, IAT )
            ELSE
               TEXT = FIELDS( ISHOW )
            END IF

*  See if the pattern matches the item.
            IF( PAT .NE. ' ' ) THEN
               MATCH = AST_CHRSUB( TEXT, PAT, RESULT, STATUS )
            ELSE
               RESULT = TEXT
               MATCH = .TRUE.
            END IF

*  If required, test the NDF exists.
            IF( MATCH .AND. EXISTS .AND. STATUS .EQ. SAI__OK ) THEN
               CALL NDF_FIND( DAT__ROOT, RESULT, INDF, STATUS )
               CALL NDF_ANNUL( INDF, STATUS )
               IF( STATUS .NE. SAI__OK ) THEN
                  CALL ERR_ANNUL( STATUS )
                  MATCH = .FALSE.
               END IF
            END IF

*  If so...
            IF( MATCH ) THEN

*  Display the required item.
               CALL MSG_SETC( 'I', RESULT )
               CALL MSG_OUT( ' ', '^I', STATUS )

*  Add it to the log group.
               CALL GRP_PUT1( IGRP2, RESULT, 0, STATUS )

*  Write the first NDF to an output parameter.
               IF( I .EQ. FIRST ) THEN
                  ILEN = CHR_LEN( RESULT )
                  IF( ILEN .EQ. 0 ) ILEN = 1
                  CALL PAR_PUT0C( 'VALUE', RESULT( : ILEN ), STATUS )
               END IF

*  Count the number of matches
               NMATCH = NMATCH + 1
            END IF
         END DO

*  Restore the default line breaking in the screen output created by MSG_OUT.
         CALL MSG_TUNE( 'SZOUT', 79, STATUS )

*  Create the log file.
         CALL GRP_LIST( 'LOGFILE', 0, 0, ' ', IGRP2, STATUS )

*  Write the group size to an output parameter.
         CALL PAR_PUT0I( 'SIZE', SIZE1, STATUS )

*  Write the number of matches to an output parameter.
         CALL PAR_PUT0I( 'NMATCH', NMATCH, STATUS )

      END IF

*  Tidy up.
*  ========
  999 CONTINUE

*  Free resourcee.
      IF( IGRP0 .NE. GRP__NOID ) CALL GRP_DELET( IGRP0, STATUS )
      IF( IGRP1 .NE. GRP__NOID ) CALL GRP_DELET( IGRP1, STATUS )
      CALL GRP_DELET( IGRP2, STATUS )

*  Add a context report if anything went wrong.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDFECHO_ERR', 'NDFECHO: Unable to list NDF '//
     :                 'paths.', STATUS )
      END IF

      END
