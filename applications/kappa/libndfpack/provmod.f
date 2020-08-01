      SUBROUTINE PROVMOD( STATUS )
*+
*  Name:
*     PROVMOD

*  Purpose:
*     Modifies provenance information for an NDF.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL PROVMOD( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application modifies the provenance information stored in
*     the PROVENANCE extension of an NDF.

*  Usage:
*     provmod ndf ancestor path

*  ADAM Parameters:
*     ANCESTOR = LITERAL (Read)
*        Specifies the indices of one or more ancestors that are to be
*        modified.  An index of zero refers to the supplied NDF itself.
*        A positive index refers to one of the NDFs listed in the
*        ANCESTORS table in the PROVENANCE extension of the NDF.  The
*        maximum number of ancestors is limited to 100 unless "ALL" or
*        "*" is specified.  The supplied parameter value can take any of
*        the following forms.
*
*        - "ALL" or "*" --  All ancestors.
*
*        - "xx,yy,zz" -- A list of ancestor indices.
*
*        - "xx:yy" --  Ancestor indices between xx and yy inclusively.
*        When xx is omitted, the range begins from 0; when yy is
*        omitted, the range ends with the maximum value it can take,
*        that is the number of ancestors described in the PROVENANCE
*        extension.
*
*        - Any reasonable combination of above values separated by
*        commas.  ["ALL"]
*     CREATOR = LITERAL (Read)
*        If the supplied string includes no equals signs, then it is a
*        new value for the "CREATOR" string read from each of the
*        ancestors being modified. If the supplied string includes one
*        or more equals signs, then it specifies one or more
*        substitutions to be performed on the "CREATOR" string read from
*        each of the ancestors being modified. See "Substitution Syntax"
*        below.  If null (!) is supplied, the CREATOR item is left
*        unchanged.  [!]
*     DATE = LITERAL (Read)
*        If the supplied string includes no equals signs, then it is a
*        new value for the "DATE" string read from each of the ancestors
*        being modified. If the supplied string includes one or more
*        equals signs, then it specifies one or more substitutions to be
*        performed on the "DATE" string read from each of the ancestors
*        being modified.  See "Substitution Syntax" below.  If null (!)
*        is supplied, the DATE item is left unchanged.  [!]
*     MORETEXT = GROUP (Read)
*        This parameter is accessed only if a single ancestor is being
*        modified (see Parameter ANCESTORS). It gives information to
*        store in the MORE component of the ancestor (any existing
*        information is first removed). If a null (!) value is supplied,
*        then existing MORE component is left unchanged.
*
*        The supplied value should be either a comma-separated list of
*        strings or the name of a text file preceded by an up-arrow
*        character "^", containing one or more comma-separated list of
*        strings. Each string is either a "keyword=value" setting, or
*        the name of a text file preceded by an up-arrow character "^".
*        Such text files should contain further comma-separated lists
*        which will be read and interpreted in the same manner (any
*        blank lines or lines beginning with "#" are ignored). Within a
*        text file, newlines can be used as delimiters as well as
*        commas.
*
*        Each individual setting should be of the form:
*
*           <keyword>=<value>
*
*        where <keyword> is either a simple name, or a dot-delimited
*        hierarchy of names (e.g. "camera.settings.exp=1.0"). The
*        <value> string should not contain any commas.  [!]
*     NDF = NDF (Update)
*        The NDF data structure.
*     PATH = LITERAL (Read)
*        If the supplied string includes no equals signs, then it is a
*        new value for the "PATH" string read from each of the ancestors
*        being modified. If the supplied string includes one or more
*        equals signs, then it specifies one or more substitutions to be
*        performed on the "PATH" string read from each of the ancestors
*        being modified.  See "Substitution Syntax" below.  If null (!)
*        is supplied, the PATH item is left unchanged.  [!]

*  Examples:
*     provmod ff path=/home/dsb/real-file.sdf
*        This modifies any ancestor within the NDF called ff by setting
*        its PATH to "/home/dsb/real-file.sdf".
*     provmod ff ancestor=3 moretext="obsidss=acsis_00026_20080322T055855_1"
*        This modifies ancestor Number 3 by storing a value of
*        "acsis_00026_20080322T055855_1" for key "obsidss" within the
*        additonal information for the ancestor. Any existing additional
*        information is removed.
*     provmod ff path='(_x)$=_y'
*        This modifies any ancestor within the NDF called ff that has a
*        path ending in "_x" by replacing the final "_x" with "_y".
*     provmod ff path='(_x)$=_y'
*        This modifies any ancestor within the NDF called ff that has a
*        path ending in "_x" by replacing the final "_x" with "_y".
*     provmod ff path='(.*)_(.*)=$2=$1'
*        This modifies any ancestor within the NDF called ff that has a
*        path consisting of two parts separated by an underscore by
*        swapping the parts.  If there is more than one underscore in
*        the ancestor path, then the final underscore is used (because
*        the initial quantifier ".*" is greedy).
*     provmod ff path='(.*?)_(.*)=$2=$1'
*        This modifies any ancestor within the NDF called ff that has a
*        path consisting of two parts separated by an underscore by
*        swapping the parts.  If there is more than one underscore in
*        the ancestor path, then the first underscore is used (because
*        the initial quantifier ".*?" is not greedy).

*  Substitution Syntax:
*     The syntax for the CREATOR, DATE, and PATH parameter values is a
*     minimal form of regular expression.  The following atoms are
*     allowed.
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
*     significance can be matched by preceeding them with a backslash
*     (\) in which case their special significance is ignored (note,
*     this does not apply to the characters in the set dDsSwW).
*
*     Note, minus signs ("-") within brackets have no special
*     significance, so ranges of characters must be specified
*     explicitly.
*
*     The following quantifiers are allowed.
*
*     "*" -- Matches zero or more of the preceeding atom, choosing the
*            largest possible number that gives a match.
*     "*?"-- Matches zero or more of the preceeding atom, choosing the
*           smallest possible number that gives a match.
*     "+" -- Matches one or more of the preceeding atom, choosing the
*            largest possible number that gives a match.
*     "+?"-- Matches one or more of the preceeding atom, choosing the
*            smallest possible number that gives a match.
*     "?" -- Matches zero or one of the preceeding atom.
*     "{n}" -- Matches exactly "n" occurrences of the preceeding atom.

*     The following constraints are allowed.
*
*     "^" -- Matches the start of the test string.
*     "$" -- Matches the end of the test string.
*
*     Multiple templates can be concatenated, using the "|" character to
*     separate them.  The test string is compared against each one in
*     turn until a match is found.
*
*     A template should use parentheses to enclose the sub-strings that
*     are to be replaced, and the set of corresponding replacement
*     values should be appended to the end of the string, separated by
*     "=" characters.  The section of the test string that matches the
*     first parenthesised section in the template string will be
*     replaced by the first replacement string.  The section of the test
*     string that matches the second parenthesised section in the
*     template string will be replaced by the second replacement string,
*     and so on.
*
*     The replacement strings can include the tokens "$1", "$2", etc.
*     The section of the test string that matched the corresponding
*     parenthesised section in the template is used in place of the
*     token.
*
*     See the "Examples" section above for how to use these facilities.

*  Related Applications:
*     KAPPA: PROVADD, PROVREM, PROVSHOW.

*  Copyright:
*     Copyright (C) 2008 Science & Technology Facilities Council.
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
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     26-FEB-2008 (DSB):
*        Original version.
*     25-APR-2008 (DSB):
*        Allow new values to be speicfied literally as well as by
*        substitution.
*     29-APR-2008 (DSB):
*        Added Parameter MORETEXT.
*     25-JUN-2009 (DSB):
*        Updated to use new provenance API.
*     7-DEC-2010 (DSB):
*        Use NDG_ANTMP rather than DAT_ANNUL to annul temporary HDS
*        objects created by NDG. Using DAT_ANNUL does not erase such
*        temporary objects form the HDS temp file.
*     {enter_further_changes_here}

*-
*  Type Definitions:
      IMPLICIT NONE              ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CNF_PAR'          ! CNF constants and functions
      INCLUDE 'PAR_ERR'          ! PAR error constants
      INCLUDE 'AST_PAR'          ! AST constants and functions
      INCLUDE 'DAT_PAR'          ! HDS constants
      INCLUDE 'NDG_PAR'          ! NDG constants
      INCLUDE 'GRP_PAR'          ! GRP constants

*  Status:
      INTEGER STATUS

*  External References:
      LOGICAL CHR_SIMLR          ! Case blind string comparison
      INTEGER CHR_LEN            ! Used length of a string

*  Local Constants:
      INTEGER MXANC              ! Max. no. of selected ancestors
      PARAMETER( MXANC = 100 )

*  Local Variables:
      CHARACTER ANC*20           ! ANCESTOR parameter value
      CHARACTER CRESUB*400       ! Substitution string
      CHARACTER DATSUB*400       ! Substitution string
      CHARACTER PTHSUB*400       ! Substitution string
      CHARACTER RESULT*400       ! result of substitutions
      CHARACTER TEST*400         ! String to be tested
      INTEGER I                  ! Ancestor index
      INTEGER IANC( MXANC )      ! Indices of selected ancestors
      INTEGER IGRP               ! Identifier for group holding input text
      INTEGER INDF               ! NDF identifier
      INTEGER IPROV              ! Identifier for provenance structure
      INTEGER IPW1               ! Pointer to work space
      INTEGER J                  ! Ancestor count
      INTEGER KM                 ! KeyMap holding provenance info
      INTEGER L                  ! String len
      INTEGER MANC               ! Number of selected ancestors
      INTEGER MORE               ! KeyMap holding more information
      INTEGER NANC               ! Total number of ancestors
      INTEGER SIZE               ! Number of elements in the group
      LOGICAL DOALL              ! Modify all ancestors?
      LOGICAL CHANGED            ! Has the component changed?
*.


*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Obtain an identifier for the NDF.
      CALL LPG_ASSOC( 'NDF', 'UPDATE', INDF, STATUS )

*  Read provenance information from the NDF.
      CALL NDG_READPROV( INDF, ' ', IPROV, STATUS )

*  Get the number of ancestors described in the NDFs PROVENANCE
*  extension.
      CALL NDG_COUNTPROV( IPROV, NANC, STATUS )

*  Get the indices of the ancestors to be modified.  Since KPG1_GILST
*  limits the number of values that can be supplied, first check the
*  parameter value directly to see if it set to "ALL".  If so, we bypass
*  KPG1_GILST, setting a flag instead to show that all ancestors should
*  be modified.
      CALL PAR_GET0C( 'ANCESTOR', ANC, STATUS )
      IF( CHR_SIMLR( ANC, 'ALL' ) .OR. ANC .EQ. '*' ) THEN
         DOALL = .TRUE.
         MANC = NANC + 1
      ELSE
         DOALL = .FALSE.
         CALL PSX_CALLOC( NANC + 1, '_INTEGER', IPW1, STATUS )
         CALL KPG1_GILST( 0, NANC, MXANC, 'ANCESTOR',
     :                    %VAL( CNF_PVAL( IPW1 ) ), IANC, MANC,
     :                    STATUS )
         CALL PSX_FREE( IPW1, STATUS )
      END IF

*  If an error occurred, exit.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Get the substitutions.
      CALL PAR_GET0C( 'CREATOR', CRESUB, STATUS )
      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         CRESUB = ' '
      END IF

      CALL PAR_GET0C( 'DATE', DATSUB, STATUS )
      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         DATSUB = ' '
      END IF

      CALL PAR_GET0C( 'PATH', PTHSUB, STATUS )
      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         PTHSUB = ' '
      END IF

*  If only a single ancestor is being modified, get additional
*  information as a set of text strings using the MORETEXT parameter.
      IF( MANC .EQ. 1 .AND. STATUS .EQ. SAI__OK ) THEN

* Get a GRP group of strings.
         IGRP = GRP__NOID
         CALL KPG1_GTGRP( 'MORETEXT', IGRP, SIZE, STATUS )

* If none supplied, annull the error.
         IF( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            MORE = AST__NULL

*  Otherwise, convert the GRP group to an AST KeyMap and then delete the
*  group.
         ELSE
            CALL KPG1_KYMAP( IGRP, MORE, STATUS )
            CALL GRP_DELET( IGRP, STATUS )
         END IF

      ELSE
         MORE = AST__NULL
      END IF

*  Loop round all the ancestors that are to be modified.
      DO J = 1, MANC

*  Get the index of the next ancestor to be modified.
         IF( DOALL ) THEN
            I = J - 1
         ELSE
            I = IANC( J )
         END IF

*  Get an AST KeyMap holding the existing provenance information for
*  the specified ancestor.
         CALL NDG_GETPROV( IPROV, I, KM, STATUS )

*  If new MORE info has been specified, use its KeyMap in place
*  of the old one.
         IF( MORE .NE. AST__NULL ) THEN
            CALL AST_MAPPUT0A( KM, 'MORE', MORE, ' ', STATUS )
         END IF

*  Is the CREATOR string to be modified?
         IF( CRESUB .NE. ' ' ) THEN

*  Assume for now the CREATOR value will not change.
            CHANGED = .FALSE.

*  If the value given for the CREATOR parameter does not include an "="
*  sign then it is an explicit new value to be stored in the provenance.
            IF( INDEX( CRESUB, '=' ) .EQ. 0 ) THEN
               TEST = CRESUB
               CHANGED = .TRUE.

*  Otherwise it specifies modifications to be applied to the existing
*  CREATOR string (if any). Get any such string from the KeyMap, skipping
*  over this bit if the KeyMap does not contain a CREATOR value.
            ELSE IF( AST_MAPGET0C( KM, 'CREATOR', TEST, L,
     :                             STATUS ) ) THEN

*  If the specifier matches the CREATOR string, perform the substitutions
*  and use the resulting string in place of the current CREATOR string.
               IF( AST_CHRSUB( TEST( : L ), CRESUB, RESULT,
     :                         STATUS ) ) THEN
                  TEST = RESULT
                  CHANGED = .TRUE.
               END IF
            END IF

*  If the CREATOR string has changed, put the new value in the KeyMap.
            IF( CHANGED ) THEN
               L = MAX( 1, CHR_LEN( TEST ) )
               CALL AST_MAPPUT0C( KM, 'CREATOR', TEST( : L ), ' ',
     :                            STATUS )
            END IF
         END IF

*  Do the same for the DATE string.
         IF( DATSUB .NE. ' ' ) THEN
            CHANGED = .FALSE.

            IF( INDEX( DATSUB, '=' ) .EQ. 0 ) THEN
               TEST = DATSUB
               CHANGED = .TRUE.

            ELSE IF( AST_MAPGET0C( KM, 'DATE', TEST, L,
     :                             STATUS ) ) THEN

               IF( AST_CHRSUB( TEST( : L ), DATSUB, RESULT,
     :                         STATUS ) ) THEN
                  TEST = RESULT
                  CHANGED = .TRUE.
               END IF
            END IF

            IF( CHANGED ) THEN
               L = MAX( 1, CHR_LEN( TEST ) )
               CALL AST_MAPPUT0C( KM, 'DATE', TEST( : L ), ' ',
     :                            STATUS )
            END IF
         END IF

*  Do the same for the PATH string.
         IF( PTHSUB .NE. ' ' ) THEN
            CHANGED = .FALSE.

            IF( INDEX( PTHSUB, '=' ) .EQ. 0 ) THEN
               TEST = PTHSUB
               CHANGED = .TRUE.

            ELSE IF( AST_MAPGET0C( KM, 'PATH', TEST, L,
     :                             STATUS ) ) THEN

               IF( AST_CHRSUB( TEST( : L ), PTHSUB, RESULT,
     :                         STATUS ) ) THEN
                  TEST = RESULT
                  CHANGED = .TRUE.
               END IF
            END IF

            IF( CHANGED ) THEN
               L = MAX( 1, CHR_LEN( TEST ) )
               CALL AST_MAPPUT0C( KM, 'PATH', TEST( : L ), ' ',
     :                            STATUS )
            END IF
         END IF

*  Store the modified provenance information back in the NDF.
         CALL NDG_MODIFYPROV( IPROV, I, KM, STATUS )

*  Annul the KeyMap holding the ancestor information.
         CALL AST_ANNUL( KM, STATUS )

      END DO

*  Store the modified provenance information back in the NDF.
      CALL NDG_WRITEPROV( IPROV, INDF, .FALSE., STATUS )

*  Arrive here if an error occurs.
 999  CONTINUE

*  Free the Provenance information.
      CALL NDG_FREEPROV( IPROV, STATUS )

*  Annul the NDF identifier.
      CALL NDF_ANNUL( INDF, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'PROVMOD_ERR', 'PROVMOD: Failed to modify '//
     :                 'provenance information in an NDF.', STATUS )
      END IF

      END
