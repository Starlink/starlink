      SUBROUTINE PROVREM( STATUS )
*+
*  Name:
*     PROVREM

*  Purpose:
*     Removes selected provenance information from an NDF.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL PROVREM( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application removes selected ancestors, either by hiding them,
*     or deleting them from the provenance information stored in a given NDF.
*     The `generation gap' caused by removing an ancestor is bridged by
*     assigning all the direct parents of the removed ancestor to each of
*     the direct children of the ancestor.
*
*     The ancestors to be removed can be specified either by giving
*     their indices (Parameter ANCESTOR), or by comparing each ancestor
*     with a supplied pattern matching template (Parameter PATTERN).
*
*     If an ancestor is hidden rather than deleted (see Parameter HIDE),
*     the ancestor is retained within the NDF, but a flag is set telling
*     later applications to ignore the ancestor (exactly how the flag is
*     used will depend on the particular application).

*  Usage:
*     provrem ndf pattern item

*  ADAM Parameters:
*     ANCESTOR = LITERAL (Read)
*        Specifies the indices of one or more ancestors that are to be
*        removed.  If a null (!) value is supplied, the ancestors to be
*        removed are instead determined using the PATTERN parameter.
*        Each supplied index must be positive and refers to one of the
*        NDFs listed in the ANCESTORS table in the PROVENANCE extension
*        of the NDF (including any hidden ancestors).  Note, if ancestor
*        indices are determined using the PROVSHOW command, then PROVSHOW
*        should be run with the HIDE parameter set to FALSE - otherwise
*        incorrect ancestor indices may be determined, resulting in the
*        wrong ancestors being removed by PROVREM.
*
*        The maximum number of ancestors that can be removed is limited
*        to 100 unless "ALL", "*" or "!" is specified.  The supplied
*        parameter value can take any of the following forms.
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
*        commas.  [!]
*     HIDE = _LOGICAL (Read)
*        If TRUE, then the ancestors are not deleted, but instead have a
*        flag set indicating that they have been hidden. All information
*        about hidden ancestors is retained unchanged, and can be viewed
*        using PROVSHOW if the HIDE parameter is set FALSE when running
*        PROVSHOW. [FALSE]
*     ITEM = LITERAL (Read)
*        Specifies the item of provenance information that is checked
*        against the pattern matching template specified for parameter
*        PATTERN.  It can be "PATH", "CREATOR" or "DATE".  ["PATH"]
*     NDF = NDF (Update)
*        The NDF data structure.
*     PATTERN = LITERAL (Read)
*        Specifies a pattern matching template using the syntax
*        described below in "Pattern Matching Syntax".  Each ancestor
*        listed in the PROVENANCE extension of the NDF is compared with
*        this template, and each ancestor that matches is removed.  The
*        item of provenance information to be compared to the pattern is
*        specified by Parameter ITEM.
*     REMOVE = _LOGICAL (Read)
*        If TRUE, then the ancestors specified by Parameter PATTERN or
*        ANCESTORS are removed.  Otherwise, these ancestors are retained
*        and all other ancestors are removed.  [TRUE]

*  Examples:
*     provrem ff ancestor=1
*        This removes the first ancestor from the NDF called ff.
*     provrem ff ancestor=all
*        This erases all provenance information.
*     provrem ff pattern='_xb$|_yb$' hide=yes
*        This hides, but does not permanently delete, all ancestors that
*        have paths that end with "_xb" or "_yb".  Note, provenance paths
*        do not include a trailing ".sdf" string.
*     provrem ff pattern='_ave'
*        This removes all ancestors that have paths that contain the
*        string "_ave" anywhere.
*     provrem ff pattern='_ave' remove=no
*        This removes all ancestors that have paths that do not contain
*        the string "_ave" anywhere.
*     provrem ff pattern='_d[^/]*$'
*        This removes all ancestors that have file base-names that begin
*        with "_d" . The pattern matches "_d" followed by any number of
*        characters that are not "/", followed by the end of the string.
*     provrem ff pattern='^m51|^m31'
*        This removes all ancestors that have paths that begin with
*        "m51" or "m31".

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
*
*     The following constraints are allowed.
*
*     "^" -- Matches the start of the test string.
*     "$" -- Matches the end of the test string.
*
*     Multiple templates can be concatenated, using the "|" character to
*     separate them.  The test string is compared against each one in
*     turn until a match is found.

*  Related Applications:
*     KAPPA: PROVADD, PROVMOD, PROVSHOW.

*  Copyright:
*     Copyright (C) 2008-2009 Science & Technology Facilities Council.
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
*     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     29-FEB-2008 (DSB):
*        Original version.
*     25-JUN-2009 (DSB):
*        Updated to use new provenance API.
*     7-JUL-2009 (DSB):
*        Added Parameter HIDE.
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
      INCLUDE 'NDG_PAR'          ! NDG constants
      INCLUDE 'DAT_PAR'          ! HDS constants

*  Status:
      INTEGER STATUS

*  External References:
      LOGICAL CHR_SIMLR          ! Case blind string comparison

*  Local Constants:
      INTEGER MXANC              ! Max. no. of selected ancestors
      PARAMETER( MXANC = 100 )

*  Local Variables:
      CHARACTER AMORE*(DAT__SZLOC)! Locator for MORE in ancestor
      CHARACTER ANC*20           ! ANCESTOR parameter value
      CHARACTER ITEM*10          ! Item to check
      CHARACTER PAT*400          ! Pattern matching template
      CHARACTER RESULT*400       ! Result of substitutions
      CHARACTER TEST*400         ! String to be tested
      INTEGER I                  ! Ancestor index
      INTEGER IANC( MXANC )      ! Indices of selected ancestors
      INTEGER INDF               ! NDF identifier
      INTEGER IPROV              ! Identifier for provenance structure
      INTEGER IPW1               ! Pointer to work space
      INTEGER IR                 ! Index of next array element to read
      INTEGER IW                 ! Index of next array element to write
      INTEGER J                  ! Ancestor index
      INTEGER KM                 ! KeyMap holding provenance info
      INTEGER L                  ! Used length of returned string
      INTEGER NANC               ! Total number of ancestors
      INTEGER NREM               ! Number of ancestors removed
      LOGICAL HIDE               ! Hide the ancestors?
      LOGICAL REMOVE             ! Remove matching ancestors?
      LOGICAL THERE              ! Does component exist?
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

*  Initialise the number of ancestors to be removed.
      NREM = 0

*  See if specified ancestors are to be removed or retained.
      CALL PAR_GET0L( 'REMOVE', REMOVE, STATUS )

*  See if removed ancestors should be deleted or hidden.
      CALL PAR_GET0L( 'HIDE', HIDE, STATUS )

*  Abort if an error has occurred or there is no provenance information.
      IF( STATUS .NE. SAI__OK .OR. NANC .EQ. 0 ) GO TO 999

*  See if a NULL value has been supplied for parameter ANCESTOR.  If so,
*  annul the error, and get the PATTERN and ITEM parameters.
      CALL PAR_GET0C( 'ANCESTOR', ANC, STATUS )
      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )

         CALL PAR_GET0C( 'PATTERN', PAT, STATUS )
         CALL PAR_CHOIC( 'ITEM', 'PATH', 'PATH,DATE,CREATOR', .TRUE.,
     :                    ITEM, STATUS )

*  Allocate work space to hold the largest possible number of ancestor
*  indices to be removed.
         CALL PSX_CALLOC( NANC, '_INTEGER', IPW1, STATUS )

*  Loop round all ancestors.
         DO I = 1, NANC

*  Get an AST KeyMap holding the existing provenance information for
*  the specified ancestor.
            CALL NDG_GETPROV( IPROV, I, KM, AMORE, STATUS )

*  Get the required item of information from the ancestor, use a blank
*  string if it is not there.
            THERE = AST_MAPGET0C( KM, ITEM, TEST, L, STATUS )
            IF( .NOT. THERE ) TEST = ' '

*  Annul the KeyMap and locator holding information about the I'th ancestor.
            CALL AST_ANNUL( KM, STATUS )
            CALL NDG_ANTMP( AMORE, STATUS )

*  See if the pattern matches the item. If required, hide the ancestor, or
*  add the ancestor index to the list to be deleted.
            IF( REMOVE .EQV.
     :          AST_CHRSUB( TEST, PAT, RESULT, STATUS ) ) THEN

               NREM = NREM + 1

               IF( HIDE ) THEN
                  CALL NDG_HIDEPROV( IPROV, I, STATUS )
               ELSE
                  CALL KPG1_STORI( NANC, NREM, I,
     :                             %VAL( CNF_PVAL( IPW1 ) ), STATUS )
               END IF

            END IF
         END DO

*  Remove the required ancestors.
         IF( .NOT. HIDE ) CALL NDG_REMOVEPROV( IPROV, NREM,
     :                                         %VAL( CNF_PVAL( IPW1 ) ),
     :                                         STATUS )

*  write the modified provenance back to the NDF.
         CALL NDG_WRITEPROV( IPROV, INDF, .FALSE., STATUS )

*  Free the array holding the ancestor indices.
         CALL PSX_FREE( IPW1, STATUS )

*  Otherwise, we remove the ancestors specified by parameter ANCESTOR.
      ELSE

*  Get the indices of the ancestors to be modified.  Since KPG1_GILST
*  limits the number of values that can be supplied, first check the
*  parameter value directly to see if it set to "ALL".  If so, we just
*  hide all ancestors, or erase the provenance extension.
         IF( CHR_SIMLR( ANC, 'ALL' ) .OR. ANC .EQ. '*' ) THEN
            IF( REMOVE ) THEN
               IF( HIDE ) THEN
                  DO I = 1, NANC
                     CALL NDG_HIDEPROV( IPROV, I, STATUS )
                  END DO
                  CALL NDG_WRITEPROV( IPROV, INDF, .FALSE., STATUS )
               ELSE
                  CALL NDF_XSTAT( INDF, 'PROVENANCE', THERE, STATUS )
                  IF( THERE ) CALL NDF_XDEL( INDF, 'PROVENANCE',
     :                                       STATUS )
               END IF
               NREM = NANC
            END IF

*  Otherwise, get the list of ancestors to remove.
         ELSE
            CALL PSX_CALLOC( NANC + 1, '_INTEGER', IPW1, STATUS )
            CALL KPG1_GILST( 0, NANC, MXANC, 'ANCESTOR',
     :                       %VAL( CNF_PVAL( IPW1 ) ), IANC, NREM,
     :                       STATUS )
            CALL PSX_FREE( IPW1, STATUS )

*  Sort the ancestor indices into increasing order.
            CALL KPG1_QSRTI( NREM, 1, NREM, IANC, STATUS )

*  If we are removing all except the specified ancestors, change the
*  contents of the list to be the unspecified indices.
            IF( .NOT. REMOVE ) THEN
               IR = NREM
               DO IW = NANC, 1, -1
                  IF( IW .EQ. IANC( IR ) ) THEN
                     IANC( IW ) = 1
                     IR = IR - 1
                  ELSE
                     IANC( IW ) = 0
                  END IF
               END DO

               IW = 1
               DO IR = 1,NANC
                  IF( IANC( IR ) .EQ. 0 ) THEN
                     IANC( IW ) = IR
                     IW = IW + 1
                  END IF
               END DO

               NREM = NANC - NREM

            END IF

*  Hide or delete the required ancestors.
            IF( NREM .GT. 0 ) THEN
               IF( HIDE ) THEN
                  DO I = 1, NREM
                     CALL NDG_HIDEPROV( IPROV, IANC( I ), STATUS )
                  END DO
               ELSE
                  CALL NDG_REMOVEPROV( IPROV, NREM, IANC, STATUS )
               END IF

*  Write the modified provenance back to the NDF.
               CALL NDG_WRITEPROV( IPROV, INDF, .FALSE., STATUS )
            END IF

         END IF
      END IF

*  Arrive here if an error occurs.
 999  CONTINUE

*  Free the Provenance information.
      CALL NDG_FREEPROV( IPROV, STATUS )

*  Report the number of ancestors removed.
      IF( HIDE ) THEN
         CALL MSG_SETC( 'W', 'hidden' )
      ELSE
         CALL MSG_SETC( 'W', 'deleted' )
      END IF

      IF( NREM .EQ. 0 ) THEN
         CALL MSG_OUT( ' ', '   No ancestors ^W', STATUS )

      ELSE IF( NREM .EQ. 1 ) THEN
         CALL MSG_OUT( ' ', '   1 ancestor ^W', STATUS )

      ELSE
         CALL MSG_SETI( 'N', NREM )
         CALL MSG_OUT( ' ', '   ^N ancestors ^W', STATUS )
      END IF

      CALL MSG_BLANK( STATUS )

*  Annul the NDF identifier.
      CALL NDF_ANNUL( INDF, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'PROVREM_ERR', 'PROVREM: Failed to remove '//
     :                 'provenance information in an NDF.', STATUS )
      END IF

      END
