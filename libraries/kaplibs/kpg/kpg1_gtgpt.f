      SUBROUTINE KPG1_GTGPT( PARAM, DELIM, LAST, IGRP, SIZE, STATUS )
*+
*  Name:
*     KPG1_GTGPT

*  Purpose:
*     Obtains a group of strings from the environment including some
*     that are transient.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_GTGPT( PARAM, DELIM, LAST, IGRP, SIZE, STATUS )

*  Description:
*     This routine obtains group of strings using the specified
*     parameter, and returns them in a GRP group (see SUN/150).
*
*     The user specifies the strings by supplying a GRP group expression
*     for the parameter value.  If the final character in the supplied
*     string is the group "flag character" (a minus sign by default),
*     then the user is re-prompted for another group expression, and the
*     strings specified by the second group expression are appended to
*     the returned group.  This continues until a group expression is
*     supplied that does not end with the continuation character, or a
*     null value is supplied (which is annulled).
*
*     The group comprises an optional persistent part that will be
*     written as the parameter's current value, and an optional
*     temporary component that only exists for the duration of the
*     current application.  The division is determined by the first
*     appearance within the string supplied of a delimiter string or
*     character (the DELIM argument).  For instance, assume the
*     delimiter is '+'.  If the supplied value is
*     'smooth,box=5+filter=gauss', the persistent value would be
*     'smooth,box=5' and the temporary value 'filter=gauss'.  Supplying
*     merely the temporary value, such as '+filter=gauss' would return
*     the existing current value of the parameter as the persistent
*     component, and the temporary component is as before.
*
*     Normally, the "current value" for the parameter on exit would be
*     the final group expression.  If more than one group expression was
*     supplied, then this will not represent the entire group.  For this
*     reason, this routine concatenates all the group expressions
*     supplied, and stores the total group expression as the parameter
*     value before exiting.  It also stores the concatenated persistent
*     components and writes this as the current value.  Since a new
*     value is stored for the parameter, the parameter should not be
*     given an access mode of READ in the interface file.
*
*     If a null value is supplied the first time the parameter value is
*     obtained, then the PAR__NULL status is returned, and SIZE is
*     returned as zero (a valid GRP identifier is still returned
*     however).  If a null value is supplied on a subsequent access to
*     the parameter, then the PAR__NULL status is annulled, and the
*     group is returned containing any values obtained earlier.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The name of the parameter to use.
*     DELIM = CHARACTER * ( * ) (Given)
*        The delimiter string used to indicator where the temporary
*        part of a value begins.  Any text before the delimiter behaves
*        as if it were obtained using PAR_GET0C.  After, and not
*        including, the delimiter the value is deemed to be temporary
*        and is never incorporated into the parameter's current value.
*        The delimiter should not be an alphanumeric character or
*        underscore.
*     LAST = LOGICAL (Given)
*        If this is the last or only invocation of this routine for a
*        given parameter in a task, set this TRUE.  For multiple
*        invocations except the last, set this FALSE.  Setting to FALSE
*        causes the full concatenated expression supplied by the user to
*        be stored in the parameter's current value, rather than just
*        the persistent-attribute list a TRUE value generates needed
*        when the task ends.
*     IGRP = INTEGER (Given and Returned)
*        The group to use.  If this is GRP__NOID then a new group is
*        created with default control characters and its identifier is
*        returned.  If the group already exists, its contents are
*        discarded before adding new strings.
*     SIZE = INTEGER (Returned)
*        The total size of the returned group.  Returned equal to zero
*        if an error occurs.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine subverts the parameter system in that it uses an
*     additional component in the parameter file to store the current
*     value.  This enables the current value to be accessed regardless
*     of the value for the parameter supplied on the command line that
*     would otherwise replace the previous current value with the full
*     new value including the delimiter and temporary value.
*     -  The additional component's name is the parameter name (up to 13
*     characters) followed by CV (for current value).  In practice
*     truncation should not be an issue.
*     -  This routine should not be used with multiple parameters in the
*     same task whose new current-value component in the parameter file
*     would be the same.  Again in practice this is extremely unlikely.
*     -  It has not been proven to work if invoked more than twice for
*     the same parameter, as there are no known occurrences of this in
*     Starlink applications.

*  Copyright:
*     Copyright (C) 2010 Science & Technology Facilities Council.
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
*     MJC: Malcolm J. Currie  (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2010 August 26 (MJC):
*        Original version from merging PAR_TAPVC and DSB's KPG1_GTGRP.
*     2010 October 6 (MJC):
*        Use SUBPAR_PUTNAME instead of HDS to write concatenated
*        persistent and temporary attributes.  Add two further messages
*        for verbose mode.  Erase the current-value object if it is
*        undefined, say because a null value was supplied.
*     2010 October 11 (MJC):
*        Some restructuring to simplify, and allow for continuation
*        via a DELIM suffix to PARAM.
*     2010 October 12 (MJC):
*        Add LAST argument instead of testing the parameter name for a
*        delimiter suffix.
*     2010 October 13 (MJC):
*        Allow for the case where the first value for the parameter
*        comprises only temporary attributes, by setting the parameter
*        value to null.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Data-system conastants
      INCLUDE 'GRP_PAR'          ! GRP constants
      INCLUDE 'PAR_ERR'          ! PAR error constants

*  Arguments Given:
      CHARACTER PARAM*(*)
      CHARACTER*(*) DELIM
      LOGICAL LAST

*  Arguments Given and Returned:
      INTEGER IGRP

*  Arguments Returned:
      INTEGER SIZE

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string

*  Local Constants:
      INTEGER CHARSZ             ! Length of character components in
      PARAMETER ( CHARSZ = 132 ) ! parameter file

      INTEGER MXGEX              ! Max. length of total group expression
      PARAMETER( MXGEX = 1024 )

      CHARACTER*2 SUFFIX         ! Suffix to add to parameter name
      PARAMETER ( SUFFIX = 'CV' )! to store current value

      CHARACTER*1 DDELIM
      PARAMETER ( DDELIM = '+' ) ! Default indicator of temporary value

*  Local Variables:
      INTEGER ADDED              ! No. of strings added to group
      LOGICAL CFLAG              ! More strings remain to be given?
      INTEGER CLEN               ! Length of current value
      INTEGER CPLEN              ! Length of current value parameter
      LOGICAL CREATE             ! Create new CV object?
      CHARACTER*(CHARSZ) CURVAL  ! Current value on entry to the task
      CHARACTER*1 DC             ! Delimiter character group expressions
      INTEGER DELLEN             ! Number of characters in delimeter
      INTEGER DELIND             ! Index of delimeter in supplied value
      CHARACTER*1 FC             ! Flag character for group expressions
      LOGICAL FIRST              ! Processing first line of text
      INTEGER FLEN               ! Length of total string to append
      CHARACTER*(DAT__SZLOC) FLOC ! Locator the parameter file
      LOGICAL IFC                ! TRUE if .IFC found (not used)
      CHARACTER*1024 IFNAME       ! Name of interface module (not used)
      INTEGER IPAR               ! SUBPAR pointer to the parameter
      CHARACTER*(MXGEX) GRPEXP   ! The total group expression
      INTEGER NC                 ! No. of characters in GRPEXP so far
      INTEGER NCP                ! No. of characters in PGRPEX so far
      INTEGER NEXP               ! No. of group expressions obtained
      INTEGER PARLEN             ! Length of parameter name
      CHARACTER*1024 PFNAME       ! Name of parameter file
      INTEGER PLEN               ! Length of persistent value to append
      CHARACTER*(MXGEX) PGRPEX   ! Persistent group expression
      CHARACTER*(DAT__SZLOC) PLOC ! Locator top-level of parameter file
      CHARACTER*(DAT__SZNAM) PNAMCV ! Parameter name current-value obj
      CHARACTER*(GRP__SZGEX) PVALUE ! Single persistent group expression
      INTEGER SFLEN              ! Length of suffix
      INTEGER SLEN               ! Length of supplied group expression
      CHARACTER*(GRP__SZGEX) TEXT ! A single group expression
      LOGICAL TEMPV              ! Temporary component of value present?
      LOGICAL THERE              ! Current value object exists?
      INTEGER TLEN               ! Length of temporary value to append
      CHARACTER*(DAT__SZNAM) TSKNAM ! Name of task
      INTEGER TSTAT              ! Temporary status
      CHARACTER*(GRP__SZGEX) TVALUE ! Single temporary group expression
      LOGICAL VERB               ! Run in verbose mode?

*.

*  Initialise the pointer to the returned group size.
      SIZE = 0

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain the current-value persistent copy of the parameter.
*  ==========================================================

*  Test for the multiple invocations flag, namely the delimiter at
*  the end of parameter name.
      DELLEN = LEN( DELIM )
      PARLEN = CHR_LEN( PARAM )

*  Form name of the object that will store a copy of the current value
*  of the parameter in the parameter file.  In virtually all cases
*  parameter names will comprise the original name with the suffix
*  appended.  However, allow for those that are now.  Now in theory
*  there could be two long parameter names (perhaps in a series) which
*  when truncated could form the same object name.  In practice and
*  especially for the intended use with style parameters this is not
*  going to be an issue.
      SFLEN = LEN( SUFFIX )
      PNAMCV = PARAM( : PARLEN )
      IF ( PARLEN .LT. DAT__SZNAM - SFLEN + 1 ) THEN
         CPLEN = PARLEN
         CALL CHR_APPND( 'CV', PNAMCV, CPLEN )
      ELSE
         PNAMCV = PARAM( 1: DAT__SZNAM - SFLEN ) // SUFFIX
      END IF

*  Find the parameter-file name associated with the executable image.
      CALL SUBPAR_TSKNM( TSKNAM, PFNAME, IFNAME, IFC, STATUS )

*  During activation of the parameter system, there are checks that
*  the parameter file exists otherwise one is created.  Therefore
*  no check is made here.
      CALL HDS_OPEN( PFNAME, 'UPDATE', FLOC, STATUS )
      CALL DAT_THERE( FLOC, PNAMCV, THERE, STATUS )

      CURVAL = ' '
      CLEN = 0
      CREATE = .NOT. THERE
      IF ( THERE ) THEN
         CALL CMP_GET0C( FLOC, PNAMCV, CURVAL, STATUS )
         CLEN = CHR_LEN( CURVAL )

*  Character value size seems to be hardwired to 132 in SUBPAR rather
*  than using a parameter in an include file.
      ELSE
         CALL DAT_NEW0C( FLOC, PNAMCV, CHARSZ, STATUS )
      END IF

*  Set up group.
*  =============

*  If a null identifier was supplied, create a new group with default
*  control characters.
      IF ( IGRP .EQ. GRP__NOID ) THEN
         CALL GRP_NEW( 'KAPPA group', IGRP, STATUS )

*  Otherwise, ensure the supplied group is empty.
      ELSE
         CALL GRP_SETSZ( IGRP, 0, STATUS )
      END IF

*  Get the demimiter and flag control characters for the group.
      CALL GRP_GETCC( IGRP, 'DELIMITER', DC, STATUS )
      CALL GRP_GETCC( IGRP, 'FLAG', FC, STATUS )

*  Initialise the total group expression given so far.
      GRPEXP = ' '
      NC = 0

*  Initialise the persistent group expression given so far.
      PGRPEX = ' '
      NCP = 0

*  Get a SUBPAR pointer for the parameter.
      CALL SUBPAR_FINDPAR( PARAM( : PARLEN ), IPAR, STATUS )

*  Initialise the number of group expression obtained.
      NEXP = 0

*  Read the group values
*  =====================

*  Default persistent and temporary values.
      TVALUE = ' '
      TLEN = 0
      PVALUE = CURVAL
      PLEN = CLEN

      FIRST = .TRUE.
      FLEN = 0

*  Allow for continuation lines.
      CFLAG = .TRUE.
      TEMPV = .FALSE.
      DO WHILE ( CFLAG .AND. STATUS .EQ. SAI__OK )

*  Get the group of strings from the environment.
         CALL GRP_GROUP( PARAM( : PARLEN ), GRP__NOID, IGRP, SIZE,
     :                   ADDED, CFLAG, STATUS )

*  If successful, increment the number of group expressions obtained,
*  and get the group expression supplied for the parameter as an
*  uninterpreted text string.
         IF ( STATUS .EQ. SAI__OK ) THEN
            NEXP = NEXP + 1
            CALL SUBPAR_GETNAME( IPAR, TEXT, STATUS )

*  If not successful, annul the error.  Use any existing
*  current value.
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERR_ANNUL( STATUS )

*  Otherwise, ignore blank strings.
            ELSE IF ( TEXT .NE. ' ' ) THEN

*  Get the used length of the string.
               SLEN = CHR_LEN( TEXT )

*  If the group expression was flagged, replace the last non-blank character
*  with a delimiter character if it is a flag character.
               IF ( CFLAG .AND. TEXT( SLEN : SLEN ) .EQ. FC ) THEN
                  TEXT( SLEN : SLEN ) = DC
               END IF

*  Divide the composite value into the persistent and temporary parts.
*  ===================================================================

*  Is this a continuation of the temporary value?
               IF ( TEMPV ) THEN

*  Removed unnecessary duplicate delimiter on subsequent text if this is
*  more of the temporary values.
                  CALL CHR_RMCHR( DELIM, TEXT )
                  TVALUE = TEXT

*  Is the a temporary-value flag in the latest string from the
*  parameter?
               ELSE
                  DELIND = INDEX( TEXT, DELIM )
                  IF ( DELIND .GT. 0 ) THEN
                     TVALUE = TEXT( DELIND + DELLEN: )

*  There is a temporary part to the value.
                     TEMPV = .TRUE.
                  END IF
               END IF

*  If there was no persistent part we want to retain the previous
*  current value, so effectively the stored parameter value is
*  unchanged.  Otherwise extract the new persistent part of the
*  value.
               IF ( TEMPV ) THEN
                  IF ( DELIND .EQ. 1 ) THEN
                     PVALUE = CURVAL
                  ELSE IF ( DELIND .NE. 1 ) THEN
                     PVALUE = TEXT( 1:DELIND - 1 )
                  END IF

*  Get the used length of the persistent and temporary values.
                  PLEN = CHR_LEN( PVALUE )
                  TLEN = CHR_LEN( TVALUE )

*  There was just a new persistent value supplied.
               ELSE
                  PVALUE = TEXT
                  PLEN = SLEN
               END IF
            END IF
         END IF

*  Find out how much of the text can be added to the total group
*  expression.
         FLEN = MIN( PLEN, MXGEX - NC )

*  Append this part of the string to the total group expression given so far.
         IF ( FLEN .GT. 0 ) THEN
            CALL CHR_APPND( PVALUE( : FLEN ), GRPEXP, NC )
         END IF

*  Append this part of the string to the persistent group expression
*  given so far.
         PLEN = MIN( PLEN, MXGEX - NCP )
         IF ( PLEN .GT. 0 ) THEN
            CALL CHR_APPND( PVALUE( : PLEN ), PGRPEX, NCP )
         END IF

*  Find out how much of the temporary value can be added to the total
*  group expression.
          FLEN = MIN( TLEN, MXGEX - NC )

          IF ( FLEN .GT. 0 ) THEN
             CALL CHR_APPND( DC, GRPEXP, NC )
             CALL CHR_APPND( TVALUE( : FLEN ), GRPEXP, NC )
          END IF

*  Cancel the parameter association in order to get more strings
*  through the parameter, unless there are no more to obtain.
         IF ( CFLAG ) CALL PAR_CANCL( PARAM( :PARLEN ), STATUS )

         FIRST = .FALSE.
      END DO

*  If a null parameter was supplied, annull the error unless only 1 group
*  expression was supplied.
      IF ( STATUS .EQ. PAR__NULL .AND. NEXP .GT. 1 ) THEN
         CALL ERR_ANNUL( STATUS )
      END IF

*  Report the total group expression if in verbose mode.
      CALL KPG1_VERB( VERB, 'KAPPA', STATUS )
      IF ( VERB ) THEN
         CALL MSG_SETC( 'P', PARAM( : PARLEN ) )

         IF ( NC .GT. 0 ) THEN
            CALL MSG_SETC( 'G', GRPEXP( : NC ) )
            CALL MSG_OUT( 'KPG1_GTGPT_MSG1', 'The following group '//
     :                    'expression was obtained for parameter '//
     :                    '%^P: ^G', STATUS )
         ELSE
            CALL MSG_OUT( 'KPG1_GTGPT_MSG2', 'A blank group '//
     :                    'expression was obtained for parameter %^P.',
     :                    STATUS )
         END IF

         IF ( NCP .GT. 0 ) THEN
            CALL MSG_SETC( 'G', PGRPEX( : NCP ) )
            CALL MSG_OUT( 'KPG1_GTGPT_MSG3', 'The following '//
     :                    'persistent group expression was obtained '//
     :                    'for parameter %^P: ^G', STATUS )
         ELSE
            CALL MSG_OUT( 'KPG1_GTGPT_MSG4', 'A blank persistent '//
     :                    'group expression was obtained for '//
     :                    'parameter %^P.', STATUS )
         END IF

      END IF

*  Store the attributes
*  ====================

*  Store a persistent current value.
*  ---------------------------------

*  Write a duplicate of the persistent value to be recovered at the
*  next usage when the true parameter's current value may have been
*  overwritten with a new composite value on the command line.
      IF ( NCP .GT. 0 ) THEN
         CALL CMP_PUT0C( FLOC, PNAMCV, PGRPEX( : NCP ), STATUS )

*  There was an error, such as a null value supplied.  Thus any new
*  parameterCV object is not needed, and indeed will cause a problem
*  on the next invocation because it is undefined.
      ELSE IF ( CREATE .AND. NCP .EQ. 0 ) THEN
         CALL ERR_MARK
         TSTAT = STATUS
         CALL ERR_ANNUL( STATUS )
         CALL DAT_ERASE( FLOC, PNAMCV, STATUS )
         CALL ERR_RLSE
         STATUS = TSTAT
      END IF

*  Tidy the no-longer-needed parameter-file locator.
      CALL DAT_ANNUL( FLOC, STATUS )

*  Save attributes for NDG.
*  ------------------------

*  Since the division between the classes of group members and
*  additional syntax will confuse GRP's counting and list of members,
*  flush the group and add the full list of attributes to the group.
      CALL GRP_SETSZ( IGRP, 0, STATUS )
      CALL GRP_GRPEX( GRPEXP( : NC ), GRP__NOID, IGRP, SIZE,
     :                ADDED, CFLAG, STATUS )

*  Register the returned group with NDG so that its contents will be
*  appended to the end of any default history records written out by the
*  NDF library.
      CALL NDG_ADDGH( PARAM( : PARLEN ), IGRP, STATUS )

*  Save the group expression to the parameter.
*  -------------------------------------------

*  Store a concatenated expression as the current value for the
*  parameter.  Also write to the parameter when there will be a further
*  call to this routine using the same parameter and there is temporary
*  component to the list (see below).  Do not record a total group
*  expression merely comprising a single flag character.
      IF ( GRPEXP( : NC ) .NE. FC .OR.
     :     ( .NOT. LAST .AND. TLEN .GT. 0 ) ) THEN
         GRPEXP = ' '
         NC = 0
         CALL CHR_APPND( PVALUE, GRPEXP, NC )

*  Since the routine may be called more than once in an application,
*  a second invocation needs to access the original concatenated total
*  group expression, including the temporary value.  Here it is stored
*  to the parameter value and hence returned by GRP_GROUP in the loop
*  above for the subsequent invocation.  For the final invocation,
*  only the persistent value should be stored.  Note this division
*  does not affect the group attributes stored in IGRP.
         IF ( .NOT. LAST .AND. TLEN .GT. 0 ) THEN
            CALL CHR_APPND( DELIM, GRPEXP, NC )
            CALL CHR_APPND( TVALUE, GRPEXP, NC )
            CALL SUBPAR_PUTNAME( IPAR, GRPEXP( : NC ), STATUS )

*  Why this works is not clear.  It aseems that the second call to
*  SUBPAR_PUTNAME fails, leaving the full set of persistent and
*  temporary attributes instead of the just the persistent formed.
*  I guess that some state is set in SUBPAR and the routines have
*  different state flags.
         ELSE IF ( NC .GT. 0 ) THEN
            CALL SUBPAR_CURSAV( IPAR, GRPEXP( : NC ), STATUS )

*  Allow for a null persistent value by removing the current value.
         ELSE
            CALL SUBPAR_CURSAV( IPAR, '!', STATUS )
         END IF
      END IF

*  If an error occurred, return a group size of zero.
      IF ( STATUS .NE. SAI__OK ) SIZE = 0

      END
