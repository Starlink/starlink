      SUBROUTINE NDG_ASSOC( PARAM, VERB, IGRP, SIZE, FLAG, STATUS )
*+
*  Name:
*     NDG_ASSOC

*  Purpose:
*     Store names of existing NDFs specified through the environment.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDG_ASSOC( PARAM, VERB, IGRP, SIZE, FLAG, STATUS )

*  Description:
*     A group expression is obtained from the environment using the
*     supplied parameter. The expression is parsed (using the
*     facilities of the GRP routine GRP_GROUP, see SUN/150) to produce
*     a list of explicit names for existing NDFs which are appended to the
*     end of the supplied group (a new group is created if none is
*     supplied). If an error occurs while parsing the group expression, the
*     user is re-prompted for a new group expression. NDF identifiers for
*     particular members of the group can be obtained using NDG_NDFAS.

*  Arguments:
*     PARAM = CHARACTER*(*) (Given)
*        The parameter with which to associate the group expression.
*     VERB = LOGICAL (Given)
*        If TRUE then errors which occur whilst accessing supplied NDFs
*        are flushed so that the user can see them before re-prompting for
*        a new NDF ("verbose" mode). Otherwise, they are annulled and
*        a general "Cannot access file xyz" message is displayed before
*        re-prompting.
*     IGRP = INTEGER (Given and Returned)
*        The identifier of the group in which the NDF names are to be
*        stored. A new group is created if the supplied value is GRP__NOID.
*        It should be deleted when no longer needed using GRP_DELET.
*     SIZE = INTEGER (Returned)
*        The total number of NDF names in the returned group.
*     FLAG = LOGICAL (Returned)
*        If the group expression was terminated by the GRP "flag
*        character", then FLAG is returned true. Otherwise it is
*        returned false. Returned .FALSE. if an error occurs.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  Any file names containing wildcards or "[..]" globbing patterns are
*     expanded into a list of NDF names. The supplied strings are intepreted
*     by a shell (/bin/tcsh if it exists, otherwise /bin/csh, otherwise
*     /bin/sh), and so may contain shell meta-characters (e.g. twiddle, $HOME,
*     even command substitution and pipes - but pipe characters "|" need to
*     be escaped using a backslash "\" to avoid them being interpreted as GRP
*     editing characters).
*     -  Each supplied name may include an HDS path. For instance,
*     "/home/dsb/mydata.a.c(1).b" refers to an NDF stored in component
*     "a.c(1).b" in the HDS container file /home/dsb/mydata.sdf. Note,
*     wild cards are not allowed within HDS component paths (i.e. they
*     are only allowed within the specification of the container file).
*     -  If an HDS object is specified which is not an NDF, then the
*     object will be searched for NDF components. This search is
*     recursive, in that any components of the specified object are also
*     searched. The supplied name will be expanded into a group of names,
*     one for each NDF found within the specified HDS object. Note, NDFs
*     are not themselves searched for other NDFs. That is, the expanded
*     group of names will not include any NDF which is contained within
*     another NDF (i.e. NDFs which are stored as an extension item of
*     another NDF are not included in the group). For instance, if the
*     string "fred" is given, the HDS file fred.sdf will be searched for
*     NDFs and the returned group will contain references for all NDFs
*     found within fred.sdf.
*     - If the environment variable NDF_FORMATS_IN is defined (see
*     SSN/20) then only the highest priority file with any give file name
*     is included in the returned group. The priority of a file is
*     determined by its file type. Native NDFs (.sdf) have highest
*     priority. After that, priority decreases along the list of file
*     types specified in NDF_FORMATS_OUT. If no file type is given by
*     the user, the highest priority available file type is used. If an
*     explicit file type is given, then that file type is used.
*     -  Care should be taken if a trailing string enclosed in square
*     brackets is appended to the end of the file name. These are
*     interpreted first as a globbing pattern. Thus "fred[12]" would
*     match files with base names "fred1" and "fred2". If the pattern
*     does not match any existing files, then the trailing "[..]" string
*     is next interpreted as a foreign extension specifier. Thus if fred.fit
*     is a multi-extension FITS file, "fred[12]" would be interpreted as
*     the twelth image extension in fred.fit only if files cannot be found
*     with basenames "fred1" or "fred2".
*     -  NDFs contained within HDS files are opened in order to ensure
*     that they are valid NDFs. The user is notified if there are no
*     valid NDFs matching a supplied name, and they are asked to supply
*     a replacement parameter value. No check is made that any foreign
*     data files contain valid NDFs since this would involve a potentially
*     expensive data conversion. So, for instance, "*.fit" could pick up
*     FITS catalogues as well as FITS images. If a foreign data file does
*     not contain a valid NDF, an error will be reported when the NDF is
*     accessed using NDG_NDFAS.
*     -  Each element in the returned group contains a full specification
*     for an NDF. Several other groups are created by this routine, and
*     are associated with the returned group by means of a GRP "owner-slave"
*     relationship. These supplemental groups are automatically deleted
*     when the returned group is deleted using GRP_DELET. The returned
*     group should not be altered using GRP directly because corresponding
*     changes may need to be made to the supplemental groups. Routines
*     NDG_SETSZ, NDG_GTSUP and NDG_PTSUP are provided to manipulate the
*     entire chain of groups. The full chain (starting from the head) is
*     as follows:
*
*        - NDF slice specifications
*        - HDS paths
*        - File types
*        - Base file names
*        - Directory paths
*        - Full NDF specification (this is the returned group IGRP)
*
*     -  If an error is reported the group is returned unaltered. If
*     no group is supplied, an empty group is returned.
*     -  A null value (!) can be given for the parameter to indicate
*     that no more NDFs are to be specified. The corresponding error
*     is annulled before returning unless no NDFs have been added to
*     the group.
*     -  If the last character in the supplied group expression is
*     a colon (:), a list of the NDFs represented by the group
*     expression (minus the colon) is displayed, but none are
*     actually added to the group. The user is then re-prompted for
*     a new group expression.

*  Copyright:
*     Copyright (C) 1992, 1993 Science & Engineering Research Council.
*     Copyright (C) 1999, 2000 Central Laboratory of the Research Councils.
*     Copyright (C) 2005 Particle Physics & Astronomy Research Council.
*     Copyright (C) 2009 Science & Technology Facilities Council.
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
*     12-AUG-1992 (DSB):
*        Original version.
*     5-FEB-1993 (DSB):
*        Facility added for listing files represented by a group
*        expression by terminating the group expression with a colon.
*     9-9-1999 (DSB):
*        Improved the prologue.
*     10-APR-2000 (DSB):
*        Added argument VERB.
*     15-SEP-2005 (TIMJ):
*        Check that return variables from CHR_FANDL are non-zero
*     20-SEP-2006 (DSB):
*        Remove erroneous "%" signs from error messages.
*     20-OCT-2009 (DSB):
*        Register the returned group with NDG_ADDGH so that it is
*        included in NDF history.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP constants.
      INCLUDE 'GRP_ERR'          ! GRP error constants.
      INCLUDE 'PAR_ERR'          ! Parameter system error constants.
      INCLUDE 'NDG_ERR'          ! NDG error constants.

*  Arguments Given:
      CHARACTER PARAM*(*)
      LOGICAL   VERB

*  Arguments Given and Returned:
      INTEGER   IGRP

*  Arguments Returned:
      INTEGER   SIZE
      LOGICAL   FLAG

*  Status:
      INTEGER   STATUS             ! Global status

*  Local Variables:
      CHARACTER GRPEXP*(GRP__SZGEX)! Group expression
      CHARACTER NAME*(GRP__SZNAM)  ! Good NDF file name
      CHARACTER STRING*(GRP__SZNAM)! String identifying a bad NDF
      INTEGER FIRST                ! Index of first non-blank character
      INTEGER I                    ! Loop count
      INTEGER IGRP2                ! A group to contain any bad NDF names
      INTEGER IPAR                 ! SUBPAR parameter identifier
      INTEGER ISTAT                ! Temporary status
      INTEGER LAST                 ! Index of last non-blank character
      INTEGER SIZE0                ! The initial size fo the group
      INTEGER SZBAD                ! Size of group holding list of bad NDFs
      LOGICAL AGAIN                ! True if the user is to be re-prompted
      LOGICAL LIST                 ! True if a listing of files is required
*.

*  Ensure that a false value is returned for FLAG if an error has
*  already occured.
      FLAG = .FALSE.

*  Ensure that variables are initialised even if bad status
*  is set inside the routien
      GRPEXP = ' '
      LIST = .FALSE.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Ensure that error reports are deferred.
      CALL ERR_MARK

*  Overwrite any value SIZE may have on entry.
      SIZE = 0

*  If a group has been supplied, get its initial size.
      IF( IGRP .NE. GRP__NOID ) THEN
         CALL GRP_GRPSZ( IGRP, SIZE0, STATUS )
      ELSE
         SIZE0 = 0
      END IF

*  Create a group to hold any bad NDF names, and the reassons why they
*  are bad.
      CALL GRP_NEW( 'BAD DATA SETS', IGRP2, STATUS )

*  Get a group expression from the environment using the supplied
*  parameter.
 10   CONTINUE
      CALL SUBPAR_FINDPAR( PARAM, IPAR, STATUS )
      CALL SUBPAR_GETNAME( IPAR, GRPEXP, STATUS )

*  If the first and last characters are single quotes, remove them.
*  Check for status and make sure that FIRST and LAST do not addres
*  element 0 in the string
      IF (STATUS .EQ. SAI__OK) THEN
         CALL CHR_FANDL( GRPEXP, FIRST, LAST )
         IF (FIRST .NE. 0 .AND. LAST .NE. 0) THEN
            IF( GRPEXP( FIRST : FIRST ) .EQ. '''' .AND.
     :           GRPEXP( LAST : LAST ) .EQ. '''' ) THEN
               GRPEXP( FIRST : FIRST ) = ' '
               GRPEXP( LAST : LAST ) = ' '
            END IF
         END IF
      END IF

*  If the last character is a colon remove it and set a flag
*  indicating that the names are to be listed but not included in the
*  returned group.
*  Check for status and make sure that FIRST and LAST do not addres
*  element 0 in the string
      IF (STATUS .EQ. SAI__OK) THEN
         CALL CHR_FANDL( GRPEXP, FIRST, LAST )
         IF (FIRST .NE. 0 .AND. LAST .NE. 0) THEN
            IF( GRPEXP( LAST : LAST ) .EQ. ':' ) THEN
               LIST = .TRUE.
               GRPEXP( LAST : LAST ) = ' '
            ELSE
               LIST = .FALSE.
            END IF
         END IF
      END IF

*  Expand the group expression into a list of NDF names and append
*  them to the end of the specified group.
      CALL NDG_ASEXP( GRPEXP, VERB, IGRP2, IGRP, SIZE, FLAG, STATUS )

*  If some of the files were not valid NDFs, annul the error and then
*  re-report a more friendly message for each bad NDF.
      IF( STATUS .EQ. NDG__NOFIL ) THEN
         IF( VERB ) THEN
            CALL ERR_FLUSH( STATUS )
         ELSE
            CALL ERR_ANNUL( STATUS )
         END IF

*  Set up a temporary bad inherited status which can be passed to
*  ERR_REP.
         ISTAT = SAI__ERROR

*  Get the size of the group which holds the names of the bad NDFs.
         CALL GRP_GRPSZ( IGRP2, SZBAD, STATUS )

*  Loop round each bad name.
         DO I = 1, SZBAD

*  Get the next bad NDF name.
            CALL GRP_GET( IGRP2, I, 1, STRING, STATUS )

*  Abort if anything has gone wrong with the GRP routines.
            IF( STATUS .NE. SAI__OK ) GO TO 999

*  Report it, using the temporary (bad) inherited status.
            CALL MSG_SETC( 'NDF', STRING )
            CALL ERR_REP( 'NDG_ASSOC_ERR1', '  Cannot access ^NDF',
     :                    ISTAT )

         END DO

*  Set the size of the group holding the bad NDFs back to zero.
         CALL NDG_SETSZ( IGRP2, 0, STATUS )
         SIZE = 0

*  Abort if anything has gone wrong with the GRP routines.
         IF( STATUS .NE. SAI__OK ) GO TO 999

*  Set a bad status to correspond with the messages reported with the
*  temporary status.
         STATUS = SAI__ERROR

*  Indicate that the user should be re-prompted.
         AGAIN = .TRUE.

*  If there was something wrong with the format of the supplied group
*  expression, indicate that the user is to re-prompted.
      ELSE IF( STATUS .EQ. GRP__BADME .OR.
     :         STATUS .EQ. GRP__DEEP .OR.
     :         STATUS .EQ. GRP__FIOER .OR.
     :         STATUS .EQ. GRP__NULNM ) THEN
         AGAIN = .TRUE.

*  If all went well, but the group expression ended in a colon,
*  list the new names added to the group, and indicate that a new
*  group is required. Flush each report individually to avoid the
*  possibilioty of the EMS stack overflowing if many NDFs have
*  been specified.
      ELSE IF( LIST .AND. STATUS .EQ. SAI__OK ) THEN

         ISTAT = SAI__ERROR
         CALL ERR_REP( ' ', ' ', ISTAT )
         CALL ERR_FLUSH( ISTAT )

         IF( SIZE0 .LT. SIZE ) THEN
            DO I = SIZE0 + 1, SIZE
               CALL GRP_GET( IGRP, I, 1, NAME, STATUS )
               CALL MSG_SETC( 'NAME', NAME )
               ISTAT = SAI__ERROR
               CALL ERR_REP( ' ','    ^NAME', ISTAT )
               CALL ERR_FLUSH( ISTAT )
            END DO
         ELSE
            ISTAT = SAI__ERROR
            CALL ERR_REP( ' ', 'No NDFs given.', ISTAT )
            CALL ERR_FLUSH( ISTAT )
         END IF

         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', ' ', STATUS )
         AGAIN = .TRUE.

*  If any other error occurred, or if no error occurred, the user will
*  not be re-prompted.
      ELSE
         AGAIN = .FALSE.
      END IF

*  If the user is to be re-prompted...
      IF( AGAIN ) THEN

*  Ask the user to give a new parameter value.
         CALL MSG_SETC( 'P', PARAM )
         CALL ERR_REP( 'NDG_ASSOC_ERR2', '  Please give a new value '//
     :                 'for parameter %^P', STATUS )
         CALL ERR_REP( ' ', ' ', STATUS )

*  Flush the errors so that the user sees them.
         CALL ERR_FLUSH( STATUS )

*  Cancel the parameter value.
         CALL SUBPAR_CANCL( IPAR, STATUS )

*  Annul any errors produced by the previous line.
         IF( STATUS .NE. SAI__OK ) CALL ERR_ANNUL( STATUS )

*  Set the group back to its previous size.
         CALL NDG_SETSZ( IGRP, SIZE0, STATUS )
         SIZE = SIZE0

*  Go back for a re-prompt.
         GO TO 10

      END IF

*  Register the returned group with NDG so that its contents will be
*  appended to the end of any default history records written out by the
*  NDF library.
      IF( SIZE .GT. 0 ) CALL NDG_ADDGH( PARAM, IGRP, STATUS )

*  Delete the group used to hold the list of bad NDFs.
 999  CONTINUE
      CALL GRP_DELET( IGRP2, STATUS )

*  If a null parameter value was given, annul the error. If no NDFS
*  have been added to the group re-report it with a more friendly
*  message.
      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         IF( SIZE .LE. SIZE0 ) THEN
            STATUS = PAR__NULL
            CALL MSG_SETC( 'P', PARAM )
            CALL ERR_REP( 'NDG_ASSOC_ERR3', 'A null group of NDFs was'//
     :                    ' given for parameter ^P.', STATUS )
         END IF

*  If the parameter request was aborted, annul the error and re-report
*  it with a more friendly message.
      ELSE IF ( STATUS .EQ. PAR__ABORT ) THEN
         CALL ERR_ANNUL( STATUS )
         STATUS = PAR__ABORT
         CALL MSG_SETC( 'P', PARAM )
         CALL ERR_REP( 'NDG_ASSOC_ERR4', 'Aborted attempt to '//
     :                 'associate a group of NDFs with parameter ^P.',
     :                 STATUS )

*  If any other error occurred, add a context message.
      ELSE IF( STATUS .NE. SAI__OK ) THEN
         CALL MSG_SETC( 'P', PARAM )
         CALL ERR_REP( 'NDG_ASSOC_ERR5', 'Unable to associate a group'//
     :                 ' of NDFs with parameter ^P.', STATUS )
      END IF

*  Release the error stack.
      CALL ERR_RLSE

      END
