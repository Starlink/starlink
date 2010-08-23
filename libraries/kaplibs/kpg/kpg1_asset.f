      SUBROUTINE KPG1_ASSET( APP, PARAM, IOBJ, STATUS )
*+
*  Name:
*     KPG1_ASSET

*  Purpose:
*     Allows the specification of attribute values for an AST Object.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_ASSET( APP, PARAM, IOBJ, STATUS )

*  Description:
*     This routine allows the user to set attributes for an AST Object (see
*     SUN/210). The value to which an attribute is set in the returned
*     Object is determined as follows.
*
*     -  If the user supplies a value for the attribute using the given
*     parameter, then the Object is returned with the attribute set to the
*     supplied value. The user supplies the attribute values as a GRP
*     group expression in which each element is an AST attribute setting.
*
*     -  Otherwise, if the Object already had an explicit value set for the
*     attribute on entry (i.e. if AST_TEST returns .TRUE. for the
*     attribute), then the value is unchanged on exit.
*
*     -  Otherwise, a search is made for a default value for the attribute
*     using the search path described below. If a default value is found
*     for the attribute then the Object is returned with the attribute set
*     to the default value. The use of these defaults can be suppressed
*     by including the string "CLEAR" as the first element in the group of
*     attribute values supplied for the specified environment parameter.
*
*     Defaults are specified as a group of attribute setting strings within a
*     "defaults" text file. This file is found using the following search
*     path:
*
*     1) If the environment variable <APP>_<PARAM> is defined (<APP> and
*     <PARAM> in upper case), its value is taken to be the full path to
*     the defaults file.
*
*     2) If <APP>_<PARAM> is not defined, the file $HOME/<app>_<param>.def
*     is used (<app> and <param> in lower case).
*
*     3) If the file $HOME/<app>_<param>.def cannot be accessed, the file
*     $KAPPA_DIR/<app>_<param>.def is used.
*
*     4) If the file $KAPPA_DIR/<app>_<param>.def cannot be accessed, the
*     value of environment variable KAPPA_<PARAM> is taken to be the full
*     path to the defaults file.
*
*     5) If KAPPA_<PARAM> is not defined, the file $HOME/kappa_<param>.def
*     is used.
*
*     6) If the file $HOME/kappa_<param>.def cannot be accessed, the file
*     $KAPPA_DIR/kappa_<param>.def is used.
*
*     Each attribute setting within a group of settings should be of the
*     form "<name>=<value>", where <name> is taken to be the name of a
*     Object attribute (or synonym set by KPG1_ASPSY), and <value> is taken
*     to be the value to assign to the attribute. These attributes are
*     described in SUN/210. No error is reported if unrecognised attribute
*     names or illegal attribute values are specified.
*
*     Before being used, the attribute settings are edited to replace any
*     synonyms by their corresponding AST attribute names established by
*     earlier calls to KPG1_ASPSY. Colour names are also replaced by
*     corresponding PGPLOT colour indices.
*
*  Arguments:
*     APP = CHARACTER * ( * ) (Given)
*        The name of the calling application in the form
*        <package>_<application> (e.g. "KAPPA_DISPLAY"), for use in messages.
*     PARAM = CHARACTER * ( * ) (Given)
*        The name of the parameter to use for getting the group expression.
*     IOBJ = INTEGER (Given and Returned)
*        An AST pointer to the Object to be modified. Returned unchanged if
*        an error occurs.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  Colour attribute values may be supplied in any form recognised
*     by KPG1_PGCOL (e.g. colour name, MIN, MAX, integer index, etc.),
*     and the nearest colour in the current KAPPA pallette is used.
*     -  If a null value is supplied for the parameter, the error is
*     annulled and the Object is returned unchanged (except for any
*     defaults obtained using the usual search path).

*  Copyright:
*     Copyright (C) 1998, 1999 Central Laboratory of the Research Councils.
*     Copyright (C) 2005, 2006 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     12-AUG-1998 (DSB):
*        Original version.
*     14-OCT-1999 (DSB):
*        Added options for different defaults to be specified for each
*        application (items 1, 2 and 3 in the above defaults search path).
*        Changed the final default from $KAPPA_DIR/<param>.def to
*        $KAPPA_DIR/kappa_<param>/def to be consistent with the other
*        files.
*     16-DEC-2005 (DSB):
*        Add initialisation of default value for new pseudo-attribute DrawDSB.
*     17-FEB-2006 (DSB):
*        Add initialisation of default value for new pseudo-attribute
*        FileInTitle.
*     13-JUL-2007 (DSB):
*        Prevent KAPPA pseudo-attributes being reset when overlaying a
*        temporary style on top of a previously established style.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations
      INCLUDE 'PAR_ERR'          ! PAR error constants
      INCLUDE 'PSX_ERR'          ! PSX error constants

*  Arguments Given:
      CHARACTER APP*(*)
      CHARACTER PARAM*(*)

*  Arguments Given and Returned:
      INTEGER IOBJ

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER CLASS*20         ! Object class name
      CHARACTER GRPEXP*(GRP__SZGEX)! Group expression to read file
      CHARACTER DFILE*(GRP__SZFNM)! Defaults file name
      CHARACTER TEXT*(GRP__SZNAM)! Gerenal character string
      INTEGER ADDED              ! No. of elements added to group
      INTEGER DSIZE              ! Size of defaults group
      INTEGER I                  ! String index
      INTEGER LEXP               ! String length
      INTEGER IAT                ! Length of a string
      INTEGER IGRP1              ! GRP identifier for user-supplied strings
      INTEGER IGRP2              ! GRP identifier for default strings
      INTEGER IOBJ2              ! AST pointer to modified Object
      INTEGER ISTART             ! Index of 1st user-supplied string to use
      INTEGER SIZE               ! No. of strings in group
      LOGICAL BADAT              ! Is error related to an invalid setting?
      LOGICAL CHNGED             ! Has supplied Object been changed?
      LOGICAL CLEAR              ! Should defaults be cleared?
      LOGICAL FLAG               ! Was group ewxpression flagged?
      LOGICAL VERB               ! Issue warnings about bad attributes?
*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Save the class of the supplied Object. Use "AST Object" if an error
*  occurs.
      CLASS = AST_GETC( IOBJ, 'CLASS', STATUS )
      IF( STATUS .NE. SAI__OK ) CLASS = 'AST Object'

*  If the Object is a Plot, reset KAPPA pseudo-attributes to default
*  values. Do not do this if overlaying a temporary style onto of a
*  previously defined style.
      IF( CLASS .EQ. 'Plot' .AND. PARAM .NE. 'TEMPSTYLE' ) THEN
         CALL GRF_SETTBG( 0 )
         CALL KPG1_SETASTDSB( .TRUE. )
         CALL KPG1_SETASTFIT( .FALSE. )
      END IF

*  Take a copy of the Object, and indicate it has not yet been changed.
      IOBJ2 = AST_COPY( IOBJ, STATUS )
      CHNGED = .FALSE.

*  See if we are running in verbose mode.
      CALL KPG1_VERB( VERB, 'KAPPA', STATUS )

*  Get a group of attribute settings from the user, and see if defaults
*  are to be applied before applying the supplied settings.
*  ====================================================================

*  Get a group of Object attributes.
      IGRP1 = GRP__NOID
      CALL KPG1_GTGRP( PARAM, IGRP1, SIZE, STATUS )

*  Assume defaults are to be used.
      CLEAR = .FALSE.
      ISTART = 1

*  If a null value was supplied for the parameter annul the error.
      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         SIZE = 0

*  Otherwise, if some elements were supplied, see if element 1 was "CLEAR"
*  and set the starting index to 2 so that the first element is ignored
*  from now on.
      ELSE IF( SIZE .GT. 0 ) THEN
         CALL GRP_GET( IGRP1, 1, 1, TEXT, STATUS )
         CALL CHR_RMBLK( TEXT )
         CALL CHR_UCASE( TEXT )
         CLEAR = ( TEXT .EQ. 'CLEAR' )
         IF( CLEAR ) ISTART = 2
      END IF

*  Display the parameter name in verbose mode.
      IF( VERB ) THEN
         CALL MSG_BLANK( STATUS )
         CALL MSG_SETC( 'P', PARAM )
         CALL MSG_OUT( 'KPG1_ASSET_MSG1', 'Attribute settings '//
     :                 'obtained using parameter ^P:', STATUS )
      END IF

*  If required, apply the default attribute settings to the supplied
*  Object.
*  =================================================================
      IF( .NOT. CLEAR ) THEN

*  No defaults as yet.
         DSIZE = 0

*  Create a group to hold the default attribute settings.
         CALL GRP_NEW( 'Default attributes', IGRP2, STATUS )

*  Is the environment variable "<APP>_<PARAM>" defined?
         TEXT = ' '
         IAT = 0
         CALL CHR_APPND( APP, TEXT, IAT )
         CALL CHR_APPND( '_', TEXT, IAT )
         CALL CHR_APPND( PARAM, TEXT, IAT )
         CALL CHR_UCASE( TEXT ( : IAT ) )
         CALL CHR_RMBLK( TEXT ( : IAT ) )
         CALL PSX_GETENV( TEXT( : IAT ), DFILE, STATUS )

*  If so, attempt to read the contents of the file into the group.
         IF( STATUS .EQ. SAI__OK ) THEN
            IF( DFILE .NE. ' ' ) THEN
               GRPEXP = '^'
               LEXP = 1
               CALL CHR_APPND( DFILE, GRPEXP, LEXP )
               CALL GRP_GRPEX( GRPEXP( : LEXP ), GRP__NOID, IGRP2,
     :                         DSIZE, ADDED, FLAG, STATUS )

*  Annul or flush (if verbose) the error if the file could not be read.
               IF( STATUS .NE. SAI__OK ) THEN
                  IF( VERB ) THEN
                     CALL ERR_FLUSH( STATUS )
                  ELSE
                     CALL ERR_ANNUL( STATUS )
                  END IF
                  DSIZE = 0
               END IF

            END IF

*  If the environment variable <APP>_<PARAM> was not defined, annul the
*  error, and attempt to read defaults from the file $HOME/<app>_<param>.def
         ELSE IF( STATUS .EQ. PSX__NOENV ) THEN
            CALL ERR_ANNUL( STATUS )

*  Get the name of the file.
            CALL CHR_LCASE( TEXT ( : IAT ) )
            CALL CHR_APPND( '.def', TEXT, IAT )

*  Get the full file path.
            CALL KPG1_FLPTH( 'HOME', TEXT( : IAT ), DFILE, LEXP,
     :                       STATUS )

*  Attempt to read the contents of the file into the group.
            GRPEXP = '^'
            LEXP = 1
            CALL CHR_APPND( DFILE, GRPEXP, LEXP )
            CALL GRP_GRPEX( GRPEXP( : LEXP ), GRP__NOID, IGRP2,
     :                         DSIZE, ADDED, FLAG, STATUS )

*  Annul or flush (if verbose) the error if the file could not be read.
            IF( STATUS .NE. SAI__OK ) THEN
               IF( VERB ) THEN
                  CALL ERR_FLUSH( STATUS )
               ELSE
                  CALL ERR_ANNUL( STATUS )
               END IF
               DSIZE = 0

*  Next, try to use $KAPPA_DIR/<app>_<param>.def. Get the full file path.
               CALL KPG1_FLPTH( 'KAPPA_DIR', TEXT( : IAT ), DFILE,
     :                          LEXP, STATUS )

*  Attempt to read the contents of the file into the group.
               GRPEXP = '^'
               LEXP = 1
               CALL CHR_APPND( DFILE, GRPEXP, LEXP )
               CALL GRP_GRPEX( GRPEXP( : LEXP ), GRP__NOID, IGRP2,
     :                         DSIZE, ADDED, FLAG, STATUS )

*  Annul or flush (if verbose) the error if the file could not be read.
               IF( STATUS .NE. SAI__OK ) THEN
                  IF( VERB ) THEN
                     CALL ERR_FLUSH( STATUS )
                  ELSE
                     CALL ERR_ANNUL( STATUS )
                  END IF
                  DSIZE = 0

*  Is the environment variable "KAPPA_<PARAM>" defined?
                  TEXT = 'KAPPA_'
                  IAT = 6
                  CALL CHR_APPND( PARAM, TEXT, IAT )
                  CALL CHR_UCASE( TEXT ( : IAT ) )
                  CALL CHR_RMBLK( TEXT ( : IAT ) )
                  CALL PSX_GETENV( TEXT( : IAT ), DFILE, STATUS )

*  If so, attempt to read the contents of the file into the group.
                  IF( STATUS .EQ. SAI__OK ) THEN
                     IF( DFILE .NE. ' ' ) THEN
                        GRPEXP = '^'
                        LEXP = 1
                        CALL CHR_APPND( DFILE, GRPEXP, LEXP )
                        CALL GRP_GRPEX( GRPEXP( : LEXP ), GRP__NOID,
     :                               IGRP2, DSIZE, ADDED, FLAG, STATUS )

*  Annul or flush (if verbose) the error if the file could not be read.
                        IF( STATUS .NE. SAI__OK ) THEN
                           IF( VERB ) THEN
                              CALL ERR_FLUSH( STATUS )
                           ELSE
                              CALL ERR_ANNUL( STATUS )
                           END IF
                           DSIZE = 0
                        END IF

                     END IF

*  If the environment variable KAPPA_<PARAM> was not defined, annul the
*  error, and attempt to read defaults from the file $HOME/kappa_<param>.def
                  ELSE IF( STATUS .EQ. PSX__NOENV ) THEN
                     CALL ERR_ANNUL( STATUS )

*  Get the name of the file.
                     CALL CHR_LCASE( TEXT ( : IAT ) )
                     CALL CHR_APPND( '.def', TEXT, IAT )

*  Get the full file path.
                     CALL KPG1_FLPTH( 'HOME', TEXT( : IAT ), DFILE,
     :                                 LEXP, STATUS )

*  Attempt to read the contents of the file into the group.
                     GRPEXP = '^'
                     LEXP = 1
                     CALL CHR_APPND( DFILE, GRPEXP, LEXP )
                     CALL GRP_GRPEX( GRPEXP( : LEXP ), GRP__NOID, IGRP2,
     :                               DSIZE, ADDED, FLAG, STATUS )

*  Annul or flush (if verbose) the error if the file could not be read.
                     IF( STATUS .NE. SAI__OK ) THEN
                        IF( VERB ) THEN
                           CALL ERR_FLUSH( STATUS )
                        ELSE
                           CALL ERR_ANNUL( STATUS )
                        END IF
                        DSIZE = 0

*  Next, try to use $KAPPA_DIR/kappa_<param>.def. Get the full file path.
                        CALL KPG1_FLPTH( 'KAPPA_DIR', TEXT( : IAT ),
     :                                   DFILE, LEXP, STATUS )

*  Attempt to read the contents of the file into the group.
                        GRPEXP = '^'
                        LEXP = 1
                        CALL CHR_APPND( DFILE, GRPEXP, LEXP )
                        CALL GRP_GRPEX( GRPEXP( : LEXP ), GRP__NOID,
     :                               IGRP2, DSIZE, ADDED, FLAG, STATUS )

*  Annul or flush (if verbose) the error if the file could not be read.
                        IF( STATUS .NE. SAI__OK ) THEN
                           IF( VERB ) THEN
                              CALL ERR_FLUSH( STATUS )
                           ELSE
                              CALL ERR_ANNUL( STATUS )
                           END IF
                           DSIZE = 0

                        END IF

                     END IF

                  END IF

               END IF

            END IF

         END IF

*  If we have a group of defaults, use them.
         IF( DSIZE .GT. 0 .AND. STATUS .EQ. SAI__OK ) THEN
            CHNGED = .TRUE.

*  Loop round each attribute setting.
            DO I = 1, DSIZE
               CALL GRP_GET( IGRP2, I, 1, TEXT, STATUS )

*  Display each attribute setting in verbose mode.
               IF( VERB .AND. TEXT .NE. ' ' ) THEN
                  CALL MSG_SETC( 'S', TEXT )
                  CALL MSG_OUT( 'KPG1_ASSET_MSG2', '  ^S', STATUS )
               END IF

*  Apply it to the Object. Do not over-write any existing attribute values.
*  Do not report an error (unless in verbose mode) if the setting cannot be
*  used, since it may be a synonym used by another application.
               CALL KPG1_ASSTS( TEXT, VERB, .FALSE., IOBJ2, BADAT,
     :                          STATUS )

*  Flush any errors reported in verbose mode about bad attributes.
               IF( VERB .AND. BADAT .AND. STATUS .NE. SAI__OK ) THEN
                  CALL ERR_FLUSH( STATUS )
               END IF

            END DO

         END IF

*  Delete the group holding the default attribute settings.
         CALL GRP_DELET( IGRP2, STATUS )

      END IF

*  Apply the attribute settings obtained from the user to the supplied
*  Object.
*  ====================================================================
*  Only do anything if there are some elements to use in the group.
      IF( SIZE .GE. ISTART .AND. STATUS .EQ. SAI__OK ) THEN
         CHNGED = .TRUE.

*  Loop round each attribute setting.
         DO I = ISTART, SIZE
            CALL GRP_GET( IGRP1, I, 1, TEXT, STATUS )

*  Display each attribute setting in verbose mode.
            IF( VERB .AND. TEXT .NE. ' ' ) THEN
               CALL MSG_SETC( 'S', TEXT )
               CALL MSG_OUT( 'KPG1_ASSET_MSG2', '  ^S', STATUS )
            END IF

*  Apply it to the Object, over-writing any existing attribute value. Do not
*  report an error (unless in verbose mode) if the setting cannot be used,
*  since it may be a synonym used by another application.
            CALL KPG1_ASSTS( TEXT, VERB, .TRUE., IOBJ2, BADAT,
     :                       STATUS )

*  Flush any errors reported in verbose mode about bad attributes.
            IF( VERB .AND. BADAT .AND. STATUS .NE. SAI__OK ) THEN
               CALL ERR_FLUSH( STATUS )
            END IF

         END DO

      END IF

*  Tidy up.
*  ========
 999  CONTINUE

*  If all has gone well, and the Object has been changed, annul the supplied
*  Object and return the modified one.
      IF( STATUS .EQ. SAI__OK .AND. CHNGED ) THEN
         CALL AST_ANNUL( IOBJ, STATUS )
         IOBJ = IOBJ2

*  If not, just annul the local Object.
      ELSE
         CALL AST_ANNUL( IOBJ2, STATUS )
      END IF

*  Delete the group.
      IF( IGRP1 .NE. GRP__NOID ) CALL GRP_DELET( IGRP1, STATUS )

*  If a parameter abort value was supplied, re-report the error with
*  a more appropriate message.
      IF( STATUS .EQ. PAR__ABORT ) THEN
         CALL ERR_ANNUL( STATUS )
         STATUS = PAR__ABORT

         CALL MSG_SETC( 'PARAM', PARAM )
         CALL MSG_SETC( 'CLASS', CLASS )
         IF( INDEX( 'AEIOUaeiou', CLASS( 1 : 1 ) ) .GT. 0 ) THEN
            CALL MSG_SETC( 'ART', 'an' )
         ELSE
            CALL MSG_SETC( 'ART', 'a' )
         END IF

         CALL ERR_REP( 'KPG1_ASSET_3', 'Aborted attempt to set the '//
     :                 'attributes of ^ART ^CLASS using parameter '//
     :                 '%^PARAM.', STATUS )

*  Add a context message to any other error.
      ELSE IF( STATUS .NE. SAI__OK ) THEN

         CALL MSG_SETC( 'PARAM', PARAM )
         CALL MSG_SETC( 'CLASS', CLASS )
         IF( INDEX( 'AEIOUaeiou', CLASS( 1 : 1 ) ) .GT. 0 ) THEN
            CALL MSG_SETC( 'ART', 'an' )
         ELSE
            CALL MSG_SETC( 'ART', 'a' )
         END IF

         CALL ERR_REP( 'KPG1_ASSET_3', 'Failed to set the '//
     :                 'attributes of ^ART ^CLASS using parameter '//
     :                 '%^PARAM.', STATUS )
      END IF

      END
