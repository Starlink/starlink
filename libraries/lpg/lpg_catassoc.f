      SUBROUTINE LPG_CATASSOC( PARAM, MODE, CI, STATUS )
*+
*  Name:
*     LPG_CATASSOC

*  Purpose:
*     Obtain an identifier for an existing catalogue via the parameter
*     system.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL LPG_CATASSOC( PARAM, MODE, CI, STATUS )

*  Description:
*     This routine should be called in place of CAT_ASSOC within
*     applications that process groups of catalogues.
*
*     On the first invocation of the application, a group of names of
*     existing catalogues will be obtained from the environment using the
*     specified parameter, and a CAT identifier for the first one
*     will be returned. If more than one catalogue was supplied for the
*     parameter then the application may be invoked again (see
*     LPG_AGAIN), in which case this routine will return an identifier
*     for the next catalogue in the group supplied on the first invocation.
*
*     If an application attempts to get a new catalogue by cancelling the
*     parameter (PAR_CANCL), the returned catalogue is NOT the next one in
*     the group, but is obtained by prompting the user for a single catalogue.
*
*     The monolith routine should arrange to invoke the application
*     repeatedly until one or more of its catalogue parameters have been
*     exhausted (i.e. all its values used). See LPG_AGAIN.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        Name of the parameter.
*     MODE = CHARACTER * ( * ) (Given)
*        Type of catalogue access required: 'READ', or 'WRITE'.
*     CI = INTEGER (Returned)
*        The catalogue identifier.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1999, 2000, 2002 Central Laboratory of the Research Councils.
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
*     {enter_new_authors_here}

*  History:
*     13-SEP-1999 (DSB):
*        Original version.
*     10-APR-2000 (DSB):
*        Added argument VERB.
*     11-JUN-2002 (DSB):
*        Removed erroneous VERB argument from CAT_ASSOC call.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS constants.
      INCLUDE 'CAT_PAR'          ! catalogue constants
      INCLUDE 'GRP_PAR'          ! GRP constants
      INCLUDE 'SUBPAR_PAR'       ! SUBPAR constants
      INCLUDE 'LPG_CONST'        ! LPG private constants
      INCLUDE 'PAR_ERR'          ! PAR error constants

*  Global Variables:
      INCLUDE 'LPG_COM'          ! LPG global variables
*        PNAME( LPG__MXPAR ) = CHARACTER * ( DAT__SZNAM ) (Read and Write)
*           The names of the data file parameters used by the application.
*        IGRP( LPG__MXPAR ) = INTEGER (Read and Write)
*           The identifier for the GRP groups holding the names
*           supplied for each data file parameter.
*        SIZE( LPG__MXPAR ) = INTEGER (Read and Write)
*           The number of files supplied for each data file parameter.
*        NPAR = INTEGER (Read and Write)
*           The number of data file parameters used by the application.
*        NRUN = INTEGER (Read)
*           The number of times the application has been invoked so far.
*        OLD( LPG__MXPAR ) = LOGICAL (Write)
*           A flag for each data file parameter indicating if the parameter is
*           used to access existing (i.e. old) data files. If not, the
*           parameter is used to access new data files to be created by the
*           application.
*        REP( LPG__MXPAR ) = LOGICAL (Read and Write)
*           A flag for each data file parameter indicating if the parameter
*           value has been reported yet by the current invocation of the
*           application.
*        VERB = LOGICAL (Read)
*           A flag indicating if the values used for each multi-valued
*           parameter should be displayed each time the parameter is accessed.
*           Also produces more verbose error messages when catalogues
*           cannot be accessed.
*        DISAB = (Read)
*           A flag indicating if looping is currently disabled.

*  Arguments Given:
      CHARACTER PARAM*(*)
      CHARACTER MODE*(*)

*  Arguments Returned:
      INTEGER CI

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL LPG1_INIT         ! Ensure LPG common blocks are initialised

*  Local Variables:
      CHARACTER FIELDS(5)*(GRP__SZNAM)! Supplemental information for an catalogue
      CHARACTER NAME*(GRP__SZNAM)! Catalogue name
      CHARACTER UPAR*(DAT__SZNAM)! Upper case parameter name
      INTEGER I                  ! Loop count
      INTEGER IPAR               ! LPG common block slot index
      INTEGER STATE              ! Parameter state
      LOGICAL FLAG               ! Get more catalogues?
*.

*  Set an initial value for the CI argument.
      CI = CAT__NOID

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If looping is currently disabled, just call CAT_ASSOC and exit.
      IF( DISAB ) THEN
         CALL CAT_ASSOC( PARAM, MODE, CI, STATUS )
         GO TO 999
      END IF

*  Convert the supplied parameter name to upper case.
      UPAR = PARAM
      CALL CHR_UCASE( UPAR )

*  See if the supplied parameter matches any of the parameters which have
*  already been accessed. If so, get the index of the matching parameter
*  within the LPG common arrays.
      IPAR = 0
      DO I = 1, NPAR
         IF( PNAME( I ) .EQ. UPAR ) THEN
            IPAR = I
            GO TO 10
         END IF
      END DO
 10   CONTINUE

*  If this parameter has not been accessed before...
      IF( IPAR .EQ. 0 ) THEN

*  Reserve the next available common block slot for this parameter.
*  Report an error if all slots are in use.
         NPAR = NPAR + 1
         IF( NPAR .GT. LPG__MXPAR ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI( 'MX', LPG__MXPAR )
            CALL ERR_REP( 'LPG_CATASSOC_ERR1', 'Too many data file '//
     :                    'parameters (>^MX) accessed by this '//
     :                    'application (programming error).', STATUS )
            GO TO 999
         END IF

         PNAME( NPAR ) = UPAR
         OLD( NPAR ) = .TRUE.

*  Get a group of catalogues from the environment using the supplied parameter.
*  Loop until a group expression is given which is not terminated by a
*  flag character.
         FLAG = .TRUE.
         DO WHILE( FLAG .AND. STATUS .EQ. SAI__OK )
            CALL CTG_ASSOC( UPAR, VERB, IGRP( NPAR ), SIZE( NPAR ),
     :                      FLAG, STATUS )
            IF( FLAG ) THEN
               CALL PAR_CANCL( UPAR, STATUS )
               CALL MSG_SETC( 'P', UPAR )
               CALL MSG_OUT( 'LPG_CATASSOC_MSG1', 'Please supply more'//
     :                       ' values for parameter %^P.', STATUS )
            END IF

         END DO

*  If no error occurred getting the catalogue names, but no catalogue names
*  were supplied, report a PAR__NULL status.
         IF( STATUS .EQ. SAI__OK .AND. SIZE( NPAR ) .EQ. 0 ) THEN
            STATUS = PAR__NULL
            CALL MSG_SETC( 'PARAM', UPAR )
            CALL ERR_REP( 'LPG_CATASSOC_ERR2', 'Null catalogue '//
     :                    'specified for the ''%^PARAM'' parameter.',
     :                    STATUS )
         END IF

*  If a PAR__NULL status exists, store a null group identifier in common
*  for this parameter, and reset the size to zero.
         IF( STATUS .EQ. PAR__NULL ) THEN
            IF( IGRP( NPAR ) .NE. GRP__NOID ) THEN
               CALL GRP_DELET( IGRP( NPAR ), STATUS )
            END IF
            SIZE( NPAR ) = 0
         END IF

*  Use this common block slot.
         IPAR = NPAR

*  If the parameter has been accessed before...
      ELSE

*  See if the application has cancelled the parameter value. If so,
*  we get a new catalogue using the parameter directly. This will probably
*  result in the user being prompted for a new parameter value.
         CALL PAR_STATE( UPAR, STATE, STATUS )
         IF( STATE .EQ. SUBPAR__CANCEL ) THEN
            CALL CTG_ASSO1( PARAM, VERB, MODE, CI, FIELDS, STATUS )

*  Store the new value in the group, replacing the old value, and store
*  the new list of names as the parameter's current value.
            IF( SIZE( IPAR ) .GT. 1 ) THEN
               CALL CTG_PTSUP( IGRP( IPAR ), NRUN, FIELDS, STATUS )
               CALL LPG1_PTPAR( UPAR, IGRP( NPAR ), STATUS )
            END IF

*  Otherwise, report a PAR__NULL error if the parameter has no GRP group
*  associated with it.
         ELSE IF( IGRP( IPAR ) .EQ. GRP__NOID .AND.
     :            STATUS .EQ. SAI__OK ) THEN
            STATUS = PAR__NULL
            CALL MSG_SETC( 'PARAM', UPAR )
            CALL ERR_REP( 'LPG_CATASSOC_ERR3', 'Null catalogue '//
     :                    'specified for the ''%^PARAM'' parameter.',
     :                    STATUS )
         END IF

      END IF

*  Get the catalogue identifier unless one was obtained above.
      IF( CI .EQ. CAT__NOID .AND. STATUS .EQ. SAI__OK ) THEN

*  Get the catalogue from the group. If the group only contains one catalogue
*  name, use it on all invocations of the application.
         IF( SIZE( IPAR ) .EQ. 1 ) THEN
            CALL CTG_CATAS( IGRP( IPAR ), 1, MODE, CI, STATUS )
            CALL GRP_GET( IGRP( IPAR ), 1, 1, NAME, STATUS )
         ELSE
            CALL CTG_CATAS( IGRP( IPAR ), NRUN, MODE, CI, STATUS )
            CALL GRP_GET( IGRP( IPAR ), NRUN, 1, NAME, STATUS )
         END IF

*  Tell the user which catalogue is being used, if required, and if it has not
*  already been reported.
         IF( VERB .AND. .NOT. REP( IPAR ) ) THEN
            CALL MSG_SETC( 'N', NAME )
            CALL MSG_SETC( 'P', UPAR )
            CALL MSG_OUT( 'LPG_CATASSOC_MSG2', '%^P = ^N', STATUS )
            REP( IPAR ) = .TRUE.
         END IF

      END IF

*  Tidy up.
 999  CONTINUE

*  If an error occurred, annul the catalogue.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_BEGIN( STATUS )
         CALL CAT_TRLSE( CI, STATUS )
         CALL ERR_END( STATUS )
      END IF

      END
