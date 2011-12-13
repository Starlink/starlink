      SUBROUTINE LPG_CREPL( PARAM, PLACE, STATUS )
*+
*  Name:
*     LPG_CREPL

*  Purpose:
*     Create a new NDF placeholder via the parameter system.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL LPG_CREPL( PARAM, PLACE, STATUS )

*  Description:
*     This routine should be called in place of NDF_CREPL within
*     applications that process lists of NDFs.
*
*     On the first invocation of the application, a group of names for
*     some new NDFs will be obtained from the environment using the
*     specified parameter. The first name will be used to create an NDF
*     placeholder with the requested attributes, and an identifier for the
*     placeholder will be returned. If more than one name was supplied for
*     the parameter then the application may be invoked again (see
*     LPG_AGAIN), in which case this routine will return an identifier
*     for another placeholder with the next name in the group supplied on
*     the first invocation.
*
*     If a modification element is included in the group expression
*     supplied for the parameter on the first invocation of the
*     application, the placeholder are based on the names of the
*     first group of existing data files (NDFs or catalogues) to be
*     accessed by the application.
*
*     If an application attempts to get a new NDF by cancelling the
*     parameter (PAR_CANCL), the name used to create the returned NDF is
*     NOT the next one in the group, but is obtained by prompting the
*     user for a single new placeholder.
*
*     The monolith routine should arrange to invoke the application
*     repeatedly until one or more of its NDF parameters have been
*     exhausted (i.e. all its values used). See LPG_AGAIN.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        Name of the parameter.
*     PLACE = INTEGER (Returned)
*        NDF placeholder identifying the nominated position in the
*        data system.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2010 Science & Technology Facilities Council.
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
*     3-NOV-2010 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS constants.
      INCLUDE 'GRP_PAR'          ! GRP constants.
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'PAR_ERR'          ! PAR error constants
      INCLUDE 'SUBPAR_PAR'       ! SUBPAR constants
      INCLUDE 'LPG_CONST'        ! LPG private constants

*  Global Variables:
      INCLUDE 'LPG_COM'          ! LPG global variables
*        PNAME( LPG__MXPAR ) = CHARACTER * ( DAT__SZNAM ) (Read and Write)
*           The names of the data file parameters used by the application.
*        IGRP( LPG__MXPAR ) = INTEGER (Read and Write)
*           The identifier for the GRP groups holding the data file names
*           supplied for each data file parameter.
*        SIZE( LPG__MXPAR ) = INTEGER (Read and Write)
*           The number of data files supplied for each data file parameter.
*        NPAR = INTEGER (Read and Write)
*           The number of data file parameters used by the application.
*        NRUN = INTEGER (Read)
*           The number of times the application has been invoked so far.
*        OLD( LPG__MXPAR ) = LOGICAL (Read and Write)
*           A flag for each data file parameter indicating if the parameter is
*           used to access existing (i.e. old) data files. If not, the parameter
*           is used to access new data files to be created by the application.
*        REP( LPG__MXPAR ) = LOGICAL (Read and Write)
*           A flag for each data file parameter indicating if the parameter value
*           has been reported yet by the current invocation of the
*           application.
*        VERB = LOGICAL (Read)
*           A flag indicating if the values used for each multi-valued
*           parameter should be displayed each time the parameter is accessed.
*        DISAB = (Read)
*           A flag indicating if looping is currently disabled.

*  Arguments Given:
      CHARACTER PARAM*(*)

*  Arguments Returned:
      INTEGER PLACE

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL LPG1_INIT         ! Ensure LPG common blocks are initialised

*  Local Variables:
      CHARACTER NAME*(GRP__SZNAM)! Name of an NDF
      CHARACTER UPAR*(DAT__SZNAM)! Upper case parameter name
      INTEGER I                  ! Loop count
      INTEGER IGRP0              ! Basis group for modifications
      INTEGER IPAR               ! LPG common block slot index
      INTEGER IRUN               ! The parameter index to use
      INTEGER STATE              ! Parameter state
      LOGICAL FLAG               ! Get more NDFs?
      INTEGER JUNK               ! Unused NDF identifier
*.

*  Set an initial value for the PLACE argument.
      PLACE = NDF__NOPL

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If looping is currently disabled, just call NDF_CREPL and exit.
      IF( DISAB ) THEN
         CALL NDF_CREPL( PARAM, PLACE, STATUS )
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
         IF( NPAR .GT. LPG__MXPAR .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI( 'MX', LPG__MXPAR )
            CALL ERR_REP( 'LPG_CREPL_ERR1', 'Too many data file '//
     :                    'parameters (>^MX) accessed by this '//
     :                    'application (programming error).', STATUS )
            GO TO 999
         END IF

         PNAME( NPAR ) = UPAR
         OLD( NPAR ) = .FALSE.

*  Modification elements will use the first group of existing data files
*  (NDFs or ctalogues) as the basis group. Find the index of the first
*  parameter associated with existing data files.
         IGRP0 = GRP__NOID
         DO I = 1, NPAR - 1
            IF( OLD( I ) .AND. IGRP( I ) .NE. GRP__NOID ) THEN
               IGRP0 = IGRP( I )
               GO TO 20
            END IF
         END DO
 20      CONTINUE

*  Get a group of names from the environment using the supplied parameter.
*  Loop until a group expression is given which is not terminated by a
*  flag character.
         FLAG = .TRUE.
         DO WHILE( FLAG .AND. STATUS .EQ. SAI__OK )
            CALL NDG_CREAT( UPAR, IGRP0, IGRP( NPAR ), SIZE( NPAR ),
     :                      FLAG, STATUS )
            IF( FLAG ) THEN
               CALL PAR_CANCL( UPAR, STATUS )
               CALL MSG_SETC( 'P', UPAR )
               CALL MSG_OUT( 'LPG_CREPL_MSG1', 'Please supply more '//
     :                       'values for parameter %^P.', STATUS )
            END IF

         END DO

*  If no error occurred getting the NDF names, but no NDF names were
*  supplied, report a PAR__NULL status.
         IF( STATUS .EQ. SAI__OK .AND. SIZE( NPAR ) .EQ. 0 ) THEN
            STATUS = PAR__NULL
            CALL MSG_SETC( 'PARAM', UPAR )
            CALL ERR_REP( 'LPG_CREPL_ERR2', 'Null NDF structure '//
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

*  Store the expanded list of names as the parameter's current value.
         IF( IGRP( NPAR ) .NE. GRP__NOID ) THEN
            CALL LPG1_PTPAR( UPAR, IGRP( NPAR ), STATUS )
         END IF

*  Use this common block slot.
         IPAR = NPAR

*  If the parameter has been accessed before...
      ELSE

*  See if the application has cancelled the parameter value. If so,
*  we get a new NDF using the parameter directly. This will probably
*  result in the user being prompted for a new parameter value.
         CALL PAR_STATE( UPAR, STATE, STATUS )
         IF( STATE .EQ. SUBPAR__CANCEL ) THEN
            CALL LPG_CRPL1( PARAM, PLACE, NAME, STATUS )

*  Store the new value in the group, replacing the old value, and store
*  the new list of names as the parameter's current value.
            IF( SIZE( IPAR ) .GT. 1 ) THEN
               CALL GRP_PUT( IGRP( IPAR ), 1, NAME, NRUN, STATUS )
               CALL LPG1_PTPAR( UPAR, IGRP( NPAR ), STATUS )
            END IF

*  Otherwise, report a PAR__NULL error if the parameter has no GRP group
*  associated with it.
         ELSE IF( IGRP( IPAR ) .EQ. GRP__NOID .AND.
     :            STATUS .EQ. SAI__OK ) THEN
            STATUS = PAR__NULL
            CALL MSG_SETC( 'PARAM', UPAR )
            CALL ERR_REP( 'LPG_CREPL_ERR3', 'Null NDF structure '//
     :                    'specified for the ''%^PARAM'' parameter.',
     :                    STATUS )
         END IF

      END IF

*  Get the placeholder identifier unless one was obtained above.
      IF( PLACE .EQ. NDF__NOPL .AND. STATUS .EQ. SAI__OK ) THEN

*  If the group only contains one name, use it on all invocations of the
*  application.
         IF( SIZE( IPAR ) .EQ. 1 ) THEN
            IRUN = 1
         ELSE
            IRUN = NRUN
         END IF

*  If the requested output NDF is already in use as an input NDF, we
*  create a temporary output NDF (if allowed) which is copied to the
*  correct place once the application has completed. Modify the entry in
*  the group  if necessary to refer to a temporary output NDF.
         CALL LPG1_ADDTM( IGRP( IPAR ), IRUN, STATUS )

*  Get the NDF from the group.
         CALL NDG_NDFPL( IGRP( IPAR ), IRUN, PLACE, STATUS )

*  Tell the user which NDF is being used, if required, and if it has not
*  already been reported.
         IF( VERB .AND. .NOT. REP( IPAR ) ) THEN
            CALL GRP_GET( IGRP( IPAR ), IRUN, 1, NAME, STATUS )
            CALL MSG_SETC( 'N', NAME )
            CALL MSG_SETC( 'P', UPAR )
            CALL MSG_OUT( 'LPG_CREPL_MSG2', '%^P = ^N', STATUS )
            REP( IPAR ) = .TRUE.
         END IF

      END IF

*  Tidy up.
 999  CONTINUE

*  If an error occurred, annul the placeholder by calling an NDF routine
*  that uses placeholder.
      IF( STATUS .NE. SAI__OK ) CALL NDF_COPY( NDF__NOID, PLACE, JUNK,
     :                                         STATUS )

      END
