      LOGICAL FUNCTION LPG_AGAIN( STATUS )
*+
*  Name:
*     LPG_AGAIN

*  Purpose:
*     Decide if the application should be executed again.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = LPG_AGAIN( STATUS )

*  Description:
*     This routine is used to allow multiple invocations of an application
*     within an Starlink monolith to process a group of data files. The
*     initialization routine LPG_START should be called prior to this
*     routine. This routine returns a logical flag indicating if the
*     application should be invoked again. A typical way to use this
*     routine within a monolith is as follows:

*        CALL LPG_START( VERB, DELAY, DISAB, STATUS )
*        DO WHILE ( LPG_AGAIN( STATUS ) )
*           IF ( ACTION .EQ. 'ADD' ) THEN
*              CALL ADD( STATUS )
*           ELSE IF ( ACTION .EQ. 'SUB' ) THEN
*              CALL SUB( STATUS )
*           ELSE IF...
*           ...
*           END IF
*        END DO

*     The application corresponding to the required action will always be
*     invoked once. The applications should use the routines LPG_ASSOC,
*     LPG_PROP, LPG_CREAT and LPG_CREP to get identifier for NDFs, in
*     place of the corresponding routines from the NDF library.
*
*     For catalogues, routines LPG_CATASSOC and LPG_CATCREAT should be
*     used in place of CAT_ASSOC and CAT_CREAT.
*
*     LPG_AGAIN returns a .TRUE. value until a group of data files is
*     exhausted, where-upon it deletes all its groups and returns a
*     .FALSE. value.
*
*     On the first invocation of the application, groups of data files are
*     obtained whenever one of the above LPG routines is used to get an NDF
*     or CAT identifier, and an identifier corresponding to the first name in
*     each group is returned to the application. On subsequent invocations,
*     the names in the groups obtained during the first invocation are used
*     without obtaining new parameter values from the environment. The
*     index of the returned data file within each group is increment by 1
*     each time the application is invoked.
*
*     If an application is invoked more than once, all other parameters
*     retain the values they had at the end of the first invocation.
*     Applications that use this scheme should avoid having parameters
*     with "VPATH=DYNAMIC" in the interface file, since the dynamic
*     default calculated on the first invocation will then be re-used for
*     all subsequent invocations, which may be inappropriate. A better
*     scheme is to have "VPATH=DEFAULT", "PPATH=DYNAMIC" and "DEFAULT=!".
*     The code should then annul any PAR__NULL status after accessing
*     the parameter, and use the previously calculated dynamic default
*     value for the parameter. With this scheme, the parameter value is
*     "!" at the end of the first invocation, and so retains this value
*     for all subsequent invocations, resulting in appropriate dynamic
*     defaults being used.
*
*     A situation in which the above suggestion does not work is if an
*     application sometimes sets a dynamic default, and sometimes doesn't.
*     In this case, you do not want to have VPATH=DEFAULT,DEFAULT=! because
*     this would require the application to abort in the cases where there
*     is no dynamic default available. It is probably better in these
*     cases to have VPATH=PROMPT,PPATH=DYNAMIC and accept the fact that
*     the user will be prompted for a parameter that was previously
*     defaulted.
*
*     Some applications test to see if a parameter was specified on the
*     command line, and vary their behaviour accordingly. This is done
*     by checking the state of the parameter before accessing it, a
*     state of PAR__ACTIVE (or SUBPAR__ACTIVE) indicating that the
*     parameter already has a value. This is correct on the first
*     invocation, but not on subsequent invocations because the first
*     invocation may have set a parameter value, resulting in subsequent
*     invocations thinking that the parameter was given on the command
*     line. To avoid this, applications should use LPG_STATE in place of
*     PAR_STATE. LPG_STATE remembers the state of the parameter on the
*     first invocation, and returns that state, rather than the current
*     parameter state, on subsequent invocations.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Returned Value:
*     LPG_AGAIN = LOGICAL
*        .TRUE. if the application should be executed again.

*  Copyright:
*     Copyright (C) 1999, 2004 Central Laboratory of the Research Councils.
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
*     13-AUG-1999 (DSB):
*        Original version.
*     15-MAR-2004 (DSB):
*        Added facility for using a single NDF as both input and output.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS constants.
      INCLUDE 'PAR_ERR'          ! PAR error constants.
      INCLUDE 'LPG_CONST'        ! LPG private constants
      INCLUDE 'GRP_PAR'          ! GRP constants.

*  Global Variables:
      INCLUDE 'LPG_COM'          ! LPG global variables
*        PNAME( LPG__MXPAR ) = CHARACTER * ( DAT__SZNAM ) (Read and Write)
*           The names of the daat file parameters used by the application.
*        IGRP( LPG__MXPAR ) = INTEGER (Read and Write)
*           The identifier for the GRP groups holding the daat file names
*           supplied for each daat file parameter.
*        SIZE( LPG__MXPAR ) = INTEGER (Read and Write)
*           The number of data files supplied for each parameter.
*        NPAR = INTEGER (Read and Write)
*           The number of data file parameters used by the application.
*        NRUN = INTEGER (Read and Write)
*           The number of times the application has been invoked so far.
*        OLD( LPG__MXPAR ) = LOGICAL (Read and Write)
*           A flag for each data file parameter indicating if the parameter
*           is used to access existing (i.e. old) daat files. If not, the
*           parameter is used to access new data files to be created by the
*           application.
*        REP( LPG__MXPAR ) = LOGICAL (Write)
*           A flag for each data file parameter indicating if the parameter
*           value has been reported yet by the current invocation of the
*           application.
*        VERB = LOGICAL (Read)
*           A flag indicating if the values used for each multi-valued
*           parameter should be displayed each time the parameter is accessed.
*        DELAY = REAL (Read)
*           The inter-invocation delay, in seconds.
*        DISAB = LOGICAL (Read)
*           A flag indicating if looping is currently disabled.
*        TMPLST = INTEGER (Read and Write)
*           A GRP identifier for a group holding the full specification
*           for any temporary output NDFs created during the previous
*           invocation of the application. A temporary output NDF is
*           created if the output NDF requested by the user may already
*           be open by the NDF system. In this case the temporary NDF
*           is copied to the requested position once the application has
*           finished.  The TMPLST group holds adjacent pairs of file
*           specs; the first one in each pair is the spec of the temporary
*           output NDF, the second is the spec of the requested output NDF.
*        OPNLST = INTEGER (Read and Write)
*           A GRP identifier for a group holding the full specification
*           for any existing NDFs which have been opened for read-only
*           input by this invocation of the application.
*        REPLACE = LOGICAL (Read)
*           Should the user be allowed to use the same input as both
*           input and output? If so, a temporary NDF will be used to
*           store the output while the application is running. Once the
*           application has finsished, the existing input NDF will be
*           replaced by a copy of the temporary NDF. If REPLACE is false
*           an error will be reported if an attempt is amde to use a
*           single NDF as both input and output.

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER PARS*255         ! List of over-supplied parameters
      INTEGER I                  ! Loop count
      INTEGER IAT                ! Used length of PARS string
      INTEGER ISTAT              ! Saved status on entry
      INTEGER N                  ! No. of over-supplied parameters
      INTEGER SURP               ! No. of surplus data files
      LOGICAL ALLONE             ! All parameters have only a single data file?
      LOGICAL FLUSH              ! Flush an error in the previous invocation?
*.

*  Initialise.
      LPG_AGAIN = .FALSE.

*  Save the initial status value and set a new value for this routine.
      ISTAT = STATUS
      STATUS = SAI__OK

*  Create a new error context.
      CALL ERR_MARK

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Increment the number of times the application has been run.
      NRUN = NRUN + 1

*  If this is the first invocation of the application, return .TRUE. so
*  that the application is executed at least once.
      IF( NRUN .EQ. 1 ) THEN
         LPG_AGAIN = .TRUE.

*  If the application has already been executed, we execute it again so
*  long as none of the data file groups have been exhausted. Do nothing
*  if looping is currently diabled. This causes a .TRUE. value to be
*  returned on the first invocation, but .FALSE. for subsequent invocations.
      ELSE IF( .NOT. DISAB ) THEN

*  Ensure that any temporary output NDFs created by the previous invocation
*  of the application have been copied to the appropriate permanent
*  location.
         CALL LPG1_TMPCP( STATUS )

*  Assume to begin with that there are sufficient unused data files to run
*  the application again.
         LPG_AGAIN = .TRUE.

*  Assume for now that all data file parameters are associated with only a
*  single data file.
         ALLONE = .TRUE.

*  Check every known data file parameter. If any parameter has been
*  exhausted (i.e. if all the data files supplied for the parameter have
*  been used), then the application cannot be called again. The exception
*  to this is that if a parameter only has 1 data file associated with it,
*  the same data file is used as many times as required (i.e. the group is
*  never exhausted), but only if there is at least one other parameter
*  which has more than 1 data file.
         DO I = 1, NPAR
            IF( SIZE( I ) .GT. 1 ) THEN
               ALLONE = .FALSE.
               IF( NRUN .GT. SIZE( I ) ) THEN
                  LPG_AGAIN = .FALSE.
                  GO TO 10
               END IF
            END IF
         END DO
 10      CONTINUE

*  If no multi-valued data file parameters were found, do not re-run the
*  application.
         IF( ALLONE ) LPG_AGAIN = .FALSE.

*  If any group has been exhausted, see if there are any groups which
*  have not been exhausted.
         IF( .NOT. LPG_AGAIN .AND. .NOT. ALLONE ) THEN
            PARS = ' '
            IAT = 0
            N = 0
            DO I = 1, NPAR
               IF( NRUN .LE. SIZE( I ) ) THEN
                  IF( N .GT. 0 ) CALL CHR_APPND( ',', PARS, IAT )
                  CALL CHR_APPND( PNAME( I ), PARS, IAT )
                  N = N + 1
                  SURP = SIZE( I ) - NRUN + 1
               END IF
            END DO

*  If any un-exhausted groups were found, warn the user that some data files
*  were not used.
            IF( N .GT. 0 .AND. STATUS .EQ. SAI__OK ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETC( 'P', PARS( : IAT ) )

               IF( N .EQ. 1 ) THEN
                  IF( SURP.EQ. 1 ) THEN
                     CALL ERR_REP( 'LPG_AGAIN_ERR1', 'One surplus '//
     :                             'data file supplied for parameter '//
     :                             '%^P has been ignored.', STATUS )
                  ELSE
                     CALL MSG_SETI( 'S', SURP )
                     CALL ERR_REP( 'LPG_AGAIN_ERR2', '^S surplus data'//
     :                             'files supplied for parameter %^P '//
     :                             'have been ignored.', STATUS )
                  END IF

               ELSE
                  CALL MSG_SETI( 'N', N )
                  CALL ERR_REP( 'LPG_AGAIN_ERR3', 'Surplus data files'//
     :                          ' supplied for the following ^N '//
     :                          'parameter have been ignored: ^P.',
     :                          STATUS )
               END IF

*  Since this error is not fatal, flush it.
               CALL ERR_FLUSH( STATUS )

            END IF

         END IF

*  If we are invoking the application again...
         IF( LPG_AGAIN ) THEN

*  Delay for the required interval.
            IF( DELAY .GT. 0.0 ) CALL LPG1_SLEEP( DELAY, STATUS )

*  Print out a divider on the screen to separate the screen output from
*  the two invokations, if required, and indicate that the parameter values
*  used in the next invocation of the application have not yet been reported.
            IF( VERB ) THEN
               CALL MSG_BLANK( STATUS )

               DO I = 1, NPAR
                  REP( I ) = .FALSE.
               END DO
            END IF

         END IF

      END IF

*  If the previous invocation of the application resulted in an error,
*  we should flush it so that the application can continue to process any
*  remaining data files. Since we are currently in a new error reporting
*  environment, we cannot flush the error now, so we set a flag indicating
*  if the error should be flushed once we have returned to the original error
*  reporting environment. We flush if:
*
*     1) There is no error status in the current reporting context. Such
*        an error would indicate a failure in the looping infrastructure which
*        we cannot recover from.
*     2) There was an error on entry (recorded in ISTAT).
*     3) The error on entry was not PAR__ABORT. This means the user can enter
*        !! to abort the whole loop.
*     4) We are about to invoke the application again (i.e. errors in the
*        final invocation are not flushed).
*     5) We have already performed at least one invocation (i.e. the error
*        did not occur in the ADAM fixed part, or the calling monolith
*        routine).
      FLUSH = ( STATUS .EQ. SAI__OK .AND. ISTAT .NE. SAI__OK .AND.
     :          ISTAT .NE. PAR__ABORT .AND. LPG_AGAIN .AND.
     :          NRUN .GT. 1 )

*  Re-instate the original error reporting environment. First, annul any
*  errors created in this routine if an error existed on entry.
      IF ( ISTAT .NE. SAI__OK ) THEN
         CALL ERR_ANNUL( STATUS )
         STATUS = ISTAT
      END IF

*  Release the current error context.
      CALL ERR_RLSE

*  If an error in the previous invocation is to be flushed, warn the
*  user, and flush the error.
      IF( FLUSH ) THEN
         CALL ERR_REP( 'LPG_AGAIN_ERR', 'Continuing to process '//
     :                 'remaining data files...', STATUS )
         CALL ERR_FLUSH( STATUS )
      END IF

*  If the application is to be run again, initialise the list of existing NDFs
*  currently opened by the application,and the list of temporary output
*  NDFs currently created nu the application.
      IF( LPG_AGAIN ) THEN

         IF( OPNLST .NE. GRP__NOID ) THEN
            CALL GRP_SETSZ( OPNLST, 0, STATUS )
         ELSE
            CALL GRP_NEW( ' ', OPNLST, STATUS )
         END IF

         IF( TMPLST .NE. GRP__NOID ) THEN
            CALL GRP_SETSZ( TMPLST, 0, STATUS )
         ELSE
            CALL GRP_NEW( ' ', TMPLST, STATUS )
         END IF

      END IF

*  If an error occurred, indicate that the application should not be
*  executed again.
      IF( STATUS .NE. SAI__OK ) LPG_AGAIN = .FALSE.

*  If the application is not to be executed again, tidy up.
      IF( .NOT. LPG_AGAIN ) CALL LPG1_TIDY( STATUS )

      END
