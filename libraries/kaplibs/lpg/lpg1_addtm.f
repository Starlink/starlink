      SUBROUTINE LPG1_ADDTM( IGRP1, INDX, STATUS )
*+
*  Name:
*     LPG1_ADDTM

*  Purpose:
*     Copy temporary output NDFs totheir correct location.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL LPG1_ADDTM( IGRP1, INDX, STATUS )

*  Description:
*     This routine replaces an output NDF name with a temporary NDF name
*     if the output NDF is already being used as an input NDF. It also
*     saves the association between the requested NDF and the temporary
*     NDF in a GRP group.

*  Arguments:
*     IGRP1 = INTEGER (Given)
*        The GRP identifier for an NDG group holding the requested output
*        NDF specification.
*     INDX = INTEGER (Given)
*        The index within IGRP1 of the output NDF specification.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15-MAR-2004 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'LPG_CONST'        ! LPG private constants

*  Global Variables:
      INCLUDE 'LPG_COM'          ! LPG global variables
*        TMPLST = INTEGER (Read)
*           A GRP identifier for a group holding the full specification
*           for any temporary output NDFs created during the previous
*           invocation of the application. A temporary output NDF is
*           created if the output NDF requested by the user may already
*           be open by the NDF system. In this case the temporary NDF
*           is copied to the requested position once the application has
*           finished.  The TMPLST group holds adjacent pairs of file
*           specs; the first one in each pair is the spec of the temporary 
*           output NDF, the second is the spec of the requested output NDF.
*        OPNLST = INTEGER (Read)
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

*  Arguments Given:
      INTEGER IGRP1
      INTEGER INDX

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER TMPNAM*(GRP__SZNAM)    ! Temporary output NDF spec
      CHARACTER REQSPC*(GRP__SZNAM)    ! Requested output NDF spec
      CHARACTER FIELDS(6)*(GRP__SZNAM) ! Supplemental information for an NDF
      INTEGER IAT                      ! Index of final "/"
      INTEGER FINDX                    ! Index of requested NDF in opened list
*.

*  Check the inherited status. 
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Do nothing if the replacing of input NDFs with corresponding output
*  NDFs has not been enabled.
      IF( REPLACE ) THEN

*  See if the requested output NDF specification has already been used as
*  an input NDF.
         CALL GRP_GET( IGRP1, INDX, 1, REQSPC, STATUS )
         CALL GRP_INDEX( REQSPC, IGRP1, 1, FINDX, STATUS )

*  We only replace the requested NDF with a temporary NDF if the
*  requested NDF is already being used as an input NDF.
         IF( FINDX .GT. 0 ) THEN

*  Create a name for the temporary output NDF.
            CALL LPG1_TMPNM( TMPNAM, STATUS )
            IF( TMPNAM .EQ. ' ' .AND. STATUS .EQ. SAI__OK ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( ' ', 'LPG1_ADDTM: Unable to create '//
     :                       'a temporary output NDF in the current '//
     :                       'directory.', STATUS )
               GO TO 999
            END IF

*  Locate the last "/" in the temporary filename.
            CALL NDG1_LASTO( TMPNAM, '/', IAT, STATUS )

*  Store the modified supplemental information for the specified element.
            FIELDS( 1 ) = ' '
            FIELDS( 2 ) = ' '
            FIELDS( 3 ) = '.sdf'
            FIELDS( 4 ) = TMPNAM( IAT + 1 : )
            IF( IAT .GT. 0 ) THEN
               FIELDS( 5 ) = TMPNAM( : IAT )
            ELSE
               FIELDS( 5 ) = ' '
            END IF
            FIELDS( 6 ) = TMPNAM
            CALL NDG_PTSUP( IGRP1, INDX, FIELDS, STATUS )

*  Store the temporary NDF spec and the requested NDF spec in the list of 
*  temporary NDFs in the LPG common block.
            CALL GRP_PUT( TMPLST, 1, TMPNAM, 0, STATUS )
            CALL GRP_PUT( TMPLST, 1, REQSPC, 0, STATUS )

         END IF
      END IF

 999  CONTINUE

      END
