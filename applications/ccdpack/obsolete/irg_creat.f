      SUBROUTINE IRG_CREAT( PARAM, GID0, TERMC, GID, SIZE, ADDED, TERM,
     :                      STATUS )
*+
*  Name:
*     IRG_CREAT

*  Purpose:
*     Obtain a group of NDF names to be created.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRG_CREAT( PARAM, GID0, TERMC, GID, SIZE, ADDED, TERM,
*                     STATUS )

*  Description:
*     A group expression is obtained from the ADAM environment using the
*     supplied parameter. The expression is parsed (using the facilities
*     of the IRH routine IRH_GROUP, see ID/9) to produce a list of
*     explicit file names which are appended to the group identified by
*     GID. If GID has the value IRH__NOID on entry, then a new group is
*     created and GID is returned holding the new group identifier.  If
*     a new group is created, it is given a title consisting of the
*     supplied parameter name prefixed with the value of the symbolic
*     constant IRG__PREFX.
*
*     If, on entry, GID0 holds a valid group identifier, then the group
*     identified by GID0 is used as the basis for any modification
*     element contained in the group expression obtained from the
*     environment. If GID0 does not hold a valid group identified on
*     entry (for instance, if it is assigned the symbolic value
*     IRH__NOID by the calling application), then no check is made for
*     modification elements in the group expression.
*
*  Arguments:
*     PARAM = CHARACTER*(*) (Given)
*        The ADAM parameter with which to associate the group.
*     GID0 = INTEGER (Given)
*        An identifier for the group to be used as the basis for any
*        modification elements. If GID0 is invalid then any modification
*        elements included in the user-supplied group expression are
*        included literally in the output group.
*     TERMC = CHARACTER (Given)
*        The termination character to be searched for (see argument
*        TERM). Only the first character in TERMC is used. Characters
*        which form part of the syntax of a file specification ($:[.;)
*        should not be used as these may be mis-interpreted.
*     GID = INTEGER (Given and Returned)
*        An identifier for the group to which the supplied file
*        specifications are to be appended. If this is supplied equal to
*        the symbolic value IRH__NOID, then a new group is created and
*        its identifier returned in GID.
*     SIZE = INTEGER (Returned)
*        The total number of file specifications in the returned
*        group.
*     ADDED = INTEGER (Returned)
*        The number of file specifications added to the group as a
*        result of this call.
*     TERM = LOGICAL (Returned)
*        If the group expression was terminated by the character
*        specified by argument TERMC, then TERM is returned true.
*        Otherwise it is returned false.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1-JUL-1991 (DSB):
*        Original version.
*     27-FEB-1992 (PDRAPER):
*        Removed I90_PAR and argument string concatenation.
*     27-FEB-1997 (PDRAPER):
*        Removed all NDF and SDF specific code. We now expect
*        data files to be foreign format too.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL__ data constants
      INCLUDE 'IRH_PAR'          ! IRH constants.
      INCLUDE 'IRH_ERR'          ! IRH error values.
      INCLUDE 'IRG_ERR'          ! IRG error values.
      INCLUDE 'IRG_PAR'          ! IRG constants.

*  Global Variables:
      INCLUDE 'IRG_COM'          ! IRG common blocks.
*        GCM_OUT( IRH__MAXG ) = LOGICAL (Read and Write)
*           If true, then the group is an output group. Otherwise it is
*           an input group.

*  Arguments Given:
      CHARACTER PARAM*(*)
      INTEGER   GID0
      CHARACTER TERMC*(*)

*  Arguments Given and Returned:
      INTEGER GID

*  Arguments Returned:
      INTEGER SIZE
      INTEGER ADDED
      LOGICAL TERM

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Function giving used length of a
                                 ! string.

*  Local Variables:
      CHARACTER FILE * ( IRH__SZNAM ) ! Indirection file in which current name was specified.
      CHARACTER FSPEC * ( IRH__SZNAM ) ! Current full file spec.
      CHARACTER FSPEC0 * ( IRH__SZNAM ) ! Full file spec from input group.
      CHARACTER LINE * ( IRH__SZNAM ) ! Buffer for writing a line.
      CHARACTER NAME * ( IRH__SZNAM ) ! Current name.
      CHARACTER NAME0 * ( IRH__SZNAM ) ! Name from input group.
      CHARACTER SLICE * ( IRH__SZNAM ) ! Any slice specification included in a group name.
      CHARACTER TYPE * ( IRH__SZNAM ) ! File type (eg ".SDF").
      INTEGER DEPTH             ! Indirection depth at which current name was specified.
      INTEGER I                 ! Loop count.
      INTEGER MODGRP            ! Identifier for group used as basis for current name.
      INTEGER MODIND            ! Index of name used as basis for current name.
      INTEGER NAMEND            ! Position of first character beyond the end of the file name.
      INTEGER SIZE0             ! Size of input group.
      INTEGER STRLEN            ! Length of string
      INTEGER TMPGID            ! Identifier for temporary group holding names only.
      LOGICAL INGRP             ! True if GID0 identifies a valid group.
*.

*  Check inherited global status. 
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the supplied value of GID is IRH__NOID, create a new output group.
      IF( GID .EQ. IRH__NOID ) THEN
         LINE = ' '
         LINE = IRG__PREFX//PARAM
         CALL IRH_NEW( LINE, GID, STATUS )
         GCM_OUT( GID ) = .TRUE.
         GCM_AMODE (GID ) = 'WRITE'

*  Otherwise, check that any group identified by GID is a group created
*  by IRG. This is assumed to be true if the group title starts with the
*  string given by symbolic constant IRG__PREFX and the group has a
*  valid access mode.
      ELSE 
         CALL IRG1_CHECK( GID, .TRUE., STATUS )

*  Check that the group is an output group. If not, report an error.
         IF( .NOT. GCM_OUT( GID ) ) THEN
            STATUS = IRG__IN
            CALL ERR_REP( 'IRG_CREAT_ERR1',
     : 'IRG_CREAT: An input group was supplied when an output group '//
     : 'was required', STATUS )
         END IF
      END IF

*  Check that any group identified by GID0 is a group created by IRG.
      CALL IRG1_CHECK( GID0, .TRUE., STATUS )

*  If the GID0 identifier was invalid, annul the error and set a flag
*  to indicate that no group has been supplied to act as a basis for any
*  modification elements.
      IF( STATUS .EQ. IRH__INVID ) THEN
         CALL ERR_ANNUL( STATUS )
         INGRP = .FALSE.
         TMPGID = IRH__NOID

*  Otherwise, create a new group containing just the file name field
*  from the total file specifications given in the group identified by
*  GID0.
      ELSE
         INGRP = .TRUE.
         LINE = IRG__PREFX//'TEMPORARY GROUP'
         CALL IRH_NEW( LINE, TMPGID, STATUS )
         CALL IRH_GRPSZ( GID0, SIZE0, STATUS )

         DO I = 1, SIZE0
            CALL IRH_GET( GID0, I, 1, FSPEC0, STATUS )

*  Remove any slice specification from the input name.
            CALL IRG1_SLICE( FSPEC0, SLICE, NAMEND, STATUS )

*  Get the file name and add it to the temporary group.      
            CALL IRG1_FSPEC( FSPEC0, ' ', 'NAME', NAME0, STATUS )
            CALL IRH_PUT( TMPGID, 1, NAME0, 0, STATUS )
         END DO
      END IF

*  Call IRH_GROUP to create a group containing names specified using
*  the supplied ADAM parameter.
      CALL IRH_GROUP( PARAM, TMPGID, GID, TERMC, SIZE, ADDED, TERM,
     :                STATUS )

*  Go through each name in the group, expanding it to a full file
*  specification.
      DO I = 1, SIZE

*  Get the next name from the group, together with supplementary
*  information describing how the name was specified.
         CALL IRH_INFO( GID, I, NAME, MODGRP, MODIND, DEPTH, FILE,
     :                  STATUS )

*  Remove any slice specification.
         CALL IRG1_SLICE( NAME, SLICE, NAMEND, STATUS )

*  If the name was specified by a modification element based on the
*  temporary group created earlier...
         IF( MODGRP .EQ. TMPGID .AND. INGRP ) THEN

*  ...get the file spec used as the basis for the current name and
*  remove any slice specification.
            CALL IRH_GET( GID0, MODIND, 1, FSPEC0, STATUS )
            CALL IRG1_SLICE( FSPEC0, SLICE, NAMEND, STATUS )

*  Expand the name using the original name to supply defaults for any
*  fields of the file specification which the user did not specify.
            CALL IRG1_FSPEC( NAME, FSPEC0, ' ', FSPEC, STATUS )

         ELSE
*  Otherwise just use the name.
            FSPEC = NAME
         END IF

*  Replace the supplied name with the file specification.
         CALL IRH_PUT( GID, 1, FSPEC, I, STATUS )
      END DO

*  Annul the temporary group.
      IF( INGRP ) CALL IRH_ANNUL( TMPGID, STATUS )

*  If an error occured, give a context message.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL MSG_SETC( 'P', PARAM )
         CALL ERR_REP( 'IRG_CREAT_ERR2',
     : 'IRG_CREAT: Unable to associate a group of output files with '//
     : 'parameter ^P', STATUS )      
      END IF

      END
* $Id$
