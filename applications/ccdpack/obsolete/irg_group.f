      SUBROUTINE IRG_GROUP( PARAM, TERMC, AMODE, GID, SIZE, ADDED, TERM,
     :                      STATUS )
*+
*  Name:
*     IRG_GROUP

*  Purpose:
*     Obtain a set of existing NDFs from the ADAM environment and store
*     them in a group.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRG_GROUP( PARAM, TERMC, AMODE, GID, SIZE, ADDED, TERM,
*                     STATUS )

*  Description:
*     A group expression is obtained from the ADAM environment using a
*     single prompt for the supplied parameter. The expression is
*     parsed (using the facilities of the IRH routine IRH_GROUP, see
*     ID/9) to produce a list of explicit .SDF files. If any of the
*     specified files do not exist, an error is reported identifying
*     the missing files and the status IRG__NOFIL is returned (the
*     returned group contains any files which were found succesfully).
*     If any other error is reported the group is returned unaltered
*     (SIZE is returned equal to its original size and ADDED is
*     returned equal to zero).  The full file specifications (minus
*     file type and version number but including any NDF slice
*     specifications) of the files which were found succesfully are
*     appended to the group identified by argument GID.  If GID has the
*     value IRH__NOID on entry, then a new group is created and GID is
*     returned holding the new group identifier.  If a new group is
*     created, it is given a title consisting of the supplied parameter
*     name prefixed by the value of symbolic constant IRG__PREFX.

*  Arguments:
*     PARAM = CHARACTER*(*) (Given)
*        The ADAM parameter with which to associate the group
*        expression.
*     TERMC = CHARACTER (Given)
*        The termination character to be searched for (see argument
*        TERM). Only the first character in TERMC is used. Characters
*        which form part of the syntax of a file specification ($:[.;)
*        should not be used as these may be mis-interpreted.
*     AMODE = CHARACTER (Given)
*        The access mode required for the NDFs contained in the
*        specified .SDF files. This should be 'READ', 'WRITE' or
*        'UPDATE' or any abbreviation.
*     GID = INTEGER (Given and Returned)
*        An identifier for the group to which the supplied .SDF file
*        specifications are to be appended. If this is supplied equal to
*        the symbolic value IRH__NOID, then a new group is created and
*        its identifier returned in GID.
*     SIZE = INTEGER (Returned)
*        The total number of .SDF file specifications in the returned
*        group.
*     ADDED = INTEGER (Returned)
*        The number of .SDF file specifications added to the group as a
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
*     2-JUL-1991 (DSB):
*        Original version.
*     30-JAN-1992 (DSB):
*        Modified to return found files when other files could not be
*        found. Also modified to include NDF slice specifications in
*        names stored in the group.
*     27-FEB-1992 (PDRAPER):
*        Removed I90_PAR and string concatenation in argument list.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL__ data constants
      INCLUDE 'IRG_PAR'          ! IRG constants.
      INCLUDE 'IRG_ERR'          ! IRG error values.
      INCLUDE 'IRH_PAR'          ! IRH constants.

*  Global Variables:
      INCLUDE 'IRG_COM'          ! IRG common blocks.
*        GCM_AMODE( IRH__MAXG ) = CHARACTER (Read and Write)
*           Access mode (READ, WRITE or UPDATE) for each group.
*           the corresponding GROUP structure.
*        GCM_OUT( IRH__MAXG ) = LOGICAL (Read and Write)
*           If true, then the group is an output group. Otherwise it is
*           an input group.

*  Arguments Given:
      CHARACTER PARAM * ( * )
      CHARACTER TERMC * ( * )
      CHARACTER AMODE * ( * )

*  Arguments Given and Returned:
      INTEGER   GID

*  Arguments Returned:
      INTEGER   SIZE
      INTEGER   ADDED
      LOGICAL   TERM

*  Status:
      INTEGER   STATUS             ! Global status

*  Local Variables:
      CHARACTER AAMODE * ( IRG__SZMOD ) ! Verified group access mode.
      LOGICAL   FNF             ! True if any specified NDFs could not be found.
      INTEGER   OLDGID          ! The input value of GID
      INTEGER   SIZE0           ! The size of the group on entry to this routine.
      CHARACTER COMMEN * ( IRH__SZNAM ) ! Comment string

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Store the value of GID on entry.
      OLDGID = GID

*  If the supplied value of GID is IRH__NOID, create a new input group.
      IF( GID .EQ. IRH__NOID ) THEN
         COMMEN = ' '
         COMMEN = IRG__PREFX//PARAM
         CALL IRH_NEW( COMMEN, GID, STATUS )

*  Verify that the supplied access mode is legal.
         CALL IRG1_VMODE( AMODE, AAMODE, STATUS )

*  Otherwise, check that any group identified by GID is a group created
*  by IRG. This is assumed to be true if the group title starts with the
*  string given by symbolic constant IRG__PREFX.
      ELSE
         CALL IRG1_CHECK( GID, .TRUE., STATUS )

*  Check that the group is an input group. If not, report an error.
         IF( GCM_OUT( GID ) ) THEN
            STATUS = IRG__OUT
            CALL ERR_REP( 'IRG_GROUP_ERR1',
     : 'IRG_GROUP: An output group was supplied when an input group '//
     : 'was required', STATUS )
         END IF

*  Remember the original access mode.
         AAMODE = GCM_AMODE( GID )
      END IF

*  Get the original size of the group.
      CALL IRH_GRPSZ( GID, SIZE0, STATUS )

*  Call IRH_GROUP to append a set of files names (specified using
*  the supplied ADAM parameter) to the group.
      CALL IRH_GROUP( PARAM, IRH__NOID, GID, TERMC, SIZE, ADDED, TERM,
     :                STATUS )

*  Expand wild cards, and check that all the .SDF files added to the
*  group exist.  IRG1_NDFCH gives warning messages if any do not exist.
*  The final group consists of a set of full file specifications.
*  Entries added before calling this routine are uneffected.
      CALL IRG1_NDFCH( SIZE0 + 1, GID, FNF, STATUS )

*  Update the SIZE and ADDED arguments to take account of the new group
*  members produced as a result of the expansion of any wild cards.
      CALL IRH_GRPSZ( GID, SIZE, STATUS )
      ADDED = SIZE - SIZE0

*  If the returned group is not the same as the supplied group, Set up
*  the global variables describing the returned group.
      IF( GID .NE. OLDGID ) THEN
         GCM_AMODE( GID ) = AAMODE
         GCM_OUT( GID ) = .FALSE.
      END IF

*  If an error has been reported, set the group back to its original
*  size.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_BEGIN( STATUS )

         SIZE = SIZE0
         ADDED = 0
         CALL IRH_SETSZ( GID, SIZE0, STATUS )

         CALL ERR_END( STATUS )
      END IF

*  If some files could not be found, set an error status. Note, the NDFs
*  which WERE found are left in the returned group.
      IF( FNF .AND. STATUS .EQ. SAI__OK ) STATUS = IRG__NOFIL

*  If an error occured give a context message.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL MSG_SETC( 'P', PARAM )
         CALL ERR_REP( 'IRG_GROUP_ERR1',
     : 'IRG_GROUP: Error obtaining a group of files using'//
     : ' parameter ^P', STATUS )
      END IF

      END
* $Id$
