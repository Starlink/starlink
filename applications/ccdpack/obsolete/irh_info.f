      SUBROUTINE IRH_INFO( IDH, INDEX, NAME, MODGRP, MODIND, DEPTH,
     :                     FILE, STATUS )
*+
*  Name:
*     IRH_INFO

*  Purpose:
*     Retrieve a name with supplementary information from a group.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRH_INFO( IDH, INDEX, NAME, MODGRP, MODIND, DEPTH, FILE,
*                    STATUS )

*  Description:
*     This routine returns a single name from a group, together with
*     various items of supplementary information about the name. This
*     information consists of:
*
*     1) If the name was specified by means of a modification element,
*     then the the IRH identifier of the group used as a basis for the
*     modification element is returned in MODGRP. If the name was not
*     specified by a modification element, then zero is returned.
*     Note, if the group upon which the modification was based has been
*     annulled, then a value of zero is returned.
*
*     2) If the name was specified by means of a modification element,
*     then the index of the original name (upon which the returned name
*     was based) is returned in MODIND. This is an index into the group
*     identified by MODGRP. If MODGRP is zero, then MODIND will also be
*     zero.
*
*     3) The number of levels of indirection at which the name was
*     specified is returned in DEPTH. Names given explicitly within a
*     response to a parameter prompt have a DEPTH value of zero. Names
*     given explicitly within a DEPTH zero indirection element have
*     DEPTH 1. Names given explicitly within a DEPTH 1 indirection
*     element, have DEPTH 2, etc.
*
*     4) The name of the indirection text file within which the name was
*     explicitly given, is returned in FILE. If the name was specified
*     at a depth of zero then FILE is returned blank.

*  Arguments:
*     IDH = INTEGER (Given)
*        IRH group identifier.
*     INDEX = INTEGER (Given)
*        Index (within the group specified by IDH) of the required name.
*        If the supplied value is outside the bounds of th egroup, then
*        "null" information is returned, as described below.
*     NAME = CHARACTER (Returned)
*        The name stored at the index given by INDEX, within the group
*        given by IDH. The "null" value is blank.
*     MODGRP = INTEGER (Returned)
*        The group identifier containing the modified name (see above).
*        The "null" value is zero.
*     MODIND = INTEGER (Returned)
*        The index of the modified name within the group identified by
*        MODGRP (see above). The "null" value is zero.
*     DEPTH = INTEGER (Returned)
*        The depth of indirection at which the name was specified (see
*        above). The "null" value is zero.
*     FILE = CHARACTER (Returned)
*        The file name within which the name was explcitly given (see
*        above). The "null" value is blank.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     17-JUN-1991 (DSB):
*        Original version.
*     26-FEB-1992 (PDRAPER):
*        Removed I90_PAR reference. Added DAT_PAR.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS DAT constants
      INCLUDE 'IRH_PAR'          ! IRH constants.
      INCLUDE 'IRH_ERR'          ! IRH error values.

*  Global Variables:
      INCLUDE 'IRH_COM'          ! IRH common blocks.
*        HCM_GSIZE( IRH__MAXG ) = INTEGER (Read)
*           The index of the last entry in each group.

*  Arguments Given:
      INTEGER IDH
      INTEGER INDEX

*  Arguments Returned:
      CHARACTER NAME*(*)
      INTEGER MODGRP
      INTEGER MODIND
      INTEGER DEPTH
      CHARACTER FILE*(*)

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the group identifier is not valid, report an error.
      IF( IDH .LT. 1 .OR. IDH .GT. IRH__MAXG ) THEN
         STATUS = IRH__INVID

      ELSE IF( .NOT. HCM_VALID( IDH ) ) THEN
         STATUS = IRH__INVID

      END IF

      IF( STATUS .EQ. IRH__INVID ) THEN
         CALL ERR_REP( 'IRH_INFO_ERR1',
     :                 'IRH_INFO: Invalid group identifier supplied',
     :                 STATUS )
      END IF

*  If the index is outside the bounds of the group, return null
*  information.
      IF( INDEX .LE. 0 .OR. INDEX .GT. HCM_GSIZE( IDH ) ) THEN
         NAME = ' '
         MODGRP = 0
         MODIND = 0
         DEPTH = 0
         FILE = ' '

*  Otherwise get the information.
      ELSE
         CALL IRH1_GTELM( IDH, INDEX, NAME, DEPTH, FILE, MODGRP, MODIND,
     :                    STATUS )

*  Set the index of the original name to zero, if MODGRP does not
*  identify a good group.
         IF( MODGRP .EQ. 0 ) MODIND = 0

      END IF

      END
* $Id$
