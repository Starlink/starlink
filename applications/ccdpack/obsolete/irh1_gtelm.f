      SUBROUTINE IRH1_GTELM( IDH, INDEX, NAME, LEVEL, FILE, MODGP,
     :                       MODIN, STATUS )
*+
*  Name:
*     IRH1_GTELM

*  Purpose:
*     Put an element into a group, including supplementary information.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRH1_GTELM( IDH, INDEX, NAME, LEVEL, FILE, MODGP,
*                      MODIN, STATUS )

*  Description:
*     This routine retrieves the information describing a given element
*     from the group identified by IDH. If the supplied index is
*     outside the bounds of the group, a blank value is returned for
*     NAME.

*  Arguments:
*     IDH = INTEGER (Given)
*        The IRH identifier for the group from which information is to
*        be retrieved. No check is performed on the validity of this
*        value.
*     INDEX = INTEGER (Given)
*        The index within the group of the element which is to be
*        retrieved. 
*     NAME = CHARACTER (Returned)
*        The text retrieved from the NAMES array.
*     LEVEL = INTEGER (Returned)
*        The indirection depth at which the name was specified. Zero
*        is returned if the name was given directly, instead of by
*        an indirection element.
*     FILE = CHARACTER (Returned)
*        The name of the indirection file in which the name was
*        specified. A blank is returned if the name was given directly,
*        instead of by an indirection element.
*     MODGP = INTEGER (Returned)
*        The IRH identifier of the group used as a basis for the name
*        if it was created as a result of a modification element. A
*        value of zero is returned if the name was not created as a
*        result of a modification element.
*     MODIN = INTEGER (Returned)
*        The index within the group specified by MODGP, of the name
*        used as a basis for the returned name.  
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-JUN-1991 (DSB):
*        Original version.
*     26-FEB-1992 (PDRAPER):
*        Added traling string lengths and DAT_PAR.
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

*  Global Variables:
      INCLUDE 'IRH_COM'          ! IRH common blocks.
*        HCM_NMPNT( IRH__MAXG ) = INTEGER (Read)
*           Pointers to the mapped NAMES array of each group.
*        HCM_MGPNT( IRH__MAXG ) = INTEGER (Read)
*           Pointers to the mapped MOD_GROUP array of each group.
*        HCM_MIPNT( IRH__MAXG ) = INTEGER (Read)
*           Pointers to the mapped MOD_INDEX array of each group.
*        HCM_LVPNT( IRH__MAXG ) = INTEGER (Read)
*           Pointers to the mapped LEVEL array of each group.
*        HCM_FLPNT( IRH__MAXG ) = INTEGER (Read)
*           Pointers to the mapped FILE array of each group.
*        HCM_GSIZE( IRH__MAXG ) = INTEGER (Read)
*           The index of the last entry in each group.

*  Arguments Given:
      INTEGER IDH
      INTEGER INDEX

*  Arguments Returned:
      CHARACTER NAME*(*)
      INTEGER LEVEL
      CHARACTER FILE*(*)
      INTEGER MODGP
      INTEGER MODIN

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the index is within the bounds of the group...
      IF( INDEX .GT. 0 .AND. INDEX .LE. HCM_GSIZE( IDH ) ) THEN      

*  Get the name from the NAMES array. NB, the final argument specifies
*  the length of each character string in the mapped NAMES array, and
*  is required by UNIX. There is no corresponding dummy argument in the
*  code for IRH1_IGET.
         CALL IRH1_IGET( HCM_GSIZE( IDH ), %VAL( HCM_NMPNT( IDH ) ),
     :                INDEX, NAME, STATUS, %VAL( IRH__SZNAM ) )

*  Get the supplementary information.
         CALL IRH1_GSUPP( INDEX, HCM_GSIZE( IDH ),
     :               %VAL( HCM_LVPNT( IDH ) ), %VAL( HCM_FLPNT( IDH ) ),
     :               %VAL( HCM_MGPNT( IDH ) ), %VAL( HCM_MIPNT( IDH ) ),
     :               LEVEL, FILE, MODGP, MODIN, STATUS,
     :               %VAL( IRH__SZNAM ) )

*  If the index was out of bounds, return null information.
      ELSE
         NAME = ' '
         LEVEL = 0
         FILE = ' '
         MODGP = 0
         MODIN = 0

      END IF

      END
* $Id$
