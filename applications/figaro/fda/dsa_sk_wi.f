      SUBROUTINE DSA_SEEK_WIDTH( DSAREF, AXIS, EXIST,
     :   SINGLE, WIDTH, STATUS )
*+
*  Name:
*     DSA_SEEK_WIDTH

*  Purpose:
*     Determine whether or not axis width array exists.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA_SEEK_WIDTH( DSAREF, AXIS, EXIST, SINGLE, WIDTH, STATUS )

*  Description:
*     This routine looks to see if an NDF contains a width array for a
*     certain axis. A width array, must have the same dimensions as the
*     corresponding centre array.
*
*     Contrary to earlier implementations, this routine makes no check
*     of the validitiy of the width array.

*  Arguments:
*     DSAREF = CHARACTER * ( * ) (Given)
*        The reference name associated with the NDF.
*     AXIS = INTEGER (Given)
*        The number of the axis in question.
*     EXIST = LOGICAL (Returned)
*        True if there is a width array.
*     SINGLE = LOGICAL (Returned)
*        Always false.
*     WIDTH = DOUBLE PRECISION (Returned)
*        Always 1D0.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     26 Aug 1988 (ks):
*        Original version.
*     14 Feb 1989 (ks):
*        Comments reformatted.
*     08 Dec 1989 (ks):
*        Size requirements tightened - now must match corresponding axis
*        array.
*     11 Dec 1989 (ks):
*        Now uses DSA__ routines for structure details.
*     21 Aug 1992 (ks):
*        Automatic portability modifications ("INCLUDE" syntax etc)
*        made.
*     23 Aug 1992 (ks):
*        Remove unused variable declarations.
*     29 Aug 1992 (ks):
*        "INCLUDE" filenames now upper case.
*     20 Feb 1996 (hme):
*        FDA library.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants

*  Global Variables:
      INCLUDE 'DSA_COMMON'       ! DSA global variables

*  Arguments Given:
      CHARACTER * ( * ) DSAREF
      INTEGER AXIS

*  Arguments Returned:
      LOGICAL EXIST
      LOGICAL SINGLE
      DOUBLE PRECISION WIDTH

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL AXISND             ! Whether axis is N-D
      INTEGER SLOT               ! The reference slot
      CHARACTER * ( DAT__SZNAM ) NAME ! Component name
      CHARACTER * ( DAT__SZLOC ) DELLOC ! Ignored
      CHARACTER * ( DAT__SZLOC ) ARYLOC ! Centre array locator
      CHARACTER * ( DAT__SZLOC ) TLOC1 ! HDS locator
      CHARACTER * ( DAT__SZLOC ) TLOC2 ! HDS locator

*.

*  Check inherited global status.
      IF ( STATUS .NE. 0 ) RETURN

*  Begin error context and translate status.
      CALL ERR_MARK
      STATUS = SAI__OK

*  Look up reference slot.
      CALL DSA1_RFND( DSAREF, SLOT, STATUS )

*  Fixed return values.
      SINGLE = .FALSE.
      WIDTH  = 1D0

*  If axis is N-dimensional.
      CALL DSA1_AXISND( SLOT, AXIS,
     :   AXISND, ARYLOC, DELLOC, NAME, STATUS )
      IF ( AXISND ) THEN

*     Find array name.
*     If 'DATA' locate grandparent, else locate parent.
         CALL DAT_NAME( ARYLOC, NAME, STATUS )
         IF ( NAME .EQ. 'DATA' ) THEN
            CALL DAT_PAREN( ARYLOC, TLOC2, STATUS )
            CALL DAT_PAREN( TLOC2,  TLOC1, STATUS )
            CALL DAT_ANNUL( TLOC2, STATUS )
         ELSE
            CALL DAT_PAREN( ARYLOC, TLOC1, STATUS )
         END IF

*     Check for presence of a component named 'WIDTH'.
         CALL DAT_THERE( TLOC1, 'WIDTH', EXIST, STATUS )

*     Annul the locators.
         CALL DAT_ANNUL( TLOC1,  STATUS )
         CALL DAT_ANNUL( ARYLOC, STATUS )
         CALL DAT_ANNUL( DELLOC, STATUS )

*  Else (axis is one-dimensional).
      ELSE

*     Enquire state of the NDF width component.
         CALL NDF_ASTAT( DSA__REFID1(SLOT), 'WIDTH', AXIS,
     :      EXIST, STATUS )

      END IF

*  Translate status and end error context.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_FLUSH( STATUS )
         STATUS = 1
      ELSE
         STATUS = 0
      END IF
      CALL ERR_RLSE

*  Return.
      END
