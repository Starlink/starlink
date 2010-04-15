      SUBROUTINE DSA_SEEK_QUALITY( DSAREF, EXIST, STATUS )
*+
*  Name:
*     DSA_SEEK_QUALITY

*  Purpose:
*     Determine whether or not an NDF has a quality component.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA_SEEK_QUALITY( DSAREF, EXIST, STATUS )

*  Description:
*     This routine looks to see if the NDF related to the given
*     DSA reference name has a quality array. The idea is that an
*     application can have a look before accessing the quality.
*
*     This routine reports only on the existence of the quality
*     component and makes no statement as to whether it actually flags
*     any pixels as bad. Bad pixels in the data component (or variance
*     component) are ignored by this routine.

*  Arguments:
*     DSAREF = CHARACTER * ( * ) (Given)
*        The DSA reference name associated with the NDF to be searched.
*     EXIST = LOGICAL (Returned)
*        True if the quality array exists.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     13 Jul 1988 (ks):
*        Original version.
*     22 Feb 1990 (ks):
*        Now used DSA__ routines to get array name.
*     21 Aug 1992 (ks):
*        Automatic portability modifications
*        ("INCLUDE" syntax etc) made.
*     29 Aug 1992 (ks):
*        "INCLUDE" filenames now upper case.
*     17 Feb 1995 (ks):
*        Now sets and checks QF_HANDLING.
*     26 Nov 1995 (hme):
*        FDA library.
*     19 Feb 1996 (hme):
*        Translate between application-side status and Starlink status.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants

*  Global Variables:
      INCLUDE 'DSA_COMMON'       ! DSA global variables

*  Arguments Given:
      CHARACTER * ( * ) DSAREF

*  Arguments Returned:
      LOGICAL EXIST

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER SLOT               ! The reference slot

*.

*  Check inherited global status.
      IF ( STATUS .NE. 0 ) RETURN

*  Begin error context and translate status.
      CALL ERR_MARK
      STATUS = SAI__OK

      CALL DSA1_RFND( DSAREF, SLOT, STATUS )
      CALL NDF_STATE( DSA__REFID1(SLOT), 'QUALITY', EXIST, STATUS )

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
