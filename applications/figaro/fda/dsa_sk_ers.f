      SUBROUTINE DSA_SEEK_ERRORS( DSAREF, EXIST, STATUS )
*+
*  Name:
*     DSA_SEEK_ERRORS

*  Purpose:
*     Determine whether or not a variance array exists.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA_SEEK_ERRORS( DSAREF, EXIST, STATUS )

*  Description:
*     This routine looks to see if an NDF contains a variance component.

*  Arguments:
*     DSAREF = CHARACTER * ( * ) (Given)
*        The reference name associated with the NDF.
*     EXIST = LOGICAL (Returned)
*        True/false if the variance component exists/does not exist.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     22 Jun 1987 (ks):
*        Original version.
*     12 Mar 1990 (ks):
*        Now uses DSA__ routines rather than assuming the original
*        Figaro data format.
*     21 Aug 1992 (ks):
*        Automatic portability modifications ("INCLUDE" syntax etc)
*        made.
*     29 Aug 1992 (ks):
*        "INCLUDE" filenames now upper case.
*     18 Dec 1995 (hme):
*        FDA library.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER * ( * ) DSAREF

*  Arguments Returned:
      LOGICAL EXIST

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. 0 ) RETURN

      CALL DSA_SEEK_VARIANCE( DSAREF, EXIST, STATUS )

      END
