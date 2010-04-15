      SUBROUTINE DSA_GET_ACTUAL_NAME( DSAREF, NAME, STATUS )
*+
*  Name:
*     DSA_GET_ACTUAL_NAME

*  Purpose:
*     Return the actual name of an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA_GET_ACTUAL_NAME( DSAREF, NAME, STATUS )

*  Description:
*     The actual name contains the disk, directory, filename,
*     extension, version number and if a structure other than that at
*     the top of the file is meant, the structure information as well.

*  Arguments:
*     DSAREF = CHARACTER * ( * ) (Given)
*        The reference name associated with the NDF.
*     NAME = CHARACTER * ( * ) (Returned)
*        The name of the NDF.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     16 Jun 1987 (ks):
*        Original version.
*     21 Aug 1992 (ks):
*        Automatic portability modifications ("INCLUDE" syntax etc)
*        made.
*     29 Aug 1992 (ks):
*        "INCLUDE" filenames now upper case.
*     02 Feb 1996 (hme):
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
      CHARACTER * ( * ) NAME

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER SLOT               ! The reference slot
      INTEGER IGNORE             ! Ignored

*.

*  Check inherited global status.
      IF ( STATUS .NE. 0 ) RETURN

*  Begin error context and translate status.
      CALL ERR_MARK
      STATUS = SAI__OK

*  Look up reference slot.
      CALL DSA1_RFND( DSAREF, SLOT, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  Put NDF name into message token, then load message using that token
*  into returned variable.
      CALL NDF_MSG(  'DSA_NDFNAM', DSA__REFID1(SLOT) )
      CALL MSG_LOAD( 'DSA_NDFNAM', '^DSA_NDFNAM', NAME, IGNORE, STATUS )

*  Tidy up.
 500  CONTINUE

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
