      SUBROUTINE DSA_SEEK_IMAGINARY( DSAREF, EXIST, STATUS )
*+
*  Name:
*     DSA_SEEK_IMAGINARY

*  Purpose:
*     Determine whether or not a data component is complex.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA_SEEK_IMAGINARY( DSAREF, EXIST, STATUS )

*  Description:
*     This routine looks to see if an NDF contains a data array that is
*     complex, i.e. has an imaginary part as well as a real part.

*  Arguments:
*     DSAREF = CHARACTER * ( * ) (Given)
*        The reference name associated with the NDF.
*     EXIST = LOGICAL (Returned)
*        Whether the data are complex.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     02 Feb 1989 (ks):
*        Original version.
*     01 May 1990 (ks):
*        Modified to support additional data formats.
*     21 Aug 1992 (ks):
*        Automatic portability modifications ("INCLUDE" syntax etc)
*        made.
*     29 Aug 1992 (ks):
*        "INCLUDE" filenames now upper case.
*     05 Mar 1996 (hme):
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

*  Look up reference slot and enquire complex value flag.
      CALL DSA1_RFND( DSAREF, SLOT, STATUS )
      CALL NDF_CMPLX( DSA__REFID1(SLOT), 'DATA', EXIST, STATUS )

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
