      SUBROUTINE DSA_DELETE_IMAGINARY( DSAREF, STATUS )
*+
*  Name:
*     DSA_DELETE_IMAGINARY

*  Purpose:
*     Convert complex data into real data.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA_DELETE_IMAGINARY( DSAREF, STATUS )

*  Description:
*     This routine converts a complex main data array into a real one.
*     This routine does not return error status if the main data array
*     were real in the first place.

*  Arguments:
*     DSAREF = CHARACTER * ( * ) (Given)
*        The reference name associated with the NDF.
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
*        Now uses DSA__DELETE_IMAGINARY to do most of the work, and so
*        can handle different data formats.
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

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER SLOT               ! The reference slot
      CHARACTER * ( NDF__SZTYP ) NDFTYP ! The non-full data type

*.

*  Check inherited global status.
      IF ( STATUS .NE. 0 ) RETURN

*  Begin error context and translate status.
      CALL ERR_MARK
      STATUS = SAI__OK

*  Look up reference slot, find numeric type, make it the full type.
*  Do the type conversion for variance as well, in case it exists.
*  (If the data are indeed complex, then their full type will be
*  something like 'COMPLEX_REAL', but their numeric type will be
*  something like '_REAL'. Enquiring the type gets us the latter,
*  setting the full type to become a simple numeric type implies
*  deletion of the imaginary part.)
      CALL DSA1_RFND( DSAREF, SLOT, STATUS )
      CALL NDF_TYPE(  DSA__REFID1(SLOT), 'DATA', NDFTYP, STATUS )
      CALL NDF_STYPE( NDFTYP, DSA__REFID1(SLOT), 'DATA', STATUS )
      CALL NDF_TYPE(  DSA__REFID1(SLOT), 'VARIANCE', NDFTYP, STATUS )
      CALL NDF_STYPE( NDFTYP, DSA__REFID1(SLOT), 'VARIANCE', STATUS )

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
