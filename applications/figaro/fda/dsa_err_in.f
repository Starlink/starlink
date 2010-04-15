      SUBROUTINE DSA_ERROR_INFORMATION( DSAREF,
     :   ERRORS, VARIANCE, NONE, STATUS )
*+
*  Name:
*     DSA_ERROR_INFORMATION

*  Purpose:
*     Determine what form of error information is available.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA_ERROR_INFORMATION( DSAREF,
*        ERRORS, VARIANCE, NONE, STATUS )

*  Description:
*     This routine returns three logical variables that indicate if
*     error information for a structure is held as a) an error array,
*     giving the uncertainties in the data for each point, b) a variance
*     array, giving the variance in the data for each point, or c) not
*     held at all. Since the error array is not an option in the current
*     implementation, this boils down to looking for variance and
*     setting the returned VARIANCE and NONE accordingly.

*  Arguments:
*     DSAREF = CHARACTER * ( * ) (Given)
*        The reference name associated with the NDF.
*     ERRORS = LOGICAL (Returned)
*        Always false.
*     VARIANCE = LOGICAL (Returned)
*        Whether variance exists.
*     NONE = LOGICAL (Returned)
*        This is always the opposite of VARIANCE.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     07 Dec 1989 (ks):
*        Original version.
*     12 Mar 1990 (ks):
*        Modified to allow for data formats that have error information
*        in variance arrays.
*     21 Aug 1992 (ks):
*        Automatic portability modifications ("INCLUDE" syntax etc)
*        made.
*     29 Aug 1992 (ks):
*        "INCLUDE" filenames now upper case.
*     21 Feb 1996 (hme):
*        FDA library.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      CHARACTER * ( * ) DSAREF

*  Arguments Returned:
      LOGICAL ERRORS
      LOGICAL VARIANCE
      LOGICAL NONE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:

*.

      IF ( STATUS .NE. 0 ) RETURN

      ERRORS = .FALSE.
      CALL DSA_SEEK_VARIANCE( DSAREF, VARIANCE, STATUS )
      NONE = .NOT. VARIANCE

      END
