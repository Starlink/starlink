      SUBROUTINE COF_DATEH( DATE, STATUS )
*+
*  Name:
*     COF_DATEH

*  Purpose:
*     Converts the KAPPA-style date into a form used by NDF history.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COF_DATEH( DATE, STATUS )

*  Description:
*     This makes a few minor modifications to the date string obtained
*     from a KAPPA-style date format into a form used by NDF history
*     records.  Specifically two spaces around the month are
*     replaced by hyphens, and the second and third letters of the month
*     are made uppercase.

*  Arguments:
*     DATE = CHARACTER * ( * ) (Given and Returned)
*        On input the NDF format for a date and time, namely
*        YYYY Mmm DD HH:MM:SS.SSS.  On exit, the ISO-order format for a
*        date and time, namely YYYY-MMM-DD HH:MM:SS.SSS
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  [optional_subroutine_items]...
*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1997 March 4 (MJC):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF_ constants

*  Arguments Given and Returned:
      CHARACTER * ( * ) DATE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( NDF__SZHDT ) DUMMY ! Work variable for building the
                                 ! output string
      CHARACTER * ( 2 ) ONTH     ! Work variable for converting the
                                 ! month's second and third characters
                                 ! to lowercase

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Converted the month to uppercase.
      ONTH = DATE( 7:8 )
      CALL CHR_UCASE( ONTH )

*  Form the output string and copy back into the supplied argument.
      DUMMY = DATE( 1:4 )//'-'//DATE( 6:6 )//ONTH//'-'//DATE( 10: )
      DATE = DUMMY

      END
