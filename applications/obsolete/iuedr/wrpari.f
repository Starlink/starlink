      SUBROUTINE WRPARI( NAME, MAXVAL, IVALUE, STATUS )
*+
*  Name:
*     SUBROUTINE WRPARI

*  Purpose:
*     Write INTEGER parameter values.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL WRPARI( NAME, MAXVAL, IVALUE, STATUS )

*  Arguments:
*     NAME = BYTE( 16 ) (Given)
*        Parameter name.
*     MAXVAL = INTEGER (Given)
*        Number of values to be written.
*     IVALUE = INTEGER( MAXVAL ) (Given)
*        Array of values to be written.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     DMILLS: Dave Mills (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     24-MAR-81 (JRG):
*       AT4 version.
*     29-SEP-88 (PCTR):
*       IUEDR Vn. 2.0
*       Conversion to FORTRAN.
*     01-JUN-89 (PCTR):
*       IUEDR Vn. 2.1
*       Conversion to SGP/16 style.
*     01-OCT-92 (DMILLS):
*       IUEDR Vn. 3.0
*       ADAMised version to run on multiple hardware platforms.
*     20-JAN-95 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Local Constants:
      INTEGER MAXNAME          ! Maximum parameter name length.
      PARAMETER ( MAXNAME = 16 )

*  Arguments Given:
      BYTE NAME( MAXNAME )     ! Parameter name.

      INTEGER MAXVAL           ! Maximum number of values in array.

      INTEGER IVALUE( MAXVAL ) ! List of values.

*  Status:
      INTEGER STATUS           ! Global status.

*  Local Variables:
      INTEGER NCHAR            ! Character count.

      CHARACTER*( MAXNAME ) LNAME  ! CHARACTER version of parameter name.
*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Generate CHARACTER version of parameter name.
      CALL GEN_STOC( NAME, MAXNAME, LNAME, NCHAR )

*   Write parameter value(s).
      IF ( MAXVAL .GT. 1 ) THEN
         CALL PAR_DEF1I( LNAME, MAXVAL, IVALUE, STATUS )
         CALL PAR_PUT1I( LNAME, MAXVAL, IVALUE, STATUS )

      ELSE
         CALL PAR_DEF0I( LNAME, IVALUE, STATUS )
         CALL PAR_PUT0I( LNAME, IVALUE, STATUS )
      END IF

      END
