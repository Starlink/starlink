      SUBROUTINE WRPARC( NAME, MAXVAL, SVALUE, STATUS )
*+
*  Name:
*     SUBROUTINE WRPARC

*  Purpose:
*     Write string parameter values.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL WRPARC( NAME, MAXVAL, SVALUE, STATUS )

*  Arguments:
*     NAME = BYTE( 16 ) (Given)
*        Parameter name.
*     MAXVAL = INTEGER (Given)
*        Maximum length of value string in characters.
*     SVALUE = BYTE( MAXVAL ) (Given)
*        The parameter value.
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
*     23-SEP-88 (PCTR):
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

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Local Constants:
      INTEGER MAXNAME         ! Maximum parameter name length.
      PARAMETER ( MAXNAME = 16 )

*  Arguments Given:
      BYTE NAME( MAXNAME )    ! Parameter name.

      INTEGER MAXVAL          ! Maximum number of characters in value.

      BYTE SVALUE( MAXVAL )   ! List of values (list of 1).

*  Status:
      INTEGER STATUS          ! Global status.

*  Local Variables:
      INTEGER NCHAR           ! Character count.

      CHARACTER*( 80 ) ISTRING ! CHARACTER version of parameter value.
      CHARACTER*( MAXNAME ) LNAME  ! CHARACTER version of parameter name.
*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Generate CHARACTER versions of parameter name and value.
      CALL GEN_STOC( NAME, MAXNAME, LNAME, NCHAR )

      CALL GEN_STOC( SVALUE, MAXVAL, ISTRING, NCHAR )

*   Write parameter value.
      CALL PAR_DEF0C( LNAME, ISTRING( :NCHAR ), STATUS )
      CALL PAR_PUT0C( LNAME, ISTRING( :NCHAR ), STATUS )

      END
