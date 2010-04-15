      SUBROUTINE IRM_GTCUN( PARAM, DEFAUL, UNITS, STATUS )
*+
*  Name:
*     IRM_GTCUN

*  Purpose:
*     Get a CRDD units system from the environment.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRM_GTCUN( PARAM, DEFAUL, UNITS, STATUS )

*  Description:
*     This routine returns a system of units in mixed case (unlike
*     PAR_CHOIC which always returns strings in upper case).

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The parameter name.
*     DEFAUL = CHARACTER * ( * ) (Given)
*        The suggested default value.
*     UNITS = CHARACTER * ( * ) (Returned)
*        The units string obtained from the environment.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     11-NOV-1992 (DSB):
*        Original version.
*     2-AUG-1993 (DSB):
*        Name changed from TRACC0 to IRM_GTCUN.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'I90_DAT'          ! IRAS90 data.
      INCLUDE 'IRC_PAR'          ! IRC_ constants.

*  Arguments Given:
      CHARACTER PARAM*(*)
      CHARACTER DEFAUL*(*)

*  Arguments Returned:
      CHARACTER UNITS*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string.

*  Local Variables:
      INTEGER END                ! Position of last required character
                                 ! in UNTLIS.
      INTEGER START              ! Position of first required character
                                 ! in UNTLIS.
      INTEGER ULEN               ! Used length of UNITS.

      CHARACTER UCLIST*(IRC__SZULS)! Upper case version of UNTLIS.
      CHARACTER UNTLIS*(IRC__SZULS)! List of valid units.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the list of legal CRDD units.
      CALL IRC_IUNIT( UNTLIS, STATUS )

*  Get a value from the enironment. The returned string is in upper
*  case.
      CALL PAR_CHOIC( 'UNITS', DEFAUL, UNTLIS, .FALSE., UNITS,
     :                 STATUS )

*  Find the corresponding  string in the original case.
      UCLIST = UNTLIS
      CALL CHR_UCASE( UCLIST )

      ULEN = CHR_LEN( UNITS )
      START = INDEX( UCLIST, UNITS( : ULEN ) )
      END = START + ULEN - 1

      UNITS = UNTLIS( START : END )

      END
