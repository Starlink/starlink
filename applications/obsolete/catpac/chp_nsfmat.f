      SUBROUTINE
     : CHP_NSFMAT( NSFORMAT, NSFLAG, CHIFORMAT, STATUS)
*+
*  Name:
*     CHP_NSFMAT

*  Purpose:
*     Non Standard ForMAT test

*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL CHP_NSFMAT( NSFORMAT, NSFLAG, CHIFORMAT, STATUS)
*
*  Description:
*     Test a format against the allowed Non standard formats.

*  Arguments:
*     NSFORMAT = CHARACTER * ( CHP__SZCFMT ) (Given)
*        Non standard formats to be tested.
*     NSFLAG = LOGICAL (Returned)
*        Results flag.
*     CHIFORMAT = CHARACTER * ( CHP__SZCFMT ) (Returned)
*        Standard formats for underlying CHI.
*     STATUS = INTEGER (Given and Returned)
*        Global status.
*
*  Notes:

*  Anticipated Errors:

*  Authors:
*     ARW: Alan R Wood (FIIS/RAL)
*     {enter_new_authors_here}

*  History:
*     1_OCT_1993 (ARW):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'   ! Standard SAE constants
      INCLUDE 'CHP_PAR'   ! Standard CHP constants
      INCLUDE 'CHI_PAR'   ! Standard CHI constants

*  Global Variables:
      INCLUDE 'CHP_CMN'   ! Standard CHP common area.

*  Arguments Given:
      CHARACTER * ( * ) NSFORMAT

*  Arguments Returned:
      LOGICAL NSFLAG
      CHARACTER * ( * ) CHIFORMAT

*  Status:
      INTEGER STATUS             ! Global status

*  External references:
      LOGICAL CHR_SIMLR

*  Local Variables:

*.
*  Check inherited global status.
*
      IF ( STATUS .NE. SAI__OK ) RETURN
*
        if (chr_simlr(nsformat, 'HH MM SS.S') .or.
     :   chr_simlr(nsformat, 'HH:MM:SS.S') .or.
     :   chr_simlr(nsformat, 'HH MM SS') .or.
     :   chr_simlr(nsformat, 'HH:MM:SS') .or.
     :   chr_simlr(nsformat, 'HH MM') .or.
     :   chr_simlr(nsformat, 'HH:MM') .or.
     :   chr_simlr(nsformat, 'DEGREE') .or.
     :   chr_simlr(nsformat, 'SDD MM SS.S') .or.
     :   chr_simlr(nsformat, 'SDD:MM:SS.S') .or.
     :   chr_simlr(nsformat, 'SDD MM SS') .or.
     :   chr_simlr(nsformat, 'SDD:MM:SS') .or.
     :   chr_simlr(nsformat, 'SDD MM') .or.
     :   chr_simlr(nsformat, 'SDD:MM')) then
          nsflag = .TRUE.
          chiformat = 'F10.6'
        else
          nsflag = .FALSE.
          chiformat = nsformat
        endif
      end
