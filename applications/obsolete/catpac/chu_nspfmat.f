      SUBROUTINE
     : CHU_NSPFMAT( NSFORMAT, NSFLAG, PRIFORMAT, STATUS)
*+
*  Name:
*     CHU_NSPFMAT

*  Purpose:
*     Non Standard ForMAT test

*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL CHU_NSPFMAT( NSFORMAT, NSFLAG, PRIFORMAT, STATUS)
*
*  Description:
*     Test a format against the allowed Non standard formats.

*  Arguments:
*     NSFORMAT = CHARACTER * ( CHP__SZCFMT ) (Given)
*        Non standard formats to be tested.
*     NSFLAG = LOGICAL (Returned)
*        Results flag.
*     PRIFORMAT = CHARACTER * ( CHP__SZCFMT ) (Returned)
*        Standard formats for printing
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
      CHARACTER * ( * ) PRIFORMAT

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
        if (chr_simlr(nsformat(:10), 'HH MM SS.S') .or.
     :   chr_simlr(nsformat(:10), 'HH:MM:SS.S')) then
          nsflag = .TRUE.
          priformat = 'A10'
        elseif (chr_simlr(nsformat(:8), 'HH MM SS') .or.
     :   chr_simlr(nsformat(:8), 'HH:MM:SS')) then
          nsflag = .TRUE.
          priformat = 'A8'
        elseif (chr_simlr(nsformat(:5), 'HH MM') .or.
     :   chr_simlr(nsformat(:5), 'HH:MM')) then
          nsflag = .TRUE.
          priformat = 'A5'
        elseif (chr_simlr(nsformat(:5), 'DEGREE')) then
          nsflag = .TRUE.
          priformat = 'A6'
        elseif (chr_simlr(nsformat(:11), 'SDD MM SS.S') .or.
     :   chr_simlr(nsformat(:11), 'SDD:MM:SS.S')) then
          nsflag = .TRUE.
          priformat = 'A11'
        elseif (chr_simlr(nsformat(:9), 'SDD MM SS') .or.
     :   chr_simlr(nsformat(:9), 'SDD:MM:SS')) then
          nsflag = .TRUE.
          priformat = 'A9'
        elseif (chr_simlr(nsformat(:6), 'SDD MM') .or.
     :   chr_simlr(nsformat(:6), 'SDD:MM')) then
          nsflag = .TRUE.
          priformat = 'A6'
        else
          priformat = nsformat
          nsflag = .FALSE.
        endif
      end
