      SUBROUTINE
     : CHI_OPEN( STATUS )
*+
*  Name:
*     CHI_OPEN

*  Purpose:
*     OPEN the CHI system.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHI_OPEN( STATUS )
*
*  Description:
*     Opens the CHI system and performs house keeping tasks.
*     CHI_OPEN sould be the first CHI call in your application
*     and CHI_CLOSE the last.

*  Arguments:
*     STATUS = INTEGER * ( CHI__SZNAME ) (Given and Returned)
*
*  Anticipated Errors:
*     None

*  Authors:
*     ARW: Alan R Wood (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1-JUL-1993 (ARW):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'   ! Standard SAE constants
      INCLUDE 'CHI_PAR'   ! Standard CHI constants
      INCLUDE 'CHI_ERR'   ! Standard CHI errors
      INCLUDE 'CHIPAR_PAR'   ! Standard CHI parser constants
      INCLUDE 'CHIPAR1_PAR'  ! Standard CHI parser constants
      INCLUDE 'CHIPAR_ERR'   ! Standard CHI parser errors

*  Global Variables:
      INCLUDE 'CHIWRK_CMN'   ! CHI comon area

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER ICOUNT

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
*
        do icount = 1, chi__mxass
          opcatnames(icount) = '9999'
          numopencats = 0
          database(icount) = ' '
        enddo
*
*   Clear the chp_cmn curcat
*
*        curcat = ' '
*
        call chi_hopen( status)
*        call chi_fopen( status)
*        call chi_copen( status)
*        call chi_ropen( status)
*
      END
