      SUBROUTINE
     : CHI_CLOSE( STATUS )
*+
*  Name:
*     CHI_CLOSE

*  Purpose:
*     CLOSE the CHI system.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHI_CLOSE( STATUS )
*
*  Description:
*     Closes the CHI system and performs house keeping task to release
*     resources. CHI_OPEN should be the first CHI call in your
*     application and CHI_CLOSE the last.

*  Arguments:
*     STATUS = INTEGER * ( CHI__SZNAME ) (Given and Returned)
*        Global status.

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
      INTEGER CD                 ! Input catalogue descriptor
      CHARACTER * ( 3 ) DBNAME ! Database name
      CHARACTER * ( CHI__SZNAME ) CATNAME ! Catalogue name.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
*
        do icount = 1, chi__mxass
*
*    Call CLOSECAT on the appropriate low level system
*
          catname = opcatnames(icount)
          dbname = opcatdbtyp(icount)(1:3)
          if (catname(1:4) .ne. '9999') then
            if (dbname .eq. 'HDS') then
              call chi_hclocat( opcatorigcd(icount), status)
*            elseif (dbname .eq. 'ASC') then
*              call chi_sclocat( opcatorigcd(icount), status)
*            elseif (dbname .eq. 'FIT') then
*              call chi_fclocat( opcatorigcd(icount), status)
*            elseif (dbname .eq. 'CDF') then
*              call chi_cclocat( opcatorigcd(icount), status)
*            elseif (dbname .eq. 'BIN') then
*              call chi_bclocat( cd, status)
*            elseif (dbname .eq. 'CDF') then
*              call chi_cclocat( cd, status)
*            elseif (dbname .eq. 'FIT') then
*              call chi_fclocat( cd, status)
*            elseif (dbname .eq. 'REX') then
*              call chi_rclocat( cd, status)
            endif
          endif
*
        enddo
        numopencats = 0
*
      END
