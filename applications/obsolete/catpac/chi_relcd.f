      SUBROUTINE
     : CHI_RELCD( DBNAME, INPUT, STATUS )
*+
*  Name:
*     CHI_RELCD

*  Purpose:
*     Release the catalogue descriptor for a catalogue.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHI_RELCD( DBNAME, INPUT, STATUS )
*
*  Description:
*     Not part of the CHI interface definition. Use CHI_RELCD in conjunction
*     with CHI_GETCD. If the catalogue is open close it and release the
*     catalogue descriptor.

*  Arguments:
*     DBNAME = CHARACTER * ( 3 ) (Given)
*        Database name.
*     INPUT = CHARACTER * ( CHI__SZNAME ) (Given)
*        Name of the catalogue as known in the ADC system.
*     STATUS = INTEGER
*        Global status.

*  Anticipated Errors:
*     CHI__CATNOTFND

*  Authors:
*     ARW: Alan R Wood (STARLINK)
*     {enter_new_authors_here}

*  History:
*     26-Aug-1992 (ARW):
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

*  Arguments Given:
      CHARACTER * ( * ) DBNAME
      CHARACTER * ( * ) INPUT

*  Arguments Returned:

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER ICOUNT
      LOGICAL OPENFLG
      LOGICAL NOTFOUND

*.

*  Check inherited global status.
*
      IF ( STATUS .NE. SAI__OK ) RETURN
*
        icount = 1
        openflg = .false.
        do while (icount .le. chi__mxass .and. .not. openflg)
          if (opcatnames(icount) .eq. input .and.
     :        opcatdbtyp(icount) .eq. dbname) then
*              if (dbname .eq. 'BIN') then
*                call chi_bclocat(opcds(icount), status)
              if (dbname .eq. 'HDS') then
                call chi_hclocat(opcatorigcd(icount), status)
              endif
              opcatnames(icount) = 'DUMMY'
              opcatorigcd(icount) = 8888
              opcatdbtyp(icount) = 'DUM'
          endif
          icount = icount + 1
        enddo
*
      END
