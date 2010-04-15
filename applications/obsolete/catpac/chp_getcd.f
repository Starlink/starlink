      SUBROUTINE
     : CHP_GETCD( INPUT, ACCMODE, CD, STATUS )
*+
*  Name:
*     CHP_GETCD

*  Purpose:
*     Get the catalogue descriptor for a catalogue.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHP_GETCD( INPUT, ACCMODE, CD, STATUS )
*
*  Description:
*     Not part of the CHI interface definition. Only use CHI_GETCD if you
*     are really sure about what is going on.

*  Arguments:
*     INPUT = CHARACTER * ( CHI__SZNAME ) (Given)
*        Name of the catalogue.
*     ACCMODE = LOGICAL (Given)
*        Access mode TRUE for update.
*     CD = INTEGER (Returned)
*        Catalogue descriptor
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
      INCLUDE 'CHP_PAR'   ! Standard CHP constants

*  Global Variables:
      INCLUDE 'CHP_CMN'   ! CHP comon area

*  Arguments Given:
      CHARACTER * ( * ) INPUT
      LOGICAL ACCMODE

*  Arguments Returned:
      INTEGER CD

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER ICOUNT
      LOGICAL OPENFLG
      LOGICAL NOTFOUND
      CHARACTER * (132) LIB
      CHARACTER * (132) PATH
      INTEGER NC
      INTEGER CHR_LEN
      CHARACTER * (12) DIRNAM
      PARAMETER ( DIRNAM = 'CATPAC_CATS')

*.

*  Check inherited global status.
*

      IF ( STATUS .NE. SAI__OK ) RETURN
*
        icount = 1
        openflg = .false.
        do while (icount .le. chp__numcats .and. .not. openflg)
          if (opcatnames(icount) .eq. input ) then
              openflg = .true.
              cd = icount
          endif
          icount = icount + 1
        enddo
*
*   If the catalogue is not already open open it
*
        if (.not. openflg) then
*
*    Find the next free element
*
          notfound = .true.
          icount = 0
          do while (icount .le. chp__numcats .and. notfound)
            icount = icount + 1
            if (opcatnames(icount) .eq. '9999') then
              cd = icount
              notfound = .false.
              opcatnames(icount) = input
              opcatsys(icount) = .FALSE.
              call chp_loadcat(input, cd, status)
        do icount = 1, CPnumcols(cd)
        enddo
              if (status .ne. SAI__OK) then
                call err_annul(status)
                call psx_getenv(DIRNAM, PATH, STATUS)
                nc = chr_len(path)
                lib = path( :nc)
                call chp_loadscat(lib, input, cd, status)
                opcatsys(icount) = .TRUE.
              endif
            endif
          enddo
        endif
*
      END
