      SUBROUTINE
     : CHI_GETCD( DBNAME, INPUT, ACCMODE, CD, STATUS )
*+
*  Name:
*     CHI_GETCD

*  Purpose:
*     Get the catalogue descriptor for a catalogue.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHI_GETCD( DBNAME, INPUT, ACCMODE, CD, STATUS )
*
*  Description:
*     Not part of the CHI interface definition. Only use CHI_GETCD if you
*     are really sure about what is going on. CHI_GETCD allows you direct
*     access to the underlying database. The database must be of a type that
*     allows you to open a catalogue, and identify it with a catalogue
*     descriptor (CD), manipulate it using the descriptor and close it. ADC and
*     binary are this type of database. CHI_GETCD gets you the CD. If the
*     catalogue was not already open it is opened for you.

*  Arguments:
*     DBNAME = CHARACTER * ( 3 ) (Given)
*        Database name.
*     INPUT = CHARACTER * ( CHI__SZNAME ) (Given)
*        Name of the catalogue as known in the ADC system.
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
      LOGICAL ACCMODE

*  Arguments Returned:
      INTEGER CD

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER ICOUNT
      LOGICAL OPENFLG
      LOGICAL NOTFOUND
      CHARACTER*(16) ACCESS
      CHARACTER * (132) LIB
      CHARACTER * (132) PATH
      CHARACTER * (12) DIRNAM
      PARAMETER (DIRNAM = 'CATPAC_CATS')
      INTEGER NC
      INTEGER CHR_LEN

*.

*  Check inherited global status.
*

      IF ( STATUS .NE. SAI__OK ) RETURN
*
        if (accmode) then
          access = 'WRITE'
        else
          access = 'READ'
        endif
        icount = 1
        openflg = .false.
        do while (icount .le. chi__mxass .and. .not. openflg)
          if (opcatnames(icount) .eq. input .and.
     :        opcatdbtyp(icount) .eq. dbname) then
            if (catsort(icount) .eq. .FALSE.  .and.
     :         access .eq. opacmode(icount)) then
              openflg = .true.
              cd = opcatorigcd(icount)
            else
              cd = opcatorigcd(icount)
              if (dbname .eq. 'HDS') then
                call chi_hclocat( cd, status)
*              elseif (dbname .eq. 'BIN') then
*                call chi_bclocat( cd, status)
*              elseif (dbname .eq. 'CDF') then
*                call chi_cclocat( cd, status)
*              elseif (dbname .eq. 'FIT') then
*                call chi_fclocat( cd, status)
              endif
              opcatnames(icount) = '8888'
              opcatdbtyp(icount) = 'DUM'
              opcatorigcd(icount) = 0
              opacmode(icount) = ' '
            endif
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
          do while (icount .le. chi__mxass .and. notfound)
            icount = icount + 1
            if (opcatnames(icount) .eq. '9999') then
              cd = icount
              notfound = .false.
              opcatnames(icount) = input
              opacmode(icount) = access
              numopencats = numopencats + 1
              if (dbname .eq. 'BIN') then
*                if (accmode) then
*                  call chi_bopencat(input, 'WRITE', cd, status)
*                else
*                  call chi_bopencat(input, 'READ', cd, status)
*                endif
*                if (status .eq. SAI__OK) then
*                  opcds(icount) = cd
*                  database(icount) = 'BIN'
*                  catsort(icount) = .FALSE.
*                endif
              elseif (dbname .eq. 'HDS') then
                call chi_hopcat(input, accmode, cd, status)
                if (status .eq. SAI__OK) then
                  opcatorigcd(icount) = cd
                  opcatdbtyp(icount) = 'HDS'
                  catsort(icount) = .FALSE.
                endif
                if (status .ne.SAI__OK) then
                  call err_annul( status )
                  call psx_getenv( dirnam, path, status)
                  nc = chr_len(path)
                  lib = path( :nc)//input
                  call chi_hopcat(lib, accmode, cd, status)
                  if (status .eq. SAI__OK) then
                    opcatorigcd(icount) = cd
                    opcatdbtyp(icount) = 'HDS'
                    catsort(icount) = .FALSE.
                  endif
                endif
*              elseif (dbname .eq. 'CDF') then
*                call chi_cxopcat(input, accmode, cd, status)
*                if (status .eq. SAI__OK) then
*                  opcds(icount) = cd
*                  database(icount) = 'CDF'
*                  catsort(icount) = .FALSE.
*                endif
*              elseif (dbname .eq. 'FIT') then
*                call chi_fopcat(input, accmode, cd, status)
*                if (status .eq. SAI__OK) then
*                  opcds(icount) = cd
*                  database(icount) = 'FIT'
*                  catsort(icount) = .FALSE.
*                endif
*              elseif (dbname .eq. 'REX') then
*                call chi_ropcat(input, accmode, cd, status)
*                if (status .eq. SAI__OK) then
*                  opcds(icount) = cd
*                  database(icount) = 'REX'
*                  catsort(icount) = .FALSE.
*                endif
              endif
            endif
          enddo
        endif
*
      END
