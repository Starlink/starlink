      SUBROUTINE
     : CHI_GALLCD( INPUT, NUMCOLS, CNAMES, CFORMATS, CTYPES,
     : CUNITS, CCOMMENTS, CMDATAACC, CDATAACC, STATUS)
*+
*  Name:
*     CHI_GALLCD

*  Purpose:
*     Get ALL Columns Details.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHI_GALLCD( INPUT, NUMCOLS, CNAMES, CFORMATS, CTYPES,
*     CUNITS, CCOMMENTS, CMDATAACC, CDATAACC, STATUS )
*
*  Description:
*     Gets all the information about all the columns in a catalogue. Each
*     column has associated with it a name, format, type, units,
*     comment and access information.

*  Arguments:
*     INPUT = CHARACTER * ( CHI__SZNAME ) (Given)
*        Name of the catalogue from which the column information is
*        required.
*     NUMCOLS = INTEGER (Returned)
*        Number of columns in the catalogue.
*     CNAMES( CHI__NUMCOLS ) = CHARACTER * ( CHI__SZCNAME ) (Returned)
*        Names of the columns in the catalogue.
*     CFORMATS( CHI__NUMCOLS ) = CHARACTER * ( CHI__SZCFMT ) (Returned)
*        Formats of the columns in the catalogue.
*     CTYPES( CHI__NUMCOLS ) = CHARACTER * ( 1 ) (Returned)
*        Types of the columns in the catalogue.
*     CUNITS( CHI__NUMCOLS ) = CHARACTER * ( CHI__SZCUNIT ) (Returned)
*        Units of the columns in the catalogue.
*     CCOMMENTS( CHI__NUMCOLS ) = CHARACTER * ( CHI__SZCCMT ) (Returned)
*        Comments associated with the columns in the catalogue.
*     CMDATAACC( CHI__NUMCOLS ) = LOGICAL (Returned)
*        Access to metadata associated with the columns in the catalogue.
*     CDATAACC( CHI__NUMCOLS ) = LOGICAL (Returned)
*        Access to data associated with the columns in the catalogue.
*     STATUS = INTEGER (Given and Returned)
*        Global status.

*  Anticipated Errors:
*     CHI__CATNOTFND

*  Authors:
*     ARW: Alan R Wood (FIIS/RAL)
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
      INCLUDE 'SAE_PAR'    ! Standard SAE constants
      INCLUDE 'CHI_PAR'    ! Standard CHI constants
      INCLUDE 'CHI_ERR'    ! Standard CHI errors

*  Arguments Given:
      CHARACTER * ( * ) INPUT

*  Arguments Returned:
      INTEGER NUMCOLS
      CHARACTER * ( * ) CNAMES( * )
      CHARACTER * ( * ) CFORMATS( * )
      CHARACTER * ( * ) CTYPES( * )
      CHARACTER * ( * ) CUNITS( * )
      CHARACTER * ( * ) CCOMMENTS( * )
      LOGICAL CMDATAACC( * )
      LOGICAL CDATAACC( * )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( 3 ) DBNAME ! Database name
      CHARACTER * ( CHI__SZNAME ) CATNAME ! Catalogue name.
      INTEGER CD
      INTEGER COUNTER

*.

*  Check inherited global status.
*
      IF ( STATUS .NE. SAI__OK ) RETURN
*
*    Split the input name into database part and catname part.
*
      call chi_splitname(input, dbname, catname, status)
*
*    Call GALLCD on the appropriate low level system
*
      if (dbname .eq. 'HDS') then
        call chi_getcd(dbname, catname, .FALSE. ,cd, status )
        call chi_hgallcd(cd, numcols, cnames, cformats, ctypes,
     :  cunits, ccomments, cmdataacc, cdataacc, status)
*
*      elseif (dbname .eq. 'BIN') then
*        call chi_getcd(dbname, catname, .FALSE. ,cd, status )
*        call chi_bgallcd(cd, numcols, cnames, cformats, ctypes,
*     :  cunits, ccomments, status)
*
*      elseif (dbname .eq. 'CDF') then
*        call chi_getcd(dbname, catname, .FALSE. ,cd, status )
*        call chi_cgallcd(cd, numcols, cnames, cformats, ctypes,
*     :  cunits, ccomments, cmdataacc, cdataacc, status)
*
*      elseif (dbname .eq. 'FIT') then
*        call chi_getcd(dbname, catname, .FALSE. ,cd, status )
*        call chi_fgallcd(cd, numcols, cnames, cformats, ctypes,
*     :  cunits, ccomments, cmdataacc, cdataacc, status)
*
*      elseif (dbname .eq. 'REX') then
*        call chi_getcd(dbname, catname, .FALSE. ,cd, status )
*        call chi_rgallcd(cd, numcols, cnames, cformats, ctypes,
*     :  cunits, ccomments, cmdataacc, cdataacc, status)
*
      endif
*    Temp fix
       do counter = 1, numcols
         cmdataacc(counter) = .TRUE.
         cdataacc(counter) = .TRUE.
       enddo
*
*
      END
*-
