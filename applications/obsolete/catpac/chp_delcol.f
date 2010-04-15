      SUBROUTINE
     : CHP_DELCOL( INPUT, NUMCOLS, NAMES, STATUS )
*+
*  Name:
*     CHP_DELCOL

*  Purpose:
*     DELete given COLumns from a catalogue.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHP_DELCOL( INPUT, NUMCOLS, NAMES, STATUS )
*
*  Description:
*     Delete given columns from a catalogue. If the column does not exist in
*     the catalogue no error is reported.

*  Arguments:
*     INPUT = CHARACTER * ( CHP__SZNAME ) (Given)
*        Catalogue name.
*     NUMCOLS = INTEGER (Given)
*        Number of columns to be deleted.
*     NAMES( CHP__NUMCOLS ) = CHARACTER * ( CHP__SZCNAME ) (Given)
*        Name of the column to be deleted.
*     STATUS = INTEGER (Given and Returned)
*        Global status.

*  Notes:
*     The catalogue is RESET to its first entry on exit from this routine.
*
*     If the column or columns can not be deleted an insufficient privilege to
*     update error will be reported.
*
*  Anticipated Errors:
*     CHP__CATNOTFND
*     CHP__INSPRIVUP

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
      INCLUDE 'SAE_PAR'   ! Standard SAE constants
      INCLUDE 'CHP_PAR'   ! Standard CHP constants
      INCLUDE 'CHP_ERR'   ! Standard CHP errors

*  Global Variables:
      INCLUDE 'CHP_CMN'   ! Standard CHP variables

*  Arguments Given:
      CHARACTER * ( * ) INPUT
      INTEGER NUMCOLS
      CHARACTER * ( CHP__SZCNAME ) NAMES( * )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER CA
      INTEGER CB
      INTEGER CC
      INTEGER CD1
      INTEGER NVCCOUNT
      LOGICAL FND

*.

*  Check inherited global status.
*
      IF ( STATUS .NE. SAI__OK) RETURN
*
*  Loop through all the columns in the catalogue copying the columns that are
*  not in the delete list.
*
      call chp_getcd(input, .TRUE., cd1, status)

      ca = 1
      do cc = 1, CPnumcols(cd1)
        fnd = .FALSE.
        do cb = 1, numcols
          if (EPname(cd1,cc) .EQ. names(cb)) then
            fnd = .TRUE.
          endif
        enddo
        if ( .NOT. fnd) then
          if (ca .EQ. cc) then
            continue
          else
            EPname(cd1,ca) = EPname(cd1,cc)
            EPformat(cd1,ca) = EPformat(cd1,cc)
            EPtype(cd1,ca) = EPtype(cd1,cc)
            EPunits(cd1,ca) = EPunits(cd1,cc)
            EPcomment(cd1,ca) = EPcomment(cd1,cc)
            EPmdataacc(cd1,ca) =  EPmdataacc(cd1,cc)
            EPdataacc(cd1,ca) = EPdataacc(cd1,cc)
            EPdelind(cd1,ca) = EPdelind(cd1,cc)
            EPprefdis(cd1,ca) = EPprefdis(cd1,cc)
            EPcoldes(cd1,ca) = EPcoldes(cd1,cc)
            EPassert(cd1,ca) = EPassert(cd1,cc)
            EPdomchk(cd1,ca) = EPdomchk(cd1,cc)
            EParrshp(cd1,ca) = EParrshp(cd1,cc)
            EPdatelm(cd1,ca) = EPdatelm(cd1,cc)
            EPnsflag(cd1,ca) = EPnsflag(cd1,cc)
            EPnsformat(cd1,ca) = EPnsformat(cd1,cc)
            EPhidden(cd1,ca) = EPhidden(cd1,cc)
            EPorig(cd1,ca) = EPorig(cd1,cc)
            EParrdim(cd1,ca,1) = EParrdim(cd1,cc,1)
            EParrdim(cd1,ca,2) = EParrdim(cd1,cc,2)
            EParrdim(cd1,ca,3) = EParrdim(cd1,cc,3)
            EParrdim(cd1,ca,4) = EParrdim(cd1,cc,4)
            EParrdim(cd1,ca,5) = EParrdim(cd1,cc,5)
            EParrdim(cd1,ca,6) = EParrdim(cd1,cc,6)
            EParrdim(cd1,ca,7) = EParrdim(cd1,cc,7)
            EPassexp(cd1,ca) = EPassexp(cd1,cc)
            EPvcexp(cd1,ca) = EPvcexp(cd1,cc)
            EPvcflag(cd1,ca) = EPvcflag(cd1,cc)
            EPnvcdep(cd1,ca) = EPnvcdep(cd1,cc)
            do nvccount = 1, EPnvcdep(cd1,ca)
              EPvcdep(cd1,ca,nvccount) = EPvcdep(cd1,cc,nvccount)
            enddo
          endif
          ca = ca + 1
        endif
      enddo
      CPnumcols(cd1) = ca - 1
*
         call chi_dcols(input, numcols, names, status)
*
      END
