      SUBROUTINE
     : CHP_CREDUP( INPUT, OUTPUT, ESTNUMENTS, STATUS )
*+
*  Name:
*     CHP_CREDUP

*  Purpose:
*     CREate a DUPlicate catalogue with no entries.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHP_CREDUP( INPUT, OUTPUT, ESTNUMENTS, STATUS )
*
*  Description:
*     Create a duplicate catalogue with the same columns but no entries.
*     The duplicate catalogue is the same in all respects including parameters,
*     virtual columns, non standard formats and catalogue notes. The CHP
*     routines that write data into this catalogue will be more
*     efficient if you can provide a good estimate for the size of the
*     catalogue. (The number of entries).

*  Arguments:
*     INPUT = CHARACTER * ( CHP__SZNAME ) (Given)
*        Name of the catalogue to be duplicated.
*     OUTPUT = CHARACTER * ( CHP__SZNAME ) (Given)
*        Name of the duplicate catalogue to be created.
*     ESTNUMENTS = INTEGER (Given)
*        Estimate for the number of entries that will be put into the catalogue.
*     STATUS = INTEGER (Given and Returned)
*        Global status.

*  Notes:
*     Both the input and output catalogues are RESET to their first entries
*     on exit from this routine.
*
*     If the catalogue can not be created an insufficient privilege to create
*     error will be reported.

*  Anticipated Errors:
*     CHP__CATNOTFND
*     CHP__INSPRIVCRE

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
      CHARACTER * ( * ) OUTPUT
      INTEGER ESTNUMENTS

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER ED
      INTEGER PD
      INTEGER CD1
      INTEGER CD2
      INTEGER NDCOUNT
      INTEGER ASCOUNT
      INTEGER EXCOUNT
      INTEGER NVCCOUNT

*.

*  Check inherited global status.
*
      IF ( STATUS .NE. SAI__OK ) RETURN
*
      call chi_credup(input, estnuments, output, status)
      call chp_getcd(input, .TRUE., cd1, status)
      call chp_getncd(output, cd2, status)

      CPnumext(cd2) = CPnumext(cd1)
      CPexttype(cd2) = CPexttype(cd1)
      CPsorjexp(cd2) = CPsorjexp(cd1)
      CPcatnotes(cd2) = CPcatnotes(cd1)
      do excount = 1, CPnumext(cd1)
        CPorignames(cd2,excount) = CPorignames(cd1,excount)
      enddo
      CPnofndi(cd2) = 0

      CPnofasorts(cd2) = 0

      CPname(cd2) = output
      CPdelind(cd2) = CPdelind(cd1)
      CPnumcols(cd2) = CPnumcols(cd1)
      CPnumpars(cd2) = CPnumpars(cd1)
*
*
*
      do ed = 1, CPnumcols(cd1)
        EPorig(cd2,ed) = EPorig(cd1,ed)
        EPtype(cd2,ed) = EPtype(cd1,ed)
        EPname(cd2,ed) = EPname(cd1,ed)
        EPformat(cd2,ed) = EPformat(cd1,ed)
        EPunits(cd2,ed) = EPunits(cd1,ed)
        EPcomment(cd2,ed) = EPcomment(cd1,ed)
        EPmdataacc(cd2,ed) =  EPmdataacc(cd1,ed)
        EPdataacc(cd2,ed) = EPdataacc(cd1,ed)
        EPdatelm(cd2,ed) = EPdatelm(cd1,ed)
        EPvcflag(cd2,ed) = EPvcflag(cd1,ed)
        EPvcexp(cd2,ed) = EPvcexp(cd1,ed)
        EPnvcdep(cd2,ed) = EPnvcdep(cd1,ed)
        do nvccount = 1, EPnvcdep(cd1,ed)
          EPvcdep(cd2,ed,nvccount) = EPvcdep(cd1,ed,nvccount)
        enddo
        EPdelind(cd2,ed) = EPdelind(cd1,ed)
        EPprefdis(cd2,ed) = EPprefdis(cd1,ed)
        EParrshp(cd2,ed) = EParrshp(cd1,ed)
        do nvccount = 1, EParrshp(cd1,ed)
          EParrdim(cd2,ed,nvccount) = EParrdim(cd1,ed,nvccount)
        enddo
        EPassert(cd2,ed) = EPassert(cd1,ed)
        EPassexp(cd2,ed) = EPassexp(cd1,ed)
        EPdomchk(cd2,ed) = EPdomchk(cd1,ed)
        EPcoldes(cd2,ed) = EPcoldes(cd1,ed)
        EPnsflag(cd2,ed) = EPnsflag(cd1,ed)
        EPnsformat(cd2,ed) = EPnsformat(cd1,ed)
        EPhidden(cd2,ed) = EPhidden(cd1,ed)
      enddo


      do pd = 1, CPnumpars(cd1)
        PPorig(cd2,pd) = PPorig(cd1,pd)
        PPtype(cd2,pd) = PPtype(cd1,pd)
        PPname(cd2,pd) = PPname(cd1,pd)
        PPunit(cd2,pd) = PPunit(cd1,pd)
        PPformat(cd2,pd) = PPformat(cd1,pd)
        PPcomment(cd2,pd) = PPcomment(cd1,pd)
        PPcharval(cd2,pd) = PPcharval(cd1,pd)
        PPptrval(cd2,pd) = PPptrval(cd1,pd)
        PPmdataacc(cd2,ed) =  PPmdataacc(cd1,ed)
        PPdataacc(cd2,ed) = PPdataacc(cd1,ed)
        PPdatelm(cd2,pd) = PPdatelm(cd1,pd)
        PPdelind(cd2,pd) = PPdelind(cd1,pd)
        PPprefdis(cd2,pd) = PPprefdis(cd1,pd)
        PParrshp(cd2,pd) = PParrshp(cd1,pd)
        do nvccount = 1, PParrshp(cd1,pd)
          PParrdim(cd2,pd,nvccount) = PParrdim(cd1,pd,nvccount)
        enddo
        PPpardes(cd2,pd) = PPpardes(cd1,pd)
        PPnsflag(cd2,pd) = PPnsflag(cd1,pd)
        PPnsformat(cd2,pd) = PPnsformat(cd1,pd)
*
      enddo
*
*
      END
