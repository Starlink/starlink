      SUBROUTINE
     : CHP_SEARCH( INPUT, OUTPUT, CRITERIA, STATUS )
*+
*  Name:
*     CHP_SEARCH

*  Purpose:
*     SEARCH a catalogue for entries that meet a criteria.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHP_SEARCH( INPUT, OUTPUT, CRITERIA, STATUS )
*
*  Description:
*     Create a new catalogue containing only entries that meet the given
*     criteria. If an invalid expression error is reported CRITERIA
*     is returned containing diagnostic information.

*  Arguments:
*     INPUT = CHARACTER * ( CHP__SZNAME ) (Given)
*        Name of the catalogue from which the entries are to be selected.
*     OUTPUT = CHARACTER * ( CHP__SZNAME ) (Given)
*        Name of the new catalogue containing only the selected entries.
*     CRITERIA = CHARACTER * ( CHP__SZEXP ) (Given)
*        Criteria to be applied to each entry in the input catalogue to
*        determine if this entry is to be copied into the output catalogue.
*     STATUS = INTEGER (Given and Returned)
*        Global status.

*  Notes:
*
*     The input and output catalogues are RESET to their first entries on
*     exit from this routine.
*
*     If an invalid expression is given an invalid expression error will be
*     reported.
*
*     If the catalogue can not be created an insufficient privilege to create
*     error will be reported.

*  Anticipated Errors:
*     CHP__CATNOTFND
*     CHP__IVLDEXP
*     CHP__INSPRIVCRE

*  Authors:
*     ARW: Alan R Wood (STARLINK)
*     {enter_new_authors_here}

*  History:
*     26-NOV-1991 (ARW):
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
      CHARACTER * ( * ) CRITERIA

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
      call chp_getcd(input, .TRUE., cd1, status)
      call chp_getncd(output, cd2, status)

      CPname(cd2) = output
      CPnuments(cd2) = CPnuments(cd1)
      CPnumext(cd2) = CPnumext(cd1)
      do excount = 1, CPnumext(cd1)
        CPorignames(cd2,excount) = CPorignames(cd1,excount)
      enddo
      CPexttype(cd2) = CPexttype(cd1)
      CPnumcols(cd2) = CPnumcols(cd1)
      CPnumpars(cd2) = CPnumpars(cd1)
      CPcatnotes(cd2) = CPcatnotes(cd1)
      CPdelind(cd2) = CPdelind(cd1)
      CPnofndi(cd2) = 0
*      do ndcount = 1, CPnofndi(cd1)
*        CPnumndcols(cd2,ndcount) = CPnumndcols(cd1,ndcount)
*        CP1icol(cd2,ndcount) = CP1icol(cd1,ndcount)
*        CP2icol(cd2,ndcount) = CP2icol(cd1,ndcount)
*        CP3icol(cd2,ndcount) = CP3icol(cd1,ndcount)
*        CP4icol(cd2,ndcount) = CP4icol(cd1,ndcount)
*        CP5icol(cd2,ndcount) = CP5icol(cd1,ndcount)
*        CP6icol(cd2,ndcount) = CP6icol(cd1,ndcount)
*        CP7icol(cd2,ndcount) = CP7icol(cd1,ndcount)
*        CPnddatelm(cd2,ndcount) = CPnddatelm(cd1,ndcount)
*        CPndorig(cd2,ndcount) =  CPndorig(cd1,ndcount)
*      enddo

      CPnofasorts(cd2) = 0
*      do ascount = 1, CPnofasorts(cd1)
*        CPnumascols(cd2,ascount) = CPnumascols(cd1,ascount)
*        CPfascol(cd2,ascount) = CPfascol(cd1,ascount)
*        CPfasdir(cd2,ascount) = CPfasdir(cd1,ascount)
*        CPsascol(cd2,ascount) = CPsascol(cd1,ascount)
*        CPsasdir(cd2,ascount) = CPsasdir(cd1,ascount)
*        CPtascol(cd2,ascount) = CPtascol(cd1,ascount)
*        CPtasdir(cd2,ascount) = CPtasdir(cd1,ascount)
*        CPasdatelm(cd2,ascount) = CPasdatelm(cd1,ascount)
*        CPasorig(cd2,ascount) = CPasorig(cd1,ascount)
*      enddo
*
      do ed = 1, CPnumcols(cd1)
        EPname(cd2,ed) = EPname(cd1,ed)
        EPformat(cd2,ed) = EPformat(cd1,ed)
        EPtype(cd2,ed) = EPtype(cd1,ed)
        EPunits(cd2,ed) = EPunits(cd1,ed)
        EPcomment(cd2,ed) = EPcomment(cd1,ed)
        EPmdataacc(cd2,ed) =  EPmdataacc(cd1,ed)
        EPdataacc(cd2,ed) = EPdataacc(cd1,ed)
        EPdelind(cd2,ed) = EPdelind(cd1,ed)
        EPprefdis(cd2,ed) = EPprefdis(cd1,ed)
        EPcoldes(cd2,ed) = EPcoldes(cd1,ed)
        EPassert(cd2,ed) = EPassert(cd1,ed)
        EPdomchk(cd2,ed) = EPdomchk(cd1,ed)
        EParrshp(cd2,ed) = EParrshp(cd1,ed)
        EPdatelm(cd2,ed) = EPdatelm(cd1,ed)
        EPnsflag(cd2,ed) = EPnsflag(cd1,ed)
        EPnsformat(cd2,ed) = EPnsformat(cd1,ed)
        EPhidden(cd2,ed) = EPhidden(cd1,ed)
        EPorig(cd2,ed) = EPorig(cd1,ed)
        EParrdim(cd2,ed,1) = EParrdim(cd1,ed,1)
        EParrdim(cd2,ed,2) = EParrdim(cd1,ed,2)
        EParrdim(cd2,ed,3) = EParrdim(cd1,ed,3)
        EParrdim(cd2,ed,4) = EParrdim(cd1,ed,4)
        EParrdim(cd2,ed,5) = EParrdim(cd1,ed,5)
        EParrdim(cd2,ed,6) = EParrdim(cd1,ed,6)
        EParrdim(cd2,ed,7) = EParrdim(cd1,ed,7)
        EPassexp(cd2,ed) = EPassexp(cd1,ed)
        EPvcexp(cd2,ed) = EPvcexp(cd1,ed)
        EPvcflag(cd2,ed) = EPvcflag(cd1,ed)
        EPnvcdep(cd2,ed) = EPnvcdep(cd1,ed)
        do nvccount = 1, EPnvcdep(cd1,ed)
          EPvcdep(cd2,ed,nvccount) = EPvcdep(cd1,ed,nvccount)
        enddo
      enddo


      do pd = 1, CPnumpars(cd1)
        PPname(cd2,pd) = PPname(cd2,pd)
        PPunit(cd2,pd) = PPunit(cd2,pd)
        PPformat(cd2,pd) = PPformat(cd2,pd)
        PPtype(cd2,pd) = PPtype(cd2,pd)
        PPcharval(cd2,pd) = PPcharval(cd2,pd)
        PPmdataacc(cd2,ed) =  PPmdataacc(cd1,ed)
        PPdataacc(cd2,ed) = PPdataacc(cd1,ed)
        PPcomment(cd2,pd) = PPcomment(cd2,pd)
        PPptrval(cd2,pd) = PPptrval(cd2,pd)
        PPorig(cd2,pd) = PPorig(cd2,pd)
        PPdatelm(cd2,pd) = PPdatelm(cd2,pd)
        PPdelind(cd2,pd) = PPdelind(cd2,pd)
        PPprefdis(cd2,pd) = PPprefdis(cd2,pd)
        PPpardes(cd2,pd) = PPpardes(cd2,pd)
        PPnsflag(cd2,pd) = PPnsflag(cd2,pd)
        PPnsformat(cd2,pd) = PPnsformat(cd2,pd)
*
        PParrshp(cd2,pd) = PParrshp(cd2,pd)
        PParrdim(cd2,pd,1) = PParrdim(cd1,pd,1)
        PParrdim(cd2,pd,2) = PParrdim(cd1,pd,2)
        PParrdim(cd2,pd,3) = PParrdim(cd1,pd,3)
        PParrdim(cd2,pd,4) = PParrdim(cd1,pd,4)
        PParrdim(cd2,pd,5) = PParrdim(cd1,pd,5)
        PParrdim(cd2,pd,6) = PParrdim(cd1,pd,6)
        PParrdim(cd2,pd,7) = PParrdim(cd1,pd,7)
      enddo
*
      call chi_search( input, output, criteria, status)
*
      END
