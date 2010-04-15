      SUBROUTINE
     : CHP_JOIN( INPUT1, INPUT2, OUTPUT, EXPRESS, STATUS )
*+
*  Name:
*     CHP_JOIN

*  Purpose:
*     JOIN two catalogues.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHP_JOIN( INPUT1, INPUT2, OUTPUT, EXPRESS, STATUS )
*
*  Description:
*     Create a new catalogue by joining two catalogues. The effect of the join
*     is as follows. Consider a large catalogue that contains all the columns
*     from the INPUT1 catalogue and all the columns from the INPUT2 catalogue.
*     Into this catalogue put an entry for each combination of entries in
*     catalogues INPUT1 and INPUT2. The resulting catalogue will have N*M
*     entries where N is the number of entries in the INPUT1 catalogue and
*     M the number in the INPUT2 catalogue. Now search this catalogue for
*     those entries that satisfy the given expression.
*
*     Another way of looking at join is to say. Take every entry in turn
*     from catalogue INPUT1. Match this entry with every entry in
*     catalogue INPUT2 and if the EXPRESSion in satisfied combine both entries
*     to write to a new catalogue.
*

*  Arguments:
*     INPUT1 = CHARACTER * ( CHP__SZNAME ) (Given)
*        Name of the first join input catalogue.
*     INPUT2 = CHARACTER * ( CHP__SZNAME ) (Given)
*        Name of the second join input catalogue.
*     OUTPUT = CHARACTER * ( CHP__SZNAME ) (Given)
*        Name of the new catalogue.
*     EXPESS = CHARACTER * ( CHP__SZEXP ) (Given)
*        Expression to be applied during the join.
*     STATUS = INTEGER (Given and Returned)
*        Global status.

*  Notes:
*     Both the input catalogues and the output catalogue are RESET to their
*     first entries on exit from this routine.
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
*     ARW: Alan R Wood (FIIS/RAL)
*     {enter_new_authors_here}

*  History:
*     26-JUL-1993 (ARW):
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
      CHARACTER * ( * ) INPUT1
      CHARACTER * ( * ) INPUT2
      CHARACTER * ( * ) OUTPUT
      CHARACTER * ( * ) EXPRESS

*  Status:
      INTEGER STATUS             ! Global status

*  External Variables:
      INTEGER CHR_LEN

*  Local Variables:
      INTEGER LENINPUT
      INTEGER LENCNAME
      INTEGER LENTOT
      INTEGER LENDIFF
      INTEGER COUNT
      INTEGER COUNT1
      INTEGER COUNT2
      INTEGER ED
      INTEGER PD
      INTEGER CD1
      INTEGER CD2
      INTEGER CD3
      INTEGER NDCOUNT
      INTEGER ASCOUNT
      INTEGER EDCOUNT
      INTEGER PDCOUNT
      INTEGER EXCOUNT
      INTEGER NVCCOUNT

*.

*  Check inherited global status.
*
      IF ( STATUS .NE. SAI__OK ) RETURN
*
      call chp_getcd(input1, .TRUE., cd1, status)
      call chp_getcd(input2, .TRUE., cd2, status)
      call chp_getncd(output, cd3, status)
*
      count = cd3
       do count1 = 1, CHP__NUMCOLS

          EPorig(count,count1) = 0
*
          EPtype(count, count1) = ' '
*
          call chr_fill(' ',EPname(count,count1))
          call chr_fill(' ',EPformat(count,count1))
          call chr_fill(' ',EPunits(count,count1))
          call chr_fill(' ',EPcomment(count,count1))
          EPmdataacc(count,count1) = .FALSE.
          EPdataacc(count,count1) = .FALSE.
          EPdatelm(count,count1) = 0
          EPvcflag(count,count1) = .FALSE.
          call chr_fill(' ',EPvcexp(count,count1))
          do count2 = 1, CHP__NUMCOLS
            call chr_fill(' ',EPvcdep(count,count1,count2))
          enddo
          EPnvcdep(count,count1) = 0
          EPdelind(count,count1) = .FALSE.
          EPprefdis(count,count1) = .FALSE.
          EParrshp(count,count1) = 0
          do count2 = 1, CHP__MXDIM
            EParrdim(count,count1,count2) = 0
          enddo
          EPassert(count, count1) = .FALSE.
          call chr_fill(' ',EPassexp(count,count1))
          EPdomchk(count,count1) = .FALSE.
          EPcoldes(count, count1) = 0
          EPnsflag(count, count1 ) = 0
          call chr_fill(' ',EPnsformat(count,count1))
          EPhidden(count,count1) = .FALSE.
        enddo

        do count1 = 1, CHP__NUMPARS

          PPorig(count,count1) = 0
          PPtype(count, count1) = ' '
          call chr_fill(' ',PPname(count,count1))
          call chr_fill(' ',PPunit(count,count1))
          call chr_fill(' ',PPformat(count,count1))
          call chr_fill(' ',PPcomment(count,count1))
          call chr_fill(' ',PPcharval(count,count1))
          PPptrval(count,count1) = 0
          PPmdataacc(count,count1) = .FALSE.
          PPdataacc(count,count1) = .FALSE.
          PPdatelm(count, count1) = 0
          PPdelind(count,count1) = .FALSE.
          PPprefdis(count,count1) = .FALSE.
          PParrshp(count,count1) = 0
          do count2 = 1, CHP__MXDIM
            PParrdim(count, count1, count2) = 0
          enddo
          PPpardes(count, count1) = .FALSE.
          PPnsflag(count, count1) = .FALSE.
          call chr_fill(' ',PPnsformat(count,count1))
        enddo

        CPnumext(count) = 0
        CPexttype(count) = .FALSE.
        call chr_fill(' ',CPsorjexp(count))
        call chr_fill(' ',CPcatnotes(count))
        do count1 = 1, CHP__MXJCATS
          call chr_fill(' ',CPorignames(count,count1))
        enddo
        CPnofndi(count) = 0
        do count1 = 1, CHP__NUMCOLS
          CPndorig(count,count1) = 0
          CPnddatelm(count,count1) = 0
          CPnumndcols(count,count1) = 0
          call chr_fill(' ',CP1icol(count,count1))
          call chr_fill(' ',CP2icol(count,count1))
          call chr_fill(' ',CP3icol(count,count1))
          call chr_fill(' ',CP4icol(count,count1))
          call chr_fill(' ',CP5icol(count,count1))
          call chr_fill(' ',CP6icol(count,count1))
          call chr_fill(' ',CP7icol(count,count1))
        enddo
        CPnofasorts(count) = 0
        do count1 = 1, CHP__NUMCOLS
          CPasorig(count,count1) = 0
          CPasdatelm(count,count1) = 0
          CPnumascols(count,count1) = 0
          call chr_fill(' ',CPfascol(count,count1))
          call chr_fill(' ',CPsascol(count,count1))
          call chr_fill(' ',CPtascol(count,count1))
          CPfasdir(count,count1) = 'T'
          CPsasdir(count,count1) = 'T'
          CPtasdir(count,count1) = 'T'
        enddo

        call chr_fill(' ',CPname(count))
        CPdelind(count) = .FALSE.
        CPnuments(count) = 0
        CPnumcols(count) = 0
        CPnumpars(count) = 0
*
*
      CPname(cd3) = output
      CPnuments(cd3) = 0
      CPnumext(cd3) = 0
*      do excount = 1, CPnumext(cd1)
*        CPorignames(cd2,excount) = CPorignames(cd1,excount)
*      enddo
*      CPexttype(cd2) = CPexttype(cd1)
      CPnumcols(cd3) = CPnumcols(cd1) + CPnumcols(cd2)
      CPnumpars(cd3) = CPnumpars(cd1) + CPnumpars(cd2)
      CPcatnotes(cd3) = CPcatnotes(cd1)
      CPdelind(cd3) = .TRUE.
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
      ed = 0
      pd = 0
      edcount = 0
      do while (edcount .lt. CPnumcols(cd1))
        edcount = edcount + 1
        ed = ed + 1
        EPname(cd3,ed) = input1
        lencname = chr_len(EPname(cd1,ed))
        leninput = chr_len(input1)
        if (leninput .gt. 4) then
          leninput = 4
        endif
        call chr_appnd('_',EPname(cd3,ed),leninput)
        lentot = leninput + lencname + 1
        lendiff = CHP__SZCNAME - lentot
        if ( lendiff .lt. 0 ) then
          lencname = CHP__SZCNAME - (leninput + 1)
        endif
        call chr_appnd(EPname(cd1,ed)(1:lencname),EPname(cd3,ed),
     :    leninput)
        EPformat(cd3,ed) = EPformat(cd1,ed)
        EPtype(cd3,ed) = EPtype(cd1,ed)
        EPunits(cd3,ed) = EPunits(cd1,ed)
        EPcomment(cd3,ed) = EPcomment(cd1,ed)
        EPmdataacc(cd3,ed) =  EPmdataacc(cd1,ed)
        EPdataacc(cd3,ed) = EPdataacc(cd1,ed)
        EPdelind(cd3,ed) = EPdelind(cd1,ed)
        EPprefdis(cd3,ed) = EPprefdis(cd1,ed)
        EPcoldes(cd3,ed) = EPcoldes(cd1,ed)
        EPassert(cd3,ed) = EPassert(cd1,ed)
        EPdomchk(cd3,ed) = EPdomchk(cd1,ed)
        EParrshp(cd3,ed) = EParrshp(cd1,ed)
        EPdatelm(cd3,ed) = EPdatelm(cd1,ed)
        EPnsflag(cd3,ed) = EPnsflag(cd1,ed)
        EPnsformat(cd3,ed) = EPnsformat(cd1,ed)
        EPhidden(cd3,ed) = EPhidden(cd1,ed)
        EPorig(cd3,ed) = EPorig(cd1,ed)
        EParrdim(cd3,ed,1) = EParrdim(cd1,ed,1)
        EParrdim(cd3,ed,2) = EParrdim(cd1,ed,2)
        EParrdim(cd3,ed,3) = EParrdim(cd1,ed,3)
        EParrdim(cd3,ed,4) = EParrdim(cd1,ed,4)
        EParrdim(cd3,ed,5) = EParrdim(cd1,ed,5)
        EParrdim(cd3,ed,6) = EParrdim(cd1,ed,6)
        EParrdim(cd3,ed,7) = EParrdim(cd1,ed,7)
        EPassexp(cd3,ed) = EPassexp(cd1,ed)
        EPvcexp(cd3,ed) = EPvcexp(cd1,ed)
        EPvcflag(cd3,ed) = EPvcflag(cd1,ed)
        EPnvcdep(cd3,ed) = EPnvcdep(cd1,ed)
        do nvccount = 1, EPnvcdep(cd1,ed)
          EPvcdep(cd3,ed,nvccount) = EPvcdep(cd1,ed,nvccount)
        enddo
      enddo

      pdcount = 0
      do while (pdcount .lt. CPnumpars(cd1))
        pdcount = pdcount + 1
        pd = pd + 1
        PPname(cd3,pd) = PPname(cd1,pd)
        PPunit(cd3,pd) = PPunit(cd1,pd)
        PPformat(cd3,pd) = PPformat(cd1,pd)
        PPtype(cd3,pd) = PPtype(cd1,pd)
        PPcharval(cd3,pd) = PPcharval(cd1,pd)
        PPmdataacc(cd3,pd) =  PPmdataacc(cd1,pd)
        PPdataacc(cd3,pd) = PPdataacc(cd1,pd)
        PPcomment(cd3,pd) = PPcomment(cd1,pd)
        PPptrval(cd3,pd) = PPptrval(cd1,pd)
        PPorig(cd3,pd) = PPorig(cd1,pd)
        PPdatelm(cd3,pd) = PPdatelm(cd1,pd)
        PPdelind(cd3,pd) = PPdelind(cd1,pd)
        PPprefdis(cd3,pd) = PPprefdis(cd1,pd)
        PPpardes(cd3,pd) = PPpardes(cd1,pd)
        PPnsflag(cd3,pd) = PPnsflag(cd1,pd)
        PPnsformat(cd3,pd) = PPnsformat(cd1,pd)
*
        PParrshp(cd3,pd) = PParrshp(cd2,pd)
        PParrdim(cd3,pd,1) = PParrdim(cd1,pd,1)
        PParrdim(cd3,pd,2) = PParrdim(cd1,pd,2)
        PParrdim(cd3,pd,3) = PParrdim(cd1,pd,3)
        PParrdim(cd3,pd,4) = PParrdim(cd1,pd,4)
        PParrdim(cd3,pd,5) = PParrdim(cd1,pd,5)
        PParrdim(cd3,pd,6) = PParrdim(cd1,pd,6)
        PParrdim(cd3,pd,7) = PParrdim(cd1,pd,7)
      enddo


      edcount = 0
      do while (edcount .lt. CPnumcols(cd2))
        edcount = edcount + 1
        ed = ed + 1
        EPname(cd3,ed) = input2
        lencname = chr_len(EPname(cd2,edcount))
        leninput = chr_len(input2)
        if (leninput .gt. 4) then
          leninput = 4
        endif
        call chr_appnd('_',EPname(cd3,ed),leninput)
        lentot = leninput + lencname + 1
        lendiff = CHP__SZCNAME - lentot
        if ( lendiff .lt. 0 ) then
          lencname = CHP__SZCNAME - (leninput + 1)
        endif
        call chr_appnd(EPname(cd2,edcount)(1:lencname),
     :    EPname(cd3,ed),
     :    leninput)
        EPformat(cd3,ed) = EPformat(cd2,edcount)
        EPtype(cd3,ed) = EPtype(cd2,edcount)
        EPunits(cd3,ed) = EPunits(cd2,edcount)
        EPcomment(cd3,ed) = EPcomment(cd2,edcount)
        EPmdataacc(cd3,ed) =  EPmdataacc(cd2,edcount)
        EPdataacc(cd3,ed) = EPdataacc(cd2,edcount)
        EPdelind(cd3,ed) = EPdelind(cd2,edcount)
        EPprefdis(cd3,ed) = EPprefdis(cd2,edcount)
        EPcoldes(cd3,ed) = EPcoldes(cd2,edcount)
        EPassert(cd3,ed) = EPassert(cd2,edcount)
        EPdomchk(cd3,ed) = EPdomchk(cd2,edcount)
        EParrshp(cd3,ed) = EParrshp(cd2,edcount)
        EPdatelm(cd3,ed) = EPdatelm(cd2,edcount)
        EPnsflag(cd3,ed) = EPnsflag(cd2,edcount)
        EPnsformat(cd3,ed) = EPnsformat(cd2,edcount)
        EPhidden(cd3,ed) = EPhidden(cd2,edcount)
        EPorig(cd3,ed) = EPorig(cd2,edcount)
        EParrdim(cd3,ed,1) = EParrdim(cd2,edcount,1)
        EParrdim(cd3,ed,2) = EParrdim(cd2,edcount,2)
        EParrdim(cd3,ed,3) = EParrdim(cd2,edcount,3)
        EParrdim(cd3,ed,4) = EParrdim(cd2,edcount,4)
        EParrdim(cd3,ed,5) = EParrdim(cd2,edcount,5)
        EParrdim(cd3,ed,6) = EParrdim(cd2,edcount,6)
        EParrdim(cd3,ed,7) = EParrdim(cd2,edcount,7)
        EPassexp(cd3,ed) = EPassexp(cd2,edcount)
        EPvcexp(cd3,ed) = EPvcexp(cd2,edcount)
        EPvcflag(cd3,ed) = EPvcflag(cd2,edcount)
        EPnvcdep(cd3,ed) = EPnvcdep(cd2,edcount)
        do nvccount = 1, EPnvcdep(cd2,edcount)
          EPvcdep(cd3,ed,nvccount) = EPvcdep(cd2,edcount,nvccount)
        enddo
      enddo

      pdcount = 0
      do while (pdcount .lt. CPnumpars(cd2))
        pdcount = pdcount + 1
        pd = pd + 1
        PPname(cd3,pd) = PPname(cd2,pdcount)
        PPunit(cd3,pd) = PPunit(cd2,pdcount)
        PPformat(cd3,pd) = PPformat(cd2,pdcount)
        PPtype(cd3,pd) = PPtype(cd2,pdcount)
        PPcharval(cd3,pd) = PPcharval(cd2,pdcount)
        PPmdataacc(cd3,pd) =  PPmdataacc(cd1,pdcount)
        PPdataacc(cd3,pd) = PPdataacc(cd1,pdcount)
        PPcomment(cd3,pd) = PPcomment(cd2,pdcount)
        PPptrval(cd3,pd) = PPptrval(cd2,pdcount)
        PPorig(cd3,pd) = PPorig(cd2,pdcount)
        PPdatelm(cd3,pd) = PPdatelm(cd2,pdcount)
        PPdelind(cd3,pd) = PPdelind(cd2,pdcount)
        PPprefdis(cd3,pd) = PPprefdis(cd2,pdcount)
        PPpardes(cd3,pd) = PPpardes(cd2,pdcount)
        PPnsflag(cd3,pd) = PPnsflag(cd2,pdcount)
        PPnsformat(cd3,pd) = PPnsformat(cd2,pdcount)
*
        PParrshp(cd3,pd) = PParrshp(cd2,pdcount)
        PParrdim(cd3,pd,1) = PParrdim(cd2,pdcount,1)
        PParrdim(cd3,pd,2) = PParrdim(cd2,pdcount,2)
        PParrdim(cd3,pd,3) = PParrdim(cd2,pdcount,3)
        PParrdim(cd3,pd,4) = PParrdim(cd2,pdcount,4)
        PParrdim(cd3,pd,5) = PParrdim(cd2,pdcount,5)
        PParrdim(cd3,pd,6) = PParrdim(cd2,pdcount,6)
        PParrdim(cd3,pd,7) = PParrdim(cd2,pdcount,7)
      enddo
*
      call chi_join(input1, input2, output, express, status)
*
      END
