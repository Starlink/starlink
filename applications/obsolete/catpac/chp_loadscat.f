      SUBROUTINE
     : CHP_LOADSCAT( DIR, INPUT, CD, STATUS )
*+
*  Name:
*     CHP_LOADSCAT

*  Purpose:
*     Load the catalogue into common.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHP_LOADSCAT( DIR, INPUT, CD, STATUS )

*  Description:
*     Load information in the catalogue description files into the common area
*     The catalogue might be an join Extension catalogue, a search
*     extension catalogue or a plain extension catalogue or a regular
*     catalogue. Look through the additional information for this
*     catalogue to see if it is any type of extension. For all  3 types
*     of extension load the information from the additional information
*     from the catalogue into the common area. Then, when we know what
*     type of extension we are dealing with, we can load the information
*     about the original catalogue(s). This includes the information
*     from the underlying CHI catalogue.
*
*     If this wasn't any sort of extension just load the catalogue
*     information including that from the underlying CHI catalogue.
*
*  Arguments:
*     INPUT = CHARACTER * ( CHI__SZNAME ) (Given)
*        Name of the catalogue as known in the FF system.
*     CD = INTEGER (Given)
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
      INCLUDE 'FIO_ERR'   ! Standard FIO errors
      INCLUDE 'CHP_PAR'   ! Standard CHP constants
      INCLUDE 'CHI_PAR'   ! Standard CHI constants

*  Global Variables:
      INCLUDE 'CHP_CMN'   ! Standard CHP common area.

*  Arguments Given:
      CHARACTER * ( * ) INPUT
      CHARACTER * ( * ) DIR
      INTEGER CD

*  Arguments Returned:

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER EDCOUNT
      INTEGER PDCOUNT
      LOGICAL EDNOTFOUND
      LOGICAL PDNOTFOUND
      INTEGER CATC ! Catalogue count
      INTEGER I
      INTEGER PARTEXPNUM
      INTEGER ED         ! Element descriptor counter.
      INTEGER TED         ! Element descriptor counter.
      INTEGER PD         ! Element descriptor counter.
      INTEGER TPD         ! Element descriptor counter.
      INTEGER CDCOUNT    ! counter when looking for the next free CD
      INTEGER FDD        ! Description file FIO file descriptor
      INTEGER STARTPOS   ! Start position of the field.
      INTEGER ENDPOS     ! End of the field.
      INTEGER LENGTH     ! Length of the field.
      INTEGER CURNUMND   ! Current number of N-d indices.
      LOGICAL CDNOTFND   ! Free CD not yet found flag.
      CHARACTER * ( 16 ) CHARNUM ! Character number.
      CHARACTER * ( CHP__SZDREC ) DBUF	! Buffer for description file record
      CHARACTER * ( CHP__SZCFMT ) CHARPFMT ! Format in character form
      CHARACTER * ( CHP__SZCVAL ) CHARPNUM ! Number in character format
      CHARACTER * ( CHP__SZNAME ) NAME
      CHARACTER * ( CHP__SZCNAME ) TEMPNAME
      CHARACTER * ( 10 ) CHARNUM10
      LOGICAL EXTNOTFND
      INTEGER NUMCHICOLS
      CHARACTER * ( CHI__SZCNAME) CHINAMES( CHI__NUMCOLS )
      CHARACTER * ( CHI__SZCFMT) CHIFORMATS( CHI__NUMCOLS )
      CHARACTER * ( 1 ) CHITYPES( CHI__NUMCOLS )
      CHARACTER * ( CHI__SZCUNIT) CHIUNITS( CHI__NUMCOLS )
      CHARACTER * ( CHI__SZCCMT) CHICOMMENTS( CHI__NUMCOLS )
      LOGICAL CHIMDATAACC( CHI__NUMCOLS )
      LOGICAL CHIDATAACC( CHI__NUMCOLS )


*-
*
*.

*  Check inherited global status.
*
      IF ( STATUS .NE. SAI__OK ) RETURN
*
      call chp_opensaddf(dir, input, 'READ', 'LIST', CHP__SZDREC,
     :                   fdd, status)
*
*    Abort if failed to open file okay
*
      if (status .ne. SAI__OK) then
*        status = CHI__CATNOTFND
      endif
      CPname(cd) = input
*
*    Check through the file looking for the general catalogue
*    information. This will tell us if this is an extension
*    catalogue.
*
      extnotfnd = .TRUE.
      do while (status .eq. SAI__OK .and. extnotfnd)
         call fio_readf(fdd, Dbuf, status)
         if (status.eq.SAI__OK) then
            if (Dbuf(CHP_S_KEY:CHP_E_KEY) .eq. 'O') then
               call chr_ctoi(Dbuf(CHP_S_CATNUMEXT:CHP_E_CATNUMEXT),
     :              CPnumext(cd), status)
               extnotfnd = .FALSE.
            endif
         endif
      enddo
      if (status.eq.FIO__EOF) then
         call err_annul(status)
         CPnumext(cd) = 0
      endif
*
*   Rewind the file.
*
      call fio_rwind(fdd, status)
*
*   If this is an extension catalogue load up the information and then
*   go on to load up the information for the original catalogue.
*
        CPnumcols(cd) = 0
        CPnumpars(cd) = 0
        CPnofndi(cd) = 0
        CPnofasorts(cd) = 0
        ed = 0
        pd = 0
*
*   If this was not an extension catalogue of any type CPnumext=0 then
*   load as a normal cataloogue. This is kept separate for clarity.
*
      if (CPnumext(cd) .EQ. 0) then
*
*  Call the underlying CHI to get the details of the columns there.
*
        call chi_gallcd(input,numchicols, chinames, chiformats,
     :          chitypes, chiunits, chicomments, chimdataacc,
     :          chidataacc, status)
        CPnumcols(cd) = numchicols
        do ed = 1, numchicols
          EPname(cd,ed) = chinames(ed)
          EPformat(cd,ed) = chiformats(ed)
          EPtype(cd,ed) = chitypes(ed)
          EPunits(cd,ed) = chiunits(ed)
          EPcomment(cd,ed) = chicomments(ed)
          EPmdataacc(cd,ed) = chimdataacc(ed)
          EPdataacc(cd,ed) = chidataacc(ed)
        enddo
        ed = numchicols
*
*   Now load up any additional CHP information.
*
        do while (status .eq. SAI__OK)
          call fio_readf(fdd, Dbuf, status)
          if (status.eq.SAI__OK) then
*
*   If the key is 'A' then this is a column. The information
*   is
*
*    Column name
*    Column Delete Indicator
*    Column Preferred Display
*    Column Designation
*    Column Assert Flag.
*    Column Domain Checking Flag.
*    Column Array Shape.
*    Column Last Modified Date.
*    Virtual Column Flag.
*    Column Nulls Supported
*    Non standard format flag.
*    Non standard format.
*    Column array type
*
            if (Dbuf(CHP_S_KEY:CHP_E_KEY) .eq. 'A') then
               tempname = Dbuf(CHP_S_CNAME:CHP_E_CNAME)
               ednotfound = .TRUE.
               edcount = 1
               do while (ednotfound .and. edcount .le. CPnumcols(cd))
                  if (EPname(cd,edcount) .eq. tempname) then
                    ted = edcount
                    ednotfound = .FALSE.
                  else
                    edcount = edcount + 1
                  endif
               enddo
               if (ednotfound) then
                 CPnumcols(cd) = CPnumcols(cd) + 1
                 ed = ed + 1
                 ted = ed
                 EPname(cd,ed) = tempname
               endif
               if (Dbuf(CHP_S_CDELIND:CHP_E_CDELIND) .eq. 'T') then
                  EPdelind(cd,ted) = .TRUE.
               else
                  EPdelind(cd,ted) = .FALSE.
               endif
               if (Dbuf(CHP_S_CPREFD:CHP_E_CPREFD) .eq. 'T') then
                  EPprefdis(cd,ted) = .TRUE.
               else
                  EPprefdis(cd,ted) = .FALSE.
               endif
               if (Dbuf(CHP_S_CCOLDES:CHP_E_CCOLDES) .eq. 'T') then
                  EPcoldes(cd,ted) = .TRUE.
               else
                  EPcoldes(cd,ted) = .FALSE.
               endif
               if (Dbuf(CHP_S_CASSERT:CHP_E_CASSERT) .eq. 'T') then
                  EPassert(cd,ted) = .TRUE.
               else
                  EPassert(cd,ted) = .FALSE.
               endif
               if (Dbuf(CHP_S_CDCHK:CHP_E_CDCHK) .eq. 'T') then
                  EPdomchk(cd,ted) = .TRUE.
               else
                  EPdomchk(cd,ted) = .FALSE.
               endif
               call chr_ctoi(Dbuf(CHP_S_CARRSHP:CHP_E_CARRSHP),
     :              EParrshp(cd,ted), status)
               call chr_ctoi(Dbuf(CHP_S_CLMD:CHP_E_CLMD),
     :              EPdatelm(cd,ted), status)
               if (Dbuf(CHP_S_CNSFLAG:CHP_E_CNSFLAG) .eq. 'T') then
                  EPnsflag(cd,ted) = .TRUE.
               else
                  EPnsflag(cd,ted) = .FALSE.
               endif
               EPnsformat(cd,ted) = Dbuf(CHP_S_CNSFMAT:CHP_E_CNSFMAT)
*               call chr_ctoi(Dbuf(CHP_S_CARRTYP:CHP_E_CARRTYP),
*     :              EParrtype(cd,ted), status)
               if (Dbuf(CHP_S_CHIDDEN:CHP_E_CHIDDEN) .eq. 'T') then
                  EPhidden(cd,ted) = .TRUE.
               else
                  EPhidden(cd,ted) = .FALSE.
               endif
*
*   We can also fill in other information about this column.
*
                  EPorig(cd,ted) = 0
*
*   If the key is 'B' then this is additional column information
*   array dimensions.
*
*    Column name
*    First array dimension.
*    Second array dimension.
*    Third array dimension.
*    Forth array dimension.
*    Fifth array dimension.
*    Sixth array dimension.
*    Seventh array dimension.
*

           elseif (Dbuf(CHP_S_KEY:CHP_E_KEY) .eq. 'B') then
               tempname = Dbuf(CHP_S_CNAME:CHP_E_CNAME)
               ednotfound = .TRUE.
               edcount = 1
               do while (ednotfound .and. edcount .le. CPnumcols(cd))
                  if (EPname(cd,edcount) .eq. tempname) then
                    ted = edcount
                    ednotfound = .FALSE.
                  else
                    edcount = edcount + 1
                  endif
               enddo
               if (ednotfound) then
                 CPnumcols(cd) = CPnumcols(cd) + 1
                 ed = ed + 1
                 ted = ed
                 EPname(cd,ed) = tempname
               endif
               call chr_ctoi(Dbuf(CHP_S_CDIM1:CHP_E_CDIM1),
     :              EParrdim(cd,ted,1), status)
               call chr_ctoi(Dbuf(CHP_S_CDIM2:CHP_E_CDIM2),
     :              EParrdim(cd,ted,2), status)
               call chr_ctoi(Dbuf(CHP_S_CDIM3:CHP_E_CDIM3),
     :              EParrdim(cd,ted,3), status)
               call chr_ctoi(Dbuf(CHP_S_CDIM4:CHP_E_CDIM4),
     :              EParrdim(cd,ted,4), status)
               call chr_ctoi(Dbuf(CHP_S_CDIM5:CHP_E_CDIM5),
     :              EParrdim(cd,ted,5), status)
               call chr_ctoi(Dbuf(CHP_S_CDIM6:CHP_E_CDIM6),
     :              EParrdim(cd,ted,6), status)
               call chr_ctoi(Dbuf(CHP_S_CDIM7:CHP_E_CDIM7),
     :              EParrdim(cd,ted,7), status)
*
*   If the key is 'C' then this is additional column information
*   assert expression information
*
*    Column name
*    Continuation number.
*    Expression part.
*
           elseif (Dbuf(CHP_S_KEY:CHP_E_KEY) .eq. 'C') then
               tempname = Dbuf(CHP_S_CNAME:CHP_E_CNAME)
               ednotfound = .TRUE.
               edcount = 1
               do while (ednotfound .and. edcount .le. CPnumcols(cd))
                  if (EPname(cd,edcount) .eq. tempname) then
                    ted = edcount
                    ednotfound = .FALSE.
                  else
                    edcount = edcount + 1
                  endif
               enddo
               if (ednotfound) then
                 CPnumcols(cd) = CPnumcols(cd) + 1
                 ed = ed + 1
                 ted = ed
                 EPname(cd,ed) = tempname
               endif
               call chr_ctoi(Dbuf(CHP_S_CEXPCON:CHP_E_CEXPCON),
     :              partexpnum, status)
               startpos = partexpnum * 80 + 1
               endpos = startpos + 79
               EPassexp(cd,ted)(startpos:endpos) =
     :            Dbuf(CHP_S_CASSEXP:CHP_E_CASSEXP)
*
*   If the key is 'D' then this is additional column information
*   virtual column expression information
*
*    Column name
*    Continuation number.
*    Expression part.
*
           elseif (Dbuf(CHP_S_KEY:CHP_E_KEY) .eq. 'D') then
               tempname = Dbuf(CHP_S_CNAME:CHP_E_CNAME)
               ednotfound = .TRUE.
               edcount = 1
               do while (ednotfound .and. edcount .le. CPnumcols(cd))
                  if (EPname(cd,edcount) .eq. tempname) then
                    ted = edcount
                    ednotfound = .FALSE.
                  else
                    edcount = edcount + 1
                  endif
               enddo
               if (ednotfound) then
                 CPnumcols(cd) = CPnumcols(cd) + 1
                 ed = ed + 1
                 ted = ed
                 EPname(cd,ed) = tempname
               endif
               call chr_ctoi(Dbuf(CHP_S_CEXPCON:CHP_E_CEXPCON),
     :              partexpnum, status)
               startpos = partexpnum * 80 + 1
               endpos = startpos + 79
               EPvcexp(cd,ted)(startpos:endpos) =
     :            Dbuf(CHP_S_CASSEXP:CHP_E_CASSEXP)
               EPvcflag(cd,ted) = .TRUE.
*
*   If the key is 'F' then this is parameter information.
*
*    Parameter name
*    Parameter unit
*    Parameter format
*    Parameter value
*    Parameter comment
*
*
           elseif (Dbuf(CHP_S_KEY:CHP_E_KEY) .eq. 'F') then
               tempname = Dbuf(CHP_S_PNAME:CHP_E_PNAME)
               pdnotfound = .TRUE.
               pdcount = 1
               do while (pdnotfound .and. pdcount .le. CPnumpars(cd))
                  if (PPname(cd,pdcount) .eq. tempname) then
                    tpd = pdcount
                    pdnotfound = .FALSE.
                  else
                    pdcount = pdcount + 1
                  endif
               enddo
               if (pdnotfound) then
                 CPnumpars(cd) = CPnumpars(cd) + 1
                 tpd = CPnumpars(cd)
                 PPname(cd,tpd) = tempname
               endif
               PPunit(cd,tpd) = Dbuf(CHP_S_PUNIT:CHP_E_PUNIT)
               PPformat(cd,tpd) = Dbuf(CHP_S_PFMT:CHP_E_PFMT)
               call chp_ffmttyp(PPformat(cd,tpd), PPtype(cd,tpd),status)
               PPcharval(cd,tpd) = Dbuf(CHP_S_PVAL:CHP_E_PVAL)
               PPcomment(cd,tpd) = Dbuf(CHP_S_PCMT:CHP_E_PCMT)
               call chr_ctoi(Dbuf(CHP_S_PPTR:CHP_E_PPTR),
     :              PPptrval(cd,tpd), status)
*
*   We can also fill in other information about this parameter.
*
                  PPorig(cd,tpd) = 0
*
*   If the key is 'G' then this is parameter information.
*
*    Parameter Last Modified Date.
*    Parameter Delete Indicator
*    Parameter Preferred Display
*    Parameter Array Shape.
*
           elseif (Dbuf(CHP_S_KEY:CHP_E_KEY) .eq. 'G') then

               tempname = Dbuf(CHP_S_PNAME:CHP_E_PNAME)
               pdnotfound = .TRUE.
               pdcount = 1
               do while (pdnotfound .and. pdcount .le. CPnumpars(cd))
                  if (PPname(cd,pdcount) .eq. tempname) then
                    tpd = pdcount
                    pdnotfound = .FALSE.
                  else
                    pdcount = pdcount + 1
                  endif
               enddo
               if (pdnotfound) then
                 CPnumpars(cd) = CPnumpars(cd) + 1
                 tpd = CPnumpars(cd)
                 PPname(cd,tpd) = tempname
               endif
               call chr_ctoi(Dbuf(CHP_S_PLMD:CHP_E_PLMD),
     :              PPdatelm(cd,tpd), status)
               if (Dbuf(CHP_S_PDELIND:CHP_E_PDELIND) .eq. 'T') then
                  PPdelind(cd,tpd) = .TRUE.
               else
                  PPdelind(cd,tpd) = .FALSE.
               endif
               if (Dbuf(CHP_S_PPREFD:CHP_E_PPREFD) .eq. 'T') then
                  PPprefdis(cd,tpd) = .TRUE.
               else
                  PPprefdis(cd,tpd) = .FALSE.
               endif
               if (Dbuf(CHP_S_PPARDES:CHP_E_PPARDES) .eq. 'T') then
                  PPpardes(cd,tpd) = .TRUE.
               else
                  PPpardes(cd,tpd) = .FALSE.
               endif
               call chr_ctoi(Dbuf(CHP_S_PARRSHP:CHP_E_PARRSHP),
     :              PParrshp(cd,tpd), status)
               if (Dbuf(CHP_S_PNSFLAG:CHP_E_PNSFLAG) .eq. 'T') then
                  PPnsflag(cd,tpd) = .TRUE.
               else
                  PPnsflag(cd,tpd) = .FALSE.
               endif
               PPnsformat(cd,tpd) = Dbuf(CHP_S_PNSFMAT:CHP_E_PNSFMAT)
*
*   If the key is 'H' then this is additional parameter information
*   array dimensions.
*
*    Parameter name
*    First array dimension.
*    Second array dimension.
*    Third array dimension.
*    Forth array dimension.
*    Fifth array dimension.
*    Sixth array dimension.
*    Seventh array dimension.
*
           elseif (Dbuf(CHP_S_KEY:CHP_E_KEY) .eq. 'H') then
               tempname = Dbuf(CHP_S_PNAME:CHP_E_PNAME)
               pdnotfound = .TRUE.
               pdcount = 1
               do while (pdnotfound .and. pdcount .le. CPnumpars(cd))
                  if (PPname(cd,pdcount) .eq. tempname) then
                    tpd = pdcount
                    pdnotfound = .FALSE.
                  else
                    pdcount = pdcount + 1
                  endif
               enddo
               if (pdnotfound) then
                 CPnumpars(cd) = CPnumpars(cd) + 1
                 tpd = CPnumpars(cd)
                 PPname(cd,tpd) = tempname
               endif
               call chr_ctoi(Dbuf(CHP_S_CDIM1:CHP_E_CDIM1),
     :              PParrdim(cd,tpd,1), status)
               call chr_ctoi(Dbuf(CHP_S_CDIM2:CHP_E_CDIM2),
     :              PParrdim(cd,tpd,2), status)
               call chr_ctoi(Dbuf(CHP_S_CDIM3:CHP_E_CDIM3),
     :              PParrdim(cd,tpd,3), status)
               call chr_ctoi(Dbuf(CHP_S_CDIM4:CHP_E_CDIM4),
     :              PParrdim(cd,tpd,4), status)
               call chr_ctoi(Dbuf(CHP_S_CDIM5:CHP_E_CDIM5),
     :              PParrdim(cd,tpd,5), status)
               call chr_ctoi(Dbuf(CHP_S_CDIM6:CHP_E_CDIM6),
     :              PParrdim(cd,tpd,6), status)
               call chr_ctoi(Dbuf(CHP_S_CDIM7:CHP_E_CDIM7),
     :              PParrdim(cd,tpd,7), status)
*
*    Keyword is 'I' this is catnotes file name.
*
*   Catnotes file name
*
           elseif (Dbuf(CHP_S_KEY:CHP_E_KEY) .eq. 'I') then
               CPcatnotes(cd) = Dbuf(CHP_S_CATN:CHP_E_CATN)
*
*    Keyword is 'J' this is cathelp file name.
*
*   Cathelp file name
*
*           elseif (Dbuf(CHP_S_KEY:CHP_E_KEY) .eq. 'J') then
*               CPcathelp(cd) = Dbuf(CHP_S_CATN:CHP_E_CATN)
*
*    Keyword is 'K' this is the index and order file name.
*
*   Index and Order file name
*
*           elseif (Dbuf(CHP_S_KEY:CHP_E_KEY) .eq. 'K') then
*               CPindandord(cd) = Dbuf(CHP_S_CATN:CHP_E_CATN)
*
*    Keyword is 'L' this is the N-dimensional index information
*
*   N-Dimension index fields
*   Number of dimension
*   First field name
*   Second Field name
*   Third Field name
*   Index Last Modified Date.
*
           elseif (Dbuf(CHP_S_KEY:CHP_E_KEY) .eq. 'L') then
               CPnofndi(cd) = CPnofndi(cd) + 1
               i = CPnofndi(cd)
               call chr_ctoi(Dbuf(CHP_S_NUMDIM:CHP_E_NUMDIM),
     :              CPnumndcols(cd,i), status)
               CP1icol(cd,i) = Dbuf(CHP_S_1ICOL:CHP_E_1ICOL)
               CP2icol(cd,i) = Dbuf(CHP_S_2ICOL:CHP_E_2ICOL)
               CP3icol(cd,i) = Dbuf(CHP_S_3ICOL:CHP_E_3ICOL)
               call chr_ctoi(Dbuf(CHP_S_ILMD:CHP_E_ILMD),
     :              CPnddatelm(cd,i), status)
*
*   We can also fill in other information about this index.
*
                  CPndorig(cd,i) = 0
*
*    Keyword is 'M' this is the default sort order and can never appear
*    in an extension catalogue.
*
*
*    Keyword is 'N' this is the alternative sort order.
*
*   Number of sort fileds
*   Primary Sort field
*   Primary Sort Direction
*   Secondary Sort field
*   Secondary Sort direction
*   Tertiary Sort field
*   Tertiary Sort direction
*   Sort Default Last Modified Date.
*
           elseif (Dbuf(CHP_S_KEY:CHP_E_KEY) .eq. 'N') then
               CPnofasorts(cd) = CPnofasorts(cd) + 1
               i = CPnofasorts(cd)
               call chr_ctoi(Dbuf(CHP_S_NSORT:CHP_E_NSORT),
     :              CPnumascols(cd,CPnofasorts(cd)), status)
               CPfascol(cd,i) = Dbuf(CHP_S_FDSCOL:CHP_E_FDSCOL)
               CPfasdir(cd,i) = Dbuf(CHP_S_FDSDIR:CHP_E_FDSDIR)
               CPsascol(cd,i) = Dbuf(CHP_S_SDSCOL:CHP_E_SDSCOL)
               CPsasdir(cd,i) = Dbuf(CHP_S_SDSDIR:CHP_E_SDSDIR)
               CPtascol(cd,i) = Dbuf(CHP_S_TDSCOL:CHP_E_TDSCOL)
               CPtasdir(cd,i) = Dbuf(CHP_S_TDSDIR:CHP_E_TDSDIR)
               call chr_ctoi(Dbuf(CHP_S_DSLMD:CHP_E_DSLMD),
     :              CPasdatelm(cd,i), status)
*
*   We can also fill in other information about this alternative
*   sort.
*
                  CPasorig(cd,i) = 0
*
*    Keyword is 'O' this is catalogue extension information
*    Not applicable here.
*
*    Keyword is 'P' this is general catalogue information
*
*    Catalogue name
*    Catalogue Delete Indicator.
*    Catalogue Frequency Flag.
*    Catalogue access Flag.
*
           elseif (Dbuf(CHP_S_KEY:CHP_E_KEY) .eq. 'P') then
               Dbuf(CHP_S_CATNAME:CHP_E_CATNAME) = input
               if (Dbuf(CHP_S_CATDELIND:CHP_E_CATDELIND) .eq. 'T') then
                  CPdelind(cd) = .TRUE.
               else
                  CPdelind(cd) = .FALSE.
               endif
*               if (Dbuf(CHP_S_CATFREQ:CHP_E_CATFREQ) .eq. 'T') then
*                  CPfreq(cd) = .TRUE.
*               else
*                  CPfreq(cd) = .FALSE.
*               endif
*               if (Dbuf(CHP_S_CATACC:CHP_S_CATACC) .eq. 'T') then
*                  CPaccess(cd) = .TRUE.
*               else
*                  CPaccess(cd) = .FALSE.
*               endif
           endif
          endif
        enddo
        if (status .eq. FIO__EOF) then
          call err_annul(status)
        endif
      endif
      end
*
