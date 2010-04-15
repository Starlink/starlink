      SUBROUTINE
     : CHP_DLOADCAT( CD, STATUS )
*+
*  Name:
*     CHP_DLOADCAT

*  Purpose:
*     Down load the catalogue in common to files.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHP_DLOADCAT( CD, STATUS )

*  Description:
*     Down load the information in the common area for a given catalogue
*     into the appropriate files. The catalogue may be a Join Extension,
*     a Search Extension a Plain Extension or a Regular catalogue.
*     The approach adopted in this routine is to consider extension
*     catalogues first (CPnumext greater than 0). For these consider all
*     the columns selecting only those with origin 0 (EPorig eq 0)
*     because these belong to the extension. Write these to the
*     additional information about this catalogue. Parameters can be
*     dealt with in the same way. The information about the whole
*     catalogue is then considered, the number of entries etc.
*
*     ***NOTE*** In this implementation we have assumed that the
*     catalogues that have been extended are read only.
*
*     If this is not an extension catalogue consider all the columns.
*     For virtual columns full details will have to be written to the
*     additional information file, for real columns details excluding
*     format, units, comments, type will have to be written.
*

*  Arguments:
*     CD = INTEGER (Given)
*        Catalogue descriptor
*     STATUS = INTEGER
*        Global status.

*  Anticipated Errors:
*     CHI__CATNOTFND

*  Authors:
*     ARW: Alan R Wood (FIIS/RAL)
*     {enter_new_authors_here}

*  History:
*     1-OCT-1993 (ARW):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'   ! Standard SAE constants
      INCLUDE 'CHP_ERR'   ! Standard CHI errors
      INCLUDE 'FIO_ERR'   ! Standard FIO errors
      INCLUDE 'CHP_PAR'   ! Standard CHP constants

*  Global Variables:
      INCLUDE 'CHP_CMN'   ! Standard CHP common area.

*  Arguments Given:
      INTEGER CD

*  Arguments Returned:

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER NCHAR
      INTEGER I
      INTEGER CC
      INTEGER PC
      INTEGER NAS
      INTEGER NDC
      INTEGER PARTEXPNUM
      INTEGER ED         ! Element descriptor counter.
      INTEGER TEMPED         ! Element descriptor counter.
      INTEGER PD         ! Element descriptor counter.
      INTEGER TEMPPD         ! Element descriptor counter.
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

*-
*
*.

*  Check inherited global status.
*
      IF ( STATUS .NE. SAI__OK ) RETURN
*
*  Download the common area
*
      call chp_deladdf(CPname(cd), status)
*
      if (status .eq. CHP__CATNOTFND) then
        call err_annul(status)
      endif
*
      call chp_openaddf(CPname(cd), 'WRITE', 'LIST', CHP__SZDREC,
     :                   fdd, status)
*
*    Abort if failed to open file okay
*
      if (status .ne. SAI__OK) then
        status = CHP__CATNOTFND
      endif
*
*   Initialize the buffer.
*
            call chr_fill(' ',Dbuf)
*
*   If this is an extension catalogue down load the information for the
*   columns and parameters that belong to the catalogue EPorig and Porig
*   equal 0.
*
*
      if (CPnumext(cd) .gt. 0) then
*
*   For each column check if it has its origin in this catalogue.
*
        do cc = 1, CPnumcols(cd)
          if ( EPorig(cd,cc) .eq. 0) then
*
*   'A' for a column. The information is
*
*    Column name
*    Column Delete Indicator
*    Column Preferred Display
*    Column Designation.
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
            call chr_fill(' ',Dbuf)
            Dbuf(CHP_S_KEY:CHP_E_KEY) = 'A'
            Dbuf(CHP_S_CNAME:CHP_E_CNAME) = EPname(cd,cc)
            if (EPdelind(cd,cc)) then
              Dbuf(CHP_S_CDELIND:CHP_E_CDELIND) = 'T'
            else
              Dbuf(CHP_S_CDELIND:CHP_E_CDELIND) = 'F'
            endif
            if (EPprefdis(cd,cc)) then
              Dbuf(CHP_S_CPREFD:CHP_E_CPREFD) =  'T'
            else
              Dbuf(CHP_S_CPREFD:CHP_E_CPREFD) =  'F'
            endif
            call chr_itoc(EPcoldes(cd,cc),
     :       Dbuf(CHP_S_CCOLDES:CHP_E_CCOLDES), nchar)
            if (EPassert(cd,cc)) then
              Dbuf(CHP_S_CASSERT:CHP_E_CASSERT) = 'T'
            else
              Dbuf(CHP_S_CASSERT:CHP_E_CASSERT) = 'F'
            endif
            if (EPdomchk(cd,cc)) then
              Dbuf(CHP_S_CDCHK:CHP_E_CDCHK) = 'T'
            else
              Dbuf(CHP_S_CDCHK:CHP_E_CDCHK) = 'F'
            endif
            call chr_itoc(EParrshp(cd,cc),
     :       Dbuf(CHP_S_CARRSHP:CHP_E_CARRSHP), nchar)
            call chr_itoc(EPdatelm(cd,cc),
     :       Dbuf(CHP_S_CLMD:CHP_E_CLMD), nchar)
            if (EPnsflag(cd,cc)) then
              Dbuf(CHP_S_CNSFLAG:CHP_E_CNSFLAG) = 'T'
            else
              Dbuf(CHP_S_CNSFLAG:CHP_E_CNSFLAG) = 'F'
            endif
            Dbuf(CHP_S_CNSFMAT:CHP_E_CNSFMAT) = EPnsformat(cd,cc)
           if (EPhidden(cd,cc)) then
              Dbuf(CHP_S_CHIDDEN:CHP_E_CHIDDEN) = 'T'
            else
              Dbuf(CHP_S_CHIDDEN:CHP_E_CHIDDEN) = 'F'
            endif
            call fio_write(fdd, Dbuf, status)
*
*   If the column is an array the array shape is greater then 0.
*   'B' is used to record this additional column information
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
            if (EParrshp(cd,cc) .gt. 0) then
              Dbuf(CHP_S_KEY:CHP_E_KEY) = 'B'
              Dbuf(CHP_S_CNAME:CHP_E_CNAME) = EPname(cd,cc)
              if (EParrshp(cd,cc) .ge. 1) then
                call chr_itoc(EParrdim(cd,cc,1),
     :           Dbuf(CHP_S_CDIM1:CHP_E_CDIM1), nchar)
              endif
              if (EParrshp(cd,cc) .ge. 2) then
                call chr_itoc(EParrdim(cd,cc,2),
     :           Dbuf(CHP_S_CDIM2:CHP_E_CDIM2), nchar)
              endif
              if (EParrshp(cd,cc) .ge. 3) then
                call chr_itoc(EParrdim(cd,cc,3),
     :           Dbuf(CHP_S_CDIM3:CHP_E_CDIM3), nchar)
              endif
              if (EParrshp(cd,cc) .ge. 4) then
                call chr_itoc(EParrdim(cd,cc,4),
     :           Dbuf(CHP_S_CDIM4:CHP_E_CDIM4), nchar)
              endif
              if (EParrshp(cd,cc) .ge. 5) then
                call chr_itoc(EParrdim(cd,cc,5),
     :           Dbuf(CHP_S_CDIM5:CHP_E_CDIM5), nchar)
              endif
              if (EParrshp(cd,cc) .ge. 6) then
                call chr_itoc(EParrdim(cd,cc,6),
     :           Dbuf(CHP_S_CDIM6:CHP_E_CDIM6), nchar)
              endif
              if (EParrshp(cd,cc) .ge. 7) then
                call chr_itoc(EParrdim(cd,cc,7),
     :           Dbuf(CHP_S_CDIM7:CHP_E_CDIM7), nchar)
              endif
              call fio_write(fdd, Dbuf, status)
            call chr_fill(' ',Dbuf)
            endif
*
*   If the column has an assert expression.
*   'C' is used to record this additional
*   assert expression information
*
*    Column name
*    Continuation number.
*    Expression part.
*
            if (EPassert(cd,cc)) then
              Dbuf(CHP_S_KEY:CHP_E_KEY) = 'C'
              Dbuf(CHP_S_CNAME:CHP_E_CNAME) = EPname(cd,cc)
              Dbuf(CHP_S_CEXPCON:CHP_E_CEXPCON) = '1'
              Dbuf(CHP_S_CASSEXP:CHP_E_CASSEXP) = EPassexp(cd,cc)
              call fio_write(fdd, Dbuf, status)
              call chr_fill(' ',Dbuf)
            endif
*
*   If the column is a virtual column .
*   'D' is used to record this additional
*   virtual column expression information
*
*    Column name
*    Continuation number.
*    Expression part.
*
*
            if (EPvcflag(cd,cc)) then
              Dbuf(CHP_S_KEY:CHP_E_KEY) = 'D'
              Dbuf(CHP_S_CNAME:CHP_E_CNAME) = EPname(cd,cc)
              Dbuf(CHP_S_CEXPCON:CHP_E_CEXPCON) = '1'
              Dbuf(CHP_S_CASSEXP:CHP_E_CASSEXP) = EPvcexp(cd,cc)
              call fio_write(fdd, Dbuf, status)
              call chr_fill(' ',Dbuf)
*
*   The column is a virtual column, varable length character string or
*   a variable length array .
*   'E' is used to record this additional
*   virtual column information
*
*    Column name
*    Column Units
*    Column Format
*    Column Type
*    Column Comment
*
              Dbuf(CHP_S_KEY:CHP_E_KEY) = 'E'
              Dbuf(CHP_S_CNAME:CHP_E_CNAME) = EPname(cd,cc)
              Dbuf(CHP_S_CUNIT:CHP_E_CUNIT) = EPunits(cd,cc)
              Dbuf(CHP_S_CFMT:CHP_E_CFMT) = EPformat(cd,cc)
              Dbuf(CHP_S_CTYPE:CHP_E_CTYPE) = EPtype(cd,cc)
              Dbuf(CHP_S_CCMT:CHP_E_CCMT) = EPcomment(cd,cc)
              call fio_write(fdd, Dbuf, status)
              call chr_fill(' ',Dbuf)
            endif
          endif
        enddo
*
*   For each parameter check if it belongs to this catalogue.
*
        do pc = 1, CPnumpars(cd)
          if (PPorig(cd,pc) .eq. 0) then
*
*   'F' for a parameter. The information is
*
*    Parameter name
*    Parameter unit
*    Parameter format
*    Parameter value
*    Parameter comment
*
            Dbuf(CHP_S_KEY:CHP_E_KEY) = 'F'
            Dbuf(CHP_S_PNAME:CHP_E_PNAME) = PPname(cd,pc)
            Dbuf(CHP_S_PUNIT:CHP_E_PUNIT) = PPunit(cd,pc)
            Dbuf(CHP_S_PFMT:CHP_E_PFMT) = PPformat(cd,pc)
            Dbuf(CHP_S_PVAL:CHP_E_PVAL) = PPcharval(cd,pc)
            call chr_itoc(PPptrval(cd,pc),
     :       Dbuf(CHP_S_PPTR:CHP_E_PPTR), nchar)
            Dbuf(CHP_S_PCMT:CHP_E_PCMT) = PPcomment(cd,pc)
            call fio_write(fdd, Dbuf, status)
            call chr_fill(' ',Dbuf)
*
*   'G' for a parameter. The information is
*
*    Parameter name
*    Parameter Last Modified Date.
*    Parameter Delete Indicator
*    Parameter Preferred Display
*    Parameter Designations.
*    Parameter Array Shape.
*
            Dbuf(CHP_S_PNAME:CHP_E_PNAME) = PPname(cd,pc)
            call chr_itoc(PPdatelm(cd,pc),
     :       Dbuf(CHP_S_PLMD:CHP_E_PLMD), nchar)
            if (PPdelind(cd,pc)) then
              Dbuf(CHP_S_PDELIND:CHP_S_PDELIND) = 'T'
               else
              Dbuf(CHP_S_PDELIND:CHP_S_PDELIND) = 'F'
            endif
            if (PPprefdis(cd,pc)) then
              Dbuf(CHP_S_PPREFD:CHP_S_PPREFD) =  'T'
            else
              Dbuf(CHP_S_PPREFD:CHP_S_PPREFD) =  'F'
            endif
            call chr_itoc(PPpardes(cd,pc),
     :       Dbuf(CHP_S_PPARDES:CHP_E_PPARDES), nchar)
            call chr_itoc(PParrshp(cd,pc),
     :       Dbuf(CHP_S_PARRSHP:CHP_E_PARRSHP), nchar)
            if (PPnsflag(cd,cc)) then
              Dbuf(CHP_S_PNSFLAG:CHP_E_PNSFLAG) = 'T'
            else
              Dbuf(CHP_S_PNSFLAG:CHP_E_PNSFLAG) = 'F'
            endif
            Dbuf(CHP_S_PNSFMAT:CHP_E_PNSFMAT) = PPnsformat(cd,cc)
            call fio_write(fdd, Dbuf, status)
            call chr_fill(' ',Dbuf)
*
*   If the parameter is an array the array shape is greater then 0.
*   'H' is used to record this additional parameter information
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
            if (PParrshp(cd,cc) .gt. 0) then
              Dbuf(CHP_S_KEY:CHP_E_KEY) = 'H'
              Dbuf(CHP_S_PNAME:CHP_E_PNAME) = PPname(cd,cc)
              if (PParrshp(cd,cc) .ge. 1) then
                call chr_itoc(PParrdim(cd,cc,1),
     :           Dbuf(CHP_S_CDIM1:CHP_E_CDIM1), nchar)
              endif
              if (PParrshp(cd,cc) .ge. 2) then
                call chr_itoc(PParrdim(cd,cc,2),
     :           Dbuf(CHP_S_CDIM2:CHP_E_CDIM2), nchar)
              endif
              if (PParrshp(cd,cc) .ge. 3) then
                call chr_itoc(PParrdim(cd,cc,3),
     :           Dbuf(CHP_S_CDIM3:CHP_E_CDIM3), nchar)
              endif
              if (PParrshp(cd,cc) .ge. 4) then
                call chr_itoc(PParrdim(cd,cc,4),
     :           Dbuf(CHP_S_CDIM4:CHP_E_CDIM4), nchar)
              endif
              if (PParrshp(cd,cc) .ge. 5) then
                call chr_itoc(PParrdim(cd,cc,5),
     :           Dbuf(CHP_S_CDIM5:CHP_E_CDIM5), nchar)
              endif
              if (PParrshp(cd,cc) .ge. 6) then
                call chr_itoc(PParrdim(cd,cc,6),
     :           Dbuf(CHP_S_CDIM6:CHP_E_CDIM6), nchar)
              endif
              if (PParrshp(cd,cc) .ge. 7) then
                call chr_itoc(PParrdim(cd,cc,7),
     :           Dbuf(CHP_S_CDIM7:CHP_E_CDIM7), nchar)
              endif
              call fio_write(fdd, Dbuf, status)
              call chr_fill(' ',Dbuf)
            endif
          endif
        enddo
*
*    'I' is used to record the catnotes file name.
*
*   Catnotes file name
*
        Dbuf(CHP_S_KEY:CHP_E_KEY) = 'I'
        Dbuf(CHP_S_CATN:CHP_E_CATN) = CPcatnotes(cd)
        call fio_write(fdd, Dbuf, status)
        call chr_fill(' ',Dbuf)
*
*    'J' is not used
*
*    'K' is not used
*
*   N-dimensional index information
*
        do ndc = 1, CPnofndi(cd)
          if ( CPndorig(cd,ndc)) then
*
*    'L' used to record the N-dimensional index information
*
*   N-Dimension index fields
*   Number of dimension
*   First field name
*   Second Field name
*   Third Field name
*   Index Last Modified Date.
*
            Dbuf(CHP_S_KEY:CHP_E_KEY) = 'L'
            call chr_itoc(CPnumndcols(cd,ndc),
     :      Dbuf(CHP_S_NUMDIM:CHP_E_NUMDIM), nchar)
            Dbuf(CHP_S_1ICOL:CHP_E_1ICOL) = CP1icol(cd,ndc)
            Dbuf(CHP_S_2ICOL:CHP_E_2ICOL) = CP2icol(cd,ndc)
            Dbuf(CHP_S_3ICOL:CHP_E_3ICOL) = CP3icol(cd,ndc)
            Dbuf(CHP_S_4ICOL:CHP_E_4ICOL) = CP4icol(cd,ndc)
            Dbuf(CHP_S_5ICOL:CHP_E_5ICOL) = CP5icol(cd,ndc)
            Dbuf(CHP_S_6ICOL:CHP_E_6ICOL) = CP6icol(cd,ndc)
            Dbuf(CHP_S_7ICOL:CHP_E_7ICOL) = CP7icol(cd,ndc)
            call chr_itoc(CPnddatelm(cd,ndc),
     :       Dbuf(CHP_S_ILMD:CHP_E_ILMD), nchar)
            call fio_write(fdd, Dbuf, status)
            call chr_fill(' ',Dbuf)
          endif
        enddo
*
*    'N' is used to record the alternative sort order
*
*   Alternative sort information
*
        do nas = 1, CPnofasorts(cd)
          if ( CPasorig(cd,nas)) then
*
*   Number of sort fileds
*   Primary Sort field
*   Primary Sort Direction
*
            Dbuf(CHP_S_KEY:CHP_E_KEY) = 'N'
            call chr_itoc(CPnumascols(cd,nas),
     :      Dbuf(CHP_S_NSORT:CHP_E_NSORT), nchar)
            Dbuf(CHP_S_FDSCOL:CHP_E_FDSCOL) = CPfascol(cd,nas)
            Dbuf(CHP_S_FDSDIR:CHP_E_FDSDIR) = CPfasdir(cd,nas)
            Dbuf(CHP_S_SDSCOL:CHP_E_SDSCOL) = CPsascol(cd,nas)
            Dbuf(CHP_S_SDSDIR:CHP_E_SDSDIR) = CPsasdir(cd,nas)
            Dbuf(CHP_S_TDSCOL:CHP_E_TDSCOL) = CPtascol(cd,nas)
            Dbuf(CHP_S_TDSDIR:CHP_E_TDSDIR) = CPtasdir(cd,nas)
            call chr_itoc(CPasdatelm(cd,nas),
     :       Dbuf(CHP_S_DSLMD:CHP_E_DSLMD), nchar)
            call fio_write(fdd, Dbuf, status)
            call chr_fill(' ',Dbuf)
          endif
        enddo
*
*    'O' is used to record the general catalogue information
*
*    Catalogue name
*    Number of Extension Catalogues
*    Name of the first original catalogue if this is an extension.
*    Name of the second original catalogue if this is an extension.
*    Name of the third original catalogue if this is an extension.
*
        Dbuf(CHP_S_KEY:CHP_E_KEY) = 'O'
        Dbuf(CHP_S_CATNAME:CHP_E_CATNAME) = CPname(cd)
        call chr_itoc(CPnumext(cd),
     :       Dbuf(CHP_S_CATNUMEXT:CHP_E_CATNUMEXT), nchar)
        if (CPexttype(cd)) then
          Dbuf(CHP_S_CATEXTTYPE:CHP_E_CATEXTTYPE) = 'T'
        else
          Dbuf(CHP_S_CATEXTTYPE:CHP_E_CATEXTTYPE) = 'F'
        endif
        Dbuf(CHP_S_CATFONAME:CHP_E_CATFONAME) = CPorignames(cd,1)
        Dbuf(CHP_S_CATSONAME:CHP_E_CATSONAME) = CPorignames(cd,2)
        Dbuf(CHP_S_CATTONAME:CHP_E_CATTONAME) = CPorignames(cd,3)
        call fio_write(fdd, Dbuf, status)
        call chr_fill(' ',Dbuf)
*
*    'P' is used to record the general catalogue information
*
*    Catalogue name
*    Catalogue Delete Indicator.
*    Catalogue Frequency Flag.
*    Catalogue access Flag.
*
        Dbuf(CHP_S_KEY:CHP_E_KEY) = 'P'
        Dbuf(CHP_S_CATNAME:CHP_E_CATNAME) = CPname(cd)
        if (CPdelind(cd)) then
          Dbuf(CHP_S_CATDELIND:CHP_E_CATDELIND) = 'T'
        else
          Dbuf(CHP_S_CATDELIND:CHP_E_CATDELIND) = 'F'
        endif
        call fio_write(fdd, Dbuf, status)
        call chr_fill(' ',Dbuf)
*
        call fio_close(fdd,status)
      endif
*
*
*
*   If this is a Regular catalogue deal with it now. This section is
*   kept seperate for clarity.
*
      if (CPnumext(cd) .eq. 0) then
*
*   For each column check if it is a virtual column. If it is and
*   write its full details.
*
        do cc = 1, CPnumcols(cd)
*
*   'A' for a column. The information is
*
*    Column name
*    Column Delete Indicator
*    Column Preferred Display
*    Column Designation.
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
            call chr_fill(' ',Dbuf)
            Dbuf(CHP_S_KEY:CHP_E_KEY) = 'A'
            Dbuf(CHP_S_CNAME:CHP_E_CNAME) = EPname(cd,cc)
            if (EPdelind(cd,cc)) then
              Dbuf(CHP_S_CDELIND:CHP_E_CDELIND) = 'T'
            else
              Dbuf(CHP_S_CDELIND:CHP_E_CDELIND) = 'F'
            endif
            if (EPprefdis(cd,cc)) then
              Dbuf(CHP_S_CPREFD:CHP_E_CPREFD) =  'T'
            else
              Dbuf(CHP_S_CPREFD:CHP_E_CPREFD) =  'F'
            endif
            call chr_itoc(EPcoldes(cd,cc),
     :       Dbuf(CHP_S_CCOLDES:CHP_E_CCOLDES), nchar)
            if (EPassert(cd,cc)) then
              Dbuf(CHP_S_CASSERT:CHP_E_CASSERT) = 'T'
            else
              Dbuf(CHP_S_CASSERT:CHP_E_CASSERT) = 'F'
            endif
            if (EPdomchk(cd,cc)) then
              Dbuf(CHP_S_CDCHK:CHP_E_CDCHK) = 'T'
            else
              Dbuf(CHP_S_CDCHK:CHP_E_CDCHK) = 'F'
            endif
            call chr_itoc(EParrshp(cd,cc),
     :       Dbuf(CHP_S_CARRSHP:CHP_E_CARRSHP), nchar)
            call chr_itoc(EPdatelm(cd,cc),
     :       Dbuf(CHP_S_CLMD:CHP_E_CLMD), nchar)
            if (EPnsflag(cd,cc)) then
              Dbuf(CHP_S_CNSFLAG:CHP_E_CNSFLAG) = 'T'
            else
              Dbuf(CHP_S_CNSFLAG:CHP_E_CNSFLAG) = 'F'
            endif
            Dbuf(CHP_S_CNSFMAT:CHP_E_CNSFMAT) = EPnsformat(cd,cc)
            if (EPhidden(cd,cc)) then
              Dbuf(CHP_S_CHIDDEN:CHP_E_CHIDDEN) = 'T'
            else
              Dbuf(CHP_S_CHIDDEN:CHP_E_CHIDDEN) = 'F'
            endif
            call fio_write(fdd, Dbuf, status)
            call chr_fill(' ',Dbuf)
*
*   If the column is an array the array shape is greater then 0.
*   'B' is used to record this additional column information
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
            if (EParrshp(cd,cc) .gt. 0) then
              Dbuf(CHP_S_KEY:CHP_E_KEY) = 'B'
              Dbuf(CHP_S_CNAME:CHP_E_CNAME) = EPname(cd,cc)
              if (EParrshp(cd,cc) .ge. 1) then
                call chr_itoc(EParrdim(cd,cc,1),
     :           Dbuf(CHP_S_CDIM1:CHP_E_CDIM1), nchar)
              endif
              if (EParrshp(cd,cc) .ge. 2) then
                call chr_itoc(EParrdim(cd,cc,2),
     :           Dbuf(CHP_S_CDIM2:CHP_E_CDIM2), nchar)
              endif
              if (EParrshp(cd,cc) .ge. 3) then
                call chr_itoc(EParrdim(cd,cc,3),
     :           Dbuf(CHP_S_CDIM3:CHP_E_CDIM3), nchar)
              endif
              if (EParrshp(cd,cc) .ge. 4) then
                call chr_itoc(EParrdim(cd,cc,4),
     :           Dbuf(CHP_S_CDIM4:CHP_E_CDIM4), nchar)
              endif
              if (EParrshp(cd,cc) .ge. 5) then
                call chr_itoc(EParrdim(cd,cc,5),
     :           Dbuf(CHP_S_CDIM5:CHP_E_CDIM5), nchar)
              endif
              if (EParrshp(cd,cc) .ge. 6) then
                call chr_itoc(EParrdim(cd,cc,6),
     :           Dbuf(CHP_S_CDIM6:CHP_E_CDIM6), nchar)
              endif
              if (EParrshp(cd,cc) .ge. 7) then
                call chr_itoc(EParrdim(cd,cc,7),
     :           Dbuf(CHP_S_CDIM7:CHP_E_CDIM7), nchar)
              endif
*             if (EParracc(cd,cc)) then
*               Dbuf(CHP_S_CARRACC:CHP_E_CARRACC) = 'T'
*             else
*               Dbuf(CHP_S_CARRACC:CHP_E_CARRACC) = 'F'
*             endif
              call fio_write(fdd, Dbuf, status)
              call chr_fill(' ',Dbuf)
            endif
*
*   If the column has an assert expression.
*   'C' is used to record this additional
*   assert expression information
*
*    Column name
*    Continuation number.
*    Expression part.
*
            if (EPassert(cd,cc)) then
              Dbuf(CHP_S_KEY:CHP_E_KEY) = 'C'
              Dbuf(CHP_S_CNAME:CHP_E_CNAME) = EPname(cd,cc)
              Dbuf(CHP_S_CEXPCON:CHP_E_CEXPCON) = '1'
              Dbuf(CHP_S_CASSEXP:CHP_E_CASSEXP) = EPassexp(cd,cc)
              call fio_write(fdd, Dbuf, status)
              call chr_fill(' ',Dbuf)
            endif
*
*
*   If the column is a virtual column .
*   'D' is used to record this additional
*   virtual column expression information
*
*    Column name
*    Continuation number.
*    Expression part.
*
*
            if (EPvcflag(cd,cc)) then
              Dbuf(CHP_S_KEY:CHP_E_KEY) = 'D'
              Dbuf(CHP_S_CNAME:CHP_E_CNAME) = EPname(cd,cc)
              Dbuf(CHP_S_CEXPCON:CHP_E_CEXPCON) = '1'
              Dbuf(CHP_S_CASSEXP:CHP_E_CASSEXP) = EPvcexp(cd,cc)
              call fio_write(fdd, Dbuf, status)
              call chr_fill(' ',Dbuf)
*
*   The column is a virtual column .
*   'E' is used to record this additional
*   virtual column information
*
*    Column name
*    Column Units
*    Column Format
*    Column Type
*    Column Comment
*
              Dbuf(CHP_S_KEY:CHP_E_KEY) = 'E'
              Dbuf(CHP_S_CNAME:CHP_E_CNAME) = EPname(cd,cc)
              Dbuf(CHP_S_CUNIT:CHP_E_CUNIT) = EPunits(cd,cc)
              Dbuf(CHP_S_CFMT:CHP_E_CFMT) = EPformat(cd,cc)
              Dbuf(CHP_S_CTYPE:CHP_E_CTYPE) = EPtype(cd,cc)
              Dbuf(CHP_S_CCMT:CHP_E_CCMT) = EPcomment(cd,cc)
              call fio_write(fdd, Dbuf, status)
              call chr_fill(' ',Dbuf)
            endif
        enddo
*
*   For each parameter check if it belongs to this catalogue.
*
        do pc = 1, CPnumpars(cd)
          if (PPorig(cd,pc) .eq. 0) then
*
*   'F' for a parameter. The information is
*
*    Parameter name
*    Parameter unit
*    Parameter format
*    Parameter value
*    Parameter comment
*
            Dbuf(CHP_S_KEY:CHP_E_KEY) = 'F'
            Dbuf(CHP_S_PNAME:CHP_E_PNAME) = PPname(cd,pc)
            Dbuf(CHP_S_PUNIT:CHP_E_PUNIT) = PPunit(cd,pc)
            Dbuf(CHP_S_PFMT:CHP_E_PFMT) = PPformat(cd,pc)
            Dbuf(CHP_S_PVAL:CHP_E_PVAL) = PPcharval(cd,pc)
            call chr_itoc(PPptrval(cd,pc),
     :       Dbuf(CHP_S_PPTR:CHP_E_PPTR), nchar)
            Dbuf(CHP_S_PCMT:CHP_E_PCMT) = PPcomment(cd,pc)
            call fio_write(fdd, Dbuf, status)
            call chr_fill(' ',Dbuf)
*
*   'G' for a parameter. The information is
*
*    Parameter name
*    Parameter Last Modified Date.
*    Parameter Delete Indicator
*    Parameter Preferred Display
*    Parameter Structure Flag
*    Parameter Array Shape.
*
            Dbuf(CHP_S_KEY:CHP_E_KEY) = 'G'
            Dbuf(CHP_S_PNAME:CHP_E_PNAME) = PPname(cd,pc)
            call chr_itoc(PPdatelm(cd,pc),
     :       Dbuf(CHP_S_PLMD:CHP_E_PLMD), nchar)
            if (PPdelind(cd,pc)) then
              Dbuf(CHP_S_PDELIND:CHP_S_PDELIND) = 'T'
               else
              Dbuf(CHP_S_PDELIND:CHP_S_PDELIND) = 'F'
            endif
            if (PPprefdis(cd,pc)) then
              Dbuf(CHP_S_PPREFD:CHP_S_PPREFD) =  'T'
            else
              Dbuf(CHP_S_PPREFD:CHP_S_PPREFD) =  'F'
            endif
            call chr_itoc(PPpardes(cd,pc),
     :       Dbuf(CHP_S_PPARDES:CHP_E_PPARDES), nchar)
            call chr_itoc(PParrshp(cd,pc),
     :       Dbuf(CHP_S_PARRSHP:CHP_E_PARRSHP), nchar)
            if (PPnsflag(cd,cc)) then
              Dbuf(CHP_S_PNSFLAG:CHP_E_PNSFLAG) = 'T'
            else
              Dbuf(CHP_S_PNSFLAG:CHP_E_PNSFLAG) = 'F'
            endif
            Dbuf(CHP_S_PNSFMAT:CHP_E_PNSFMAT) = PPnsformat(cd,cc)
            call fio_write(fdd, Dbuf, status)
            call chr_fill(' ',Dbuf)
*
*   If the parameter is an array the array shape is greater then 0.
*   'H' is used to record this additional parameter information
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
            if (PParrshp(cd,cc) .gt. 0) then
              Dbuf(CHP_S_KEY:CHP_E_KEY) = 'H'
              Dbuf(CHP_S_PNAME:CHP_E_PNAME) = PPname(cd,cc)
              if (PParrshp(cd,cc) .ge. 1) then
                call chr_itoc(PParrdim(cd,cc,1),
     :           Dbuf(CHP_S_CDIM1:CHP_E_CDIM1), nchar)
              endif
              if (PParrshp(cd,cc) .ge. 2) then
                call chr_itoc(PParrdim(cd,cc,2),
     :           Dbuf(CHP_S_CDIM2:CHP_E_CDIM2), nchar)
              endif
              if (PParrshp(cd,cc) .ge. 3) then
                call chr_itoc(PParrdim(cd,cc,3),
     :           Dbuf(CHP_S_CDIM3:CHP_E_CDIM3), nchar)
              endif
              if (PParrshp(cd,cc) .ge. 4) then
                call chr_itoc(PParrdim(cd,cc,4),
     :           Dbuf(CHP_S_CDIM4:CHP_E_CDIM4), nchar)
              endif
              if (PParrshp(cd,cc) .ge. 5) then
                call chr_itoc(PParrdim(cd,cc,5),
     :           Dbuf(CHP_S_CDIM5:CHP_E_CDIM5), nchar)
              endif
              if (PParrshp(cd,cc) .ge. 6) then
                call chr_itoc(PParrdim(cd,cc,6),
     :           Dbuf(CHP_S_CDIM6:CHP_E_CDIM6), nchar)
              endif
              if (PParrshp(cd,cc) .ge. 7) then
                call chr_itoc(PParrdim(cd,cc,7),
     :           Dbuf(CHP_S_CDIM7:CHP_E_CDIM7), nchar)
              endif
              call fio_write(fdd, Dbuf, status)
              call chr_fill(' ',Dbuf)
            endif
          endif
        enddo
*
*
*    'I' is used to record the catnotes file name.
*
*   Catnotes file name
*
        Dbuf(CHP_S_KEY:CHP_E_KEY) = 'I'
        Dbuf(CHP_S_CATN:CHP_E_CATN) = CPcatnotes(cd)
        call fio_write(fdd, Dbuf, status)
        call chr_fill(' ',Dbuf)
*
*    'J' is not used
*
*
*    'K' is not used
*
*
*   N-dimensional index information
*
        do ndc = 1, CPnofndi(cd)
          if ( CPndorig(cd,ndc)) then
*
*    'L' used to record the N-dimensional index information
*
*   N-Dimension index fields
*   Number of dimension
*   First field name
*   Second Field name
*   Third Field name
*   Index Last Modified Date.
*
            Dbuf(CHP_S_KEY:CHP_E_KEY) = 'L'
            call chr_itoc(CPnumndcols(cd,ndc),
     :      Dbuf(CHP_S_NUMDIM:CHP_E_NUMDIM), nchar)
            Dbuf(CHP_S_1ICOL:CHP_E_1ICOL) = CP1icol(cd,ndc)
            Dbuf(CHP_S_2ICOL:CHP_E_2ICOL) = CP2icol(cd,ndc)
            Dbuf(CHP_S_3ICOL:CHP_E_3ICOL) = CP3icol(cd,ndc)
            call chr_itoc(CPnddatelm(cd,ndc),
     :       Dbuf(CHP_S_ILMD:CHP_E_ILMD), nchar)
            call fio_write(fdd, Dbuf, status)
            call chr_fill(' ',Dbuf)
          endif
        enddo
*
*    'N' is used to record the alternative sort order
*
*   Alternative sort information
*
        do nas = 1, CPnofasorts(cd)
          if ( CPasorig(cd,nas)) then
*
*   Number of sort fileds
*   Primary Sort field
*   Primary Sort Direction
*
            Dbuf(CHP_S_KEY:CHP_E_KEY) = 'N'
            call chr_itoc(CPnumascols(cd,nas),
     :      Dbuf(CHP_S_NSORT:CHP_E_NSORT), nchar)
            Dbuf(CHP_S_FDSCOL:CHP_E_FDSCOL) = CPfascol(cd,nas)
            Dbuf(CHP_S_FDSDIR:CHP_E_FDSDIR) = CPfasdir(cd,nas)
            Dbuf(CHP_S_SDSCOL:CHP_E_SDSCOL) = CPsascol(cd,nas)
            Dbuf(CHP_S_SDSDIR:CHP_E_SDSDIR) = CPsasdir(cd,nas)
            Dbuf(CHP_S_TDSCOL:CHP_E_TDSCOL) = CPtascol(cd,nas)
            Dbuf(CHP_S_TDSDIR:CHP_E_TDSDIR) = CPtasdir(cd,nas)
            call chr_itoc(CPasdatelm(cd,nas),
     :       Dbuf(CHP_S_DSLMD:CHP_E_DSLMD), nchar)
            call fio_write(fdd, Dbuf, status)
            call chr_fill(' ',Dbuf)
          endif
        enddo
*
*
*    'P' is used to record the general catalogue information
*
*    Catalogue name
*    Catalogue Delete Indicator.
*    Catalogue Frequency Flag.
*    Catalogue access Flag.
*
        Dbuf(CHP_S_KEY:CHP_E_KEY) = 'P'
        Dbuf(CHP_S_CATNAME:CHP_E_CATNAME) = CPname(cd)
        if (CPdelind(cd)) then
          Dbuf(CHP_S_CATDELIND:CHP_E_CATDELIND) = 'T'
        else
          Dbuf(CHP_S_CATDELIND:CHP_E_CATDELIND) = 'F'
        endif
        call fio_write(fdd, Dbuf, status)
        call chr_fill(' ',Dbuf)
*
        call fio_close(fdd,status)
      endif
*
*
*
*
*   Changes in the underlying CHI catalogue were made when the
*   requested.
*
      END
