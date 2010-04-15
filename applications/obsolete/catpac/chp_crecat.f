      SUBROUTINE
     : CHP_CRECAT( INPUT, ESTNUMENTS, NUMCOLS, NAMES, FORMATS,
     : UNITS, COMMENTS, PREFDIS, COLDES, ARRSHP, ARRDIM,
     : ASSERT, ASSEXP, DOMCHK, STATUS)
*+
*  Name:
*     CHP_CRECAT

*  Purpose:
*     CREate a new CATalogue.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHP_CRECAT( INPUT, ESTNUMENTS, NUMCOLS, NAMES, FORMATS,
*     UNITS, COMMENTS, PREFDIS, COLDES, ARRSHP, ARRDIM,
*     ASSERT, ASSEXP, DOMCHK, STATUS)
*
*  Description:
*     Creates a new regular catalogue.
*     The CHP routines that write data into this catalogue will be more
*     efficient if you can provide a good estimate for the size of the
*     catalogue. (The number of entries).
*
*     The catalogue created contains no entries, no
*     virtual columns, no parameters and no catalogue notes.
*     These can be added afterwards.
*
*     Each column consists of a name and some combination of the following:-
*     format (standard or non standard), units, comment, preferred display,
*     column designator, array
*     shape, array dimensions, assert flag, assert expression and domain check.
*
*     COLDES(i) is the column designator.
*
*     COLDES(i) = 1 This is a scalar column, if it is a character string it
*     is CHP__SZCVAL long. ARRSHP, ARRDIM are ignored.
*
*     COLDES(i) = 2 This is a structure column. FORMATS(i), UNITS(i),
*     ARRSHP(i), ARRDIM(i), ASSERT(i), ASSEXP(i) and DOMCHK(i) are all ignored.
*
*     COLDES(i) = 3 This is an array column whose size is the same for all
*     entries. Character string are always CHP__SZCVAL long. ARRSHP(i) and
*     ARRDIM(i) give the shape, dimensions of the array respectively.
*     ASSERT(i), ASSEXP(i) and DOMCHK(i) are all ignored.
*
*     COLDES(i) = 4 This is an array whose shape and dimensions may vary from
*     entry to entry. Character string are always CHP__SZCVAL long.
*     ARRSHP(i), ARRDIM(i), ASSERT(i), ASSEXP(i) and DOMCHK(i) are all ignored.
*
*     COLDES(i) = 5 This is a scalar column. It is a variable length character
*     string. FORMATS(i), ARRSHP(i), ARRDIM(i), ASSERT(i), ASSEXP(i) and
*     DOMCHK(i) are all ignored.
*
*     COLDES(i) = 6 This is an array column of character string whose shape,
*     dimensions and string length may vary from entry to entry.
*     FORMATS(i), ARRSHP(i), ARRDIM(i), ASSERT(i), ASSEXP(i) and
*     DOMCHK(i) are all ignored.

*  Arguments:
*     INPUT = CHARACTER * ( CHP__SZNAME ) (Given)
*        Name of the catalogue being created.
*     ESTNUMENTS = INTEGER (Given)
*        Estimate for the number of entries that will be put into the catalogue.
*     NUMCOLS = INTEGER (Given)
*        Number of columns in the catalogue.
*     NAMES( CHP__NUMCOLS ) = CHARACTER * ( CHP__SZCNAME ) (Given)
*        Names of the columns in the catalogue.
*     FORMATS( CHP__NUMCOLS ) = CHARACTER * ( CHP__SZCFMT ) (Given)
*        Formats of the columns in the catalogue.
*     UNITS( CHP__NUMCOLS ) = CHARACTER * ( CHP__SZCUNIT ) (Given)
*        Units of the columns in the catalogue.
*     COMMENTS( CHP__NUMCOLS ) = CHARACTER * ( CHP__SZCCMT ) (Given)
*        Comments associated with the columns in the catalogue.
*     PREFDIS( CHP__NUMCOLS ) = LOGICAL (Given)
*        Preferred display flag for the columns in the catalogue.
*     COLDES( CHP__NUMCOLS ) = INTEGER (Given)
*        The column designation.
*     ARRSHP( CHP__NUMCOLS ) = INTEGER (Given)
*        Array shape of the columns in the catalogue.
*     ARRDIM( CHP__NUMCOLS, 7 ) = INTEGER (Given)
*        Array dimensions of the columns in the catalogue.
*     ASSERT( CHP__NUMCOLS ) = LOGICAL (Given)
*        Assert flags for the columns in the catalogue.
*     ASSEXP( CHP__NUMCOLS ) = CHARACTER * ( CHP__SZASSEXP ) (Given)
*        Assert expression for columns in the catalogue.
*     DOMCHK( CHP__NUMCOLS ) = LOGICAL (Given)
*        Domain check flags for the columns in the catalogue.
*     STATUS = INTEGER (Given and Returned)
*        Global status.
*
*  Notes:
*     If the format is invalid and invalid column format error will be reported.
*
*     If the catalogue can not be created an insufficient privilege to create
*     error will be reported.

*  Anticipated Errors:
*     CHP__IVLDCFMT
*     CHP__INSPRIVCRE

*  Authors:
*     ARW: Alan R Wood (FIIS/RAL)
*     {enter_new_authors_here}

*  History:
*     1_OCT_1993 (ARW):
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
      INCLUDE 'CHI_PAR'   ! Standard CHI constants

*  Global Variables:
      INCLUDE 'CHP_CMN'   ! Standard CHP common area.

*  Arguments Given:
      CHARACTER * ( * ) INPUT
      INTEGER ESTNUMENTS
      INTEGER NUMCOLS
      CHARACTER * ( * ) NAMES( * )
      CHARACTER * ( * ) FORMATS( * )
      CHARACTER * ( * ) UNITS( * )
      CHARACTER * ( * ) COMMENTS( * )
      LOGICAL PREFDIS( * )
      INTEGER COLDES( * )
      LOGICAL ARRSHP( * )
      INTEGER ARRDIM( CHP__NUMCOLS,7 )
      LOGICAL ASSERT( * )
      CHARACTER * ( * ) ASSEXP( * )
      LOGICAL DOMCHK( * )

*  Status:
      INTEGER STATUS             ! Global status

*  External references:
      INTEGER CHR_LEN
      LOGICAL CHR_SIMLR

*  Local Variables:
      INTEGER COLCOUNT ! Column counter
      INTEGER CHID ! Column counter
      INTEGER ED ! Descriptor.
      INTEGER CD ! Catalogue Descriptor.
      INTEGER COUNT ! Catalogue Descriptor.
      INTEGER COUNT1 ! Catalogue Descriptor.
      INTEGER COUNT2 ! Catalogue Descriptor.
      CHARACTER * ( CHP__SZCFMT ) TEMPFORMAT
      CHARACTER * ( CHP__SZCVAL ) TEMPCHARVAL
      LOGICAL SEXFLAG
      LOGICAL NSFLAG
      INTEGER CHINUMCOLS
      CHARACTER * ( CHI__SZCNAME ) TEMPNAME
      CHARACTER * ( CHI__SZCNAME ) CHINAMES( CHP__NUMCOLS )
      CHARACTER * ( CHI__SZCFMT ) CHIFORMATS( CHP__NUMCOLS )
      CHARACTER * ( CHI__SZCUNIT ) CHIUNITS( CHP__NUMCOLS )
      CHARACTER * ( CHI__SZCCMT ) CHICOMMENTS( CHP__NUMCOLS )
      CHARACTER * ( CHI__SZCFMT ) CHIFORMAT

*.
*  Check inherited global status.
*
      IF ( STATUS .NE. SAI__OK ) RETURN
*
      call chp_getncd(input, cd, status)
      count = cd
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
      CPname(cd) = input
      CPnumcols(cd) = numcols
      do colcount = 1, numcols
        EPname(cd,colcount) = names(colcount)
        EPformat(cd,colcount) = formats(colcount)
        EPunits(cd,colcount) = units(colcount)
        EPcomment(cd,colcount) = comments(colcount)
        EPprefdis(cd,colcount) = prefdis(colcount)
        EPcoldes(cd,colcount) = coldes(colcount)
        EPassert(cd,colcount) = assert(colcount)
        EPassexp(cd,colcount) = assexp(colcount)
        EPdomchk(cd,colcount) = domchk(colcount)
        EParrshp(cd,colcount) = arrshp(colcount)
*
        chinames(colcount) = names(colcount)
        chiformats(colcount) = formats(colcount)
        chiunits(colcount) = units(colcount)
        chicomments(colcount) = comments(colcount)

        call chp_nsfmat(formats(colcount),nsflag,chiformat,status)

        if (nsflag) then
            EPnsflag(cd,colcount) = .TRUE.
            EPnsformat(cd,colcount) = formats(colcount)
            chiformats(colcount) = chiformat
        else
            EPnsflag(cd,colcount) = .FALSE.
        endif
      enddo
*
      call chi_crecat( INPUT, ESTNUMENTS, NUMCOLS, chinames, chiformats,
     : chiunits, chicomments, status)
*
      end
