      SUBROUTINE
     :  CHP_GALLCD(INPUT, NUMCOLS, NAMES, FORMATS, UNITS,
     : COMMENTS, PREFDIS, COLTYPES, COLDES, ARRSHP, ARRDIM, ASSERT,
     : ASSEXP, DOMCHK, MDATAACC, DATAACC, DATELM, VCFLAG,
     : VCEXP, DELIND, FMATFLAG, STATUS)
*+
*  Name:
*     CHP_GALLCD

*  Purpose:
*     Get ALL Columns Details.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHP_GALLCD(INPUT, NUMCOLS, NAMES, FORMATS, UNITS,
*     COMMENTS, PREFDIS, COLTYPES, COLDES, ARRSHP, ARRDIM, ASSERT,
*     ASSEXP, DOMCHK, MDATAACC, DATAACC, DATELM, VCFLAG,
*     VCEXP, DELIND, FMATFLAG, STATUS)
*
*  Description :
*     Get the information about all the columns. Each column has associated
*     with it a name, format, units, comment, preferred display flag, type,
*     column designator, arrary shape, array dimensions, assert flag, assert
*     expression, domain check flag, data access information, date last
*     modified, virtual column flag, virtual column expression, delete
*     indicator and format flag. For any column
*     some of these will have no meaning.
*
*     COLDES is the column designator which allows the column to be interpreted
*     correctly.
*
*     COLDES = 1 This is a scalar column, if it is a character string it
*     is CHP__SZCVAL long. VCEXP and ASSEXP may have no meaning. ARRSHP and
*     ARRDIM have no meaning.
*
*     COLDES = 2 This is a structure column. FORMAT,
*     UNITS, ARRSHP, ARRDIM, ASSERT, ASSEXP, DOMCHK, VCFLAG, VCEXP
*     and FMATFLAG have no meaning.
*
*     COLDES = 3 This is an array column whose size is the same for all
*     entries. Character string are always CHP__SZCVAL long.
*     ASSERT, ASSEXP, DOMCHK, VCFLAG, VCEXP and FMATFLAG have no meaning.
*
*     COLDES = 4 This is an array whose shape and dimensions may vary from
*     entry to entry. Character string are always CHP__SZCVAL long.
*     ARRSHP, ARRDIM, ASSERT, ASSEXP, DOMCHK, VCFLAG, VCEXP and FMATFLAG have
*     no meaning.
*
*     COLDES = 5 This is a scalar column. It is a variable length character
*     string. FORMAT, ARRSHP, ARRDIM, ASSERT, ASSEXP, DOMCHK, VCFLAG, VCEXP
*     and FMATFLAG have no meaning.
*
*     COLDES = 6 This is an array column of character string whose shape,
*     dimensions and string length may vary from entry to entry.
*     FORMAT, ARRSHP, ARRDIM, ASSERT, ASSEXP, DOMCHK, VCFLAG, VCEXP
*     and FMATFLAG have no meaning.
*
*  Arguments:
*     INPUT = CHARACTER * ( CHP__SZNAME ) (Given)
*        Name of the catalogue from which the column information is required.
*     NUMCOLS = INTEGER (Returned)
*        Number of columns in the catalogue.
*     NAMES( CHP__NUMCOLS ) = CHARACTER * ( CHP__SZCNAME ) (Returned)
*        Names of the columns in the catalogue.
*     FORMATS( CHP__NUMCOLS ) = CHARACTER * ( CHP__SZCFMT ) (Returned)
*        Formats of the columns in the catalogue.
*     UNITS( CHP__NUMCOLS ) = CHARACTER * ( CHP__SZCUNIT ) (Returned)
*        Units of the columns in the catalogue.
*     COMMENTS( CHP__NUMCOLS ) = CHARACTER * ( CHP__SZCCMT ) (Returned)
*        Comments of the columns in the catalogue.
*     PREFDIS( CHP__NUMCOLS ) = LOGICAL (Returned)
*        Preferred display flags. TRUE for display.
*     COLTYPES( CHP__NUMCOLS ) = CHARACTER * ( 1 ) (Returned)
*        Types of the columns in the catalogue.
*     COLDES( CHP__NUMCOLS )  = INTEGER (Given)
*        Column designators.
*     ARRSHP( CHP__NUMCOLS ) = INTEGER (Returned)
*        Array shapes.
*     ARRDIM( CARRSHP, CHP__NUMCOLS ) = INTEGER (Returned)
*        Array dimensions
*     ASSERT( CHP__NUMCOLS ) = LOGICAL (Returned)
*        Assert flags. TRUE if an assert expresion is present.
*     ASSEXP( CHP__NUMCOLS ) = CHARACTER * ( CHP__SZCEXP ) (Returned)
*        Assert expressions.
*     DOMCHK( CHP__NUMCOLS ) = LOGICAL (Returned)
*        Domain checking flags. TRUE if domain checking is active.
*     MDATAACC( CHP__NUMCOLS ) = LOGICAL (Returned)
*        Access to metadata associated with the columns in the catalogue.
*     DATAACC( CHP__NUMCOLS ) = LOGICAL (Returned)
*        Access to data associated with the columns in the catalogue.
*     DATELM( CHP__NUMCOLS )  = INTEGER (Given)
*        Date that columns were last modified.
*     VCFLAG( CHP__NUMCOLS ) = LOGICAL (Returned)
*        Virtual column flags. TRUE for a virtual column.
*     VCEXP( CHP__NUMCOLS ) = CHARACTER * ( CHP__SZCEXP ) (Returned)
*        Virtual column expressions.
*     DELIND( CHP__NUMCOLS ) = LOGICAL (Returned)
*        Delete indicator flags. TRUE if the column can be deleted.
*     FMATFLAG( CHP__NUMCOLS ) = LOGICAL (Returned)
*        Format flags. TRUE if a non standard format is present.
*     STATUS = INTEGER (Given and Returned)
*        Global status.

*  Anticipated Errors:
*     CHP__CATNOTFND

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

*  Global Variabless:
      INCLUDE 'CHP_CMN'   ! Standard CHP commons

*  Arguments Given:
      CHARACTER * ( * ) INPUT

*  Arguments Returned:
      INTEGER NUMCOLS
      CHARACTER * ( * ) NAMES( * )
      CHARACTER * ( * ) FORMATS( * )
      CHARACTER * ( 1 ) COLTYPES( * )
      CHARACTER * ( * ) UNITS( * )
      CHARACTER * ( * ) COMMENTS( * )
      INTEGER COLDES( * )
      LOGICAL MDATAACC( * )
      LOGICAL DATAACC( * )
      INTEGER DATELM( * )
      LOGICAL VCFLAG( * )
      CHARACTER * ( * ) VCEXP( * )
      LOGICAL DELIND( * )
      LOGICAL PREFDIS( * )
      INTEGER ARRSHP( * )
      INTEGER ARRDIM( CHP__NUMCOLS,* )
      LOGICAL ASSERT( * )
      CHARACTER * ( * ) ASSEXP( * )
      LOGICAL DOMCHK( * )
      LOGICAL FMATFLAG( * )

*  Status:
      INTEGER STATUS             ! Global status

*  External references :

*  Local variables :
      integer cd   ! catalogue descriptor.
      integer cc   ! column count
      integer nvcc ! number of dependent columns count
      integer asc  ! array shape count

*.

*  Check inherited global status.
*
      IF ( STATUS .NE. SAI__OK ) RETURN
*
*  Get the CD for the catalogue.
*
      call chp_getcd(input, .FALSE., cd, status)
*
*  Copy the contents of the common area
*
      numcols = CPnumcols(cd)
      do cc = 1, numcols
        coltypes(cc) = EPtype(cd,cc)
        names(cc) = EPname(cd,cc)
        formats(cc) = EPformat(cd,cc)
        units(cc) = EPunits(cd,cc)
        comments(cc) = EPcomment(cd,cc)
        mdataacc(cc) = EPmdataacc(cd,cc)
        dataacc(cc) = EPdataacc(cd,cc)
        datelm(cc) = EPdatelm(cd,cc)
        vcflag(cc) = EPvcflag(cd,cc)
        vcexp(cc) = EPvcexp(cd,cc)
        delind(cc) = EPdelind(cd,cc)
        prefdis(cc) = EPprefdis(cd,cc)
        arrshp(cc) = EParrshp(cd,cc)
        if (arrshp(cc) .ge. 1) then
          do asc = 1, arrshp(cc)
            arrdim(cc,asc) = EParrdim(cd,cc,asc)
          enddo
        endif
*        carrtyp(cc) = EParrtype(cd,cc)
        assert(cc) = EPassert(cd,cc)
        assexp(cc) = EPassexp(cd,cc)
        domchk(cc) = EPdomchk(cd,cc)
*        cstruct(cc) = EPstruct(cd,cc)
        fmatflag(cc) = EPnsflag(cd,cc)
        if (fmatflag(cc)) then
          formats(cc) = EPnsformat(cd,cc)
        endif
      enddo
*
      END
