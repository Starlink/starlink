      SUBROUTINE
     : CHP_GONECD(INPUT, NAME, FORMAT, UNITS, COMMENT, PREFDIS,
     : COLTYPE, COLDES, ARRSHP, ARRDIM, ASSERT, ASSEXP, DOMCHK,
     : MDATAACC, DATAACC, DATELM, VCFLAG, VCEXP, DELIND, FMATFLAG,
     : STATUS)
*+
*  Name:
*     CHP_GONECD

*  Purpose:
*     Get ONE Columns Details.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHP_GONECD(INPUT, NAME, FORMAT, UNITS, COMMENT, PREFDIS,
*     COLTYPE, COLDES, ARRSHP, ARRDIM, ASSERT, ASSEXP, DOMCHK, MDATAACC,
*     DATAACC, DATELM, VCFLAG, VCEXP, DELIND, FMATFLAG, STATUS)

*  Description :
*     Get the information about a column in a catalogue. A column has associated
*     with it a name, format, units, comment, preferred display flag, type,
*     column designator, array shape, array dimensions, assert flag, assert
*     expression, domain check flag, access information, date last modified,
*     virtual column flag, virtual column expression, delete indicator,
*     standard format flag. For any column some of these will have no
*     meaning.
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
*     NAME = CHARACTER * ( CHP__SZCNAME ) (Given)
*        Name of the column.
*     FORMAT = CHARACTER * ( CHP__SZCFMT ) (Returned)
*        Format of the column.
*     UNITS = CHARACTER * ( CHI__SZCUNIT ) (Returned)
*        Units of the column.
*     COMMENT = CHARACTER * ( CHI__SZCCMT ) (Returned)
*        Comments of the column.
*     PREFDIS = LOGICAL (Returned)
*        Preferred display flag. TRUE for display.
*     COLTYPE = CHARACTER * ( 1 ) (Returned)
*        Type of the column.
*     COLDES  = INTEGER (Given)
*        Column designator.
*     ARRSHP  = INTEGER (Given)
*        Array shape of the column.
*     ARRDIM( 7 )  = INTEGER (Given)
*        Array dimensions.
*     ASSERT = LOGICAL (Returned)
*        Assert flag. TRUE if an assert expresion is present.
*     ASSEXP = CHARACTER * ( CHI__SZCEXP ) (Returned)
*        Assert expression.
*     DOMCHK = LOGICAL (Returned)
*        Domain checking flag. TRUE if domain checking is active.
*     MDATAACC = LOGICAL (Returned)
*        Access to metadata associated with the column.
*     DATAACC = LOGICAL (Returned)
*        Access to data associated with the column.
*     DATELM  = INTEGER (Given)
*        Date that the column was last modified.
*     VCFLAG = LOGICAL (Returned)
*        Virtual column flag. TRUE for a virtual column.
*     VCEXP = CHARACTER * ( CHI__SZCEXP ) (Returned)
*        Virtual column expressions.
*     DELIND = LOGICAL (Returned)
*        Delete indicator flag. TRUE if the column can be deleted.
*     FMATFLAG = LOGICAL (Returned)
*        Format flag. TRUE if a non standard format is present.
*     STATUS = INTEGER (Given and Returned)
*        Global status.

*  Anticipated Errors:
*     CHP__CATNOTFND

*  Authors:
*     ARW: Alan R Wood (STARLINK)
*     {enter_new_authors_here}

*  History:
*     26-Oct-1993 (ARW):
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
      CHARACTER * ( * ) NAME

*  Arguments Returned:
      CHARACTER * ( * ) FORMAT
      CHARACTER * ( 1 ) COLTYPE
      CHARACTER * ( * ) UNITS
      CHARACTER * ( * ) COMMENT
      INTEGER COLDES
      INTEGER ARRSHP
      INTEGER ARRDIM( * )
      LOGICAL MDATAACC
      LOGICAL DATAACC
      INTEGER DATELM
      LOGICAL VCFLAG
      CHARACTER * ( * ) VCEXP
      LOGICAL DELIND
      LOGICAL PREFDIS
      LOGICAL ASSERT
      CHARACTER * ( * ) ASSEXP
      LOGICAL DOMCHK
      LOGICAL STRUCT
      LOGICAL FMATFLAG

*  Status:
      INTEGER STATUS             ! Global status

*  External references :

*  Local variables :
      integer cd   ! catalogue descriptor.
      integer cc   ! column count
      integer nvcc ! number of dependent columns count
      integer asc  ! array shape count
      integer numcols  ! number of columns in the catalogue

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
        coltype = EPtype(cd,cc)
        format = EPformat(cd,cc)
        units = EPunits(cd,cc)
        comment = EPcomment(cd,cc)
        mdataacc = EPmdataacc(cd,cc)
        dataacc = EPdataacc(cd,cc)
        datelm = EPdatelm(cd,cc)
        vcflag = EPvcflag(cd,cc)
        vcexp = EPvcexp(cd,cc)
        delind = EPdelind(cd,cc)
        prefdis = EPprefdis(cd,cc)
        arrshp = EParrshp(cd,cc)
        if (arrshp .ge. 1) then
          do asc = 1, arrshp
            arrdim(asc) = EParrdim(cd,cc,asc)
          enddo
        endif
*        arrtyp = EParrtype(cd,cc)
        assert = EPassert(cd,cc)
        assexp = EPassexp(cd,cc)
        domchk = EPdomchk(cd,cc)
*        cstruct = EPstruct(cd,cc)
        fmatflag = EPnsflag(cd,cc)
        if (fmatflag) then
           format = EPformat(cd,cc)
        endif
      enddo
*
      END
