      SUBROUTINE
     : CHI_XREPOCD( INPUT, CNAME, CFORMAT, CUNIT, CCOMMENT, STATUS)
*+
*  Name:
*     CHI_XREPOCD

*  Purpose:
*     Replace one columns details.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHI_XREPOCD( INPUT, CNAME, CFORMAT,
*     CUNIT, CCOMMENT, STATUS )
*
*  Description:
*     Replace one columns details. The name and type of a column can
*     never change but a column can be given a new format (consistent
*     with the type), units and comment.

*  Arguments:
*     INPUT = CHARACTER (Given)
*        Name of the catalogue.
*     CNAME = CHARACTER * ( CHI__SZCNAME ) (Given)
*        Name of the column whose details are to be updated.
*     CFORMAT = CHARACTER * ( CHI__SZCFMT ) (Given)
*        New format for the column.
*     CUNIT = CHARACTER * ( CHI__SZCUNIT ) (Given)
*        New units for the column.
*     CCOMMENT = CHARACTER * ( CHI__SZCCMT ) (Given)
*        New comment for the column.
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
      CHARACTER * ( * ) CNAME
      CHARACTER * ( * ) CFORMAT
      CHARACTER * ( * ) CUNIT
      CHARACTER * ( * ) CCOMMENT

*  Arguments Returned:

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER CATNUMCOLS
      CHARACTER * ( CHI__SZNAME ) OUTPUT
      CHARACTER * ( CHI__SZCNAME ) CATCNAMES( CHI__NUMCOLS )
      CHARACTER * ( CHI__SZCFMT ) CATCFORMATS( CHI__NUMCOLS )
      CHARACTER * ( 1 ) CATCTYPES( CHI__NUMCOLS )
      CHARACTER * ( CHI__SZCUNIT ) CATCUNITS( CHI__NUMCOLS )
      CHARACTER * ( CHI__SZCCMT ) CATCCOMMENTS( CHI__NUMCOLS )
      LOGICAL CATCMDATAACC( CHI__NUMCOLS )
      LOGICAL CATCDATAACC( CHI__NUMCOLS )
      INTEGER COLCOUNT
      LOGICAL MARKCOPY( CHI__NUMCOLS )
      INTEGER DELCOLC
      INTEGER CATNUMENTS
      INTEGER ENTSCOUNT
      CHARACTER * ( CHI__SZCVAL ) CATCHARVALS( CHI__NUMCOLS )
      DOUBLE PRECISION CATDOUBVALS ( CHI__NUMCOLS )
      INTEGER CATINTVALS ( CHI__NUMCOLS )
      LOGICAL CATLOGVALS ( CHI__NUMCOLS )
      REAL CATREALVALS ( CHI__NUMCOLS )
      CHARACTER * ( 1 ) CATCOLTYPES( CHI__NUMCOLS )
      LOGICAL CATNULLS ( CHI__NUMCOLS )
      CHARACTER * ( CHI__SZCVAL ) NEWCHARVALS( CHI__NUMCOLS )
      DOUBLE PRECISION NEWDOUBVALS ( CHI__NUMCOLS )
      INTEGER NEWINTVALS ( CHI__NUMCOLS )
      LOGICAL NEWLOGVALS ( CHI__NUMCOLS )
      REAL NEWREALVALS ( CHI__NUMCOLS )
      LOGICAL NEWNULLS ( CHI__NUMCOLS )
      INTEGER PUTCOLCOUNT
      INTEGER TNUMCOLS
      CHARACTER * ( 3 ) DBNAME
      CHARACTER * ( CHI__SZCNAME ) CATNAME

*.

*  Check inherited global status.
*
      IF ( STATUS .NE. SAI__OK ) RETURN
*
*   Get all the information about the columns.
*
      call chi_gallcd(input, catnumcols, catcnames, catcformats,
     :  catctypes, catcunits, catccomments, catcmdataacc,
     :  catcdataacc, status)
*
*    Prepare to create a new catalogue.
*
      do colcount = 1, catnumcols
        if (cname .EQ. catcnames(colcount)) then
          catcformats(colcount) = cformat
          catcunits(colcount) = cunit
          catccomments(colcount) = ccomment
        endif
      enddo
*
      call chi_gnents(input, catnuments, status)
      call chi_splitname(input, dbname, catname, status)
      output = dbname//'TEMPZ'
      call chi_crecat(output, catnuments, catnumcols, catcnames,
     :  catcformats, catcunits, catccomments, status)
*
*    Loop through the input catalogue until the end reading the data and
*    copying the data.
*
      do entscount = 1, catnuments
*
        call chi_gdnac(input,catcnames,catnumcols,catcharvals,
     :      catdoubvals, catintvals, catlogvals, catrealvals,
     :      catcoltypes, catnulls, status )
*
        call chi_putent( output,catcnames,catnumcols,1,
     :   catcharvals,catdoubvals,catintvals,catlogvals,
     :   catrealvals,catcoltypes,catnulls,status )
      enddo
*
*  Copy the catalogue back to the input.
*
      call chi_delcat( input, status )
      call chi_copcat( output, input, status )
      call chi_delcat( output, status )
*
      end
