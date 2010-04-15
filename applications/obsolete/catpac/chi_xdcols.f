      SUBROUTINE
     : CHI_XDCOLS( INPUT, NUMCOLS, CNAMES, STATUS )
*+
*  Name:
*     CHI_XDCOLS

*  Purpose:
*     Delete given columns from a catalogue.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHI_XDCOLS( INPUT, NUMCOLS, CNAMES, STATUS )
*
*  Description:
*     Delete given columns from a catalogue. If the column does not exist in
*     the catalogue no error is reported.

*  Arguments:
*     INPUT = CHARACTER * ( CHI__SZNAME ) (Given)
*        Name of the catalogue.
*     NUMCOLS = INTEGER (Given)
*        Number of columns to be deleted.
*     CNAMES( CHI__NUMCOLS ) = CHARACTER * ( CHI__SZCNAME ) (Given)
*        Name of the column to be deleted.
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
      INCLUDE 'SAE_PAR'   ! Standard SAE constants
      INCLUDE 'CHI_PAR'   ! Standard CHI constants
      INCLUDE 'CHI_ERR'   ! Standard CHI errors

*  Arguments Given:
      CHARACTER * ( * ) INPUT
      INTEGER NUMCOLS
      CHARACTER * ( CHI__SZCNAME ) CNAMES( * )

*  Status:
      INTEGER STATUS             ! Global status


*  Local Variables:
      INTEGER CATNUMCOLS
      CHARACTER * ( CHI__SZNAME ) OUTPUT
      CHARACTER * ( CHI__SZCNAME ) CATCNAMES( CHI__NUMCOLS )
      CHARACTER * ( CHI__SZCFMT ) CFORMATS( CHI__NUMCOLS )
      CHARACTER * ( 1 ) CTYPES( CHI__NUMCOLS )
      CHARACTER * ( CHI__SZCUNIT ) CUNITS( CHI__NUMCOLS )
      CHARACTER * ( CHI__SZCCMT ) CCOMMENTS( CHI__NUMCOLS )
      LOGICAL MDATAACC( CHI__NUMCOLS )
      LOGICAL DATAACC( CHI__NUMCOLS )
      INTEGER COLCOUNT
      LOGICAL MARKCOPY( CHI__NUMCOLS )
      INTEGER DELCOLC
      INTEGER NEWCOLC
      CHARACTER * ( CHI__SZCNAME ) NEWCNAMES( CHI__NUMCOLS )
      CHARACTER * ( CHI__SZCFMT ) NEWCFORMATS( CHI__NUMCOLS )
      CHARACTER * ( CHI__SZCUNIT ) NEWCUNITS( CHI__NUMCOLS )
      CHARACTER * ( CHI__SZCCMT ) NEWCCOMMENTS( CHI__NUMCOLS )
      INTEGER NUMENTS
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
      CHARACTER * ( 1 ) NEWCOLTYPES( CHI__NUMCOLS )
      LOGICAL NEWNULLS ( CHI__NUMCOLS )
      CHARACTER * ( 3 ) DBNAME
      CHARACTER * ( CHI__SZNAME ) CATNAME

*.

*  Check inherited global status.
*
      IF ( STATUS .NE. SAI__OK ) RETURN
*
*   Get all the information about the columns.
*
      call chi_gallcd(input, catnumcols, catcnames, cformats, ctypes,
     :  cunits, ccomments, mdataacc, dataacc, status)
*
*    Initialize a mark for copy array.
*
      do colcount = 1, catnumcols
        markcopy(colcount) = .TRUE.
      enddo
*
*   Check the names for those columns to be deleted, not copied.
*
      do delcolc = 1, numcols
        do colcount = 1, catnumcols
          if (cnames(delcolc) .EQ. catcnames(colcount)) then
             markcopy(colcount) = .FALSE.
          endif
        enddo
      enddo
*
*    Prepare to create a new catalogue.
*
      newcolc = 0
      do colcount = 1, catnumcols
        if (markcopy(colcount)) then
           newcolc = newcolc + 1
           newcnames(newcolc) = catcnames(colcount)
           newcformats(newcolc) = cformats(colcount)
           newcunits(newcolc) = cunits(colcount)
           newccomments(newcolc) = ccomments(colcount)
        endif
      enddo
*
*    Create a catalogue with no entries and excluding the deleted columns.
*
      call chi_gnents(input, numents, status)
      call chi_splitname(input, dbname, catname, status)
      output = dbname//'TEMPZ'
      call chi_crecat(output, numents, newcolc, newcnames,
     :  newcformats, newcunits, newccomments, status)
*
*    Loop through the input catalogue until the end reading the data and
*    copying the data.
*
        do entscount = 1, numents
*
          call chi_gdnac(input,catcnames,catnumcols,catcharvals,
     :      catdoubvals, catintvals, catlogvals, catrealvals,
     :      catcoltypes, catnulls, status )
*
*   Copy the data for the columns not deleted.
*
          newcolc = 0
          do colcount = 1, catnumcols
            if (markcopy(colcount)) then
              newcolc = newcolc + 1
              newcnames(newcolc) = catcnames(colcount)
              newcharvals(newcolc) = catcharvals(colcount)
              newdoubvals(newcolc) = catdoubvals(colcount)
              newintvals(newcolc) = catintvals(colcount)
              newlogvals(newcolc) = catlogvals(colcount)
              newrealvals(newcolc) = catrealvals(colcount)
              newcoltypes(newcolc) = catcoltypes(colcount)
              newnulls(newcolc) = catnulls(colcount)
            endif
          enddo
          call chi_putent( output, newcnames, newcolc, 1 , newcharvals,
     :    newdoubvals, newintvals, newlogvals, newrealvals,
     :    newcoltypes, newnulls, status )
       enddo
*
*  Copy the catalogue back to the input.
*
      call chi_delcat( input, status )
      call chi_copcat( output, input, status )
      call chi_delcat( output, status )
*
      end

