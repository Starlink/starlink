      SUBROUTINE
     : CHI_SEARCH( INPUT, OUTPUT, CRITERIA, STATUS )
*+
*  Name:
*     CHI_SEARCH

*  Purpose:
*     SEARCH a catalogue for entries that meet a criteria.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHI_SEARCH( INPUT, OUTPUT, CRITERIA, STATUS )
*
*  Description:
*     Create a new catalogue containing only entries that meet the given
*     criteria. If an invalid expression error is reported CRITERIA
*     is returned containing diagnostic information.

*  Arguments:
*     INPUT = CHARACTER * ( CHI__SZNAME ) (Given)
*        Name of the catalogue from which the entries are to be selected.
*     OUTPUT = CHARACTER * ( CHI__SZNAME ) (Given)
*        Name of the new catalogue containing only the selected entries.
*     CRITERIA = CHARACTER * ( CHI__SZEXP ) (Given)
*        Criteria to be applied to each entry in the input catalogue to
*        determine if this entry is to be copied into the output catalogue.
*     STATUS = INTEGER (Given and Returned)
*        Global status.

*  Notes:
*     Searching a catalogue entry by entry is very inefficient and applications
*     that use this routine will run very slowly when CHI_SEARCH has to use
*     method of searching.
*
*     The input and output catalogues are RESET to their first entries on
*     exit from this routine.


*  Anticipated Errors:
*     CHI__CATNOTFND
*     CHI__IVLDEXP

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
      INCLUDE 'CHI_PAR'   ! Standard CHI constants
      INCLUDE 'CHI_ERR'   ! Standard CHI errors

*  Arguments Given:
      CHARACTER * ( * ) INPUT
      CHARACTER * ( * ) OUTPUT
      CHARACTER * ( * ) CRITERIA

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( 3 ) DBNAME ! Database name
      CHARACTER * ( CHI__SZNAME ) CATNAME ! Catalogue name.
      CHARACTER * ( 3 ) OUTDBNAME ! Output database name
      CHARACTER * ( CHI__SZNAME ) OUTCATNAME ! Output catalogue name.

*.

*  Check inherited global status.
*
      IF ( STATUS .NE. SAI__OK ) RETURN
*
*    Parsing the criteria has two effects. It checks the criteria is
*    a valid CHI expression and it eliminates any sexagesimal formats
*    as part of the CONVERT function
*
*    (Not required CHI_XSEARCH used.)
*      call chi_1par(input, criteria, fnames, ftypes, status)
*
*    Split the input name into database part and catname part.
*
      call chi_splitname(input, dbname, catname, status)
*
*    Split the output name into database part and catname part.
*
      call chi_splitname(output, outdbname, outcatname, status)
*
*    If the input and output database system is the same then
*      call SEARCH on the appropriate low level system
*    else
*      call the more basic CHI_XSEARCH.
*
      if (dbname .eq. outdbname) then
*
        if (dbname .eq. 'BIN') then
           call chi_xsearch( input, output, criteria, status)
*
        elseif (dbname .eq. 'HDS') then
           call chi_xsearch( input, output, criteria, status)
        elseif (dbname .eq. 'CDF') then
           call chi_xsearch( input, output, criteria, status)
        elseif (dbname .eq. 'FIT') then
           call chi_xsearch( input, output, criteria, status)
        elseif (dbname .eq. 'REX') then
           call chi_xsearch( input, output, criteria, status)
        endif
      else
        call chi_xsearch( input, output, criteria, status)
      endif
*
      END
