      SUBROUTINE
     : CHI_GDNAC( INPUT, CNAMES, NUMCOLS, CHARVALS, DOUBVALS,
     : INTVALS, LOGVALS, REALVALS, COLTYPES, NULLS, STATUS )
*+
*  Name:
*     CHI_GDNAC

*  Purpose:
*     Get Data from the Next entry All Columns.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHI_GDNAC( INPUT, CNAMES, NUMCOLS, CHARVALS, DOUBVALS,
*     INTVALS, LOGVALS, REALVALS, COLTYPES, NULLS, STATUS )
*
*  Description:
*     Get all the data from the next entry in a catalogue. Get data from all
*     the columns in the catalogue. The data is taken from the next entry in the
*     catalogue.
*     Data is returned in the appropriate element of the appropriate
*     array. CNAMES will contain the names of the columns and COLTYPES the
*     type of column. So if COLTYPES(3) is an 'I' the array INTVALS(3) will
*     contain the data from the column whose name is given in CNAMES(3).

*  Arguments:
*     INPUT = CHARACTER * ( CHI__SZNAME ) (Given)
*        Name of the catalogue from which the data is required.
*     CNAMES( CHI__NUMCOLS ) = CHARACTER * ( CHI__SZCNAME ) (Returned)
*        Names of the fields whose data is required.
*     NUMCOLS = INTEGER (Returned)
*        Number of columns whose data is required.
*     INTVALS( CHI__NUMCOLS ) = INTEGER (Returned)
*        Array to receive the data from integer columns.
*     REALVALS( CHI__NUMCOLS ) = REAL (Returned)
*        Array to receive the data from real columns.
*     DOUBVALS( CHI__NUMCOLS ) = DOUBLE PRECISION (Returned)
*        Array to receive the data from double precision columns.
*     LOGVALS( CHI__NUMCOLS ) = LOGICAL (Returned)
*        Array to receive the data from logical columns.
*     CHARVALS( CHI__NUMCOLS ) = CHARACTER * ( CHI__SZCVAL ) (Returned)
*        Array to receive the data from character columns.
*     FLDTYPES( CHI__NUMCOLS ) = CHARACTER * ( 1 ) (Returned)
*        Array to receive the types of columns.
*     NULLS( CHI__NUMCOLS ) = LOGICAL (Returned)
*        Array to receive the null value flags for the columns.
*     STATUS = INTEGER (Given and Returned)
*        Global status.

*  Notes:
*     Caution should be exercised when using CHI_GDNAC. Several CHI routines
*     cause catalogues to be RESET to their first entry. They are CHI_COPCAT,
*     CHI_DCOLS, CHI_DENTS, CHI_ICOLS, CHI_IENTS, CHI_JOIN, CHI_RENAME,
*     CHI_RCOLS, CHI_RENTS, CHI_SEARCH, CHI_SORT. In addition to these a call
*     to CHI_GDNAC after a CHI_GSGNAC or CHI_GSDGSC cause the catalogue to be
*     RESET to their first entry.

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

*  Arguments Returned:
      CHARACTER * ( * ) CNAMES( * )
      INTEGER NUMCOLS
      INTEGER INTVALS ( * )
      REAL REALVALS ( * )
      DOUBLE PRECISION DOUBVALS ( * )
      LOGICAL LOGVALS ( * )
      CHARACTER * ( * ) CHARVALS( * )
      CHARACTER * ( 1 ) COLTYPES( * )
      LOGICAL NULLS ( * )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( 3 ) DBNAME ! Database name.
      CHARACTER * ( CHI__SZNAME ) CATNAME ! Catalogue name.
      INTEGER CD ! Catalogue Descriptor
      integer i
*.

*  Check inherited global status.
*
      IF ( STATUS .NE. SAI__OK ) RETURN
*
*    Split the input name into database part and catname part.
*
      call chi_splitname(input, dbname, catname, status)
*
*    Call GDNAC on the appropriate low level system
*
*
       if (dbname .eq. 'HDS') then
          call chi_getcd(dbname, catname, .FALSE., cd, status)
          call chi_hgdnac(cd, cnames, numcols, charvals,
     :    doubvals, intvals, logvals, realvals, coltypes,
     :    nulls, status)
*
*       elseif (dbname .eq. 'BIN') then
*          call chi_getcd(dbname, catname, .FALSE., cd, status)
*          call chi_bgdnac(cd, cnames, numcols, charvals,
*     :    doubvals, intvals, logvals, realvals, coltypes,
*     :    nulls, status)
*
*       elseif (dbname .eq. 'CDF') then
*          call chi_getcd(dbname, catname, .FALSE., cd, status)
*          call chi_cgdnac(cd, cnames, numcols, charvals,
*     :    doubvals, intvals, logvals, realvals, coltypes,
*     :    nulls, status)
*
*       elseif (dbname .eq. 'FIT') then
*          call chi_getcd(dbname, catname, .FALSE., cd, status)
*          call chi_fgdnac(cd, cnames, numcols, charvals,
*     :    doubvals, intvals, logvals, realvals, coltypes,
*     :    nulls, status)
*
*       elseif (dbname .eq. 'REX') then
*          call chi_getcd(dbname, catname, .FALSE., cd, status)
*          call chi_rgdnac(cd, cnames, numcols, charvals,
*     :    doubvals, intvals, logvals, realvals, coltypes,
*     :    nulls, status)
      endif
*
      END
