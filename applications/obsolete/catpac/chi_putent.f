      SUBROUTINE
     : CHI_PUTENT( INPUT, CNAMES, NUMCOLS, CHECK, CHARVALS, DOUBVALS,
     :             INTVALS, LOGVALS, REALVALS, COLTYPES, NULLFLAGS,
     :             STATUS )
*+
*  Name:
*     CHI_PUTENT

*  Purpose:
*     PUT an ENTry into a catalogue.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHI_PUTENT( INPUT, CNAMES, NUMCOLS, CHECK, CHARVALS, DOUBVALS,
*     INTVALS, LOGVALS, REALVALS, COLTYPES, NULLFLAGS, STATUS )
*
*  Description:
*     Add an entry to a catalogue. For each column name in CNAMES CHI_PUTENT
*     checks in respective position in COLTYPES to find the type of data.
*     The data for this column in the entry will be taken from the respective
*     element of the appropriate array unless the NULLFLAGS for this column is
*     TRUE indicating that the column is null. So if COLTYPES(3) is 'I' and
*     NULLFLAGS(3) is FALSE then the data
*     for the column whose name is given in CNAMES(3) will be taken from
*     INTVALS(3).
*
*     There are four levels of checking which are selected using the argument
*     CHECK. Modes 1 and 2 are efficient and should be used whenever possible.
*     You are advised not to change the level of checking when putting data
*     into a catalogue unless you are moving to a mode where more checking is
*     done
*
*     CHECK=1. The lowest level of checking. The routine processes the column
*     names given in CNAMES. Only if the column appears in the catalogue and
*     the type agrees with the that given in COLTYPES will data be put into
*     the column for this entry. All other columns take their null values. The
*     routine remembers where it found the data for each column (Eg. FLUX1 data
*     in REALVALS(5)). On subsequent calls the routine assumes that the FLUX1
*     value will be in REALVALS(5).
*
*     CHECK=2. The routine processes the column names given in CNAMES and only
*     if the all the columns in the catalogue are given in CNAMES and the
*     types are correct will the entry be put into the catalogue. If, during
*     checking, an error is found the error is reported and the name of the
*     offending column is returned in CNAMES(1). Again after
*     the first call the routine remembers where it found the data for each
*     column and on subsequent calls the routine assumes that the data will be
*     in the same place. This mode ensures that genuine data is put into the
*     catalogue. Columns cannot be overlooked.
*
*     CHECK=3. In this mode the routine processes the column names given in
*     CNAMES and their column types every time the routine is called. This
*     means that different columns can contribute to each entry. Only if
*     the column appears in the catalogue and
*     the type agrees with that given in COLTYPES will data be put into
*     the column for this entry. All other column take their null values.
*
*     CHECK=4. Again in this mode the routine processes the column names given
*     in CNAMES and their column types every time the routine is called.  Only
*     if the all the columns in the catalogue are given in CNAMES and the
*     types are correct will the entry be put into the catalogue.  If, during
*     checking, an error is found the error is reported and the name of the
*     offending column is returned in CNAMES(1). Subsequent
*     calls to the routine may have the columns in CNAMES in a different order
*     but they must all be their and the respective column types must be
*     correct.
*

*  Arguments:
*     INPUT = CHARACTER * ( CHI__SZNAME ) (Given)
*        Name of the catalogue into which the data is to be put.
*     CNAMES( CHI__NUMCOLS ) = CHARACTER * ( CHI__SZCNAME ) (Given and Returned)
*        Names of the columns whose data is being supplied.
*     NUMCOLS = INTEGER (Given)
*        Number of columns whose data is being supplied.
*     CHECK = INTEGER (Given)
*        Set to 1,2,3 or 4 according to the level of checking required.
*     INTVALS( CHI__NUMCOLS ) = INTEGER (Returned)
*        Array containing the data for integer columns.
*     REALVALS( CHI__NUMCOLS ) = REAL (Returned)
*        Array containing the data for real columns.
*     DOUBVALS( CHI__NUMCOLS ) = DOUBLE PRECISION (Returned)
*        Array containing the data for double precision columns.
*     LOGVALS( CHI__NUMCOLS ) = LOGICAL (Returned)
*        Array containing the data for logical columns.
*     CHARVALS( CHI__NUMCOLS ) = CHARACTER * ( CHI__SZCVAL ) (Returned)
*        Array containing the data for character columns.
*     COLTYPES( CHI__NUMCOLS ) = CHARACTER * ( 1 ) (Returned)
*        Array containing the types of columns.
*     NULLFLAGS( CHI__NUMCOLS ) = LOGICAL (Returned)
*        Array containing the null flags TRUE if a column is null.
*     STATUS = INTEGER (Given and Returned)
*        Global status.

*  Anticipated Errors:
*     CHI__CATNOTFND
*     CHI__COLNOTSUP
*     CHI__IVLDCOLTYP

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
      INTEGER CHECK
      INTEGER INTVALS ( * )
      REAL REALVALS ( * )
      DOUBLE PRECISION DOUBVALS ( * )
      LOGICAL LOGVALS ( * )
      CHARACTER * ( * ) CHARVALS( * )
      CHARACTER * ( 1 ) COLTYPES( * )
      LOGICAL NULLFLAGS ( * )

*  Arguments Given and Returned:
      CHARACTER * ( * ) CNAMES( * )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( 3 ) DBNAME ! Database name.
      CHARACTER * ( CHI__SZNAME ) CATNAME ! Catalogue name.
      INTEGER CD  ! Catalogue descriptor.

*.

*  Check inherited global status.
*
      IF ( STATUS .NE. SAI__OK ) RETURN
*
*    Split the input name into database part and catname part.
*
      call chi_splitname(input, dbname, catname, status)
*
*    Call PUTENT on the appropriate low level system
*
      if (dbname .eq. 'HDS') then
          call chi_getcd(dbname, catname, .TRUE., cd, status)
          call chi_hputent(cd, cnames, numcols, check, charvals,
     :  doubvals,
     : intvals, logvals,  realvals, coltypes, nullflags, status)
*
*      elseif (dbname .eq. 'BIN') then
*          call chi_getcd(dbname, catname, .TRUE., cd, status)
*          call chi_bputent(cd, cnames, numcols, check, charvals,
*     :  doubvals,
*     : intvals, logvals,  realvals, coltypes, nullflags, status)
*
*      elseif (dbname .eq. 'CDF') then
*          call chi_getcd(dbname, catname, .TRUE., cd, status)
*          call chi_cputent(cd, cnames, numcols, check, charvals,
*     :  doubvals,
*     : intvals, logvals,  realvals, coltypes, nullflags, status)
*
*      elseif (dbname .eq. 'FIT') then
*          call chi_getcd(dbname, catname, .TRUE., cd, status)
*          call chi_fputent(cd, cnames, numcols, check, charvals,
*     :  doubvals,
*     : intvals, logvals,  realvals, coltypes, nullflags, status)
*
*      elseif (dbname .eq. 'REX') then
*          call chi_getcd(dbname, catname, .TRUE., cd, status)
*          call chi_rputent(cd, cnames, numcols, check, charvals,
*     :  doubvals,
*     : intvals, logvals,  realvals, coltypes, nullflags, status)
      endif
*
      END
