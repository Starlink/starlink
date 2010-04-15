      SUBROUTINE
     : CHP_PUTENT( INPUT, CHECK, NUMCOLS, NAMES, COLTYPES, CHARVALS,
     : DOUBVALS, INTVALS, LOGVALS, REALVALS, PTRVALS,  NULLS, STATUS)
*+
*  Name:
*     CHP_PUTENT

*  Purpose:
*     PUT an ENTry into a catalogue.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHP_PUTENT( INPUT, CHECK, NUMCOLS, NAMES, COLTYPES, CHARVALS,
*     DOUBVALS, INTVALS, LOGVALS, REALVALS, PTRVALS,  NULLS, STATUS)
*
*  Description:
*     Add an entry to a catalogue allowing non standard
*     formats, fixed and variable length arrays and variable length
*     character strings. The data should be provided in arrays. Each possible
*     column type Character, Double, Integer, Logical, Real and Pointer has
*     an array CHARVALS, DOUBVALS, INTVALS, LOGVALS, REALVALS and PTRVALS
*     respecitvely.
*
*     The columns in each of these arrays appear in the same order as they
*     appear in NAMES, the given list of column names to be placed. So if
*     we have
*
*     NAMES     COL1  COL2  COL3  COL4  COL5  COL6  COL7  COL8  COL9  COL10
*
*     COLTYPES    I     R     I     L     C     I     P     L     D      I
*
*     NULLS       T     T     T     T     F     T     T     T     T      T
*
*     then INTVALS(1)  should contain the data for COL1. INTVALS(2) the data
*     for COL3, INTVALS(3) the data for COL6 and INTVALS(4) the data for COL10.
*
*     Columns that are given but are not in the catalogue or are of the wrong
*     type are ignored.
*
*     There are four levels of checking which are selected using the argument
*     CHECK. Modes 1 and 2 are efficient and should be used whenever possible.
*     You are advised not to change the level of checking when putting data
*     into a catalogue unless you are moving to a mode where more checking is
*     done
*
*     CHECK=1. The lowest level of checking. The routine processes the column
*     names given in NAMES. Only if the column appears in the catalogue and
*     the type agrees with the that given in COLTYPES will data be put into
*     the column for this entry. All other columns take their null values. The
*     routine remembers where it found the data for each column (Eg. FLUX1 data
*     in REALVALS(5)). On subsequent calls the routine assumes that the FLUX1
*     value will be in REALVALS(5).
*
*     CHECK=2. The routine processes the column names given in NAMES and only
*     if the all the columns in the catalogue are given in NAMES and the
*     types are correct will the entry be put into the catalogue. If, during
*     checking, an error is found the error is reported and the name of the
*     offending column is returned in NAMES(1). Again after
*     the first call the routine remembers where it found the data for each
*     column and on subsequent calls the routine assumes that the data will be
*     in the same place. This mode ensures that genuine data is put into the
*     catalogue. Columns cannot be overlooked.
*
*     CHECK=3. In this mode the routine processes the column names given in
*     NAMES and their column types every time the routine is called. This
*     means that different columns can contribute to each entry. Only if
*     the column appears in the catalogue and
*     the type agrees with that given in COLTYPES will data be put into
*     the column for this entry. All other column take their null values.
*
*     CHECK=4. Again in this mode the routine processes the column names given
*     in NAMES and their column types every time the routine is called.  Only
*     if the all the columns in the catalogue are given in NAMES and the
*     types are correct will the entry be put into the catalogue.  If, during
*     checking, an error is found the error is reported and the name of the
*     offending column is returned in NAMES(1). Subsequent
*     calls to the routine may have the columns in NAMES in a different order
*     but they must all be their and the respective column types must be
*     correct.
*
*
*  Arguments:
*     INPUT = CHARACTER * ( CHP__SZNAME ) (Given)
*        Name of the catalogue.
*     CHECK = INTEGER (Given)
*        Level of checking required.
*     NUMCOLS = INTEGER (Given)
*        Number of columns. (Number of names in NAMES).
*     NAMES( CHP__NUMCOLS ) = CHARACTER * ( CHP__SZCNAME ) (Given)
*        Names of the columns.
*     COLTYPES( CHP__NUMCOLS ) = CHARACTER * ( 1 ) (Given)
*        Array containing the types of columns. (C,D,I,L,R,P)
*     CHARVALS( CHP__NUMCOLS ) = CHARACTER * ( CHP__SZCVAL ) (Given)
*        Array of data containing character columns.
*     DOUBVALS( CHP__NUMCOLS ) = DOUBLE PRECISION (Given)
*        Array of data containing double precision columns.
*     INTVALS( CHP__NUMCOLS ) = INTEGER (Given)
*        Array of data containing integer columns..
*     LOGVALS( CHP__NUMCOLS ) = LOGICAL (Given)
*        Array of data containing logical columns.
*     REALVALS( CHP__NUMCOLS ) = REAL (Given)
*        Array of data containing real columns.
*     PTRVALS( CHP__NUMCOLS ) = INTEGER (Given)
*        Array of data containing pointer columns.
*     NULLS( CHP__NUMCOLS ) = LOGICAL (Given)
*        Array of the null flags for the columns.
*     STATUS = INTEGER (Given and Returned)
*        Global status.

*  Notes:
*     CHP_PUTENT only puts data into real columns that exist in the catalogue.
*     If you supply data for virtual columns or columns that do not exist in the
*     catalogue that data will be ignored.
*
*     If the entry can not be inserted an insufficient privilege to update
*     error will be reported.
*
*  Anticipated Errors:
*     CHP__CATNOTFND
*     CHP__INSPRIVUP
*
*  Authors:
*     ARW: Alan R Wood (FIIS/RAL)
*     {enter_new_authors_here}

*  History:
*     1-Oct-1993 (ARW):
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

*  Global Variables:
      INCLUDE 'CHP_CMN'   ! Standard CHP

*  Arguments Given:
      CHARACTER * ( * ) INPUT
      CHARACTER * ( * ) NAMES( * )
      INTEGER NUMCOLS
      INTEGER CHECK
      INTEGER INTVALS ( * )
      REAL REALVALS ( * )
      DOUBLE PRECISION DOUBVALS ( * )
      LOGICAL LOGVALS ( * )
      CHARACTER * ( * ) CHARVALS( * )
      INTEGER PTRVALS ( * )
      CHARACTER * ( 1 ) COLTYPES( * )
      LOGICAL NULLS ( * )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      logical chr_simlr

*  Local Variables:
      integer colcount
      logical notfnd
      character*(1) csign
      integer cd
      integer cc
      character*(chp__szcname) catfnames(chp__numcols)
      character*(chp__szcfmt) fmat
      character*(chp__szcfmt) sexfmat
      logical sexflag
      character*(12) cval
      integer ideg
      integer iabsdeg
      integer iamin
      integer iasec
      integer iramin
      integer imin
      integer ihr
      integer isec
      integer jsec
      integer w
      real rmin
      real rsec
      real rasec
      real ramin
      integer jstat
      integer fldcount
      integer len
      double precision dsec
      double precision dasec
      double precision dval

*
*.
*  Check inherited global status.
*
      IF ( STATUS .NE. SAI__OK ) RETURN
*
      call chp_getcd(input, 'WRITE', cd, status)

      do cc = 1, numcols
      notfnd = .TRUE.
      colcount = 0
        do while (notfnd .and. colcount .lt. CPnumcols(cd))
          colcount = colcount + 1
          if (EPname(cd,colcount) .eq. names(cc)) then
            notfnd = .FALSE.
            if (EPnsflag(cd,colcount)) then
              cval = charvals(cc)
              call chp_connds(cval, EPnsformat(cd,colcount),
     :              dval, status)
              realvals(cc) = dval
              coltypes(cc) = 'R'
            endif
          endif
        enddo
      enddo
*
*
*   If there are no arrays or structures put the CHI entry. If there
*   are any arrays or structures the data is stored in a seperate file.
*   The offset of the start of the array is stored in the CHI catalogue
*   as an integer column and a length of the array is stored in another
*   column. For a fixed size N-dimensional array this is the product
*   of the dimensions, for a variable length array this is just the
*   size. Only 1-dimensional variable length arrays are supported.
*
*
*   Check through the columns. If the calumn has an array shape 0 (it
*   is not and array) copy it straight the arguments for the CHI call.
*   If it is an array then INTVALS(x) is a pointer to the dynamic memory
*   and ARRLEN(x) is th size. Copy this to the array file starting at
*   the current offset and then increment the offset.xxxxx
*
*
*   Use CHI_PUTENT to put the ammended version into the catalogue
*
      call chi_putent( input, names, numcols, check, charvals,
     : doubvals, intvals, logvals, realvals, coltypes, nulls,
     : status )
*
      return
      end
