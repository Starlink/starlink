      SUBROUTINE
     : CHP_GDNAC( INPUT, NUMCOLS, NAMES, COLTYPES, COLDES, CHARVALS,
     : DOUBVALS, INTVALS, LOGVALS, REALVALS, PTRVALS, NULLS, STATUS )
*+
*  Name:
*     CHP_GDNAC

*  Purpose:
*     Get Data from the Next entry, All Columns.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHP_GDNAC( INPUT, NUMCOLS, NAMES, COLTYPES, COLDES, CHARVALS,
*     DOUBVALS, INTVALS, LOGVALS, REALVALS, PTRVALS, NULLS, STATUS )
*
*  Description:
*     Get all the data from the next entry in a catalogue allowing non standard
*     formats, fixed and variable length arrays and variable length
*     character strings. Each possible column type has an associated
*     array CHARVALS, DOUBVALS, INTVALS, LOGVALS, REALVALS and PTRVALS
*     and the data is returned in these arrays.
*     NAMES will contain the names of the columns. COLDES and COLTYPES are
*     the designation and type of the columns which allow the column to be
*     correctly interpreted.
*
*     The data in each array appear in the same order that the columns appear
*     in CNAMES, the list of column names. So if
*
*     NAMES     COL1  COL2  COL3  COL4  COL5  COL6  COL7  COL8  COL9  COL10
*
*     COLTYPES    I     R     I     L     C     I     P     L     D      I
*
*     NULLS       T     T     T     T     F     T     T     T     T      T
*
*     COLDES      1     1     1     1     1     1     2     1     1      1
*
*     then INTVALS(1) contains the data for COL1. INTVALS(2) contains the data
*     for COL3, INTVALS(3) the data for COL6 and INTVALS(4) the data
*     for COL10.
*
*     COLDES(i) = 1 This is a scalar column, if it is a character string it
*     is CHP__SZCVAL long. COLTYPES(i) will be C,D,I,L or R.
*     The null value flag is returned in NULLS(i).
*
*     COLDES(i) = 2 This is a structure column. A pointer to the structure in
*     dynamic memory is returned in PTRVALS(i). The null value flag,
*     ie. TRUE for no valid structure, is returned in NULLS(i). COLTYPES(i)
*     will be P.
*
*     COLDES(i) = 3 This is an array column whose size is the same for all
*     entries. Character string are always CHP__SZCVAL long.
*     A pointer to the array of data and the associated information is given
*     in the corresponding PTRVALS(i).
*     The null value flag, ie. TRUE for no valid array, is returned in NULLS(i).
*     COLTYPES(i) will be C,D,I,L or R reflecting the type of elements in the
*     array.
*
*     COLDES(i) = 4 This is an array whose shape and dimensions may vary from
*     entry to entry. Character string are always CHP__SZCVAL long.
*     A pointer to the array of data and the associated information is given
*     in the corresponding PTRVALS(i).
*     The null value flag, ie. TRUE for no valid array, is returned in NULLS(i).
*     COLTYPES(i) will be C,D,I,L or R reflecting the type of elements in the
*     array.
*
*     COLDES(i) = 5 This is a scalar column. It is a variable length character
*     string. A pointer to the character string and the associated information
*     is given in PTRVALS(i).
*     The null value flag is returned in NULLS(i).
*     COLTYPES(i) will be C.
*
*     COLDES(i) = 6 This is an array column of character strings whose shape,
*     dimensions and string length may vary from entry to entry.
*     A pointer to the array of data and the associated information is given
*     in PTRVALS(i).
*     The null value flag, ie. TRUE for no valid array, is returned in NULLS(i).
*     COLTYPES(i) will be C.
*

*  Arguments:
*     INPUT = CHARACTER * ( CHP__SZNAME ) (Given)
*        Name of the catalogue from which the data is required.
*     NUMCOLS = INTEGER (Returned)
*        Number of columns.
*     NAMES( CHP__NUMCOLS ) = CHARACTER * ( CHP__SZCNAME ) (Returned)
*        Names of the columns.
*     COLTYPES( CHP__NUMFLDS ) = CHARACTER * ( 1 ) (Returned)
*        Array of the types of columns. (C,D,I,L,R,P)
*     COLDES( CHP__NUMCOLS ) = INTEGER (Given)
*        The column designations.
*     CHARVALS( CHP__NUMCOLS ) = CHARACTER * ( CHP__SZCVAL ) (Returned)
*        Array of data from character columns.
*     DOUBVALS( CHP__NUMCOLS ) = DOUBLE PRECISION (Returned)
*        Array of data from double precision columns.
*     INTVALS( CHP__NUMCOLS ) = INTEGER (Returned)
*        Array of data from integer columns.
*     LOGVALS( CHP__NUMCOLS ) = LOGICAL (Returned)
*        Array of data from logical columns.
*     REALVALS( CHP__NUMCOLS ) = REAL (Returned)
*        Array of data from real columns.
*     PTRVALS( CHP__NUMCOLS ) = POINTER (Returned)
*        Array of data from pointer columns.
*     NULLS( CHP__NUMCOLS ) = LOGICAL (Returned)
*        Array of the null flags for the columns.
*     STATUS = INTEGER (Given and Returned)
*        Global status.

*  Notes:
*     This routine may allocate virtual memory. Always remember to release
*     any allocated virtual memory.

*  Anticipated Errors:
*     CHP__CATNOTFND

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
      INCLUDE 'CHI_PAR'   ! Standard CHI constants

*  Global Variables:
      INCLUDE 'CHP_CMN'   ! Standard CHP

*  Arguments Given:
      CHARACTER * ( * ) INPUT

*  Arguments Returned:
      CHARACTER * ( * ) NAMES( * )
      INTEGER NUMCOLS
      INTEGER INTVALS ( * )
      REAL REALVALS ( * )
      DOUBLE PRECISION DOUBVALS ( * )
      LOGICAL LOGVALS ( * )
      CHARACTER * ( * ) CHARVALS( * )
      INTEGER PTRVALS ( * )
      CHARACTER * ( 1 ) COLTYPES( * )
      INTEGER COLDES ( * )
      LOGICAL NULLS ( * )

*  Status:
      INTEGER STATUS             ! Global status

*  External references:
      logical chr_simlr

*  Local Variables:
      integer count
      integer temparrdim(7)
      integer cd
      integer idmsf(4)
      integer ihmsf(4)
      character*1 sign
      double precision rvald
      character*(1) csign
      integer cc
      integer cb
      integer colc
      integer sexpos
      real rval
      character*(chp__szccmt) comnt
      character*(chp__szcfmt) format
      logical sflag
      logical notfound
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
      integer tnumcols
      character * (chi__szcname) tcnames(chi__numcols)
      character * (chi__szcval) tcharvals(chi__numcols)
      double precision tdoubvals(chi__numcols)
      integer tintvals(chi__numcols)
      logical tlogvals(chi__numcols)
      real trealvals(chi__numcols)
      character * (1) tcoltypes(chi__numcols)
      logical tnulls(chi__numcols)
      logical notfnd

*  Local Data:
*
*  PI, 2PI, PI/2
      REAL*8 DPI
      PARAMETER (DPI=3.141592653589793238462643D0)
      REAL*8 D2PI
      PARAMETER (D2PI=6.283185307179586476925287D0)
      REAL*8 DPIBY2
      PARAMETER (DPIBY2 = DPI/2)
*
*  RADIANS to DEGREES
      REAL*8 DR2D
      PARAMETER (DR2D=57.29577951308232087679815D0)
*
*  RADIANS to SECONDS
      REAL*8 DR2S
      PARAMETER (DR2S=0.1375098708313975703D+05)
*
*  RADIANS to HOURS
      REAL*8 DR2H
      PARAMETER (DR2H=DR2S/3600)
*
*  RADIANS to ARC SECONDS
      REAL*8 DR2AS
      PARAMETER (DR2AS=0.2062648062470963560D+06)
*
*  DEGREES to RADIANS
      REAL*8 DD2R
      PARAMETER (DD2R=1.745329251994329576923691D-02)
*
*  SECONDS to RADIANS
      REAL*8 DS2R
      PARAMETER (DS2R=0.7272205216643039849D-04)
*
*  HOURS TO RADIANS
      REAL*8 DH2R
      PARAMETER (DH2R=DS2R*3600)
*
*  MINUTES TO RADIANS
      REAL*8 DM2R
      PARAMETER (DM2R=DS2R*60)
*
*  ARC SECONDS to RADIANS
      REAL*8 DAS2R
      PARAMETER (DAS2R=0.4848136811095359949D-05)
*
*  ARC MINUTES TO RADIANS
      REAL*8 DAM2R
      PARAMETER (DAM2R=DAS2R*60)

*.
*  Check inherited global status.
*
      IF ( STATUS .NE. SAI__OK ) RETURN
*
*    This could be an Extension join catalogue, an Extension Search
*    catalogue, an Extension Plain catalogue or a Regular catalogue.
*    Whatever the type of catalogue get the data from the underlying
*    CHI catalogue(s).
*
      call chp_getcd(input, 'WRITE', cd, status)
*
      if (CPnumext(cd) .eq. 2) then
*
*    This is an Extension join catalogue.
*
      elseif (CPnumext(cd) .eq.1) then
*
*  This may be an Extension Search catalogue or an Extension Plain
*  catalogue.
*
        if (CPexttype(cd)) then
*  This is an Extension Search catalogue.
        else
*  This is an Extension Plain catalogue.
        endif
*
      else
*
*  This must be a Regular Catalogue so get the data out of the
*  CHI catalogue.
*
        call chi_gdnac(input, tcnames, tnumcols, tcharvals,
     :  tdoubvals, tintvals,
     :  tlogvals, trealvals, tcoltypes, tnulls, status)
*
*  Each of these columns may be a column in the catalogue, an identifier to
*  an array or structure or a non standard format.
*  Every column from the underlying catalogue has a match in the CHP catalogue
*  find the match and process the column data
*
        numcols = CPnumcols(cd)
        do cb = 1, CPnumcols(cd)
          names(cb) = EPname(cd,cb)
          notfnd = .TRUE.
          cc = 1
          do while (notfnd .and. (cc .le. tnumcols))
            if (tcnames(cc) .eq. EPname(cd,cb)) then
              notfnd = .FALSE.
              coltypes(cb) = tcoltypes(cc)
              nulls(cb) = tnulls(cc)
              if ( .NOT. EPnsflag(cd,cb)) then
                if (EPtype(cd,cb) .eq. 'C') then
                   charvals(cb) = tcharvals(cc)
                elseif (EPtype(cd,cb) .eq. 'D') then
                   doubvals(cb) = tdoubvals(cc)
                elseif (EPtype(cd,cb) .eq. 'I') then
                   intvals(cb) = tintvals(cc)
                elseif (EPtype(cd,cb) .eq. 'L') then
                   logvals(cb) = tlogvals(cc)
                elseif (EPtype(cd,cb) .eq. 'R') then
                   realvals(cb) = trealvals(cc)
                endif
              else
                dval = trealvals(cc)
                call chp_condsn( dval, EPnsformat(cd,cb),
     - cval, status)
                charvals(cb) = cval
                coltypes(cb) = 'C'
              endif
*
* If the column is hidden it may be an array, structure or variable length
* character string.
*
*            else
*                 if (EPstruct(cd,cb)) then
*                   continue
*                 elseif(EParrtype(cd,cb) .eq. 0) then
*                   continue
*                 elseif(EParrtype(cd,cb) .eq. 1) then
*                   call chp_retrieve(input, EPname(cd,cb), tintvals(cc),
*     :  coltypes(cb), arrtypes(cb), arrshp(cb), temparrdim(7),
*     :  charlen(cb), charoff(cb), intvals(cb), nullarr(cb), status)
*                   numcols = numcols + 1
*                   do count = 1, 7
*                     arrdim(cb,count) = temparrdim(count)
*                   enddo
*                 elseif(EParrtype(cd,cb) .eq. 2) then
*                   continue
*                 elseif(EParrtype(cd,cb) .eq. 3) then
*                  continue
*                 elseif(EParrtype(cd,cb) .eq. 4) then
*                   continue
*                 endif
            endif
            cc = cc + 1
          enddo
        enddo
      endif
*
*      do cc = 1, CPnumcols(cd)
*        if (EPvcflag(cd,cc)) then
*          numcols = numcols + 1
*          names(numcols) = EPname(cd,cc)
*        endif
*      enddo
*
      return
      end
