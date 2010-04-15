      SUBROUTINE
     : CHI_APPLY( CVALS, DVALS, IVALS, LVALS, RVALS, NUMFLDS,
     :            CVALUE, DVALUE, IVALUE, LVALUE, RVALUE, RESTYPE,
     :            STATUS )
*+
*  Name:
*     CHI_APPLY

*  Purpose:
*     Apply an expression to a set of data.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHI_APPLY( CVALS, DVALS, IVALS, LVALS, RVALS, NUMFLDS,
*               CVALUE, DVALUE, IVALUE, LVALUE, RVALUE, RESTYPE,
*               STATUS )

*  Description:
*     This routine will not normally be required because all common uses of
*     the parser have been anticipated and included in other CHI routines.
*     CHI_SEARCH, CHI_REJECT, CHI_JOIN, CHI_UPDATE and CHI_CALCFLD call
*     the parser on you behalf. CHI_1PAR and CHI_2PAR give you direct access
*     to the parser when 1 or 2 catalogues are involved and must be called
*     before CHI_APPLY can be called. This routine applies the expression to
*     a set of data. Data must be of the correct type and supplied in the
*     correct position as defined by CHI_1PAR or CHI_2PAR. CHI_1PAR and
*     CHI_2PAR return the
*     fieldnames and the fieldtypes which specify how the data is expected
*     for CHI_APPLY. If the FNAMES(1) is STARNAME and FTYPE(1) is character
*     then CHARVALS(1) must contain the the value of the STARNAME field as
*     given in the next entry in the catalogue. If the FNAMES(3) is VALUE1
*     and FTYPE(3) is real then REALVALS(3) must contain the the value of the
*     VALUE1 field as given in the next entry in the catalogue.
*     For ease of use the CHI_1PAR routine expects values in the same form
*     CHI_GETVALALL returns so a CHI_1PAR followed by a sequence of
*     CHI_GETVALALL and CHI_APPLY will produce the correct result.


*  Arguments:
*     CVALS( CHI__NUMFLDS ) = CHARACTER*(CHI__SZCVAL) (Given)
*        Character value being supplied.
*     DVALS( CHI__NUMFLDS ) = DOUBLE PRECISION (Given)
*        Double precision value being supplied.
*     IVALS = INTEGER (Given)
*        Integer value being supplied.
*     LVALS = LOGICAL (Given)
*        Logical value being supplied.
*     RVALS = REAL (Given)
*        Real value returned.
*     NUMFLDS = INTEGER (Given)
*        Number of fields for which data is being supplied.
*     CVALUE = CHARACTER*(CHI__SZCVAL) (Returned)
*        Character value returned.
*     DVALUE = DOUBLE PRECISION (Returned)
*        Double precision value returned.
*     IVALUE = INTEGER (Returned)
*        Integer value returned.
*     LVALUE = LOGICAL (Returned)
*        Logical value returned.
*     RVALUE = REAL (Returned)
*        Real value returned.
*     RESTYPE = CHARACTER * ( 1 ) (Returned)
*        Result type (C,D,I,L,R)
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Anticipated Errors:
*     CHI__CATNOTFND

*  Authors:
*     ARW: Alan R Wood (STARLINK)
*     {enter_new_authors_here}

*  History:
*     26-FEB-1992 (ARW):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CHI_PAR'          ! CHI Constants
      INCLUDE 'CHIPAR_PAR'       ! CHI Constants
      INCLUDE 'CHI_ERR'          ! CHI Errors
      INCLUDE 'CHIPAR_ERR'       ! CHI Parser errors

*  Global Variables:
      INCLUDE 'CHIWRK_CMN'

*  Arguments Given:
      CHARACTER * ( CHI__SZCVAL ) CVALS( CHI__NUMCOLS )
      DOUBLE PRECISION DVALS( CHI__NUMCOLS )
      INTEGER IVALS( CHI__NUMCOLS )
      LOGICAL LVALS( CHI__NUMCOLS )
      REAL RVALS( CHI__NUMCOLS )
      INTEGER NUMFLDS

*  Arguments Returned:
      CHARACTER * ( CHI__SZCVAL ) CVALUE
      DOUBLE PRECISION DVALUE
      INTEGER IVALUE
      LOGICAL LVALUE
      REAL RVALUE
      CHARACTER * ( 1 ) RESTYPE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      integer rd                    ! relational descriptor
      integer k                     ! counter
      integer wptr                  ! pointer into W<t>val arrays
      integer wstop                 ! pointer into Wstring
      integer resnum                ! result number
*.

*  Check inherited global status.
*
      IF ( STATUS .NE. SAI__OK ) RETURN
*
*   Copy the given data into the work area
*
      do k = 1, numflds
        charvals(k) = cvals(k)
        doubvals(k) = dvals(k)
        intvals(k) = ivals(k)
        logvals(k) = lvals(k)
        realvals(k) = rvals(k)
      enddo
*
*   Evaluate the expression via its relational sub-expressions
*   Assume the data is in the common area.
*
      wptr= 0
      wstop= 0
      k= 0
      do while (k.lt.Xrdsize .and. status.eq.SAI__OK)
         k= k+1
         rd= Xrdlist(k)
         call chi_arelw(rd, resnum, wptr, wstop, status)
      enddo
*
*   Convert the result
*
*      if (status.eq.SAI__OK) then
         goto (101, 102, 103, 104, 105), resnum
*
*   from L
101      lvalue = Wlval(1)
         restype = 'L'
         goto 200
*
*   from I
102      ivalue= Wival(1)
         restype = 'I'
         goto 200
*
*   from R
103      rvalue= Wrval(1)
         restype = 'R'
         goto 200
*
*   from D
104      dvalue= Wdval(1)
         restype = 'D'
         goto 200
*
*   from C
105      cvalue= Wcval(1)
         restype = 'C'
*
200      continue
*
      end

