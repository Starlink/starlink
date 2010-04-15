
      SUBROUTINE
     : CHI_AQAPP( CVALUE, DVALUE, IVALUE, LVALUE, RVALUE, RESTYPE,
     :            STATUS )
*+
*  Name:
*     CHI_AQAPP

*  Purpose:
*     Apply an expression to a set of data.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHI_AQAPP( CVALUE, DVALUE, IVALUE, LVALUE, RVALUE, RESTYPE, STATUS )

*  Description:
*     This routine will not normally be required because all common uses of
*     the parser have been anticipated and included in other CHI routines.
*     CHI_SEARCH, CHI_REJECT, CHI_JOIN, CHI_UPDATE and CHI_CALNEWFLD call
*     the parser on you behalf. CHI_1PAR and CHI_2PAR give you direct access
*     to the parser when 1 or 2 catalogues are involved and must be called
*     before CHI_APPLY can be called. This routine applies the expression to
*     a set of data. Data must be of the correct type and supplied in the
*     correct position as defined by CHI_PARSE. This is consistent with the
*     routine CHI_GETVALALL, so a CHI_1PAR followed by a CHI_GETVALALL and
*     a CHI_APPLY will work. The type of the result (C,D,I,L,R) is returned
*     in RESTYPE and the resulting value will be found in the first element of
*     the appropriate array.
*

*  Arguments:
*     CVALUE = CHARACTER*(CHI__SZCVAL) (Returned)
*        Character value returned.
*     DVALUE = DOUBLE PRECISION (Returned)
*        Double precision value returned.
*     IVALUE = INTEGER (Returned)
*        Integer value returned.
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
101      lvalue= Wlval(1)
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
