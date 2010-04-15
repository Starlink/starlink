      SUBROUTINE
     : CHI_ACLRCMN( STATUS )
*+
*  Name:
*     CHI_ACLRCMN

*  Purpose:
*     Clear the common area.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHI_ACLRCMN( STATUS )

*  Description:
*     This routine clears the common area for the parser.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

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

*  Local Variables:
      INTEGER I

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
*
      IF ( STATUS .NE. SAI__OK ) RETURN
*
      Xsize = 0
      Xtype = 0
      do i = 1, CHI__MXITM
        Xlist(i) = 0
        Xqual(i) = 0
      enddo
      Xrdsize = 0
      do i = 1, CHI__MXRDS
        Xrdlist(i) = 0
      enddo
      do i = 1, CHI__MXCNS
        Xival(i) = 0
        Xrval(i) = 0.0
        Xdval(i) = 0.0
        Xcval(i) = ' '
        Xlval(i) = .TRUE.
      enddo
*
*      character*(CHI__SZSTK) Xstring ! String constants
*      character*(CHI__SZEXP) Xname  ! Name of expression
*      character*(CHI__SZFUNIT) Xunit  ! Units of the expression
*      character*(CHI__SZFCMT) Xcomnt ! Comment on the expression
*
*
*      integer Wlist(CHI__MXITM)         ! work stack
*      integer Wqual(CHI__MXITM)         ! qualifiers
*      integer Worigin(CHI__MXITM)       ! start position in input string
*      logical Wlval(CHI__MXCNS)         ! logical values
*      integer Wival(CHI__MXCNS)         ! integer values
*      real Wrval(CHI__MXCNS)            ! real values
*      double precision Wdval(CHI__MXCNS)! double values
*      character*(chi__szcval) Wcval(CHI__MXCNS)
*      logical Wshomes
*
*
*      character*(CHI__SZSTK) Wstring    ! string values
*
*      character*(CHI__SZFNAME) functions(CHI__MXFUN) ! Function names
*      integer                funcsizes(CHI__MXFUN) ! Length of function names
*      integer                functargs(CHI__MXFUN) ! Number of arguments required
*      integer                funcatype(CHI__MXFUN,CHI__MXITM) ! Data type of arguments
*      character*(CHI__SZFNVAL) functcons(CHI__MXFUN) ! Constraint
*      logical                functflag(CHI__MXFUN) ! Flag for SLA functions
*      double precision       functsave(CHI__MXFUN) ! Saved returned argument
*      character*(CHI__SZFCMT) funccomnt(CHI__MXFUN) ! Name of the result
*      character*(CHI__SZFUNIT) funcunits(CHI__MXFUN) ! units of result
*      integer                funcdtres(CHI__MXFUN) ! Data type of result
*      integer Mxelem                    ! DELETE and RECOMPILE ENTIRE LIBRARY
*      integer Etype(CHI__MXELM)         ! Element type
*      character*(CHI__SZFNAME) Ename(CHI__MXELM)      ! element name
*      character*(CHI__SZFFMT) Eformt(CHI__MXELM)     ! element format specifier
*      character*(CHI__SZFUNIT) Eunit(CHI__MXELM)      ! element units
*      character*(CHI__SZFFMT) Enfrmt(CHI__MXELM)     ! element null value format specifier
*      character*(CHI__SZFNVAL) Enull(CHI__MXELM)      ! element null value
*      character*(CHI__SZFCMT) Ecomnt(CHI__MXELM)     ! element comment
*      integer Enitems  ! Number of elements
*      integer Rstart(CHI__MXREL)     ! Index of relation start
*      integer Rsplit(CHI__MXREL)     ! Index of relation split
*      integer Rend(CHI__MXREL)       ! Index of relation end
*      integer Rcons(CHI__MXREL)      ! Index of relational constant
*      integer Rtype(CHI__MXREL)      ! Type of relational constant
*      integer Rshape(CHI__MXREL)     ! Canonical shape of relation
*      integer Rnext_free             ! Next free descriptor
*      INTEGER INTVALS ( CHI__NUMFLDS )
*      REAL REALVALS ( CHI__NUMFLDS )
*      DOUBLE PRECISION DOUBVALS ( CHI__NUMFLDS )
*      LOGICAL LOGVALS ( CHI__NUMFLDS )
*      CHARACTER * ( CHI__SZCVAL ) CHARVALS( CHI__NUMFLDS )
*      CHARACTER * ( 1 ) FLDTYPES( CHI__NUMFLDS )
*      CHARACTER * ( CHI__SZEXP ) CUREXPRESS
*
*
      end
