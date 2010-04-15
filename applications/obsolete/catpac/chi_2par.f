      SUBROUTINE
     : CHI_2PAR( INPUT1, INPUT2, EXPRESS, FNAMES, FTYPES, STATUS )
*+
*  Name:
*     CHI_2PAR

*  Purpose:
*     Parse an expression two catalogues.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHI_2PAR( INPUT1, INPUT2, EXPRESS, FNAMES, FTYPES, STATUS )

*  Description:
*     This routine will not normally be required because all common uses of
*     the parser have been anticipated and included in other CHI routines.
*     CHI_SEARCH, CHI_REJECT, CHI_JOIN, CHI_UPDATE, CHI_EVAL and
*     CHI_CALCFLD call the parser on you behalf. CHI_1PAR and CHI_2PAR give
*     you direct access to the parser when 1 or 2 catalogues are involved.
*     This routine parses an expression which contains field names from
*     two catalogues. To apply this expression to a set of data see the
*     CHI_APPLY routine. CHI_2PAR uses the arguments FNAMES and FTYPES to
*     inform the user of how the data is to be supplied. See CHI_APPLY for
*     more detail.

*  Arguments:
*     INPUT1 = CHARACTER*(CHI__SZNAME) (Given)
*        Name of the first catalogue.
*     INPUT2 = CHARACTER*(CHI__SZNAME) (Given)
*        Name of the second catalogue.
*     EXPRESS = CHARACTER*(CHI__SZEXP) (Given)
*        Expression to be parsed.
*     FNAMES(CHI__NUMFLDS) = CHARACTER*(CHI__SZFNMAE) (Returned)
*        Names of the fields.
*     FTYPES(CHI__NUMFLDS) = CHARACTER*(1) (Returned)
*        Types of the fields.
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
      CHARACTER * ( * ) INPUT1
      CHARACTER * ( * ) INPUT2
      CHARACTER * ( * ) EXPRESS

*  Arguments Returned:
      CHARACTER * ( * ) FNAMES( * )
      CHARACTER * ( 1 ) FTYPES( * )

*  Status:
      INTEGER STATUS             ! Global status

      integer chr_len
*  Local Variables:
      CHARACTER * 127 TEST
      INTEGER NUMFLDS
      CHARACTER * ( CHI__SZNAME ) OUTCAT ! Catalogue name for results
      CHARACTER * ( CHI__SZCCMT ) COMNT(CHI__MXITM) !
      INTEGER NITEMS ! Number of items found by split.
      INTEGER I,J
      INTEGER INTYPE(CHI__MXITM)
      INTEGER OUTYPE(CHI__MXITM)
      LOGICAL LVALUE
      REAL RVALUE

*.

*  Check inherited global status.
*
      IF ( STATUS .NE. SAI__OK ) RETURN
*
*    Clear the parser common area.
*
         call chi_aclrcmn( status )
*
*    CHI_AINITPAR loads the field information from the catalogues into the
*    common area. Other routines can then search through this common area
*    to find if the field is in the catalogue and the type of the field.
*
         call chi_ainit2par(input1, input2, fnames, ftypes, status)
*
*    CHI_APREPAR takes the expression and preparses it replacing and CONVERTs
*    with radian values.
*
         call chi_aprepar(express, status)
*
*    CHI_ASPLIT takes the expression and splits it into items having first
*    removed any redundant spaces and where possible converted to upper case.
*    An item is recognised if it is an expected character (Eg. *,+) or a
*    string of expected characters (.LT.,SQRT). If a string is not recognised
*    and the next item is found the the unidentified string is either a field
*    name or constant. For a list recognised items see CHIPAR_PAR and for a
*    list of recognised functions see CHI_ABLOCK because each item is given an
*    item number that has to be unique the item number for a function is 100
*    plus the function number.
*
*    By the end of the CHI_ASPLIT the stack Wstring in the common area contains
*    the item separated by !
*    Eg.   GREAT_CIRCLE(!RA1!DEC1!RA2!DEC2!)!.LT.!180!.AND.!FLAG!
*
*    The integer array Wqual(i) points to the starting position of each item.
*    Wqual(1) = 1
*    Wqual(2) = 15
*    Wqual(3) = 19
*      etc
*
*    At this poit the integer array Worigin is the same as Wqual.
*
*    The integer array Wlist(i) contains the item number. For a recognised item
*    The item number is positive and uniquely identefies the item. For a field
*    name or constant -1 is used.
*
          call chi_asplit( express, nitems, status)
*           do i = 1,nitems
*           print *,'worigin(i) = ',worigin(i)
*           print *,'wqual(i) = ',wqual(i)
*           print *,'wlist(i) = ',wlist(i)
*           print *,'wstring = ',wstring
*           enddo
*
*    CHI_ATRANS performs the reverse polish manipulation. Wlist and Wqual
*    remain unchanged but at the ent of CHI_ATRANS Worigin reflects the
*    reverse polish order of items
*
*    The integer array Xlist whose size is given by Xsize contains the item
*    numbers in reverse polish order corresponding to Worigin. So if Xlist(8)
*    is a 7 indicating a comma the Wlist(8) point to that comma in Wstring.
*
*    By this time any field names have been recognised and thier item number
*    is given as -its position in the common area (Ename(i)).
*
*    Constants are also dealt with at this stage. When a constant is recognised
*    it is given a unique negative number that does not conflict with the
*    negative numbers allocated for fieldnames. The details of the constant
*    are stored in the common area at the position given by the absolute value
*    of its negative number.
*
         call chi_atrans( nitems, status)
*
*
*           print *,'wstring = ',wstring
*           print *,'after CHI_ATRANS'
*           print *,'status = ',status
*           do i = 1,nitems
*           print *,'worigin(i) = ',worigin(i)
*           print *,'wqual(i) = ',wqual(i)
*           print *,'wlist(i) = ',wlist(i)
*           enddo
*           print *,'xsize = ',xsize
*           do i = 1,nitems
*           print *,'xqual(i) = ',xqual(i)
*           print *,'xlist(i) = ',xlist(i)
*           enddo
*
*           do i = 1,Enitems
*           print *,'Ename(i) = ',Ename(i)
*           print *,'Ecomnt(i) = ',Ecomnt(i)
*           enddo
*
*    CHI_APANAL works out the types. A dummy evaluation is done to establish
*    types. At the lowest level a constant or fieldname has a type. In the
*    case of fieldnames 1,2,3,4,5 for logical, integer, real, double, character
*    Constants -1,-2,-3,-4,-5
*    These are the intypes for these items.
*
*    The outype for these items is determined by how they are used. For example
*    the function SQRT requires that its argument is double precision. So the
*    outtype of the argument has to be double precision.
*
*    Because a dummy evaluation is carried out the types for the items are
*    correctly identified as the the evaluation unwinds. The types of all the
*    bottom level elements is checked for invalid conversions. Allowed
*    automatic conversions are only those between numerics
*
*    CHI_APANAL also deals with the comments of the items.
*
*
          call chi_apanal( intype, outype, comnt,status)
*           print *,'after CHI_APANAL'
*           print *,'status = ',status
*           do i = 1,nitems
*           print *,'item = ',i
*           print *,'intype(i) = ',intype(i)
*           print *,'outype(i) = ',outype(i)
*           print *,'comnt(i) = ',comnt(i)
*           enddo
*
*    CHI_APCONS works out the constants. By the end of CHI_ACONS
*
*    Xlist contains for a simple operator the operator identification number.
*                   for a function the function number + 100
*                   for a field name 0
*                   and for a constant a negative number whose absolute value
*                   points to the value in the W<t>val arrays.
*
*    Xqual contains for a simple operator the output type.
*                   for a function the argument count*100 plus the output type
*                   for a field name the absolute value of the allocated number
*                   for this field name*100
*                   plus the integer value of the type of this field
*                   1,2,3,4,5 for logical, integer, real, double, character
*                   and for a constant the integer value of the type of this
*                   constant plus if the constant is a character string
*                   10*the string length
*
*          print *,'in parse befor apcons status = ',status
*          print *,'neitems = ',enitems
*          do i = 1, enitems
*           print *,'i = ',i
*           print *,'ename(i) = ',ename(i)
*          print *,'eformte(i) = ',eformt(i)
*           print *,'enull)i) = ',enull(i)
*           print *,'ecomnt(i) = ',ecomnt(i)
*          enddo
          call chi_apcons( intype, outype, comnt,status)
*           print *,'after CHI_APCONS'
*           do i = 1,nitems
*           print *,'worigin(i) = ',worigin(i)
*           print *,'wqual(i) = ',wqual(i)
*           print *,'wlist(i) = ',wlist(i)
*           enddo
*           print *,'xsize = ',xsize
*           do i = 1,xsize
*           print *,'xqual(i) = ',xqual(i)
*           print *,'xlist(i) = ',xlist(i)
*           enddo
*           print *,'xstring = ',xstring
*
*
*    CHI_ARINIT works out the relational sub expression.
*    It does not alter any of the previous values in the common area
*    But searches through the expression locating sub expressions.
*    By the end of CHI_ACONS
*
*    Xlist contains for a simple operator the operator identification number.
*                   for a function the function number + 100
*                   for a field name 0
*                   and for a constant a negative number whose absolute value
*                   points to the value in the W<t>val arrays.
*
*    Xqual contains for a simple operator the output type.
*                   for a function the argument count*100 plus the output type
*                   for a field name the absolute value of the allocated number
*                   for this field name*100
*
          call chi_arinit(status)
*           print *,'after CHI_ARINIT'
*           print *,'status = ',status
*           print *,'Xrdsize = ',Xrdsize
*           do i = 1,Xrdsize
*           print *,'i = ',i
*           print *,'Rstart(i) = ',Rstart(i)
*           print *,'Rend(i) = ',Rend(i)
*           print *,'Rsplit(i) = ',Rsplit(i)
*           print *,'Rcons(i) = ',Rcons(i)
*           print *,'Rtype(i) = ',Rtype(i)
*           print *,'Rshape(i) = ',Rshape(i)
*           enddo
*          print *,'neitems = ',enitems
*          do i = 1, enitems
*           print *,'i = ',i
*           print *,'ename(i) = ',ename(i)
*           print *,'eformte(i) = ',eformt(i)
*           print *,'enull)i) = ',enull(i)
*           print *,'ecomnt(i) = ',ecomnt(i)
*           print *,'etype(i) = ',etype(i)
*          enddo
*           do i = 1,nitems
*           print *,'worigin(i) = ',worigin(i)
*           print *,'wqual(i) = ',wqual(i)
*           print *,'wlist(i) = ',wlist(i)
*           enddo
*           print *,'xsize = ',xsize
*           do i = 1,xsize
*           print *,'xqual(i) = ',xqual(i)
*           print *,'xlist(i) = ',xlist(i)
*           enddo
*           do i = 1, 5
*           print *,'xdval(i) = ',xdval(i)
*           print *,'xival(i) = ',xival(i)
*           print *,'xlval(i) = ',xlval(i)
*           print *,'xrval(i) = ',xrval(i)
*           j = chr_len(xcval(i))
*           print *,'j = ',j
*           print *,'xcval(i) = ',xcval(i)(:j)
*           enddo
*
*
      end
