      SUBROUTINE
     : CHI_ASPLIT( EXPR, NITEMS, STATUS )
*+
*  Name:
*     CHI_ASPLIT

*  Purpose:
*     Splits input expression into component items

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHI_ASPLIT( EXPR, NITEMS, STATUS )
*
*  Description:
*     ASPLIT works on the common area variables:-
*
*     Wstring a character stack of the elements of the expression.
*     Worigin an integer array whose values refer to positions in Wstring
*     Wqual an integer array used later by chi_atrans
*     Wlist an integer array When each item is recognised it is allocated an
*     integer, a comma is integer 7 for example. These numbers can be found in
*     CHIPAR1_PAR. User supplied functions are defined in CHI_ABLOCK. The
*     numbers associated with user defined functions are the function number
*     plus 100.
*
*     ASPLIT first convert the expression to uppercase except between
*     CHI__DELIM (").
*
*     Included blanks are removed.
*
*     Each character is examined in turn and checked to see if it is
*     recognised as known item or the start of a known item. If it is
*     subsequent characters may have to be examined before the item is
*     identified. When an item is identified it is loaded onto the stack in
*     the common area.
*
*     If the character is not recognised it is assumed that this character is
*     part of a constant or fieldname. This constant or fieldname continues
*     until the start of the next recognised item is found. It is the added
*     to the stack before the next recognised item.

*  Arguments:
*     EXPR = CHARACTER * ( CHI__SZEXP ) (Given)
*        Expression to be split into items.
*     NITEMS = INTEGER (Returned)
*        Number of items returned.
*     STATUS = INTEGER (Given and Returned)
*        Global status.

*  Anticipated Errors:
*     CHI__PCATNOTFND
*     CHI__IVLDEXP

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
      INCLUDE 'CHI_PAR'          ! Standard CHI constants
      INCLUDE 'CHIPAR_PAR'       ! Standard CHI parser constants
      INCLUDE 'CHIPAR1_PAR'      ! Standard CHI parser precedence constants
      INCLUDE 'CHI_ERR'          ! Standard CHI errors
*    Global constants :
      include 'CHIPAR_ERR'
*  Global variables :
      INCLUDE 'CHIWRK_CMN'

*  Arguments Given:
      CHARACTER * ( * ) EXPR

*  Arguments Returned:
      INTEGER NITEMS

*  Status:
      INTEGER STATUS             ! Global status

*  External Variables:
      integer chr_len

*  Local Constants:
      integer n_eos
      parameter (n_eos = chi__oplo)
      integer n_comma
      parameter (n_comma = chi__oplo + 6)
      integer chi_op1_lo
      parameter (chi_op1_lo = chi__oplo + 1)
      integer chi_op1_hi
      parameter (chi_op1_hi = chi__oplo + 13)
      integer chi_op4_lo
      parameter (chi_op4_lo = CHI__LOGLO - 6)
      integer chi_op5_lo
      parameter (chi_op5_lo = CHI__LOGLO + 1)
      character*1 ket
      parameter (ket = ')')
*  Local Variables:
      logical num_found       ! true if + or - is part of constant
      character*1 thischar    ! current char in input expression
      character*1 prevchar    ! previous char
      character*1 firstchar   ! 1st char in unparsed part of input
      integer start_item      ! pointer to unparsed part of input
      integer obj_len         ! length of object identified
      integer obj_num         ! id of object identified
      integer explen          ! effective length of input expression
      integer item            ! counter of objects identified
      integer wsptr           ! current top of Wstring
      integer l2
      integer matchlen
      integer i,j,k
      integer istat

      EXTERNAL chi_ablock

*.

*  Check inherited global status.
*
      IF ( STATUS .NE. SAI__OK ) RETURN
*
*    Remove trailing blanks
*
      explen = chr_len(expr)
*
*    Convert to upper case except between pairs of CHI__DELIM
*
      i = 1
      do while (i .le. explen .and. status .eq. SAI__OK)
         l2 = index ( expr(i:explen), CHI__DELIM )
         if (l2 .ne. 0) then
            l2 = i + l2
            call chr_ucase( expr(i:l2-2) )
            j = index ( expr(l2:explen), CHI__DELIM )
            if (j .eq. 0) then
               status = CHI__MISMA
               obj_len = 1
            else
               i = l2 + j
            endif
         else
            call chr_ucase( expr(i:explen) )
            i = explen + 1
         endif
      enddo
*
*    Then remove included blanks
*
      obj_len= 0
      i = 1
      do while(i .lt. explen .and. status .eq. SAI__OK)
         if (expr(i:i) .eq. ' ') then
*          Remove the included blank
            expr(i:explen-1) =  expr(i+1:explen)
            expr(explen:explen) = ' '
            explen = explen - 1
         elseif (expr(i:i) .eq. CHI__DELIM) then
*          Skip to the next string delimiter
            l2 = index(expr(i+1:), CHI__DELIM)
            if (l2 .eq. 0) then
               status = CHI__MISMA
               obj_len= 1
            else
               i = i + l2 + 1
            endif
         else
            i = i + 1
         endif
      enddo
*
*   Initialize variables
*
      item= 0
      start_item= 1
      i= start_item
      wsptr= 0
*
*   Scan looking for objects
*
      do while (i.le.explen.and.status.eq.SAI__OK)
        thischar= expr(i:i)
*
*   string constant
*
        if (thischar.eq.CHI__DELIM) then
          l2= index(expr(i+1:),thischar)
          if (l2.eq.0) then
            status= CHI__MISMA
            obj_len= 1
          else
            obj_len= l2+1
            obj_num= CHI__NULID
          endif
*
*   . - look for relational or logical operator
*
        elseif (thischar.eq.'.') then
          if (explen.ge.i+3) then
            l2= 4
            j= chi_op4_lo
            do while (obj_len.eq.0 .and. j.le.chi__ophi)
              if (expr(i+1:i+l2-1).eq.op_table(j)(2:l2)) then
                obj_len= l2
                obj_num= j
              else
                j= j+1
                if (j.eq.chi_op5_lo) then
                  if (explen.ge.i+4) then
                    l2= 5
                  else
                    j= chi__ophi+1
                  endif
                endif
              endif
            enddo
          endif
*
*   1- or 2- character operator
*
        else
          j= 1
          l2= 1
          do while (obj_len.eq.0 .and. j.le.chi_op1_hi)
            if (thischar.ne.op_table(j)(:1)) then
              j= j+1
            else
*
*   if * or / check for ** or //
*
              if (thischar.eq.'*' .or. thischar.eq.'/') then
                if (i.lt.explen .and. expr(i+1:i+1).eq.thischar) then
                  j= j+10
                  l2= 2
                endif
                obj_len= l2
                obj_num= j
*
*   not + - or (
*
              elseif (j.ge.n_comma) then
                obj_len= 1
                obj_num= j
*
*   + or - could be part of a numeric constant in E or D form
*
              elseif (thischar.eq.'+' .or. thischar.eq.'-') then
                num_found= .false.
                if (i.ge.start_item+2 .and. i.lt.explen) then
                  prevchar= expr(i-1:i-1)
                  if (prevchar.eq.'E' .or. prevchar.eq.'D') then
                    firstchar= expr(start_item:start_item)
                    if (firstchar.ge.'0' .and. firstchar.le.'9') then
                      num_found= .true.
                    endif
                  endif
                endif
                if (num_found) then
                  j= chi_op1_hi+1
                else
                  obj_len= 1
                  obj_num= j
                endif
*
*   open bracket - check if function else this is a generic element
*   and the operand is up to and including the next close bracket
*
              else
                if (i.eq.start_item) then
                  obj_len= 1
                  obj_num= j
                else
                  k= 1
                  matchlen= i-start_item
                  do while (obj_len.eq.0 .and. k.le.CHI__MXFUN)
                   if (matchlen.eq.fsizes(k) .and.
     :          expr(start_item:i-1).eq.ftions(k)(:matchlen)) then
                      obj_len= matchlen+1
                      obj_num= CHI__FBASE+k
                      i= start_item
                    else
                      k= k+1
                    endif
                  enddo
*
*   Generic - scan for matching close bracket
*
                  if (obj_len.eq.0) then
                    l2= index(expr(i+1:),ket)
                    if (l2.eq.0) then
                      status= CHI__MISMA
                      obj_len= 1
                    else
*                      if (l2.eq.1) then
*                       status= CHI__IVEXP
*                      else
*
*    Chi_adecode works out the type and format of a character string.
                      obj_len= matchlen+l2+1
                      obj_num= CHI__NULID
                      i= start_item
                    endif
                  endif
                endif
              endif
            endif
          enddo
        endif
*
*   Have identified an object.   If it starts after start_item then
*   it also delimits a previous item as expr(start_item:i-1) which
*   must be an operand
*
        if (obj_len.ne.0) then
*
*   Save 'split' status while item is stacked
*
          istat= status
          status = SAI__OK
          if (i.ne.start_item) then
            call chi_anxtitm ( expr(start_item:i-1), CHI__NULID,
     :                        wsptr, item, status)
          endif
          call chi_anxtitm ( expr(i:i+obj_len-1), obj_num,
     :                      wsptr, item, status)
*
*
*   Restore 'split' status unless stack overflowed
*
          if (status.eq.SAI__OK) then
              status= istat
          endif
*
*
*   Ready for next
*
          start_item= i+obj_len
          i= start_item
          obj_len= 0
*
*   object not yet identified
*
        else
          i= i+1
        endif
      enddo
*
*   End-of-string delimits final item as operand
*
      if (start_item.le.explen) then
        call chi_anxtitm ( expr(start_item:explen), CHI__NULID,
     :                    wsptr, item, status)
      endif
*
      nitems= item
*
      return
      end
