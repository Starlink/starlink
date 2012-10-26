*  History:
*     31 July 2000 (ajc):
*        Change TYPE * to PRINT *
*        Use format I3 to read type size
*        Report "invalid operand"
*      25 Jul 2004 (timj):
*        Be consistent in capitalisation of include file
*-----------------------------------------------------------------------
#if HAVE_CONFIG_H
#  include "config.h"
#endif

      SUBROUTINE gen_factor (string, ils, next, ierr)

*  Routine to parse the incoming string for the next complete factor,
*  and either push the value onto the stack, or if a '(' is encountered,
*  to push the () stack and then fetch the first operand following the '('
*  In the special case that the symbol parsed has an argument list (either
*  for a function or an array) then come back, push the stack, and fetch a
*  further factor.

      IMPLICIT none

*     Formal parameters:

      CHARACTER string*(*)
      INTEGER   ils
      INTEGER   next
      INTEGER   ierr

*     Operand and operator stacks

      INCLUDE  'EVAL_AE4.INC'
      INCLUDE  'CNF_PAR'

*     Local variables:

      LOGICAL   numeric
      LOGICAL   strconst
      LOGICAL   uminus
      LOGICAL   funct
      LOGICAL   lbracket
      INTEGER   fnc_index
      INTEGER   idig, fdig, edig
      INTEGER   lf, ls
      INTEGER   nbytes
      INTEGER   st, ist, iend
      CHARACTER del_oper*1
      CHARACTER type*4
      CHARACTER form*8

      INTEGER   sym_index
      INTEGER   length
      INTEGER   address
      LOGICAL   readonly
      LOGICAL   isnew

      INTEGER   ivalue
      REAL*4    rvalue
      REAL*8    dvalue
      EQUIVALENCE (ivalue, rvalue, dvalue)

*     Functions

      LOGICAL   gen_digits
      LOGICAL   gen_floating
      LOGICAL   gen_dformat
      LOGICAL   gen_eformat

      INTEGER   gen_ilen
      INTEGER   gen_readnum

*  Make sure routine "falls through" on an error

      IF (ierr.ne.0) RETURN

*  Ok, go..

CD    Print *, '-- gen_factor --'

      st   = next

*     Check that next item is not a left bracket - if it is then
*     push the stack and find the next non-blank character

    1 CONTINUE

*     Find the start of the first factor - stacking if ( found and
*     ignoring space
      DO WHILE (string(st:st).eq.'(' .AND. st.le.ils)
CD      Print *,'   "(" found = pushing stack...'
        lev = lev + 1
        st  = st  + 1
CD      Print *,'    level no', lev
        DO WHILE (string(st:st).eq.' ' .AND. st.le.ils)
          st = st + 1
        END DO
      END DO

      IF (st.gt.ils) THEN
*     We have run out of string and not found the start
*     PARSENAME drops through if IERR.NE.0 so rely on later error check
*     and benefit from the error report there
        ierr = 1
      END IF

*     Parse off the next item

      CALL gen_parsename (string, st, ist, iend, numeric, strconst,
     &                    uminus, lbracket, funct, next, ierr)
CD    Print *, '     parsed factor: ', string(ist:iend)

      IF (ierr.ne.0) THEN
            PRINT *, '-- gen_factor --'
            PRINT *, '   Invalid operand "',string(st:),'"'
            RETURN
      END IF

*     If unary-minus flag set then push unary minus operator (%) on opr stack

      IF (uminus) THEN
CD      Print *, '     Unary minus flag set - pushing operator stack...'
        nopr(lev)   = nopr(lev) + 1
        ntopr       = ntopr     + 1
        oper(ntopr) = '%'
        prio(ntopr) = 7
      END IF

*     If operand of any unary operator is bracketed then push stack...

      IF (lbracket) THEN
*       PRINT *, '    left bracket returned, starting again...'
        GO TO 1
      END IF

*     Either get the value directly (numeric string) or translate symbol.
*     If result not delayed then value is returned into workspace array
*     at next available location. Otherwise - if for example we need to
*     evaluate the array index for the symbol first - enter symbol on the
*     stack, and the "evaluate array index" operator (@) on the operator stack.

      IF (numeric) THEN

        lf = LEN (form)

        IF (gen_digits (string(ist:iend), idig)) THEN
          type   = 'I4'
          form   = 'I'
          WRITE (form(2:lf), '(I2.2)') idig
          ierr = gen_readnum (string(ist:iend), type, form, ivalue)
          address = cnf_preg(
#if HAVE_PERCENTLOC
     :   %loc(ivalue)
#elif HAVE_INTRINSIC_LOC
     :   loc(ivalue)
#else
    Do not have LOC or %LOC
#endif
     :         , isnew )
CD        Print *,'     integer value read'
        ELSE IF (gen_floating (string(ist:iend), idig, fdig)) THEN
          type   = 'R4'
          form   = 'F'
          WRITE (form(2:lf), '(I2.2,''.'',I3.3)') idig+fdig+1,
     &                                             MAX (0, fdig)
          ierr = gen_readnum (string(ist:iend), type, form, rvalue)
          address = cnf_preg(
#if HAVE_PERCENTLOC
     :   %loc(rvalue)
#elif HAVE_INTRINSIC_LOC
     :   loc(rvalue)
#else
    Do not have LOC or %LOC
#endif
     :        , isnew )
CD        Print *,'     real*4 value read'
        ELSE IF (gen_eformat (string(ist:iend), idig, fdig, edig)) THEN
          type   = 'R4'
          form   = 'E'
          WRITE (form(2:lf), '(I2.2,''.'',I3.3)') idig+fdig+edig+2,
     &                                             MAX (0, fdig)
          ierr = gen_readnum (string(ist:iend), type, form, rvalue)
          address = cnf_preg(
#if HAVE_PERCENTLOC
     :   %loc(rvalue)
#elif HAVE_INTRINSIC_LOC
     :   loc(rvalue)
#else
    Do not have LOC or %LOC
#endif
     :        , isnew )
CD        Print *,'     real*4 value read'
        ELSE IF (gen_dformat (string(ist:iend), idig, fdig, edig)) THEN
          type   = 'R8'
          form   = 'E'
          WRITE (form(2:lf), '(I2.2,''.'',I3.3)') idig+fdig+edig+2,
     &                                             MAX (0, fdig)
          ierr = gen_readnum (string(ist:iend), type, form, dvalue)
          address = cnf_preg(
#if HAVE_PERCENTLOC
     :   %loc(dvalue)
#elif HAVE_INTRINSIC_LOC
     :   loc(dvalue)
#else
    Do not have LOC or %LOC
#endif
     :        , isnew )
CD        Print *,'     real*8 value read'
        END IF

      ELSE IF (strconst) THEN
        CALL gen_hdnorm (string(ist:iend), string(ist:iend), ls, ierr)
        WRITE (type, '(''C'',I3.3)') ls
        address = cnf_preg(
#if HAVE_PERCENTLOC
     :   %loc(string(ist:ist))
#elif HAVE_INTRINSIC_LOC
     :   loc(string(ist:ist))
#else
    Do not have LOC or %LOC
#endif
     :      , isnew )
CD      Print *,'     string constant read: length =', ls
CD      Print *,'     variable type = ', TYPE
CD      Print *,'     ', string(ist:ist+ls-1)

      ELSE
        fnc_index = 0

        IF (funct) THEN
          CALL gen_inqfunc (string(ist:iend), fnc_index,
     &                      type, length, ierr)
          IF (fnc_index.gt.0) THEN
            address  = fnc_index
            del_oper = '$'
          END IF
        END IF

        IF (fnc_index.eq.0) THEN
          CALL gen_inqsymb (string(ist:iend), sym_index, type, length,
     &                      address, readonly, ierr)
          IF (sym_index.eq.0) THEN
            PRINT *, '-- gen_factor --'
            PRINT *, '   symbol "',string(ist:iend),'" not defined!'
            ierr = 5
            RETURN
          ELSE
            del_oper = '@'
          END IF
        END IF

        IF (funct) THEN
CD        Print *,'    symbol -- has an argument -- pushing all stacks'
*         Push symbol on symbol stack
          nsymb = nsymb + 1
          sym_address(nsymb) = address
*         Push 'complete when index done' operator on operator stack
          ntopr     = ntopr     + 1
          nopr(lev) = nopr(lev) + 1
          oper(ntopr) = del_oper
          prio(ntopr) = 8
        END IF
      END IF

      IF (ierr.ne.0) RETURN

*     Find out how many bytes required for this operand, allocate
*     space from the workspace array, and if not a delayed operand
*     copy value to workspace

      READ (type(2:gen_ilen(type)), '(I3)') nbytes
      IF (.not.funct) CALL xcopy (nbytes,%val(cnf_pval(address)),
     :                            wksp(next_ws))

*     Update the operand stack

      ntopnd            = ntopnd + 1
      nopnd(lev)        = nopnd(lev) + 1
      opnd_type(ntopnd) = type
      opnd_addr(ntopnd) = cnf_preg(
#if HAVE_PERCENTLOC
     :   %LOC(wksp(next_ws))
#elif HAVE_INTRINSIC_LOC
     :   LOC(wksp(next_ws))
#else
    Do not have LOC or %LOC
#endif
     :    , isnew )

*     Update the workspace pointer (note: allocate 8 bytes min for 64-bit AXP)

CD    PRINT *, '     updating next_ws, from ', next_ws
CD    PRINT *, '     (using nbytes = ', nbytes, ')'
      next_ws = next_ws + 2*((nbytes-1)/8 + 1)
CD    PRINT *, '     to next_ws = ', next_ws

CD    Print *, '     ---------------------------'
CD    Print *, '        Operand stack summary'
CD    Print *, '      # operands: ', ntopnd
CD    Print *, '      last operand; type: ', type,' @ ',opnd_addr(ntopnd)
CD    Print *, '      next available workspace @ word ', next_ws
CD    Print *, '     ---------------------------'

      IF (funct) THEN
        st = next
        GO TO 1
      END IF

      RETURN
      END
