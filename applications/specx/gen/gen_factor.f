*-----------------------------------------------------------------------

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

      INCLUDE  'eval_ae4.inc'

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

D     Type *, '-- gen_factor --'

      st   = next

*     Check that next item is not a left bracket - if it is then
*     push the stack and find the next non-blank character

    1 CONTINUE

      DO WHILE (string(st:st).eq.'(' .AND. st.le.ils)
D       Type *,'   "(" found = pushing stack...'
        lev = lev + 1
        st  = st  + 1
D       Type *,'    level no', lev
        DO WHILE (string(st:st).eq.' ' .AND. st.le.ils)
          st = st + 1
        END DO
      END DO

      IF (st.gt.ils) THEN
        ierr = 1
        RETURN
      END IF

*     Parse off the next item

      CALL gen_parsename (string, st, ist, iend, numeric, strconst,
     &                    uminus, lbracket, funct, next, ierr)
D     Type *, '     parsed factor: ', string(ist:iend)

      IF (ierr.ne.0) RETURN

*     If unary-minus flag set then push unary minus operator (%) on opr stack

      IF (uminus) THEN
D       Type *, '     Unary minus flag set - pushing operator stack...'
        nopr(lev)   = nopr(lev) + 1
        ntopr       = ntopr     + 1
        oper(ntopr) = '%'
        prio(ntopr) = 7
      END IF

*     If operand of any unary operator is bracketed then push stack...

      IF (lbracket) THEN
*       TYPE *, '    left bracket returned, starting again...'
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
          address = %loc(ivalue)
D         Type *,'     integer value read'
        ELSE IF (gen_floating (string(ist:iend), idig, fdig)) THEN
          type   = 'R4'
          form   = 'F'
          WRITE (form(2:lf), '(I2.2,''.'',I3.3)') idig+fdig+1, 
     &                                             MAX (0, fdig)
          ierr = gen_readnum (string(ist:iend), type, form, rvalue)
          address = %loc(rvalue)
D         Type *,'     real*4 value read'
        ELSE IF (gen_eformat (string(ist:iend), idig, fdig, edig)) THEN
          type   = 'R4'
          form   = 'E'
          WRITE (form(2:lf), '(I2.2,''.'',I3.3)') idig+fdig+edig+2,
     &                                             MAX (0, fdig)
          ierr = gen_readnum (string(ist:iend), type, form, rvalue)
          address = %loc(rvalue)
D         Type *,'     real*4 value read'
        ELSE IF (gen_dformat (string(ist:iend), idig, fdig, edig)) THEN
          type   = 'R8'
          form   = 'E'
          WRITE (form(2:lf), '(I2.2,''.'',I3.3)') idig+fdig+edig+2,
     &                                             MAX (0, fdig)
          ierr = gen_readnum (string(ist:iend), type, form, dvalue)
          address = %loc(dvalue)
D         Type *,'     real*8 value read'
        END IF

      ELSE IF (strconst) THEN
        CALL gen_hdnorm (string(ist:iend), string(ist:iend), ls, ierr)
        WRITE (type, '(''C'',I3.3)') ls
        address = %loc(string(ist:ist))
D       Type *,'     string constant read: length =', ls
D       Type *,'     variable type = ', TYPE
D       Type *,'     ', string(ist:ist+ls-1)

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
            TYPE *, '-- gen_factor --'
            TYPE *, '   symbol "',string(ist:iend),'" not defined!'
            ierr = 5
            RETURN
          ELSE
            del_oper = '@'
          END IF
        END IF

        IF (funct) THEN
D         Type *,'    symbol -- has an argument -- pushing all stacks'
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

      READ (type(2:gen_ilen(type)), '(I)') nbytes
      IF (.not.funct) CALL xcopy (nbytes,%val(address),wksp(next_ws))

*     Update the operand stack

      ntopnd            = ntopnd + 1
      nopnd(lev)        = nopnd(lev) + 1
      opnd_type(ntopnd) = type
      opnd_addr(ntopnd) = %LOC(wksp(next_ws))

*     Update the workspace pointer (note: allocate 8 bytes min for 64-bit AXP)

D     TYPE *, '     updating next_ws, from ', next_ws
D     TYPE *, '     (using nbytes = ', nbytes, ')'
      next_ws = next_ws + 2*((nbytes-1)/8 + 1)
D     TYPE *, '     to next_ws = ', next_ws

D     Type *, '     ---------------------------'
D     Type *, '        Operand stack summary'
D     Type *, '      # operands: ', ntopnd
D     Type *, '      last operand; type: ', type,' @ ',opnd_addr(ntopnd)
D     Type *, '      next available workspace @ word ', next_ws
D     Type *, '     ---------------------------'

      IF (funct) THEN
        st = next
        GO TO 1
      END IF

      RETURN
      END
