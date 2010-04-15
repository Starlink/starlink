*+  CHI_ATRANS - Translates expression into reverse Polish
      subroutine chi_atrans ( nitems, status)
*    Description :
*     Translates expression into reverse Polish.
*    Invocation
*     CALL CHI_ATRANS ( NITEMS, STATUS)
*    Parameters :
*     NITEMS = INTEGER(INPUT)
*          Number of items in expression
*     STATUS = INTEGER(UPDATE)
*          Status variable
*    Method :
*     <description of how the application works>
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     Alan Wood (STADAT::ARW) and Esther Gershuny (rlvad::ejg)
*    History :
*     30-Jan-1992: Original (rlvs::ejg)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      include 'sae_par'
      include 'chi_par'
      include 'chipar_par'
      include 'chipar_err'
*    External references :
      integer chi_aprecop
      external chi_aprecop
*    Import
      integer nitems
*    Status :
      integer status
*    Global variables :
      include 'chiwrk_cmn'       ! work stacks holding item lists
*    Local Constants :
      integer szitm
      parameter (szitm = CHI__SZCNAME + CHI__SZCUNIT + 5)
      integer n_bra
      parameter (n_bra = chi__oplo+1)
      integer n_ket
      parameter (n_ket = n_bra+6)
      integer n_comma
      parameter (n_comma = n_bra+5)
*    Local variables :
      logical unstackflag           ! controls operator stack
      integer op_stack(CHI__MAXOP)  ! operator stack
      integer op_ptr                 ! operator stack pointer
      integer op_num                 ! operator id
      integer wtop                   ! current top of Wlist
      integer newprec               ! precedence of current operator
      integer ed                    ! element descriptor
      integer i,j,l2
      character*(szitm) operand
*-
      if (status.ne.SAI__OK) then
         return
      endif
*
*   Transform array of items into reverse Polish list
*
      ed = 0
      Xsize= 0
      op_ptr= 0
      wtop= nitems
      i= 0
      do while (i.lt.nitems .and. status.eq.SAI__OK)
        i= i+1
        op_num= Wlist(i)
*
*  operand
*
        if (op_num.eq.CHI__NULID) then
           j= Wqual(i)
           l2= index(Wstring(j:),'!') - 1
           if (l2.le.0) then
              status= CHI__PRSER
           else
              operand= Wstring(j:j+l2-1)
              call chi_apelm( operand, ed, status)
              call chi_apush(-ed, Xlist, Xsize, CHI__MXITM,
     :                   CHI__TOOIT, status)
              Worigin(Xsize)= j
           endif
*  open bracket - stack with precedence -2
        elseif (op_num.eq.n_bra) then
           call chi_apush(i, op_stack, op_ptr, CHI__MAXOP,
     :                   CHI__TOOOP, status)
*  close bracket - unstack operators to output string until open bracket
        elseif (op_num.eq.n_ket) then
           unstackflag= .true.
           do while (unstackflag .and. status.eq.SAI__OK)
                call chi_apull(j,op_stack,op_ptr,CHI__MISMA,status)
                if (Wlist(j).eq.n_bra) then
                   unstackflag= .false.
                else
                   call chi_apush(Wlist(j), Xlist, Xsize,
     :                           CHI__MXITM, CHI__TOOIT, status)
                   Worigin(Xsize)= Wqual(j)
                endif
                op_ptr= op_ptr-1
           enddo
*  other operator - unstack to output string till lower precedence
*  operator found, then stack new operator
        else
           newprec= chi_aprecop(op_num,status)
           if (op_num.eq.n_comma) newprec= newprec+1
           unstackflag= .true.
           do while (unstackflag .and. op_ptr.gt.0)
                call chi_apull(j,op_stack,op_ptr,CHI__UFLOP,status)
                if (newprec.gt.chi_aprecop(Wlist(j),status)) then
                   unstackflag= .false.
                else
                   call chi_apush(Wlist(j), Xlist, Xsize,
     :                           CHI__MXITM, CHI__TOOIT, status)
                   Worigin(Xsize)= Wqual(j)
                   op_ptr= op_ptr-1
                endif
           enddo
           call chi_apush(i, op_stack, op_ptr, CHI__MAXOP,
     :                   CHI__TOOOP, status)
           if (op_num.gt.CHI__FBASE) then
              if (i.eq.nitems) then
                status= CHI__IVSYN
              else
                l2= index(Wstring(Wqual(i):),'!') + Wqual(i) - 2
                call chi_apush(n_bra, Wlist, wtop, CHI__MXITM,
     :                        CHI__TOOIT, status)
                Wqual(wtop)= l2
                call chi_apush(wtop, op_stack, op_ptr, CHI__MAXOP,
     :                        CHI__TOOOP, status)
                if (Wlist(i+1).ne.n_ket) then
                  call chi_apush(n_comma, Wlist, wtop, CHI__MXITM,
     :                          CHI__TOOIT, status)
                  Wqual(wtop)= l2
                  call chi_apush(wtop, op_stack, op_ptr, CHI__MAXOP,
     :                          CHI__TOOOP, status)
                endif
              endif
           endif
        endif
      enddo
*
*  end_of_string - unstack all remaining operators
*
      do while (op_ptr.gt.0 .and. status.eq.SAI__OK)
         call chi_apull(j,op_stack,op_ptr,CHI__UFLOP,status)
         if (Wlist(j).eq.n_bra .or. Wlist(j).eq.n_ket) then
            status= CHI__MISMA
         endif
         call chi_apush(Wlist(j), Xlist, Xsize,
     :                 CHI__MXITM, CHI__TOOIT, status)
         Worigin(Xsize)= Wqual(j)
         i= Xsize
         op_ptr= op_ptr-1
      enddo
*
*
      return
      end
