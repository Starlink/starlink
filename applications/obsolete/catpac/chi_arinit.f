*+  CHI_ARINIT - Analyse expression into component relations
      subroutine chi_arinit( status)
*    Description :
*     Analyse expression into its component relational
*     sub-expressions
*    Invocation :
*     CALL CHI_RINIT( STATUS)
*    Parameters :
*     STATUS = INTEGER(UPDATE)
*          Status
*    Method :
*     Set up relational table entry with pointers to start, end,
*     and constant in Xlist, and indicating type of constant.
*     Identify simple shapes:
*       0:  logical operator only
*       1:  constant
*       2:  element
*       3:  expression (not terminated by relop)
*       4:  element constant relop
*       5:  constant element relop
*       6:  element unop constant relop
*       7:  constant element unop relop
*       8:  expression constant relop
*       9:  constant expression relop
*      10:  expression expression relop
*    Authors :
*     Alan Wood (STADAT::ARW) Esther Gershuny (RLVS::EJG)
*    History :
*     12-Feb-1992: Original
*    Type definitions :
      implicit none
*    Global constants :
      include 'sae_par'             ! SAI symbolic constnats
      include 'chi_par'
      include 'chipar_par'
      include 'chipar_err'
*    Import :
*    Status :
      integer status
*    Global variables :
      include 'chiwrk_cmn'
*    Local constants :
      integer u_plus
      parameter (u_plus=CHI__UNOP+1)
      integer u_minus
      parameter (u_minus=u_plus+1)
*    Local variables :
      logical new             ! TRUE at start of new relation
      logical goback          ! Stack control
      integer jcon(2)         ! Indexes in Xlist(*,xd) of constants
      integer jelm(2)         ! Indexes in Xlist(*,xd) of elements
      integer jumin(2)        ! Indexes in Xlist(*,xd) of unary minus's
      integer jop(2)          ! Indexes in Xlist(*,xd) of other ops
      integer limlo(2)        ! Lower limits
      integer limhi(2)        ! Upper limits
      integer rd              ! Relational descriptor
      integer item            ! item in Xlist
      integer last            ! Last item in sub-expression
      integer nargs           ! Operand count
      integer j               ! Loop counter
      integer wp              ! Workstack pointer
      integer k, i            ! Counters
*-
*   Begin
*
      if (status.ne.SAI__OK) then
         return
      endif
*
*   Validate parameters
*
      if (status.ne.SAI__OK) then
         return
      endif
*
*   Initialize for analysing relation
      new= .true.
      wp= 0
      j= 0
      do while (j.lt.Xsize)
         j= j+1
*
*    Go through the sub-expressions looking for limits
         item= Xlist(j)
*
*    Assign & initialize rd at start of new relation
         if (new) then
            new= .false.
            Rnext_free  = Rnext_free + 1
      if (Rnext_free .ge. 1 .and. Rnext_free .le. CHI__MXREL) then
         rd  =  Rnext_free
      endif
            if (status.eq.SAI__OK) then
               if (Xrdsize.lt.CHI__MXRDS) then
                  Xrdsize= Xrdsize+1
                  Xrdlist(Xrdsize)= rd
                  Rsplit(rd) = 0
                  Rend(rd) = 0
                  Rcons(rd) = 0
                  Rtype(rd) = 0
                  Rshape(rd) = 0
                  Rstart(rd)= j
               else
                  status= CHI__TOORX
               endif
            else
               status= CHI__NOREL
            endif
         endif
*
*   Element or constant:  stack it
         if (item.le.0) then
            wp= wp+1
            Wlist(j)= wp
            Wqual(j)= rd
*
*   Logical operator
         elseif (item.ge.CHI__LOGLO .and. item.le.CHI__LOGHI) then
            if (j.ne.Rstart(rd)) then
               j= j-1
            endif
            Rend(rd)= j
            new= .true.
*
*   Other operator
         else
            if (item.gt.CHI__FBASE) then
               nargs= Xqual(j) / 100
            elseif (item.ge.CHI__UNOP) then
               nargs= 1
            else
               nargs= 2
            endif
            wp= wp - nargs + 1
            if (nargs.gt.0) then
               goback= .true.
            else
               goback= .false.
            endif
            k= j-1
            do while (goback .and. k.gt.0)
               if (Wlist(k).le.wp) then
                  goback= .false.
               endif
               Wlist(k)= wp
               if (Wqual(k).ne.rd) then
                  if (rd .ge. 1 .and. rd .le. CHI__MXREL) then
                    Rnext_free  = min(rd, Rnext_free)
                  endif
                  Xrdlist(Xrdsize)= 0
                  Xrdsize= Xrdsize-1
                  rd= Wqual(k)
                  Rend(rd)= j
               endif
               k= k-1
            enddo
            Wlist(j)= wp
            Wqual(j)= rd
*
*   Relational operator
            if (item.ge.CHI__RELLO .and. item.le.CHI__RELHI) then
               Rend(rd)= j
               new= .true.
            endif
         endif
      enddo
*
*   Complete any outstanding part rd
      if (.not. new) then
          Rend(rd)= Xsize
      endif
*
*   Analyse each relation, setting Rshape & Rsplit
      do k= 1, Xrdsize
         rd= Xrdlist(k)
         last= Xlist(Rend(rd))
*
*   Single item
         if (Rstart(rd).eq.Rend(rd)) then
            Rsplit(rd)= Rend(rd)
*   Logical operator
            if (last.ge.CHI__LOGLO .and. last.le.CHI__LOGHI) then
               Rshape(rd)= 0
               Rtype(rd)= mod ( Xqual(Rend(rd)), 100 ) / 10
*   Constant
            elseif (last.lt.0) then
               Rshape(rd)= 1
               Rcons(rd)= Rend(rd)
               Rtype(rd)= mod ( Xqual(Rend(rd)), 10 )
*   Element
            elseif (last.eq.0) then
               Rshape(rd)= 2
               Rtype(rd)= mod ( Xqual(Rend(rd)), 10 )
            else
*   Other
               status= CHI__PRSER
            endif
*
*   Last is not an operator
         elseif (last.le.0) then
            status= CHI__PRSER
*
*   Last is operator (or function), but not relational or logical
         elseif (last.lt.CHI__RELLO .or. last.gt.CHI__RELHI) then
            Rshape(rd)= 3
            Rsplit(rd)= Rend(rd)
            Rtype(rd)= mod ( Xqual(Rend(rd)), 100 ) / 10
*
*   Last is relational operator:  full analysis
         else
            wp= 0
            do j = Rstart(rd), Rend(rd)-1
               item= Xlist(j)
               if (item.le.0) then
                  wp= wp+1
                  Wlist(j)= wp
               else
                  if (item.gt.CHI__FBASE) then
                     nargs= Xqual(j) / 100
                  elseif (item.gt.CHI__UNOP) then
                     nargs= 1
                  else
                     nargs= 2
                  endif
                  if (nargs.gt.1) then
                     goback= .true.
                  else
                     goback= .false.
                  endif
                  wp= wp-nargs+1
                  Wlist(j)= wp
                  i= j-1
                  do while (goback .and. i.ge.Rstart(rd))
                     if (Wlist(i).le.wp) then
                        goback= .false.
                     else
                        Wlist(i)= wp
                     endif
                     i= i-1
                  enddo
               endif
            enddo
*
*   Check operands of final (relational) operator
            if (wp.ne.2) then
               status= CHI__PRSER
*
*   Set Rsplit
            else
               i= Rend(rd)-1
               do while (Wlist(i).eq.2)
                  i= i-1
               enddo
               Rsplit(rd)= i
*
*   Now set Rshape
*   Check occurrences of elms, consts, unops, others on each side
               limlo(1)= Rstart(rd)
               limhi(1)= Rsplit(rd)
               limlo(2)= Rsplit(rd)+1
               limhi(2)= Rend(rd)-1
               do i= 1, 2
                  jcon(i)= 0
                  jelm(i)= 0
                  jumin(i)= 0
                  jop(i)= 0
                  do j= limlo(i), limhi(i)
                     if (Xlist(j).lt.0) then
                        if (jcon(i).eq.0) then
                           jcon(i)= j
                        else
                           jcon(i)= -1
                        endif
                     elseif (Xlist(j).eq.0) then
                        if (jelm(i).eq.0) then
                           jelm(i)= j
                        else
                           jelm(i)= -1
                        endif
                     elseif (Xlist(j).eq.u_minus) then
                        if (jumin(i).eq.0) then
                           jumin(i)= j
                        else
                           jumin(i)= -1
                        endif
                     elseif (Xlist(j).ne.u_plus) then
                        jop(i)= j
                     endif
                  enddo
               enddo
*
*   Set Rshape
               if (jcon(2).gt.0 .and. jelm(2).eq.0 .and.
     :              jop(2).eq.0 .and. jumin(2).eq.0) then
*   rhs is a constant
                  if (jop(1).eq.0 .and. jcon(1).eq.0) then
                     if (jelm(1).gt.0) then
                        if (jumin(1).eq.0) then
                           Rshape(rd)= 4
                        elseif (jumin(1).eq.jelm(1)+1) then
                           Rshape(rd)= 6
                        else
                           Rshape(rd)= 8
                        endif
                     else
                        Rshape(rd)= 8
                     endif
                  else
                     Rshape(rd)= 8
                  endif
                  Rcons(rd)= jcon(2)
                  Rtype(rd)= mod ( Xqual(Rcons(rd)), 10)
               elseif (jcon(1).gt.0 .and. jelm(1).eq.0 .and.
     :              jop(1).eq.0 .and. jumin(1).eq.0) then
*   lhs is a constant
                  if (jop(2).eq.0 .and. jcon(2).eq.0) then
                     if (jelm(2).gt.0) then
                        if (jumin(2).eq.0) then
                           Rshape(rd)= 5
                        elseif (jumin(2).eq.jelm(2)+1) then
                           Rshape(rd)= 7
                        else
                           Rshape(rd)= 9
                        endif
                     else
                        Rshape(rd)= 9
                     endif
                  else
                     Rshape(rd)= 9
                  endif
                  Rcons(rd)= jcon(1)
                  Rtype(rd)= mod ( Xqual(Rcons(rd)), 10)
               else
*   neither lhs nor rhs is a constant
                  Rshape(rd)= 10
                  Rtype(rd)= mod ( Xqual(Rend(rd)), 100 ) / 10
               endif
            endif
         endif
      enddo
*
      return
      end
