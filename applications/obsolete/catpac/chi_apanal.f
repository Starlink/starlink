*+  CHI_APANAL - Sort out types in expression parse system
      subroutine chi_apanal ( intype, outype, comnt, status)
*    Description :
*     Dummy evalation of the reverse Polish stack to sort out
*     operand and function argument types, units and comments.
*    Invocation :
*     CALL CHI_APANAL ( INTYPE, OUTYPE, COMNT, STATUS )
*    Parameters :
*     INTYPE = INTEGER(*)(OUTPUT)
*          Start type for each item on Xlist
*     OUTYPE = INTEGER(*)(OUTPUT)
*          Final type for each item on Xlist
*     COMNT = CHARACTER(*)(OUTPUT)
*          Comment field for each item on Xlist
*     STATUS = INTEGER(UPDATE)
*          Status variable
*    Method :
*     Perform dummy evaluation of Xlist stack, using Wlist as work
*     stack with each entry being a pointer to its provenance on
*     Xlist.  Hence calculate the UNITS and COMNT for the total
*     expression, and for each item of Xlist its intype, outype
*     and comnt:
*      If a constant then
*        intype is as returned by chi_decode
*        outype is intype
*        comnt = 'CONSTANT'
*        Xqual = transop (set when combined with known quantity)
*      Elseif an element
*        intype is as returned by chi_gettyp
*        outype = intype
*        comnt = Ecomnt
*        Xqual = transop
*      Elseif a comma
*        Xqual = 0 (flag for subsequent removal)
*      Elseif a function then
*        intype is result-type       this is an exception
*        outype = intype
*        comnt = Funccomnt
*        Xqual = argcount
*      Elseif unary operator then
*        ignore it for now
*      Else
*        match outypes of operands to get argtype, comnt
*        intype = argtype
*        if operator is relational then
*          outype = L
*        else
*          outype = intype
*        endif
*        Xqual = 0
*      Endif
*    Authors :
*     Alan Wood (STADAT::ARW) Esther Gershuny (RLVAD::EJG)
*    History :
*     30-Jan-1992: Original (rlvs::ejg)
*    Type definitions :
      implicit none
*    Global constants :
      include 'sae_par'     ! SAI symbolic constants
      include 'chi_par'
      include 'chipar_par'
      include 'chipar_err'
*    Import:
*    Export :
      integer intype(*)
      integer outype(*)
      character*(*) comnt(*)
*    Status :
      integer status
*    Global variables :
      include 'chiwrk_cmn'
*    External
      logical chr_simlr
      integer chr_len
*    Local constants :
      integer n_comma
      parameter(n_comma=CHI__OPLO+6)
      integer reloplo4     ! id of lowest 4-char relational op
      parameter (reloplo4=CHI__LOGLO-6)
      integer relophi4     ! id of highest 4-char relational op
      parameter (relophi4=CHI__LOGLO-1)
*    Local variables :
      logical logres     ! .TRUE. if expression evaluates to L_type
      integer in
      integer out
      integer j          ! pointer into Xlist(), intype(), outype()
      integer wp         ! pointer into Wlist()
      integer item       ! id of current item in Xlist
      integer ed         ! element descriptor
      integer jp         ! index in Xlist of 1st/only operand
      integer jp1        ! as jp, for 2nd operand
      integer fnumber    ! function id
      integer commas     ! count of consecutive commas in Xlist
      integer argcount   ! number of function arguments supplied
      integer argtype    ! operand/argument type
      integer restype    ! op/function result type
      integer transop    ! transformation operator
      integer kk         ! counter
      integer l2         ! string length
      character*(CHI__SZCUNIT*2) Wunit(CHI__MXITM)
*-
      if (status.ne.SAI__OK) then
        return
      endif
*
*   Establish if expression units are needed
*
      item= Xlist(Xsize)
      logres= (item.ge.CHI__LOGLO .and. item.le.CHI__LOGHI) .or.
     :        (item.ge.CHI__RELLO .and. item.le.CHI__RELHI) .or.
     :        (item.ge.reloplo4 .and. item.le.relophi4)
*
*   Sort out types, check function arg counts
*
      Xunit= 'UNKNOWN'
      Xcomnt= ' '
      j= 0
      wp = 0
      commas = 0
      do while (j.lt.Xsize .and. status.eq.SAI__OK)
        j= j+1
        item= Xlist(j)
*
*  comma
*
        if (item.eq.n_comma) then
          commas= commas+1
          Xqual(j)= 0
*
*  function call
*
        elseif (item.gt.CHI__FBASE) then
          fnumber= item - CHI__FBASE
          argcount= commas
          commas= 0
          restype= Fdtres(fnumber)
*   Check the function has the correct number of arguments
          if ((Ftargs(fnumber).gt.0 .and.
     :         argcount.ne.Ftargs(fnumber)) .or.
     :        (Ftargs(fnumber).lt.0 .and.
     :         argcount.lt.abs(Ftargs(fnumber)))) then
             status= CHI__IVFUN
          endif
*  set use-type of each arg
          do kk= 1,argcount
            jp= Wlist(wp-kk+1)
            outype(jp)= Fatype(fnumber,kk)
          enddo
*  ready to 'store' result of applying function
          wp= wp-argcount+1
          Wlist(wp)= j
          Wunit(wp)= Funits(fnumber)
          intype(j)= restype
          outype(j)= intype(j)
          comnt(j)= Fcomnt(fnumber)
          Xqual(j)= argcount
*  operand or simple operator - check for outstanding commas
*
        else
          if (commas.ne.0) then
            status= CHI__IVFUN
*
*  operand
*
          elseif (item.le.0) then
            ed= -item
            intype(j) = Etype(ed)
            outype(j)= intype(j)
            l2 = chr_len(Ecomnt(ed))
            comnt(j)= Ecomnt(ed)(:l2)
*   constants have intype < 0
            if (chr_simlr(Ecomnt(ed)(:8),'CONSTANT')) then
              intype(j)= -intype(j)
              if (outype(j).le.C_type) then
                outype(j)= -outype(j)
              endif
            endif
            Xqual(j)= 0
            wp= wp+1
            Wlist(wp)= j   ! where in Xlist it came from
            Wunit(wp)= Eunit(ed)
*
*  binary operator
*
          elseif (item.lt.CHI__UNOP) then
            if (item.ge.reloplo4 .and. item.le.relophi4) then
              item= item  - reloplo4 + CHI__RELLO
              Xlist(j)= item
            endif
            jp= Wlist(wp)           ! top of stack
            jp1= Wlist(wp-1)      ! next-to-top
            call chi_amatch(item,outype(jp1),outype(jp),argtype,
     :                        comnt(jp1),comnt(jp),comnt(j),status)
            if (status.eq.SAI__OK) then
              outype(jp1)= argtype
              outype(jp)= argtype
              if (item.ge.CHI__RELLO .and. item.le.CHI__RELHI) then
                restype= L_type
              else
                restype= argtype
              endif
*  ready to 'store' result of applying binary operator
              wp= wp-1
              Wlist(wp)= j
              if (.not. logres) then
                call chi_aunit(item,Wunit(wp),Wunit(wp+1),Wunit(wp),
     :                         status)
              endif
              intype(j)= argtype
              outype(j)= restype
              Xqual(j)= 0
            endif
          endif
        endif
      enddo
*
*  take each item of Xlist in turn and check that a conversion from the
*  input type to the output type is possible.
*
      j = 0
      do while (j.lt.Xsize .and. status.eq.SAI__OK)
        j= j+1
        if (Xlist(j).le.0) then
          in = abs(intype(j))
          out = abs(outype(j))
          if (in .eq. 1) then
            if (out .ne. 1) then
               STATUS = 1
            endif
          elseif (in.eq.2 .or. in.eq.3 .or. in.eq.4 ) then
             if (out .eq. 1) then
               STATUS = 2
             elseif ( out .eq. 5) then
               STATUS = 3
             endif
          elseif (in.eq.5) then
             if (out .eq. 1) then
                STATUS = 4
             elseif ( out.eq.2 .or. out.eq.3 .or. out.eq.4 ) then
                STATUS = 5
            endif
          endif
        endif
      enddo
*
*
*   Set units and comment of expression if appropriate
      if (status.eq.SAI__OK) then
         if (wp.ne.1) then
           status= CHI__IVSYN
         else
           if (.not. logres) then
             call chr_move(Wunit(1), Wunit(1))
             l2 = chr_len(Wunit(1))
             if (len(Xunit).ge.l2) then
                Xunit= Wunit(1)(:l2)
             endif
             Xcomnt= comnt(Xsize)
           endif
         endif
      endif
*
      return
      end
