*+  CHI_APCONS - Evaluate constants in expression parse system
      subroutine chi_apcons (intype, outype, comnt, status)
*    Description :
*     Evaluate the constants for an expression in the reverse Polish
*     stack and finalize the corresponding qualifier stack.
*    Invocation :
*     CALL CHI_APCONS ( INTYPE, OUTYPE, COMNT, STATUS )
*    Parameters :
*     INTYPE = INTEGER(*)(INPUT)
*          Start type of each item in Xlist
*     OUTYPE = INTEGER(*)(INPUT)
*          Final type of each item in Xlist
*     COMNT = CHARACTER(*)(INPUT)
*          Comment field of each item in Xlist
*     STATUS = INTEGER(UPDATE)
*          Status variable
*    Method :
*     Generate a constant in the appropriate part of W<t>val or
*     in cvalue then move it to X<t>val or Xstring.   Apply any
*     unary operator whose operand is a constant.
*    Authors :
*     Alan Wood (STADAT::ARW) Esther Gershuny (RLVAD::EJG)
*    History :
*     6-Feb-1992: Original
*    Type definitions :
      implicit none
*    Global constants :
      include 'sae_par'     ! SAI symbolic constants
      include 'chi_par'
      include 'chipar_par'
      include 'chipar_err'
*    Import:
      integer intype(*)
      integer outype(*)
      character*(*) comnt(*)
*    Status :
      integer status
*    Global variables :
      include 'chiwrk_cmn'
*    External :
*    Local constants :
      integer n_comma
      parameter(n_comma=CHI__OPLO+6)
      integer n_not
      parameter (n_not=CHI__UNOP)
*    Local variables :
      character*(CHI__SZCVAL) cval
      integer j          ! pointer into Xlist, intype, outype, comnt
      integer wp         ! pointer into Wlist()
      integer item       ! id of current item in Xlist
      integer ed         ! element descriptor
      integer jp         ! index in Xlist of 1st/only operand
      integer argcount   ! number of function arguments supplied
      integer argtype    ! operand/argument type
      integer xtopv      ! current top of X<t>val
      integer xtops      ! current top of Xstring
      integer xvptr      ! index of constant in X<t>val
      integer xsptr      ! start of current constant in Xstring
      integer tin        ! intype of current
      integer tout       ! outype of current item
      integer kk         ! counter
*-
*   Check for error in preceding pass
*
      if (status.ne.SAI__OK) then
        return
      endif
*
*  Evaluate constants and process unary operators
*  Finalize the Xlist & Xqual
*
      j= 0
      wp= 0
      xtopv= 0
      xtops= 0
      Xstring= ' '
*
*  take each item of Xlist in turn
*
      do while (j.lt.Xsize .and. status.eq.SAI__OK)
        j= j+1
        item= Xlist(j)
        if (item.gt.CHI__FBASE .or.
     :     (item.lt.CHI__UNOP .and. item.ne.n_comma) ) then
           tout = abs(outype(j))
        endif
        if (item.lt.0) then
          ed= -item
          if (intype(j).lt.0) then
*
*   constant
*
            xsptr= xtops+1
            call chi_aconval(ed,outype(j),comnt(j),tout,cval,status)
            call chi_awtox( tout, xtopv, status)
              Xqual(j)= tout
              Xlist(j)= -xtopv
*            endif
          else
*
*   element
            Xqual(j)= ed*100 + tout
            Xlist(j)= 0
          endif
*   constant or element
          wp= wp+1
          Wlist(wp)= j
*
*   function
        elseif (item.gt.CHI__FBASE) then
          argcount= Xqual(j)
          tin = abs(intype(j))
          Xqual(j)= 100*argcount + 10*tin + tout
          wp= wp-argcount+1
          Wlist(wp)= j
*
*   binary op
        elseif (item.lt.CHI__UNOP) then
          if (item.ne.n_comma) then
            tin = abs(intype(j))
            Xqual(j)= 10*tin + tout
            wp= wp-1
            Wlist(wp)= j
          endif
*
*   unary operator
        else
          jp= Wlist(wp)
          if (intype(jp).gt.0) then
*   operand is not constant
            argtype= outype(jp)
            outype(j)= outype(jp)
            if (item.eq.n_not) then
              argtype= L_type
            elseif (intype(jp).eq.L_type) then
              argtype= I_type
            endif
            outype(jp)= argtype
            tout = abs(outype(j))
            Xqual(jp)= 10*(Xqual(jp)/10) + tout
            intype(j)= outype(jp)
            tin= tout
            tout = abs(outype(j))
            Xqual(j)= 10*tin + tout
            Wlist(wp)= j
*
*   operand is constant
          else
            tout = abs(outype(j))
             xvptr= -Xlist(jp)
            call chi_aconopx(item, xvptr, tout, status)
            Xqual(j)= 0         ! flag for removal
          endif
        endif
      enddo
*
*   Finally dress stack and set expression type
*
      j= 1
      do while (j.le.Xsize .and. status.eq.SAI__OK)
        if (Xqual(j).eq.0) then
           do kk = j,Xsize-1
            Xlist(kk) = Xlist(kk+1)
            Xqual(kk) = Xqual(kk+1)
            Worigin(kk) = Worigin(kk+1)
            intype(kk) = intype(kk+1)
            outype(kk) = outype(kk+1)
            comnt(kk)= comnt(kk+1)
           enddo
          Xlist(Xsize)= 0
          Xqual(Xsize)= 0
          Xsize= Xsize-1
        else
          j= j+1
        endif
      enddo
      Xtype= mod ( Xqual(Xsize), 10)
*
      return
      end
