
*+  CHI_ARELW - Evaluate relation to work stack
      subroutine chi_arelw(rd, restype, wptr, wstop, status)
*    Description :
*     Evaluate a relational (sub-)expression from the expression
*     tables.
*    Invocation :
*     CALL CHI_ARELW ( RD, RESTYPE, WPTR, WSTOP, STATUS)
*    Parameters :
*     RD = INTEGER(INPUT)
*          Relational descriptor
*     RESTYPE = INTEGER(OUTPUT)
*          Operation result type, 1:5
*     WPTR = INTEGER(UPDATE)
*          Pointer into W<t>val arrays
*     WSTOP = INTEGER(UPDATE)
*          Pointer into Wstring
*     STATUS = INTEGER(INPUT)
*          Status variables
*    Method :
*     Format of Xlist, Xqual
*                Xlist                  Xqual
*                -----                  -----
*     simple op  op_id                  transop*100 + cvrtop
*     function   fnumber+CHI__FBASE     argcount*100 + cvrtop
*     ed         0                      ed*100 + transop*10 + typtr
*     constant   -ptr to Xstring/X<t>val (strsiz*10) + typtr
*
*     where
*       typtr is 1 of 1:5 for L I R D C
*       cvrtop = intype*10 + nextype converts intype to nextype
*       transop = 1 for hmsrad, 2 for dmsrad
*
*     Format of Wlist, Wqual
*                          Wlist              Wqual
*                          -----              -----
*     String value         ptr into Wstring   strsiz*10 + typtr
*     Non-string value     ptr into W<t>val   typtr
*
*     if expected result is logical and sub-expression is relation then
*        if there is a null value in the sub-expression then
*           nulres= .true.
*        endif
*     endif
*     if nulres then
*        push .false. onto workstack
*     else
*        do for each item in the relational sub-expression
*          if it is a constant then
*             copy it into W<t>val or Wstring
*             generate the appropriate Wlist value
*             replicate its Xqual value in Wqual
*          elseif it is a field then
*             extract it in the appropriate type from the catalogue
*             record, putting its value in W<t>val or Wstring
*             set its Wlist, Wqual
*          elseif it is a function then
*             if its arguments are type 'C' then
*                retrieve them from Wstring
*             else
*                retrieve them from Wdval
*             endif
*             apply the function
*             put (converted) result in W<t>val or Wstring
*             set its Wlist, Wqual
*          else       !ie it is a simple operator
*             perform operation, putting (converted) result into
*             W<t>val or Wstring
*             set its Wlist, Wqual
*          endif
*        enddo
*     endif
*    Authors :
*     Alan Wood (STADAT::ARW) Esther Gershuny (rlvad::ejg)
*    History :
*     12-Feb-1992: Original
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      include 'sae_par'
      include 'chi_par'
      include 'chipar_par'
      include 'chipar_err'
*    Import :
      integer rd
*    Export :
      integer restype
*    Update :
      integer wptr
      integer wstop
*    Status :
      integer status
*    Global variables :
      include 'chiwrk_cmn'
*    External :
      integer chr_len
*    Local constants :
      integer no_op
      parameter (no_op=CHI__OPLO)
*    Local variables :
      integer in
      character*(CHI__SZREC) cvalue
      character*(CHI__SZREC) cargs(CHI__MXARG)  ! fun arg arrays
      real*8 dargs(CHI__MXARG)                  !  "   "    "
      logical logres      ! .true. if sub-expr is to give logical result
      logical nulres      ! .true. if sub-expr contains a null value
      integer op_id          ! operator id
      integer fnumber        ! function id
      integer xd             ! expression descriptor
      integer ed             ! descriptor of catalogue element
      integer j              ! pointer into Xlist
      integer strsiz         ! string size
      integer argcount       ! number of params to function
      integer typtr          ! operand type, 1:5 for L,I,R,D,C
      integer funtype        ! function result type, 1:5
      integer nextype        ! type required for next use
      integer argtype        ! operator or function argument type
      integer cvrtop         ! type convert op code
      integer transop        ! transformation operator
      integer kp             ! pointer
      integer k              ! counter
*-
*
      if (status.ne.SAI__OK) then
         return
      endif
      restype= mod ( Xqual(Rend(rd)), 10)
      logres= restype .eq. L_type   .and.
     :        ( ( Xlist(Rend(rd)) .ge. CHI__RELLO .and.
     :            Xlist(Rend(rd)) .le. CHI__RELHI ) )
*
      j= Rstart(rd)
      do while (j.le.Rend(rd) .and. status.eq.SAI__OK)
*
*   constant just put it onto the working stack according to it's output type
        if (Xlist(j).lt.0) then
           wptr= wptr+1
           Wqual(wptr)= Xqual(j)
           typtr= mod(Wqual(wptr),10)
            kp= -Xlist(j)
              Wlist(wptr)= wptr
              goto (101, 102, 103, 104, 105, 190) typtr
101           Wlval(wptr)= Xlval(kp)
              goto 190
102           Wival(wptr)= Xival(kp)
              goto 190
103           Wrval(wptr)= Xrval(kp)
              goto 190
104           Wdval(wptr)= Xdval(kp)
              goto 190
105           Wcval(wptr)= Xcval(kp)
              in = chr_len(wcval(wptr))
190           continue
*
*   catalogue element convert from it's input type to it's output type and
*   put on a the workink stack.
*
        elseif (Xlist(j).eq.0) then
           ed= Xqual(j)/100
           typtr= Xqual(j) - 100*ed
           transop= typtr/10
           typtr= typtr - 10*transop
           wptr= wptr+1
              goto (201, 202, 203, 204, 205, 290) typtr
201           Wlval(wptr) = logvals(ed)
              goto 290
202           call chi_aconvtoi(ed,Wival(wptr),status)
              goto 290
203           call chi_aconvtor(ed,Wrval(wptr),status)
              goto 290
204           call chi_aconvtod(ed,Wdval(wptr),status)
              goto 290
205           Wcval(wptr) = charvals(ed)
290           continue
              Wlist(wptr)= wptr
              Wqual(wptr)= typtr
*
*   simple op. Perform the operation knowing that the argument type gives the
*   working array on which to perform the operation. The types of the operands
*   have already been converted so the values are on the correct work stack.
*   The result is converted to the output type of the argument given in nextype.
*
        elseif (Xlist(j).lt.CHI__FBASE) then
           op_id= Xlist(j)
           cvrtop= Xqual(j)
           argtype= cvrtop/10
           nextype= cvrtop - 10*argtype
*   operational type.
           goto (301, 302, 303, 304, 305) argtype
301        call chi_aopwl(op_id,nextype,wptr,wstop,status)
           goto 390
302        call chi_aopwi(op_id,nextype,wptr,wstop,status)
           goto 390
303        call chi_aopwr(op_id,nextype,wptr,wstop,status)
           goto 390
304        call chi_aopwd(op_id,nextype,wptr,wstop,status)
           goto 390
305        call chi_aopwc(op_id,nextype,wptr,wstop,status)
390        continue
*
*   function call
        else
          fnumber= Xlist(j)-CHI__FBASE
          argcount= Xqual(j)/100
          cvrtop= Xqual(j) - 100*argcount
          funtype= cvrtop/10
          nextype= cvrtop - 10*funtype
          argtype= Fdtres(fnumber)
          op_id= no_op
*   branch on argument type:  C - character
*   all non-character function calls have real*8 args
          if (argtype.ne.C_type) then
            do k= 1, argcount
               kp= Wlist(wptr-argcount+k)
               dargs(k)= Wdval(kp)
            enddo
            wptr= wptr-argcount+1
            if (funtype.eq.I_type) then
              call chi_afundi(fnumber,argcount,dargs,Wival(wptr),
     :                        status)
              call chi_aopwi(op_id,nextype,wptr,wstop,status)
            elseif (funtype.eq.D_type) then
              call chi_afundd(fnumber,argcount,dargs,Wdval(wptr),
     :                       status)
              call chi_aopwd(op_id,nextype,wptr,wstop,status)
            else
              status= CHI__NTSUP
            endif
          endif
        endif
*
*   next item
*
          j= j+1
      enddo
*
      return
      end
