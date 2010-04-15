
*+  CHP_APREPAR - Pre parse CONVERT
      subroutine chi_aprepar(expr, status)
*    Description :
*     Part of the catalogue handling interface plus.  Preparse an expression
*     converting CONVERT function into a radian value.
*    Invocation
*     call chi_aprepar(expr, status)
*    Parameters
*     EXPR=CHAR(INPUT)
*      Expression.
*     STATUS=INTEGER(UPDATE)
*      Status variable
*    Method :
*           Apply conversion.
*    Authors :
*     Alan Wood (STADAT::ARW)
*    History :
*     5 Mar 1991:  Original
*    Type Definitions :
      implicit none
*    Global constants :
      include 'sae_par'                 ! SAI Symbolic Constants
      include 'chi_err'
      include 'chipar_err'
      include 'chi_par'
      include 'chipar_par'
      include 'chiwrk_cmn'
      include 'chipar2_par'
*    Import :
      character*(*) expr
*    Export :
      integer status
*    External references :
      integer chr_len
      logical chr_simlr
*    Local variables :
      integer explen
      integer l2
      integer i
      integer j
      integer newi
      integer endi
      character*(chi__szexp) tempexpr
      character*(1) csign
      integer counter
      integer ideg
      integer iabsdeg
      integer iamin
      integer iasec
      integer iramin
      integer imin
      integer ihr
      integer isec
      integer jsec
      integer w
      real rmin
      real rsec
      real rasec
      real ramin
      integer jstat
      integer fldcount
      integer len
      double precision dsec
      double precision dasec
      double precision dval
      logical simlr
*-
*
*    Begin:
*
      if (status .ne. sai__ok) return
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
            else
               i = i + l2 + 1
            endif
         else
            i = i + 1
         endif
      enddo
*

      i = 1
      newi = 1
      call chr_fill(' ',tempexpr)
      do while(i .le. explen .and. status .eq. SAI__OK)
         if (expr(i:i) .eq. 'C') then
*   Possible start of convert
           if (expr(i:i+6) .eq. 'CONVERT') then
             i = i + 7
*   Expecting  a '('
               if (expr(i:i+10) .eq. '("DEGREE","') then
                  i = i + 11
                  endi = index(expr(i:i+14),'"')
                  if (endi .ne. 0) then
                    w = endi - 1
                    read(expr(i:endi), 10, err=1) dval
  10       format(BN, D<w>.0)
                    dval = dval * DD2R
                    write(tempexpr(newi:newi+10),'(F10.8)',err=1) dval
                    newi = newi + 10
                    i = i + w + 3
                  else
                    status = CHI__PRSER
                  endif
               elseif ((expr(i:i+15) .eq. '("SDD MM SS.S","') .or.
     :                (expr(i:i+15) .eq. '("SDD:MM:SS.S","')) then
                  i = i + 16
                  endi = index(expr(i:i+14),'"')
                  if (endi .ne. 0) then
                    w = endi - 1
                    read (expr(i:endi), '(I2,1X,I2,1X,F4.1)', err=1)
     :                    ihr, imin, rsec
                    dsec = rsec
                    call sla_dtf2r(ihr, imin, dsec, dval, jstat)
                    write(tempexpr(newi:newi+10),'(F10.8)',err=1) dval
                    newi = newi + 10
                    i = i + w + 3
                  else
                    status = CHI__PRSER
                  endif
*
               elseif ((expr(i:i+13) .eq. '("SDD MM SS","') .or.
     :                (expr(i:i+13) .eq. '("SDD MM SS","')) then
                  i = i + 14
                  endi = index(expr(i:i+14),'"')
                  if (endi .ne. 0) then
                    w = endi - 1
                    read (expr(i:endi), '(I2,1X,I2,1X,I2)', err=1)
     :                    ihr, imin, isec
                    dsec = isec
                    call sla_dtf2r(ihr, imin, dsec, dval, jstat)
                    write(tempexpr(newi:newi+10),'(F10.8)',err=1) dval
                    newi = newi + 10
                    i = i + w + 3
                  else
                    status = CHI__PRSER
                  endif
               elseif ((expr(i:i+10) .eq. '("SDD MM","') .or.
     :                (expr(i:i+10) .eq. '("SDD MM","')) then
                  i = i + 11
                  endi = index(expr(i:i+14),'"')
                  if (endi .ne. 0) then
                    w = endi - 1
                    read (expr(i:endi), '(I3, 1X, I2)', err=1)
     -                   ideg, iamin
                    dasec = 0.0
                    iabsdeg = abs(ideg)
                    call sla_daf2r(iabsdeg, iamin, dasec, dval, jstat)
                    dval = dval*sign( 1.0, float(ideg) )
                    write(tempexpr(newi:newi+10),'(F10.8)',err=1) dval
                    newi = newi + 10
                    i = i + w + 3
                  else
                    status = CHI__PRSER
                  endif
*
               elseif ((expr(i:i+14) .eq. '("HH MM SS.S","') .or.
     :                (expr(i:i+14) .eq. '("HH:MM:SS.S","')) then
                  i = i + 15
                  endi = index(expr(i:i+14),'"')
                  if (endi .ne. 0) then
                    w = endi - 1
                    read (expr(i:endi), '(I2,I2,F4.1)', err=1) ihr, imin, rsec
                    dsec = rsec
                    call sla_dtf2r(ihr, imin, dsec, dval, jstat)
                    write(tempexpr(newi:newi+10),'(F10.8)',err=1) dval
                    newi = newi + 10
                    i = i + w + 3
                  else
                    status = CHI__PRSER
                  endif
*
               elseif ((expr(i:i+12) .eq. '("HH MM SS","') .or.
     :                (expr(i:i+12) .eq. '("HH:MM:SS","')) then
                  i = i + 13
                  endi = index(expr(i:i+14),'"')
                  if (endi .ne. 0) then
                    w = endi - 1
                    read (expr(i:endi), '(I2,1XI2,1X,I2)', err=1)
     :                    ihr, imin, isec
                    dsec = isec
                    call sla_dtf2r(ihr, imin, dsec, dval, jstat)
                    write(tempexpr(newi:newi+10),'(F10.8)',err=1) dval
                    newi = newi + 10
                    i = i + w + 3
                  else
                    status = CHI__PRSER
                  endif
*
               elseif ((expr(i:i+9) .eq. '("HH MM","') .or.
     :                (expr(i:i+9) .eq. '("HH:MM","')) then
                  i = i + 10
                  endi = index(expr(i:i+14),'"')
                  if (endi .ne. 0) then
                    w = endi - 1
                    read (expr(i:endi), '(I2,1X,I2)', err=1) ihr, imin
                    isec = 0
                    dsec = isec
                    call sla_dtf2r(ihr, imin, dsec, dval, jstat)
                    write(tempexpr(newi:newi+10),'(F10.8)',err=1) dval
                    newi = newi + 10
                    i = i + w + 3
                  else
                    status = CHI__PRSER
                  endif
               else
                 status = CHI__PRSER
               endif
           else
             tempexpr(newi:newi) = expr(i:i)
             i = i + 1
             newi = newi + 1
           endif
         else
           tempexpr(newi:newi) = expr(i:i)
           i = i + 1
           newi = newi + 1
         endif
      enddo
      expr = tempexpr
  1   continue
      return
      end
