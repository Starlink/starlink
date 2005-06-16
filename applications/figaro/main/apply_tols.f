      subroutine apply_tols(ifarc,status)
*+
* Name:
*    APPLY_TOLS

* Invocation:
*    CALL APPLY_TOLS(IFARC,STATUS)
*
* Purpose:
*    Test fits against current tolerances
*
* Description:
*    Apply tolerances to a results cube. If called interactively the
*    user is given the option of altering the values used. Bad fits are
*    flagged as such.
*
* Arguments:
*    IFARC = LOGICAL (Given)
*        Whether in ARC2D or LONGSLIT, .true. for the
*                          former
*    STATUS = INTEGER (Given and returned)
*        Error status, 0=ok
* Global variables:
*    LINE_COUNT = INTEGER (Given)
*        Number of lines (include file arc_dims)
*    REJECT(SZREJT) = LOGICAL ARRAY (Given)
*        Tolerances to apply (include file arc_dims)
*    TOLERANCE(MAXTOL) = REAL ARRAY (Given)
*        (include file arc_dims)
*    WAVDIM = INTEGER (Given)
*        Number of channels in data (include file arc_dims)
*    XPTR = INTEGER (Given)
*        Pointer to X array (include file arc_dims)
*
* Functions/subroutines referenced:
*    SET_REJECT           : Set tolerances to reject on
*    SET_TOLS             : Set values of tolerances
*    CHECK_TOLS           : Check tolerances
*    PAR_WRUSER   (PAR)   : Write character string to user
*
* Authors:
*    TNW: T.N.Wilkins Manchester until 1/89, Cambridge until 9/92, then Durham
* History:
*  TNW: Altered 12/7/88 to allow tolerances to be altered without being applied
*       (for use in batch for example).
*  TNW: 20/8/91 Much altered-can handle batch case as well
*  TNW: 3/8/93 SET_TOLS now sets APPLY if SET_TOLS is used.
*  TNW: 3/2/94 Update mask in interactive as well as batch mode
*  TNW: 15/3/94 LOGFAIL incorporated into REJECT
*-
      implicit none
      logical ifarc
      integer status
      include 'arc_dims'
      include 'CNF_PAR'          ! For CNF_PVAL function
      integer istore,jstore
      integer pstat
      logical apply
      integer i,itol,menu,len1
      character*80 tolset
      integer line,nreject,ilen,chr_len,lst,ind
      logical par_given
      character*72 chars
      character*5 tolnam(maxtol+1)
      character*11 dict(MAXREJ+1)
      character*2 bss,bsn
      data bss/'\\'/
      bsn = bss(1:1)//'n'

* Note that the last entry ("EXIT") is only used in SET_TOLS

      data tolnam/'V_TOL','V_MAX','V_MIN','W_TOL','W_MAX','W_MIN',
     :'W_S_N','H_MAX','H_MIN','H_S_N','C_TOL','S_TOL','H_TOL','EXIT'/

* rejection criteria

      data dict/'HEIGHT','CENTRE','WIDTH','ERRORS','S/N','SHAPE',
     :            'SEPARATIONS','APPLY'/

      if(batch) then

* Only apply tolerances if TOLS is given explicitely in command line.

        apply = par_given('TOLS')
        if(apply) then
          call par_rdchar('TOLS',' ',tolset)

*     Look through string searching for tolerance names

          ilen = chr_len(tolset)
          ind = index(tolset,':')
          lst = 1
          if((ind.eq.0).and.(ilen.gt.0)) ind = ilen + 1
          do while(ind.gt.0)
            ind = ind + lst - 2
            if((ind-lst).ge.1) then

*     Isolate "word" and look it up in dictionary

              chars = tolset(lst:ind)
              itol = menu(dict,MAXREJ,chars,0)
              if(itol.gt.0) reject(itol) = .true.
            end if
            lst = ind + 2

*        Make sure we don't go over the end of the string!

            if(lst.le.ilen) then
              ind = index(tolset(lst:),':')

*          If we haven't got any more colons, then take this as the
*          last "word"

              if(ind.eq.0) ind = ilen + 1
            else
              ind = 0
            end if
          end do
        end if
      else

* Set tols if required

        call set_tols(ifarc,apply,status)
      end if

      if(apply) then
        nreject=0
        if(batch) then

*   print out current values of tolerances

          call print_tols(tolerance,maxtol,tolnam)
          reject(SZREJT) = .false.
        else
          call set_reject(status)
        end if

*     Write list of tolerances being applied

        call dsa_wruser('Testing on:')
        do i = 1,MAXREJ
          if(reject(i)) then
            call dsa_wruser(' ')
            call dsa_wruser(dict(i)(:chr_len(dict(i))))
          end if
        end do
        call dsa_wruser(bsn)

*     Now test the values of the parameters against the tolerances

        do jstore = 1 ,spdim2
          do istore = 1 ,spdim1
            do line = 1 ,line_count
              call check_tols(line,istore,jstore,nreject,ifarc,
     :            %VAL( CNF_PVAL(d_wptr) ),%VAL( CNF_PVAL(d_rptr) ),
     :            %VAL( CNF_PVAL(staptr) ),%VAL( CNF_PVAL(d_vptr) ))
            end do
          end do
        end do

* Give total fits rejected. Note this is really the sum of the
* cross-sections of the fits which have been rejected. i.e. 3 lines,
* averaged in 10 x-sects, rejected over 20 x-sects  = 60 rejected.

        if(nreject.eq.0) then
          call par_wruser('No fits rejected',pstat)
        else if(nreject.eq.1) then
          call par_wruser('1 fit rejected',pstat)
        else
          len1 = 0
          call chr_puti(nreject,chars,len1)
          call chr_putc(' fits rejected',chars,len1)
          call par_wruser(chars(:len1),pstat)
        end if
        call updtmsk(%VAL( CNF_PVAL(staptr) ),%VAL( CNF_PVAL(d_mptr) ))
      end if
      end
