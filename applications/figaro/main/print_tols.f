      subroutine print_tols(tol,ntol,tolnam)
*+
* Name:
*    PRINT_TOLS

* Invocation:
*    CALL PRINT_TOLS(TOL,NTOL,TOLNAM)

* Purpose:
*   Print out a summary table of the current tolerances

* Description:
*   Print out a summary table of the current tolerances
*
* Arguments:
*    TOL(NTOL) = REAL ARRAY (Given)
*        Tolerance values
*    NTOL = INTEGER (Given)
*        Number of tolerances
*    TOLNAM(NTOL) = CHARACTER*5 ARRAY (Given)
*        Tolerance names
* History:
*     Largely re-written, T.N.Wilkins Cambridge, 19/8/91
*-
      implicit none
      integer ntol
      real tol(ntol)
      character*5 tolnam(ntol)

* local
      character*79 chars
      integer len1,curtol,olen1
      integer pstat
*
*  ** Send clipping information to the terminal if online,
*     else to the batch log file (handled by VMS) if offline
*
      len1 = 0
      call chr_fill(' ',chars)
      do curtol = 1, ntol
        olen1 = len1
        call chr_putc(tolnam(curtol),chars,len1)
        call chr_putc(' = ',chars,len1)
        call chr_putr(tol(curtol),chars,len1)

*     If we're on the 3rd in a row then output using par_wruser

        if(len1.gt.55) then
          call par_wruser(chars(:len1),pstat)
          len1 = 0
          call chr_fill(' ',chars)
        else
          len1 = max(len1,olen1+25)
        end if
      end do
      if(len1.gt.0) call par_wruser(chars(:len1),pstat)
      end
