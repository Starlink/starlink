* See gau2_pro for discussion

      subroutine gau2_errs (gau2par, status)

*+
*   Description:
*     If gau2err is zero, ensure status=sai__ok.  Else set status=sai__error
*     and print out a suitable message.  Registry of codes in gau_par.
*
*   Arguments:
*     gau2par = integer(gau2len) (given)
*       Error code and status information
*     status = integer (given, and possible modified)
*       SAE inherited status
*-

*   types
      implicit none
      include 'SAE_PAR'
      include 'GAU_PAR'

*   Arguments
      integer gau2par(gau2len), status

*   Local variables
      integer errno, errinf
*   We need to pass a separate status variable to the msg_* routines.
      integer localstatus

*   It _should_ be the case that
*   (status.eq.sai__ok .eqv. gau2par(gau2status).eq.0).  We don't rely
*   on this, but if it's not true, it suggests something has gone amiss
*   elsewhere.
      if (status.eq.sai__ok .neqv. gau2par(gau2status).eq.0) then
         call msg_seti ('ST', status)
         call msg_seti ('IST', gau2par(gau2status))
         call msg_out (' ', 'GAUFIT2: warning: status inconsistency '//
     :        'status=^ST but internal status=^IST', localstatus)
      endif

      errno = gau2par(gau2status)

      if (errno .eq. 0) then
         status = sai__ok

*      ...but put the final status code in the message, just for reference
         call msg_seti ('IV', gau2par(gau2xstatus))
         call msg_out (' ', 'GAUFIT2: converged (^IV)', localstatus)
      else
         status = sai__error

         call msg_out (' ', 'GAUFIT2: algorithm failed:', localstatus)

         errinf = gau2par(gau2xstatus)

         if (errno .eq. gau2noconv) then
            call msg_out (' ', 'no convergence', localstatus)
         else if (errno .eq. gau2maxiter) then
            call msg_seti ('MXIT', errinf)
            call msg_out (' ', 'too many iterations: max=^MXIT',
     :           localstatus)
         else if (errno .eq. gau2unkerror) then
            call msg_seti ('CODE', errinf)
            call msg_out (' ', 'NSG error no. ^CODE', localstatus)
         else if (errno .eq. gau2memory) then
            call msg_out (' ', 'not enough memory', localstatus)
         else if (errno .eq. gau2drifted) then
            call msg_out (' ', 'solution drifted too far', localstatus)
         else if (errno .eq. gau2code) then
            call msg_seti ('CODE', errinf)
            call msg_out (' ', 'coding error no. ^CODE', localstatus)
         else
            call msg_seti ('CODE', errno)
            call msg_out (' ', 'Unexpected error!! no. ^CODE',
     :           localstatus)
         endif

      endif

      end
