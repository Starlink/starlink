         subroutine opt_fail(ifail)
*+
* Name:
*    OPT_FAIL

* Invocation:
*    CALL OPT_FAIL(IFAIL)

* Purpose:
*  Error returns from optimization

* Description:
*  Error returns from optimization
*
* Arguments:
*    IFAIL = INTEGER (Given)
*        NAG error number
* Global variables:
*    OPT_LU = INTEGER (Given)
*        logical unit for output In opt_cmn
*
      implicit none
      include 'opt_cmn'
*-

* status

      integer status

* error messages

      character*51 message(5)
      integer ifail
      character*40 chars
      character*36 opterr
      data opterr/' O P T I M I S A T I O N   E R R O R'/
* -----------------------------------------------------------------
*
* set up the messages
*
      data message/'one of parameters outside range'
     :  ,'MAXCAL iterations completed check PARAMETERS+CODING'
     :   ,'rounding errors sig increase xtol--sol may be ok'
     :   ,'the initial point had ifl=.true. restart elsewhere'
     :   ,'an element in lsq1 is legative--error in coding'/
*
*
* Output required messages
*
      call opt_wruser(opterr,status)
      write(opt_lu,'(a)') opterr
      write (chars,'('' This solution has error number'',i4)')ifail
      call opt_wruser(chars,status)
      write(opt_lu,'(a)') chars
      call opt_wruser(message(ifail),status)
      write(opt_lu,'(a)')message(ifail)
*
* close and print work file
*
      if (keep_itt) then
        close(unit=opt_lu)
      end if
      end
