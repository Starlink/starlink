      subroutine refine_res(status)
*+
* Name:
*    REFINE_RES

* Invocation:
*    CALL REFINE_RES(STATUS)

* Purpose:
*   Prepare for refining fitting

* Description:
*   If a refined fit is requested then the .RES structure must exist
*   already. If it doesn't then it must be a mistake so go back to start
*   of programme and try again
*
* Arguments:
*    STATUS = INTEGER (Given and returned)
*        Error status, 0=ok
* Global variables:
*    LINE_COUNT = INTEGER (Returned)
*        Number of lines (include file arc_dims)
*    ITERATION = INTEGER (Given and returned)
*        Iteration number (include file arc_dims)
*
* Subroutine/functions referenced:
*    ACCRES      : Access results structure
*    PAR_GIVEN   : find out if parameter given in command line
*    PAR_RDVAL   : Read value from user
*    PAR_WRUSER  : Write string to user
* History:
*  Altered TNW 15/11/89 to include GET_LINE_COUNT.
*  Altered TNW 8/12/89 to only get iteration from user if given on
*  command line.
*- -------------------------------------------------------------
      implicit none
      include 'SAE_PAR'
      include 'CNF_PAR'          ! For CNF_PVAL function
      integer status
      include 'arc_dims'
      real val,value
      integer ival
      character*39 chars
      integer pstat
      logical par_given

* read the iteration number

      call accres('data','results','fi',1,1,' ',status)
      if(status.ne.SAI__OK) return

* Get previously located number of lines

      call get_lincnt(%VAL( CNF_PVAL(d_tlptr) ),line_count,nyp)

      if(batch)then
        write(chars,'(a,i4)') 'Current value of iteration = ',
     :     iteration
        call par_wruser(chars,pstat)
      end if
*
*   Inquire desired threshold for selection. Since this option is
*  available at the main menu, we don't need to do anything unless it is
*  given on the command line.

      if(par_given('iteration'))then
        val = iteration
        call par_rdval('iteration',0.,val,val,'Integer',value)
        ival = nint(value)
        if(ival.ne.iteration) then
          iteration = ival
          write(chars,'(a,i4)')
     :           'Threshold value in mask changed to',iteration
          call par_wruser(chars,pstat)
        end if
      end if
      end
