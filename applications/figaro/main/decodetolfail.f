      subroutine decodetolfail(fails,sg_parms,sg_error)
*+
* Name:
*    DECODETOLFAIL

* Invocation:
*    CALL DECODETOLFAIL(FAILS,SG_PARMS,SG_ERROR)

* Purpose:
*  To descibe why a fit failed tolerances
*
* Description:
*  To descibe why a fit failed tolerances
*
* Arguments:
*    FAILS(8) = LOGICAL ARRAY (Given)
*        Reasons for failure
*    SG_PARMS(7) = REAL ARRAY (Given)
*        1 component parameters
*    SG_ERROR(7) = REAL ARRAY (Given)
*        1 component errors
*-
      implicit none
      real sg_parms(7)
      real sg_error(7)
      logical fails(8)
*  Local

      character*30 message(8),chars*72
      character*23 object(8)
      real sg_res(8)
      integer status,i,ignore
      integer chr_len

      data message/'height test','centre test','error test',
     : 'width test','height signal to noise test',
     :'width signal to noise test','s test','c test'/
      data object/'height=','Centre=','Errors:-','Width=',
     :  'Height signal to noise=','Width signal to noise=',' ',' '/

      call zero_real(sg_res,8)

      sg_res(1)=sg_parms(2)
      sg_res(2)=sg_parms(3)
      sg_res(4)=sg_parms(1)
      if(sg_error(2).ne.0.0)sg_res(5)=sg_parms(2)/sg_error(2)
      if(sg_error(1).ne.0.0)sg_res(6)=sg_parms(1)/sg_error(1)
      do i=1,8
        if(fails(i)) then
           call par_wruser('Failed '//message(i),status)
           if(i.eq.3) then
             write(chars,'(a,''(W,Ht,Cen)'',3f11.3)',iostat=ignore)
     :           object(i)(:chr_len(object(i))),sg_error(1),sg_error(2),
     :           sg_error(3)
           else
             write(chars,'(a,f12.4)',iostat=ignore)
     :           object(i)(:chr_len(object(i))),sg_res(i)
           end if
           call par_wruser(chars,status)
        end if
      end do
      end
