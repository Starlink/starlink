      subroutine changed( STATUS )
*+
* Name:
*    CHANGED

* Invocation:
*    CALL CHANGED( STATUS )
* Purpose:
*  Indicate fits invalidated due to "cleaning" of an image.

* Description:
*  To indicate which fits have changed due to "cleaning" of image duing
*  fitting (this is due to bits missed previously). This is set at data
*  being different by more than 1, or 1% of the mean value whichever is
*  larger. This situation can arise with data badly affected with cosmic
*  rays where some are initially missed.

* Parameters:
*    IMAGE = FILE (Read)
*        Name of image for input
*    IMAGE2 = FILE (Read)
*        Name of image for input
*
*   T.N.Wilkins, Cambridge, 18-MAY-1990
*
* History:
*      AJH 1/99 changed map_data access mode from r,w to READ,WRITE
*
*-
      implicit none
      integer status
      include 'SAE_PAR'
      include 'arc_dims'
      include 'CNF_PAR'          ! For CNF_PVAL function
      integer specdim,dims(2)
      include 'DYNAMIC_MEMORY'
      integer flagptr,d_s2ptr,slot,dyn_element
      logical nocube
*

* Initialise common arrays

      status = SAI__OK
      call arc_init(.true.)
      call two_open('image',2,status)

      call map_res(.false.,.false.,nocube,status)
      if(nocube) then
        status = SAI__ERROR
      else
        call map_data(.false.,'READ',status)
        call refine_res(status)
      end if
*
*   Open 2nd input spectrum, and get its dimensions and name
*
      call dsa_input('data2','image2',status)
*
*  Get dimensions of input data
*
      call dsa_data_size('data2',2,specdim,dims,nelm,status)
      call dsa_map_data('data2','READ','float',d_s2ptr,slot,status)
      d_s2ptr = dyn_element(d_s2ptr)

      call getvm(line_count*spdim1*4,flagptr,slot,status)

      if((specdim.ne.2).or.(dims(1).ne.wavdim).or.
     :       (dims(2).ne.spdim1).or.(status.ne.SAI__OK)) goto 500

* Compare 2 files

      call compare(dynamic_mem(d_sptr),dynamic_mem(d_s2ptr),
     :       dynamic_mem(flagptr),%VAL(CNF_PVAL(d_tlptr)),
     :       %VAL(CNF_PVAL(d_trptr)),dynamic_mem(d_xptr))
 500  continue
      call unmap_res(status)
      call dsa_close(status)
      end
