      subroutine map_data(ifcomb,mode,status)
*+
* Name:
*    MAP_DATA

* Invocation:
*    CALL MAP_DATA(IFCOMB,MODE,STATUS)

* Purpose:
*    Map the data

* Description:
*   Maps the data - this cannot be done in two_open since further
*   structures etc. are added later for the case of new analysis.
*
* Arguments:
*    IFCOMB = LOGICAL (Given)
*        If called from comb
*    MODE = CHARACTER*1 (Given)
*        Mode for mapping spatial axis arrays:
*                          -   "READ" - read-only
*                          -   "WRITE" - write
*    STATUS = INTEGER (Given and returned)
*        Error status
*
* Subroutines/functions referenced:
*     DSA_MAP_AXIS_DATA  : Map axis data
*     DSA_MAP_DATA       : Map main data array
*     DYN_ELEMENT        : Convert address to element of DYNAMIC_MEM
*     GETWORK            : Get work array
*
* History:
*   Altered T.N.Wilkins Manchester 8/88 to use dsa routines. Not
*   interested in whether X array exists.
*  TNW 14/10/88 To get work array
*  TNW 4/11/88 To map axis 2 in COMB
*  TNW 8/12/88 To use GETWORK
*  TNW 9/89 CAVAD STATUS added to argument list
*  AJH 1/99 Changed mode to be READ or WRITE from R or W
*
*-
      implicit none
      include 'arc_dims'

      character*5 mode
      logical ifcomb
      integer status
      integer dyn_element
      integer slot,sptr,nels,naxis


      if(ifcomb) then
        naxis = 2
        nels = spdim1
      else
        nels = wavdim
        naxis = 1
      end if
*
*  Map the X-array (or Y array in COMB)
*
      call dsa_map_axis_data('data',naxis,'READ','float',d_xptr,slot,
     :     status)
      d_xptr = dyn_element(d_xptr)
*
*  Map the data
*
      call dsa_map_data('data','READ','float',sptr,slot,status)
      d_sptr = dyn_element(sptr)
      call getwork(nels,'float',d_vsptr,slot,status)

*  See if there is an error array, and map it if present

      call dsa_seek_errors('data',errpre,status)
      if(errpre) then
        call dsa_map_errors('data','READ','float',errptr,slot,status)
        errptr = dyn_element(errptr)
      end if
      if (spdim2.gt.1) then
        call dsa_map_axis_data('data',2,mode,'float',xptr,slot,status)
        xptr = dyn_element(xptr)
        call dsa_map_axis_data('data',3,mode,'float',yptr,slot,status)
        yptr = dyn_element(yptr)
      end if
      end

