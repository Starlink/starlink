      subroutine comp_menu(iopt,idef,igauss,ngauss,status)
*+
* Name:
*    COMP_MENU

* Invocation:
*    CALL COMP_MENU(IOPT,IDEF,IGAUSS,NGAUSS,STATUS)
*
* Purpose:
*    To obtain the users wishes on components to fix.

* Description:
*    To obtain the users wishes on components to fix.
*
* Arguments:
*   IDEF = INTEGER (Given)
*     Default (see under IOPT below)
*   NGAUSS = INTEGER (Given)
*     Number of gaussians defined
*   STATUS = INTEGER (Given and returned)
*     Error status, 0=ok
*   IGAUSS = INTEGER (Returned)
*     Number of gaussian to have parameter(s) fixed
*   IOPT = INTEGER (Returned)
*     Option selected:-
*                    1 Base
*                    2 Width
*                    3 Height
*                    4 Centre
*                    5 All
*                    6 Exit routine for fixing parameters
* History:
*   TNW 1/11/89 CAVAD QMENRT used
*-
      implicit none
      integer iopt
      integer igauss
      integer ngauss
      integer nnums
      real values(1)
      integer idef
      character*43 copts(6)
      integer status,pstat
      logical loop
      character dumc
      data copts/
     :     'BASE : Fix base',
     :     'WIDTH %FN : Fix width, N = component number',
     :     'HEIGHT %FN  : Fix height',
     :     'CENTRE %FN : Fix centre',
     :     'ALL %FN  : Fix all parameters',
     :     'EXIT : Exit-choices ok now'/

* decide on answer to this menu

      loop = .true.
      do while(loop)
        call qmenu('Fix Parameters',copts,6,idef,values,dumc,iopt,
     :       nnums,status)

        loop = .false.
        if((iopt.gt.1).and.(iopt.lt.6)) then
          igauss=nint(values(1))
          loop = (igauss.lt.1).or.(igauss.gt.ngauss)
          if(loop) call par_wruser('Invalid component',pstat)
        end if
      end do
      end
