      subroutine fibmen(iopt,limit,iteration,status)
*+
* Name:
*    FIBMEN

* Invocation:
*    CALL FIBMEN(IOPT,LIMIT,ITERATION,STATUS)
* Purpose:
*  Main menu for FIBDISP

* Description:
*  Main menu for FIBDISP

* Argments:
*    LIMIT = LOGICAL (Givenand returned)
*       If to limit range of data to consider
*    STATUS = INTEGER (Given and returned)
*       Error status, 0=ok
*    IOPT = INTEGER (Returned)
*       Menu choice
*    ITERATION = INTEGER (Given and returned)
*       Fit iteration (used for masking)

*   Subroutines referenced:
*     QMENU      : Get menu response from user
*     PAR_WRUSER : Write string to user

* History:
*   T.N.Wilkins Manchester 6-7/88
*        "      Cambridge 14/12/89 Order changed
*   Set iteration here, TNW 30/9/93
*   A C Davenhall Edinburgh 18/12/00 Changed the documented type of
*     ITERATION from INTEGER*2 to INTEGER to correspond to the actual
*     declaration.
*-
      implicit none
      integer iopt,status,oldopt
      integer iteration,newit
      integer NMENU
      parameter (NMENU = 19)
      character*50 dict(NMENU)
      logical limit
      integer dumi,OPT_IT,len1
      parameter (OPT_IT = 8)
      real value
      character dumc
      data dict/
     :     'RESULTS : Display plane of results cube',
     :     'DATA : Display plane of data cube',
     :     'PROFILE : Display line profile',
     :     'XCUT : Display X direction cut through data',
     :     'YCUT : Display Y direction cut through data',
     :     'XOUT : Output to file X direction cut through data',
     :     'YOUT : Output to file Y direction cut through data',
     :     'IT %F : Reduce iteration (now ',
     :     'CHECK : Check fits to profiles',
     :     'TOTAL : Display total intensity',
     :     'LIMIT : Start limiting displays in X and Y',
     :     'TOLS : Apply/set tolerances',
     :     'LOOK : Look at value in results array',
     :     'DELETE : Delete bad fits',
     :     'DEFINE : Define fit type for batch fitting',
     :     'OUTPUT : Create output plots etc.',
     :     'EXIT : Exit',
     :     'AUTO : Automatic fitting',
     :     'CUBAN : Cuban-style display/motion'/
      oldopt = iopt
      if(limit) then
         dict(11)(9:11) = 'op '
      else
         dict(11)(9:11) = 'art'
      end if
      iopt = OPT_IT
      do while(iopt.eq.OPT_IT)
         len1 = 30
         dumi = iteration
         call chr_puti(dumi,dict(OPT_IT),len1)
         call chr_putc(')     ',dict(OPT_IT),len1)
         call qmenu('Main Menu',dict,NMENU,oldopt,value,dumc,iopt,
     :        dumi,status)

*  If we want to change iteration do so now, if new value ok

         if(iopt.eq.OPT_IT) then
            newit = nint(value)
            if((iteration.gt.newit).and.(newit.ge.0)) then
               iteration = newit
            else
               call par_wruser('Error, invalid value for iteration',
     :              status)
            endif
         endif
      enddo
      end
