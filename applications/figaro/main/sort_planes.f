      subroutine sort_planes(fitsta,results,resvar,up)
*+
*  Name:
*     SORT_PLANES

*  Purpose:
*     Sort fits so components are in ascending order

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SORT_PLANES(FITSTA,RESULTS,RESVAR)

*  Description:
*     This routine loops over the FITSTA array, and if it finds any fits
*     with more than 1 component, checks if the centres are in order. If
*     they are not, they are sorted (both results and resvar are moved).

*  Arguments:
*      FITSTA(NCNTRL,NYP,SPDIM1,SPDIM2) = INTEGER ARRAY (Given)
*        Fit status array
*      RESULTS(MXPARS,NYP,SPDIM1,SPDIM2) = REAL ARRAY (Given and returned)
*        Fit results
*      RESVAR(MXPARS,NYP,SPDIM1,SPDIM2) = REAL ARRAY (Given and returned)
*        Variances on RESULTS
*      UP = LOGICAL (Given)
*        If to sort in ascending order

*  Global variables:
*    Include file arc_dims:
*      NCNTRL = INTEGER (Given)
*        Number of elements in FITSTA per fit
*      MXPARS = INTEGER (Given)
*        Number of elements in RESULTS per fit
*      NYP = INTEGER (Given)
*        Number of slots for lines
*      SPDIM1 = INTEGER (Given)
*        1st spatial dimension
*      SPDIM2 = INTEGER (Given)
*        2nd spatial dimension
*      LINE_COUNT = INTEGER (Given)
*        Number of lines identified

*  Authors:
*     TNW: T.N.Wilkins (Durham)
*     {enter_new_authors_here}

*  History:
*     3-AUG-1993 (TNW):
*        Original version.
*     22-NOV-1993 (TNW):
*        Allow to sort in ascending or descending order
*     {enter_changes_here}

*  Bugs:
*     Only supports multiple fits with width, height and centre only
*     for each component
*     {note_any_bugs_here}

*-

*  Type Definitions:
      implicit none             ! No implicit typing
      include 'arc_dims'
      include 'status_inc'
      integer fitsta(ncntrl,nyp,spdim1,spdim2)
      real results(mxpars,nyp,spdim1,spdim2)
      real resvar(mxpars,nyp,spdim1,spdim2)
      integer x,y,line,cmp,cpos(9),wpos(9),hpos(9),get_parnum
      real tmp
      logical loop,up,swap
      character numbers(9)
      data numbers/'1','2','3','4','5','6','7','8','9'/

      do cmp = 1, mgauss
         cpos(cmp) = get_parnum('Centre_'//numbers(cmp))
         hpos(cmp) = get_parnum('Height_'//numbers(cmp))
         wpos(cmp) = get_parnum('Width_'//numbers(cmp))
      enddo
      do y = 1, spdim2
         do x = 1, spdim1
            do line = 1, line_count
               call decode_status(ncntrl,fitsta(1,line,x,y),deccntr)
               if((deccntr(FIT_NCMP).gt.1).and.
     :              (deccntr(FIT_STAT).gt.0)) then
                  loop = .true.
                  do while(loop)
                     loop = .false.
                     do cmp = 2, deccntr(FIT_NCMP)

*       Allow to sort in either order (tests done separately in case the
*       2 values compared are the same)

                        if(up) then
                           swap = results(cpos(cmp),line,x,y).lt.
     :                       results(cpos(cmp-1),line,x,y)
                        else
                           swap = results(cpos(cmp),line,x,y).gt.
     :                       results(cpos(cmp-1),line,x,y)
                        endif
                        if(swap) then
                           loop = .true.

*       Centres

                           tmp = results(cpos(cmp),line,x,y)
                           results(cpos(cmp),line,x,y) =
     :                          results(cpos(cmp-1),line,x,y)
                           results(cpos(cmp-1),line,x,y) = tmp

                           tmp = resvar(cpos(cmp),line,x,y)
                           resvar(cpos(cmp),line,x,y) =
     :                          resvar(cpos(cmp-1),line,x,y)
                           resvar(cpos(cmp-1),line,x,y) = tmp

*       Heights

                           tmp = results(hpos(cmp),line,x,y)
                           results(hpos(cmp),line,x,y) =
     :                          results(hpos(cmp-1),line,x,y)
                           results(hpos(cmp-1),line,x,y) = tmp

                           tmp = resvar(hpos(cmp),line,x,y)
                           resvar(hpos(cmp),line,x,y) =
     :                          resvar(hpos(cmp-1),line,x,y)
                           resvar(hpos(cmp-1),line,x,y) = tmp

*       Widths

                           tmp = results(wpos(cmp),line,x,y)
                           results(wpos(cmp),line,x,y) =
     :                          results(wpos(cmp-1),line,x,y)
                           results(wpos(cmp-1),line,x,y) = tmp

                           tmp = resvar(wpos(cmp),line,x,y)
                           resvar(wpos(cmp),line,x,y) =
     :                          resvar(wpos(cmp-1),line,x,y)
                           resvar(wpos(cmp-1),line,x,y) = tmp
                        endif
                     enddo
                  enddo
               endif
            enddo
         enddo
      enddo
      end
