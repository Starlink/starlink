      subroutine setup_arc2(left,right,ifchan,status)
*+
* Name:
*    SETUP_ARC2

* Invocation:
*    CALL SETUP_ARC2(LEFT,RIGHT,IFCHAN,STATUS)

* Purpose:
*   Locate lines prior to fitting

* Description:
*       This subroutine controls the basic setup procedures which
*       require graphics in the ARC procedure.
*       There are 3 basic tasks
*                (1)   set up SEGMENTS giving higher resolution displays
*                (2)   define regions containing lines
*
*      which are carried out in the subroutines:-

* Subroutines used:
*   FIND_LINES
*      finds the lines - uses seg_disp & pick_lines
*
*
* Arguments:
*     IFCHAN = LOGICAL (Given)
*        If array referred to by VSDTR is
*                            along channel direction
*     LEFT(LINE_COUNT) = REAL ARRAY (Given)
*        Left hand tram line for the line
*     RIGHT(LINE_COUNT) = REAL ARRAY (Given)
*        right " " " " " " "  " "  " "

* Global variables:
*     WAVDIM = INTEGER (Given)
*        Number of channels in data
*     SPDIM1 = INTEGER (Given)
*        Number of cross-sections in data
*     VSDTR = INTEGER (Given)
*        Pointer to extracted part of image from (.z.data array)
*     XPTR = INTEGER (Given)
*        Pointer to channels (.x.data array)
*     LINE_COUNT = INTEGER (Given and returned)
*        total number of lines(<50)

* History:
*  Revised T.N.Wilkins Manchester
*  Minor tidying, TNW/CAVAD 25/10/89
*- -----------------------------------------------------------------
      implicit none
      include 'CNF_PAR'          ! For CNF_PVAL function
      logical ifchan
      integer dim
      integer pstat
*
* include common blocks
*
      integer status
      include 'arc_dims'
*
* character
*
      real right(nyp)
      real left(nyp)
      integer i
      integer noldlines
      character*19 chars

* Store value of line_count on entry - for use in adding line to an
* already existant list

      noldlines=line_count
      if(ifchan) then
        dim = wavdim
      else
        dim = spdim1
      end if
*
* default settings
*
      setup = .true.
*
      do i=(line_count+1),nyp
        left(i)=0.0
        right(i)=0.0
      end do
*
* Find the lines
*
      call find_lines(%VAL(CNF_PVAL(d_xptr)),%VAL(CNF_PVAL(d_vsptr)),
     :                dim,left,right,line_count,nyp,batch,xlabel,xunits,
     :                yunits,status)
*
* check for error
*
      if( (line_count-noldlines) .ge. 1 ) then
*
* sort into ascending order
*
        call arsort(left(noldlines+1),right(noldlines+1),
     :             line_count-noldlines)
*
        write(chars,'(i4,a)') line_count,' lines located'
        call par_wruser(chars,pstat)
      else if ( line_count .lt. 1) then
        setup = .false.

* line_count

      end if
      end
