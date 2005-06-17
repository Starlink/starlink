      subroutine setup_arc(status)
*+
* Name:
*    SETUP_ARC

* Invocation:
*    CALL SETUP_ARC(STATUS)
*
* Purpose:
*   Set up line locations/ids

* Description:
*    This subroutine controls the basic setup procedures which
*    require graphics in the ARC procedure.
*    There are 3 basic tasks
*           (1) set up SEGMENTS giving higher resolution displays
*           (2) define regions containing lines
*           (3) establish their wavelengths
*
*      which are carried out in the subroutines:-
*
*   FIND_LINES
*      finds the lines - uses segment & pick_lines
*
*   LINEPICK
*      allows line identifications to be tagged to each pair of
*      tram lines.
*
* Global variables:
*    WAVDIM = INTEGER (Given)
*        Number of channels in data (include file arc_dims)
*    SPDIM1 = INTEGER (Given)
*        Number of cross-sections in data (include file arc_dims)
*    D_SPTR = INTEGER (Given)
*        Pointer to image (.z.data array) (include file arc_dims)
*    D_XPTR = INTEGER (Given)
*        Pointer to channels (.x.data array) (include file arc_dims)
*    D_WPTR = INTEGER (Given)
*        pointer to wavelengths array (include file arc_dims)
*    LINE_COUNT = INTEGER (Returned)
*        total number of lines(<50) (include file arc_dims)
*    LINE_NAME(NYP) = CHARACTER*10 ARRAY (Returned)
*        the ID of each line via IDSPTR (include file arc_dims)
* Arguments:
*    STATUS = INTEGER (Given and returned)
*        Error status

* History:
*  Revised T.N.Wilkins Manchester
*  TNW 29/11/88 Change to virtual memory-use of getvm and bytesdef
*  include file.
*  Bug fix TNW/CAVAD 20/2/89
*  Change to VM for character string (more portable) TNW 14/9/89
*  TNW 25/10/89 Minor changes-removal of error message if no lines
*  located-not needed.
*  TNW 5/2/90 Altered to use DSA_AXIS_RANGE.
*  TNW 15/2/90 Template data array filled
*-
      implicit none
      include 'CNF_PAR'          ! For CNF_PVAL function
*
* include common blocks
*
      integer status
      include 'arc_dims'
*
* character
*
      integer s2,e2,s3,e3
      real dummy1,dummy2

      s2 = 1
      e2 = spdim1
      s3 = 1
      e3 = spdim2
*
*    2 D   d a t a :define a template
*
      if( spdim1 .gt. 1 ) then
        call dsa_axis_range('data',2,' ',.false.,dummy1,dummy2,s2,e2,
     :          status)
        call canaxlim(2)
      end if
      if( spdim2 .gt. 1 ) then
        call dsa_axis_range('data',3,' ',.false.,dummy1,dummy2,s3,e3,
     :          status)
        call canaxlim(3)
      end if
      call extr3(%VAL(CNF_PVAL(d_sptr)),wavdim,spdim1,spdim2,s2,e2,s3,
     :         e3,%VAL(CNF_PVAL(d_vsptr)))

      if(line_count.eq.0) then

* Fill .template.data_array

       call accres(' ','more.twodspec.template.data_array','wf',wavdim,
     :               %VAL(CNF_PVAL(d_vsptr)),' ',status)
      end if

*  Locate and identify the lines

      call setup_lines(status)

      end
