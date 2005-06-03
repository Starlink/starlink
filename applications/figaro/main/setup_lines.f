      subroutine setup_lines(status)
*+
* Name:
*    SETUP_LINES

* Invocation:
*    CALL SETUP_LINES(STATUS)
*
* Purpose:
*   Set up line locations/ids

* Description:
*    This subroutine controls the basic setup procedures which
*    require graphics in the ARC procedure.
*    There are 2 basic tasks
*             (1)   define regions containing lines
*             (2)   establish their wavelengths
*    which are carried out in the subroutines:-
*    This is basically the previous version of setup_arc, but without
*    the extraction part, so that it can be more general.

* Subroutine used:
*   FIND_LINES
*      finds the lines - uses segment & pick_lines
*
*   LINEPICK
*      allows line identifications to be tagged to each pair of
*      tram lines.
*
* Global variables:
*   WAVDIM = INTEGER (Given)
*        Number of channels in data
*   SPDIM1 = INTEGER (Given)
*        Number of cross-sections in data
*   SPTR = INTEGER (Given)
*        Pointer to image (.z.data array)
*   XPTR = INTEGER (Given)
*        Pointer to channels (.x.data array)
*   WPTR = INTEGER (Given)
*        pointer to wavelengths array
*   LINE_COUNT = INTEGER (Given)
*        total number of lines(<50)
*
*   LINE_NAME(NYP) = CHARACTER*10 ARRAY (Returned)
*        the ID of each line
*   LEFT(NYP) = REAL ARRAY (Returned)
*        Left hand tram line for the line
*   RIGHT(NYP) = REAL ARRAY (Returned)
*        right " " " " " " "  " "  " "
*
* Arguments:
*    STATUS = INTEGER (Given and returned)
*        Error status

* Authors:
*  TNW: T.N.Wilkins Cambridge until 9/92, then Durham

* History:
*  TNW: 2/1/89 Based on SETUP_ARC.
*  TNW: 22/3/91 Warning against deleting lines removed
*  TNW: 28/7/93, Change to call of linepick-no workspace
*-
      implicit none
*
* include common blocks
*
      integer status
      include 'CNF_PAR'          ! For CNF_PVAL function
      include 'arc_dims'
*
* character
*
      integer noldlines
      logical par_quest
      integer pstat
      character*19 chars
      character dynamic_chars
      include 'DYNAMIC_MEMORY'
      equivalence (dynamic_mem,dynamic_chars)

* Store value of line_count on entry - for use in adding line to an
* already existant list

      noldlines=line_count
*
* default settings
*
      setup = .true.
*
* Find the lines
*
      call find_lines(dynamic_mem(d_xptr),dynamic_mem(d_vsptr),
     :         wavdim,%VAL( CNF_PVAL(d_tlptr) ),
     :         %VAL( CNF_PVAL(d_trptr) ),
     :         line_count,nyp,batch,xlabel,xunits,zunits,status)
*
* check for error
*
      if( (line_count-noldlines) .ge. 1 ) then
*
        write(chars,'(i4,a)') line_count,' Lines located'
        call par_wruser(chars,pstat )
*
* pick line out of known line list
*
*        call linepick(dynamic_chars(idsptr:idsend),
        call linepick(idstring,
     :       %VAL( CNF_PVAL(d_wptr) ),noldlines,status)

      end if
      if ( line_count .lt. 1) then
        setup = .false.

* Edit line list if required

      else if(par_quest('Edit line list?',.false.)) then
        call edit_list(%VAL( CNF_PVAL(d_tlptr) ),
     :                 %VAL( CNF_PVAL(d_trptr) ),
     :         line_count,%VAL( CNF_PVAL(d_wptr) ),
*     :         dynamic_chars(idsptr:idsend))
     :         idstring)
      end if
      end
