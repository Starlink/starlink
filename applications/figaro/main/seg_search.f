      subroutine seg_search(x,y,in,line_count,chan1,chan2,
     :     nyp,status)
*+
* Name:
*    SEG_SEARCH

* Invocation:
*    CALL SEG_SEARCH(X,Y,IN,LINE_COUNT,CHAN1,CHAN2,
*                   NYP,STATUS)

* Purpose:
*   Automatically search for lines

* Description:
*   The workspace required for the subroutine SEARCH is set up and
*   SEARCH is called.

* Arguments:
*    X(IN) = REAL ARRAY (Given)
*       
*    Y(IN) = REAL ARRAY (Given)
*       
*    IN = INTEGER (Given)
*       
*    NYP = INTEGER (Given)
*       
*    LINE_COUNT = INTEGER (Returned)
*       
*    CHAN1(NYP) = REAL ARRAY (Returned)
*        borders of lines - for fitting
*    CHAN2(NYP) = REAL ARRAY (Returned)
*        borders of lines - for fitting
*    STATUS = INTEGER (Given and returned)
*       
*
* Subroutines referenced:
*     GETWORK          : Get work array
*     SEARCH           : Find lines automatically
*
*     DSA_FREE_WORKSPACE : Free workspace
* History:
*  Separate trams for plotting removed, TNW/Cambridge 15-APR-1991
*  TNW: 9/8/93 Cut down a lot
*-
      implicit none
      include 'SAE_PAR'
      integer nyp
      integer in,line_count
      real x(in),y(in)

      real chan1(nyp),chan2(nyp)
      integer status
      integer work,slot,smd
      include 'PRM_PAR'
      include 'DYNAMIC_MEMORY'

* Get workspace:
*   WORK   4000 (r)

      call getwork(4000+in,'float',work,slot,status)
      if(status.ne.SAI__OK) return
      smd = work + VAL__NBI * 4000
*
*  Search for lines.
*
      call search(x,y,in,line_count,chan1,chan2,nyp, dynamic_mem(work),
     :     dynamic_mem(smd),status)

      call dsa_free_workspace(slot,status)

      end
