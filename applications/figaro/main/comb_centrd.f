      subroutine comb_centrd(sg_parms,sg_error,in,sdata,sdens
     :,start,m)
*+
* Name:
*    COMB_CENTRD

* Invocation:
*    CALL COMB_CENTRD(SG_PARMS,SG_ERROR,IN,SDATA,SDENS
*     ,START,M)

* Purpose:
*  To find the centriods and widths of lines, for use in comb.
*
* Description:
*  To find the centriods and widths of lines, for use in comb.
*
* Given;-
*   IN = INTEGER (Given)
*        Dimension of SDATA and SDENS
*   SDATA(IN) = REAL ARRAY (Given)
*        Channel array (i.e. position)
*   SDENS(IN) = REAL ARRAY (Given)
*        Data values
*   START = INTEGER (Given)
*        Start of line (in channel number)
*   M = INTEGER (Given)
*        Number of elements in line
*   SG_PARMS(4) = REAL ARRAY (Returned)
*        Values of centre, e.t.c.
*   SG_ERROR(4) = REAL ARRAY (Returned)
*        Filled with 1.0e-5

* History:
*   T.N.Wilkins Manchester  14/7/87
*        "      Cambridge 21/10/91 New parameter order
*   A C Davenhall Edinburgh 14/12/00 Removed spurious final argument
*                                  in call to get_centre.
* Problems:
*  The base is assumed to be flat-the base subtraction just uses the minimum
*  value.
*-
      implicit none
      include 'PRM_PAR'
      integer in
      real sdata(in),sdens(in)
      double precision centre,sigma
      integer start,end,m,i
      real sg_parms(4),sg_error(4),height
      real EFOLD

* convert sigma to width

      parameter (EFOLD = 2.35482)
      end = start + m - 1
      height = VAL__MINR
      do i = start, end
        height = max(height,sdens(i))
      end do
      call get_centre(sdata,sdens,start,end,centre,sigma)
      sg_parms(2) = real(sigma)*EFOLD
      sg_parms(4) = real(centre)
      sg_parms(3) = height
      sg_parms(1) = 0.0
      do i = 1,4
        sg_error(i) = 1.0e-5
      end do
      end
