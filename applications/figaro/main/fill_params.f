      subroutine fill_params(params,threed,mxpars,if2arc,mgauss)
*+
* Name:
*    FILL_PARAMS

* Invocation:
*    CALL FILL_PARAMS(PARAMS,THREED,MXPARS,IF2ARC,MGAUSS)
*
* Purpose:
*  To initialise the params array, for beyond component 2
*
* Description:
*  To initialise the params array, for beyond component 2
*
* Arguments:
*    THREED = LOGICAL (Given)
*        If file is 3-dimensional
*    MXPARS = INTEGER (Given)
*        Number of parameters
*    IF2ARC = LOGICAL (Given)
*        If called from ARC2D (and 2-d data)
*    MGAUSS = INTEGER (Given)
*        Maximum number of components room allowed for
*    PARAMS(MXPARS) = CHARACTER*10 ARRAY (Returned)
*        Parameter names array
*
* Author:
*   T.N.Wilkins Manchester 16/1/89
* History:
*       "            "     30/1/89 Allow for ilen.lt.151
*       "       Cambridge  8/1/90 Made much more general-handles 3-d
*                          data as well
*       "           "      1-10/7/91 With new results structures don't
*                          need to support old arrays
*-
      implicit none
      integer mxpars
      character*10 params(mxpars)
      logical threed,if2arc
      integer mgauss,i,len1,tmp

* results Z-dim descriptors

      character*10 stpars(3)
      data stpars/'Space1_pos','Space2_pos','Pts_in_Fit'/

      do i = 1, mxpars
        call chr_fill(' ',params(i))
      end do
      if(threed) then
        tmp = 3
      else
        tmp = 1
      end if
      do i = 1, tmp
        params(i) = stpars(i)
      end do

      tmp = tmp + 1
      params(tmp) = 'Base      '

*  By using max we can deal with files (for example for COMB) for
*  which Skew etc. aren't needed

      do i = 1, mgauss
        tmp = tmp + 1
        len1 = 0
        call chr_putc('Centre_',params(tmp),len1)
        call chr_puti(i,params(tmp),len1)
        tmp = tmp + 1
        len1 = 0
        call chr_putc('Width_',params(tmp),len1)
        call chr_puti(i,params(tmp),len1)
        tmp = tmp + 1
        len1 = 0
        call chr_putc('Height_',params(tmp),len1)
        call chr_puti(i,params(tmp),len1)
      end do

      if(mxpars-tmp.ge.3) then
        tmp = tmp + 1
        params(tmp) = 'Skew_1'
        tmp = tmp + 1
        params(tmp) = 'Cauchy_1'
        tmp = tmp + 1
        params(tmp) = 'Akaike IC'

*   No room allowed for skew etc., must be ARC2D or similar

      else if(if2arc) then

*        so continuity correction required

        tmp = tmp + 1
        params(tmp) = 'Contincent'
      end if
      params(mxpars) = 'Dens_scale'
      end
