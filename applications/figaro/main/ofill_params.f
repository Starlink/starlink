      subroutine ofill_params(params,newres,threed,mxpars)
*+
* Name:
*    OFILL_PARAMS

* Invocation:
*    CALL OFILL_PARAMS(PARAMS,NEWRES,THREED,MXPARS)

* Description:
*  To initialise the old format params array.

* Purpose:
*  To initialise the old format params array, for beyond component 2
*  This is the old version, used in updating old files only.
*
* Arguments:
*     PARAMS(MXPARS) = CHARACTER*10 ARRAY (Given)
*        Params array
*     NEWRES = LOGICAL (Given)
*        If new results struture
*     THREED = LOGICAL (Given)
*        If file is 3-dimensional
*     MXPARS = INTEGER (Given)
*        Number of parameters
* History:
*   T.N.Wilkins Manchester 16/1/89
*       "            "     30/1/89 Allow for ilen.lt.151
*       "       Cambridge  8/1/90 Made much more general-handles 3-d
*                          data as well
*-
      implicit none
      integer mxpars
      character*10 params(mxpars)
      logical newres,threed
      integer mgauss,i,len1,tmp

* results Z-dim descriptors

      character*10 descriptors(8)
      character*10 st2d(3)
      character*10 st3d(6)
      character*10 olddes(4)
      data st2d/'Status    ','1st_Xsect ','Spdim1   '/
      data st3d/'Status    ','X_Position','Y_Position','Pts_in_Fit',
     :            'BlockXspat','BlockYspat'/
      data descriptors/'Centre_1  ','Width_1   ','Height_1  ',
     :      'Base      ','E_Centre_1','E_Width_1 ','E_Height_1',
     :      'E_Base    '/
      data olddes/'Skew/Cent2','Cauch/Wid2','ESkew/Hei2','ECau/ECen2'/

      do i = 1, mxpars
        call chr_fill(' ',params(i))
      end do
      if(threed) then
        if(newres) then
          tmp = 6
        else
          tmp = 4
        end if
        do i = 1, tmp
          params(i) = st3d(i)
        end do
      else
        tmp = 3
        do i = 1, tmp
          params(i) = st2d(i)
        end do
      end if

      do i = 1, 8
        params(i+tmp) = descriptors(i)
      end do
      tmp = tmp + 8

      mgauss = (mxpars-tmp-2)/6 + 1

      if(.not.(newres.or.threed)) then
        do i = 1, 4
          params(i+tmp) = olddes(i)
        end do
        tmp = tmp + 4
      end if
      do i = 2, mgauss
        if((i.gt.2).or.newres.or.threed) then
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
          tmp = tmp + 1
          len1 = 0
          call chr_putc('E_Centre_',params(tmp),len1)
          call chr_puti(i,params(tmp),len1)
        end if
        tmp = tmp + 1
        len1 = 0
        call chr_putc('E_Width_',params(tmp),len1)
        call chr_puti(i,params(tmp),len1)
        tmp = tmp + 1
        len1 = 0
        call chr_putc('E_Height_',params(tmp),len1)
        call chr_puti(i,params(tmp),len1)
      end do

      if(newres.or.threed) then
        if(mxpars-tmp.ge.6) then
          tmp = tmp + 1
          params(tmp) = 'Skew'
          tmp = tmp + 1
          params(tmp) = 'Cauchy'
          tmp = tmp + 1
          params(tmp) = 'E_Skew'
          tmp = tmp + 1
          params(tmp) = 'E_Cauchy'
          if(tmp.lt.(mxpars-2)) then
            tmp = tmp + 1
            params(tmp) = 'Akaike IC'
          end if
          tmp = tmp + 1
          params(tmp) = 'Chebyorder'

*   No room allowed for skew etc., must be ARC2D or similar

        else if(mgauss.eq.1) then

*        so continuity correction required


          tmp = tmp + 1
          params(tmp) = 'Contincent'
          tmp = tmp + 1
          params(tmp) = 'E_Contcent'
        end if
      end if
      params(mxpars) = 'Dens_scale'
      end
