      subroutine fill_out(data,dim1,line,fitsta,results,resvar,x,left,
     :                  right)
*+
* Name:
*    FILL_OUT

* Invocation:
*    CALL FILL_OUT(DATA,DIM1,LINE,FITSTA,RESULTS,RESVAR,X,LEFT,
*                       RIGHT)
* Purpose:
*    To fill the output file for synthetic spectra.

* Description:
*    To fill the output file for synthetic spectra.
*
* Arguments:
*      DIM1 = INTEGER (Given)
*        1st dimension of output data
*      LINE = INTEGER (Given)
*        Line we're copying
*      FITSTA(NCNTRL,NYP,SPDIM1,SPDIM2) = INTEGER ARRAY (Given)
*        Fit status array
*      RESULTS(MXPARS,NYP,SPDIM1,SPDIM2) = REAL ARRAY (Given)
*        Results array
*      RESVAR(MXPARS,NYP,SPDIM1,SPDIM2) = REAL ARRAY (Given)
*        Variance array
*      LEFT = REAL (Given)
*        Left tram for current line
*      RIGHT = REAL (Given)
*        Right tram for current line
*      DATA(DIM1,SPDIM1,SPDIM2) = REAL ARRAY (Given and returned)
*        Data of output file
*      X(DIM1) = REAL ARRAY (Returned)
*        X array for output file
*  Subroutines/functions referenced:
*
* Author:
*    T.N.Wilkins, Cambridge, 23-SEP-1991
* History:
*-
      implicit none
      include 'SAE_PAR'
      include 'arc_dims'
      include 'status_inc'
      include 'fit_coding_inc'
      integer dim1
      real data(dim1,spdim1,spdim2)
      integer line
      integer fitsta(ncntrl,nyp,spdim1,spdim2)
      real results(mxpars,nyp,spdim1,spdim2)
      real resvar(mxpars,nyp,spdim1,spdim2)
      real x(dim1)
      real left
      real right

*

      integer i,j,get_parnum,kk,status,k
      real gaussian,skew,cauchy
      real factor,fitpar(MAXPARMS),odensc,disper

      disper = (right - left)/real(dim1-1)

      do kk = 1, dim1
        x(kk) = left + real(kk-1)*disper
      end do

      do j = 1, spdim2
        do i = 1, spdim1
          status = SAI__OK
          call getres(results,line,i,j,fitpar,deccntr,odensc,fitsta,
     :                  status)
          if((status.eq.SAI__OK).and.(deccntr(FIT_STAT).eq.1)) then

*        Factor to allow for averaging

            factor = 1.0
            if(spdim1.gt.1) factor = factor
     :             * resvar(get_parnum('Space1_pos'),line,i,j)
            if(spdim2.gt.1) factor = factor
     :             * resvar(get_parnum('Space2_pos'),line,i,j)
            factor = 2.0/sqrt(factor)
            do k = 1, deccntr(FIT_NCMP)
              fitpar(2) = fitpar(k*3 - 1)
              fitpar(3) = fitpar(k*3)*factor
              fitpar(4) = fitpar(k*3 + 1)
              fitpar(1) = 0
              if(deccntr(FIT_MODEL).eq.GAUSSIAN_MODEL) then
                do kk = 1, dim1
                  data(kk,i,j) = data(kk,i,j) + gaussian(x(kk),fitpar)
                end do
              else if(deccntr(FIT_MODEL).eq.SKEW_MODEL) then
                do kk = 1, dim1
                  data(kk,i,j) = data(kk,i,j) + skew(x(kk),fitpar)
                end do
              else if(deccntr(FIT_MODEL).eq.CAUCHY_MODEL) then
                do kk = 1, dim1
                  data(kk,i,j) = data(kk,i,j) + cauchy(x(kk),fitpar)
                end do
              end if
            end do
          end if
        end do
      end do
      end
