      subroutine cop_2_1d_err(errors,start1,end1,start2,end2,errsub)
*+
* Name:
*    COP_2_1D_ERR

* Invocation:
*    CALL COP_2_1D_ERR(ERRORS,START1,END1,START2,END2,ERRSUB)

* Purpose:
*   Obtain errors array for fitting from 2-d array

* Description:
*   To copy the from the errors array, extracting the required part
*   and adding in quadrature.
*
* Arguments:
*    ERRORS(WAVDIM,SPDIM1) = REAL ARRAY (Given)
*        Errors array
*    START1 = INTEGER (Given)
*        Start cross-section to extract from
*    END1 = INTEGER (Given)
*        End cross-section to extract from
*    START2 = INTEGER (Given)
*        Start position extract from
*    END2 = INTEGER (Given)
*        End positionto extract from
*    ERRSUB(WAVDIM) = REAL ARRAY (Returned)
*        Extracted error array
* Global variables:
*    WAVDIM = INTEGER (Given)
*        1st dimension of errors array
*    SPDIM1 = INTEGER (Given)
*        2nd dimension of errors array
*    SPDIM2 = INTEGER (Given)
*        3rd dimension of errors array

* Authors:
*  T.N.Wilkins Cambridge 1989
* History:
*  TNW: Bug fix, 8/2/94 in dimension of errors, and index in last loop
*       for right hand side.
*-
      implicit none
      include 'arc_dims'
      integer start1,end1,start2,end2
      real errors(wavdim,spdim1,spdim2),errsub(wavdim)
      integer i,j,k
      real tmp

* First fill array with variances

      call zero_real(errsub,wavdim)
      do k = start2,end2
        do j = start1,end1
          do i = 1, wavdim
            tmp = errors(i,j,k)
            errsub(i) = errsub(i) + tmp*tmp
          end do
        end do
      end do

* Square root to get standard deviations

      do i = 1, wavdim
        errsub(i) = sqrt(errsub(i))
      end do
      end
