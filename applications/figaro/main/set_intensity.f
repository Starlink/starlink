      subroutine set_intensity(z2,intens,dim1,dim2,dim3)
*+
* Name:
*    SET_INTENSITY

* Invocation:
*    CALL SET_INTENSITY(Z2,INTENS,DIM1,DIM2,DIM3)

* Purpose:
*  Sum intensity over wavelength

* Description:
*  To put a summed intensity into the total intensity array. Note that
*  is some of the range in wavelength of the main array is undefined,
*  the total array will be the sum of the defined elements, rather than
*  undefined.
*
* Arguments:
*    DIM1 = INTEGER (Given)
*        1st dimension of cube Z array
*    DIM2 = INTEGER (Given)
*        2nd dimension of cube Z array
*    DIM3 = INTEGER (Given)
*        3rd dimension of cube Z array
*    Z2(DIM1,DIM2,DIM3) = REAL ARRAY (Given)
*        Cube Z array
*    INTENS(DIM2,DIM3) = REAL ARRAY (Returned)
*        Total intensity array
*
*  Subroutine referenced:
*    DSA_SET_RANGE                  : Set max and min values for
*                                     main data array
*
*    T.N.Wilkins Manchester 14/6/88
*    Altered TNW 11/7/88 to have separate intensity array and to
*    find maximum and minimum data values.
*    Altered TNW 19/12/88 to use DSA_SET_RANGE
*    MISS array no longer user, TNW 6/2/90
*    Bug fix, 2/3/92, TNW
*-
      implicit none
      integer dim1,dim2,dim3,i,j,k,status
      real z2(dim1,dim2,dim3),intens(dim2,dim3)
      real datmax,datmin,value
      include 'PRM_PAR'
      include 'SAE_PAR'

      status = SAI__OK

      datmax = VAL__MINR
      datmin = VAL__MAXR

* Initialise to undefined

      call gen_cfill(1,dim2*dim3,VAL__BADR,intens)

      do k = 1, dim3
        do j = 1, dim2
          do i = 1, dim1
            value = z2(i,j,k)
            if(value.ne.VAL__BADR) then
              datmax = max(datmax,value)
              datmin = min(datmin,value)
              if(intens(j,k).eq.VAL__BADR) then
                intens(j,k) = value
              else
                intens(j,k) = intens(j,k) + value
              end if
            end if
          end do
        end do
      end do
      call dsa_set_range('data',datmin,datmax,status)
      end
