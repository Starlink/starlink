
* See gau2_pro for discussion

      subroutine gau2_getrd (iv, liv, v, lv, idx, upix, elems, img,
     :     status)

*+
*   Description:
*     Copy the regression diagnostic in V(IV(67)) into the appropriate
*     elements of img.
*
*   Arguments:
*     iv = integer(liv) (Given)
*       NSG's integer work array
*     liv = integer (Given)
*       Size of IV()
*     v = real(lv) (Given)
*       NSG's real work array
*     lv = integer (Given)
*       Size of V()
*     idx = integer(upix) (Given)
*       The indexes of the good pixels in the data array.
*     upix = integer (Given)
*       The number of good pixels
*     elems = integer (Given)
*       The size of the data and image arrays
*     img = real(elems) (Returned)
*       The array to be filled with the regression diagnostic.
*     status = integer (Given and Returned)
*       The inherited status.
*-

*   Types
      implicit none
      include 'SAE_PAR'
      include 'PRM_PAR'         ! PRIMDAT primitive data constants

*   Arguments
      integer liv, lv, elems, upix
      integer iv(liv)
      doubleprecision v(lv)
      real img(elems)
      integer status

*   Arguments returned
      integer idx(upix)

*   Local variables
      integer i, j              ! loop counters


      if (status .ne. sai__ok) return

*   First, initialise the destination array with bad values
      do i=1,elems
         img(i) = VAL__BADR
      enddo

*   iv(67) is the starting subscript in V() for the regression
*   diagnostic array (NSG p.14)
      j = iv(67)
      if (j .lt. 1) then
*      This is a `can't-happen' error.
         call msg_seti ('CODE', j)
         call msg_out (' ',
     :        'No regression diagnostic available (code ^CODE)',
     :        status)
         status = sai__error
      else
         do i=1,upix
            img(idx(i)) = v(j)
            j = j + 1
         enddo
      endif

      end
