      subroutine read_coeffs(lunit,nused,kp1,aa,ylim)
*+
* Name:
*    READ_COEFFS

* Invocation:
*    CALL READ_COEFFS(LUNIT,NUSED,KP1,AA,YLIM)

* Purpose:
*  To read in the coefficients and limits from the .GMC file.

* Description:
*  To read in the coefficients and limits from the .GMC file.
*
* Arguments:
*    LUNIT = INTEGER (Given)
*        Unit for file
*    NUSED = INTEGER (Given)
*        Number of lines used
*    KP1 = INTEGER (Given)
*        Order+1
*    AA(MAX_KPLUS1,NUSED) = DOUBLE PRECISION ARRAY (Returned)
*        Coefficients
*    YLIM(2,NUSED) = DOUBLE PRECISION ARRAY (Returned)
*        Limits for Chebyshev polynomial
*
      implicit none
      integer MAX_KPLUS1
      parameter (MAX_KPLUS1=20)
*-
      integer nused,lunit,kp1
      double precision aa(MAX_KPLUS1,nused),ylim(2,nused)
      integer i,j

      do j = 1,nused
        read (lunit,1) (ylim(i,j),i=1,2)
      end do
      do j = 1,nused
        read (lunit,1) (aa(i,j),i=1,kp1)
      end do
    1 format(10(1pe13.4))
      close(unit=lunit)
      end
