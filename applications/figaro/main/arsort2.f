      subroutine arsort2(waves,ids,nlid)
*+
* Name:
*    ARSORT2

* Invocation:
*    CALL ARSORT2(WAVES,IDS,NLID)

* Purpose:
*     Sorts the identification line tables into ascending order of
*     wavelength.

* Description:
*     The Shell sorting procedure is used to reorder the elements of A
*     so that A(I).LE.A(I+1) for I=1,...,N-1. It is assumed that N.GE.1.
*     This is the NSWC SHELL routine adapted for sorting line lists, in
*     which the names must also be moved. The code was also ran through
*     SPAG to make it easier to understand.
*
* Arguments:
*     IDS(NLID) = CHARACTER*10 ARRAY (Given and returned)
*         The names of the identified arc lines
*     WAVES(NLID) = REAL ARRAY (Given and returned)
*         The wavelengths of the identified arc lines
*     NLID = INTEGER (Given)
*         The number of identified lines

* History:
*   TNW /Durham, modified 28/7/93
*-
      implicit none
      integer nlid
      real waves(nlid)
      character*10 ids(nlid)
      integer k(10)
      integer i,ii,ki,l,ll,imax,j,jmax
      real s
      character*10 ctmp
* ------------------------
      data k/1,4,13,40,121,364,1093,3280,9841,29524/
* ------------------------
*
*             selection of the increments k(i) = (3**i-1)/2
*
      if ( nlid.lt.2 ) return
      imax = 1
      do i = 3, 10
         if ( nlid.le.k(i) ) go to 100
         imax = imax + 1
      end do
*
*            stepping through the increments k(imax),...,k(1)
*
 100  continue
      i = imax
      do ii = 1, imax
         ki = k(i)
*
*             sorting elements that are ki positions apart
*               so that waves(j).le.waves(j+ki) for j=1,...,nlid-ki
*
         jmax = nlid - ki
         do j = 1, jmax
            l = j
            ll = j + ki
            s = waves(ll)
            ctmp = ids(ll)
 120        continue
            if ( s.lt.waves(l) ) then
               waves(ll) = waves(l)
               ids(ll) = ids(l)
               ll = l
               l = l - ki
               if ( l.gt.0 ) go to 120
            end if
            waves(ll) = s
            ids(ll) = ctmp
         end do
*
         i = i - 1
      end do
      end
