      real function arfind2(arcn,nlarcs,value,arcname,line_name)
*+
* Name:
*    ARFIND2

* Invocation:
*   (REAL) = ARFIND2(ARCN,NLARCS,VALUE,ARCNAME,LINE_NAME)

* Purpose:
*   Search line list for line nearest in wavelength to given value

* Description:
*     Given a wavelength, locates the nearest arc line to
*     that wavelength in a table. For use with tables sorted into
*     ascending order of wavelength.
*
* Arguments:
*    ARCN(NLARCS) =  REAL ARRAY (Given)
*        Holds the table of arc wavelengths.  These should be in ascending
*        order, and end with a 0. value or with the end of the array.
*    NLARCS = INTEGER (Given)
*        The dimension of ARCN
*    VALUE =  REAL (Given)
*        The given wavelength.
*    ARCNAME = CHARACTER*10 (Given)
*        List of arc names
*    LINE_NAME = CHARACTER*10 (Returned)
*        Name of nearest match line
* Returned value:
*    ARFIND2 = REAL
*        The nearest tabulated wavelength.

* History:
*                                        KS / CIT  13th Jan 1983
*             Adapted for arc2d to give line id as well.
*                                        TNW / MAN 17th Jan 1986
*             New algorithm, TNW 28th July 1993.
*-
      implicit none
*
*     Parameters
*
      integer nlarcs
      real arcn(nlarcs),value
      character*10 arcname(nlarcs),line_name
*
*     Local variables
*
      integer chosen,istart,iend,itest,ntest
*
*   Are we outside the bounds of the table, if so give nearest value
*
      if(value.le.arcn(1)) then
         chosen = 1
      else if(value.ge.arcn(nlarcs)) then
         chosen = nlarcs
      else

*   loop, dividing table in half on each loop to select part in which value
*   is located. Eventually (in not such a long time) we get to a table with
*   2 values and we select the nearest.

         istart = 1
         iend = nlarcs
         ntest = nlarcs
         do while(.true.)
            if(ntest.eq.2) then
               if((value - arcn(istart)) .lt. (arcn(iend) - value))
     :              then
                  chosen = istart
               else
                  chosen = iend
               endif
               goto 10
            else

*   Divide table in half

               itest = (istart+iend)/2
               if(value .lt. arcn(itest)) then
                  iend = itest
               else
                  istart = itest
               endif
               ntest = iend - istart + 1
            endif
         enddo
      endif
*
  10  continue

*  Set return values

      arfind2 = arcn(chosen)
      line_name = arcname(chosen)
      end
