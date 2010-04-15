      subroutine arc_deflin(nl,imax,ilim1,ilim2,sdens)
*+
* Name:
*    ARC_DEFLIN

* Invocation:
*    CALL ARC_DEFLIN(NL,IMAX,ILIM1,ILIM2,SDENS)

* Purpose:
*   Find maximum and limits for arc line.
* Description:
*  SPICA service routine to find the channel of the maximum of an arc line,
*  given an initial guess, and the 'limits' of that line, including as much
*  'background' as possible. The initial guess to the maximum should
*  be as close as possible to the true value, otherwise the desired
*  value may not be found. The limiting channels (ILIM1 and ILIM2)
*  include as much background as possible, up to the start of the
*  neighbouring line or the end of the spectrum.
*
* Arguments:
*    NL = INTEGER (Given)
*
*    SDENS(NL) = REAL ARRAY (Given)
*
*    IMAX = INTEGER (Given and returned)
*
*    ILIM1 = INTEGER (Returned)
*
*    ILIM2 = INTEGER (Returned)
*
*
*  Alan Bridger / RGO  10-APR-1984    For use with the arc routines
*  Revised for use in arcpartii TNW/Manchester 1985
*  Re-written, TNW/Cambridge, 25/5/90
*
*-
      implicit none
      integer nl
      real sdens(nl)
      integer imax,ilim1,ilim2,limit,direction,j
      logical look
*
*  Get nearest peak. This involves checking for any slope and "going up"
*  it if any is found
*
      do direction = -1,1,2
        do while(sdens(imax+direction).ge.sdens(imax))
          imax = imax + direction
        enddo
      end do
*
*  Get limits of line. This just alters the limit until there is a rise
*  which continues for at least 3 pixels
*
      ilim1 = imax
      look = .true.
      limit = max(1,(imax-33))
      do while(look)
        ilim1 = ilim1 - 1
        look = .false.
        if(ilim1-3.gt.limit) then
          do j = 1, 3
            if (sdens(ilim1-j).le.sdens(ilim1)) look = .true.
          enddo
        end if
      end do

      ilim2 = imax
      look = .true.
      limit = min(nl,(imax+33))
      do while(look)
        ilim2 = ilim2 + 1
        look = .false.
        if(ilim2+3.lt.limit) then
          do j = 1, 3
            if (sdens(ilim2+j).le.sdens(ilim2)) look = .true.
          enddo
        end if
      end do
      end
