      subroutine colrain(slev,nlevu)
*+
* Name:
*    COLRAIN

* Invocation:
*    CALL COLRAIN(SLEV,NLEVU)

* Purpose:
*   To set the colour table to "rainbow".
*
* Description:
*   To set the colour table to "rainbow".
*
* Arguments:
*     SLEV = INTEGER (Given)
*        Starting level to use
*     NLEVU = INTEGER (Given)
*        Number of level to use
*
* Subroutine/functions called:
*    SGS_ICURW : Inquire workstation identifier
*    GSCR      : Set colour representation

* Author:
*   T.N.Wilkins 15/7/88
*-
      implicit none
      integer slev,nlevu,i,tnlevs,j
      real levr,levg,levb

      tnlevs=nlevu/3

* Set table

      do i = 1,tnlevs-1
        j = slev + i - 1
        levr = 1.0
        levg = abs(sin(3.1415*real(i)/real(nlevu)))
        levb = 0.0
        call pgscr(j,levr,levg,levb)
      end do
      do i = tnlevs,2*tnlevs
        j = slev + i - 1
        levr = real(2*tnlevs-i)/real(tnlevs)
        levg = abs(sin(3.1415*real(i)/real(nlevu)))
        levb = 0.0
        call pgscr(j,levr,levg,levb)
      end do
      do i = 2*tnlevs+1,nlevu
        j = slev + i - 1
        levr = 0.0
        levg = abs(sin(3.1415*real(i)/real(nlevu)))
        levb = real(i-2*tnlevs)/real(tnlevs+2)
        call pgscr(j,levr,levg,levb)
      end do
      end
