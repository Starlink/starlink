      subroutine wave2vel(rstwav,obswav,eobwav,obswid,eowid,vel,evel
     :     ,vwid,evwid)
*+
* Name:
*    WAVE2VEL

* Invocation:
*    CALL WAVE2VEL(RSTWAV,OBSWAV,EOBWAV,OBSWID,EOWID,VEL,EVEL
*          ,VWID,EVWID)

* Purpose:
*  To convert observed wavelength to velocity, also the width.

* Description:
*  Errors are calculated for both.
*  This uses the relativistic formula or the non-relativistic depending
*  upon the value of IFRELATIVISTIC in common.
*  The width and error on the centre are treated as small deviations
*  on the centre (i.e. handled by differentiation).
*
* Arguments:
*   RSTWAV = REAL (Given)
*      Rest wavelength
*   OBSWAV = REAL (Given)
*      Observed wavelength
*   EOBWAV = REAL (Given)
*      Error on observed wavelength
*   OBSWID = REAL (Given)
*      Observed wavelength width
*   EOWID = REAL (Given)
*      Error on observed wavelength width
*   VEL = REAL (Returned)
*      Velocity
*   EVEL = REAL (Returned)
*      Error on velocity
*   VWID = REAL (Returned)
*      Velocity width
*   EVWID = REAL (Returned)
*      Error on velocity width
* Global variables:
*   IFRELATIVISTIC = LOGICAL (Given, include file arc_dims)
*      If to use relativistic formula

* Subroutines/functions referenced:
* Author:
*   T.N.Wilkins, Cambridge,  4-AUG-1989

* History:
*   TNW: Bug fix for differentiation, 19/11/93
*-
      implicit none
      include 'arc_dims'
      real rstwav
      real obswav
      real eobwav
      real obswid
      real eowid
      real vel
      real evel
      real vwid
      real evwid

*

      double precision C
      parameter (C = 2.9979248d5)
      double precision ratio,ratio2,rat2p1,diff
      double precision dradob

      if(ifrelativistic) then
         ratio = dble(rstwav) / dble(obswav)
         ratio2 = ratio * ratio
         rat2p1 = ratio2 + 1.0d0


* Centre

         vel = real(C * (1.0d0 - ratio2) / rat2p1)

* Error on centre


* d(r)/d(obswav)

         dradob = ratio / dble(obswav)

         diff = C * 4 * ratio * dradob / ( rat2p1 * rat2p1 )
      else
         ratio = dble(obswav) / dble(rstwav)
         vel = c * (ratio - 1.0d0)
         diff = c / rstwav
      endif

      evel = abs(eobwav * real(diff))

* Width

      vwid = abs(obswid * real(diff))

* Error on width

      evwid = abs(eowid * real(diff))

      end
