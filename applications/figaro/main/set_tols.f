      subroutine set_tols(ifarc,apply,status)
*+
* Name:
*    SET_TOLS

* Invocation:
*    CALL SET_TOLS(IFARC,APPLY,STATUS)

* Purpose:
*   To set the values of the tolerances.

* Description:
*   The user is presented with a list of the current values of the
*   tolerances, and can alter any he/she is unhappy with. When ready
*   the user can exit, to either apply tolerances, or leave them for
*   another time (this routine only returned a flag to say what to do
*   for this).

* Arguments:
*    IFARC = LOGICAL (Given)
*        If data is an arc
*    APPLY = LOGICAL (Returned)
*        If to apply tolerances
*    STATUS = INTEGER (Given and returned)
*        Global status
* Global variables:
*    TOLERANCE(MAXTOL) = REAL ARRAY (Given and returned)
*        tolerances (include file arc_dims)

* Authors:
*    TNW: T.N.Wilkins Manchester until 1/89, Cambridge until 9/92, then
* Durham
* History:
*    TNW: 18/10/88 to use ACCRES
*    TNW: 30/10/89 to use QMENU
*    TNW: 10/7/91 ACCRES removed
*    TNW: 3/8/93 Return APPLY
*    TNW: 15/3/94 QCHECK used
*-
      implicit none
      include 'arc_dims'
      logical ifarc,apply
      integer status,pstat
      integer m

*  menus

      character dumc
      integer NDICT
      parameter (NDICT = MAXTOL+2)
      character*46 dict(NDICT)
      logical duml
      data dict/
     :     'F V_TOL : Absolute error of line centre',
     :     'F V_MAX : Max allowed line centre',
     :     'F V_MIN : Min line centre',
     :     'F W_TOL : Absolute error of line width',
     :     'F W_MAX : Max allowed line width',
     :     'F W_MIN : Min allowed line width',
     :     'F W_S_N : Width signal/noise',
     :     'F H_MAX : Max allowed line height',
     :     'F H_MIN : Min allowed line height',
     :     'F H_S_N : Height signal/noise',
     :     'F C_TOL : Absolute error of Cauchy parameter',
     :     'F S_TOL : Absolute error of Skew parameter',
     :     'F H_TOL : Absolute error of height',
     :     'Q APPLY : Apply tolerances now',
     :     'Q EXIT  : Exit but don''t apply tolerances now'/

* tolerances on line centres

      if(ifarc) then
         call par_wruser(
     :        'Tolerances on position are in the units of the x array',
     :        pstat)
      else
         call par_wruser(
     :        'Tolerances on position are relative to the line''s rest'
     :        //' wavelength',pstat)
      end if

      m = 0

* print out current values of tolerances

      call qcheck('Edit Tolerances',dict,NDICT,tolerance,dumc,duml,m
     :     ,status)
      apply = m.ne.NDICT

* loop

      end
