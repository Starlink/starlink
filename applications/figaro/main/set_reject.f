      subroutine set_reject(status)
*+
* Name:
*    SET_REJECT

* Invocation:
*    CALL SET_REJECT(STATUS)

* Purpose:
*   Set tolerances

* Description:
*    Allow user to establish or remove the parameters which are to be
*    tested for failure of tolerance limits.
*
*   REJECT  controls whether fit tolerances are inforce. There are 7
*           possible tolerance parameters, each one of which is
*           controlled by an element of Reject as follows:
*            Items not included, but possibly should be?
*              1      Test and see if the position of the line in this
*                     fit block is within TOL of the previous fit block.
*                     This is primarily used in ARC fitting mode to
*                     ensure that rough curves are not obtained.
*              4      Lines with a flux less than or greater than
*                     predefined limits are rejected.
*   Correct version:
*              1      A lower and or an upper limit on the height of the
*                     fitted lines is applied. Lines outside the zone
*                     are rejected.
*              2      lines with centred outside TOL are to be rejected
*                     independent of any previous results. These modes
*                     are often invoked AFTER the event! For use
*                     in LONGSLIT a check is made relative to the line
*                     centre.
*              3      A lower and or an upper limit on the width of the
*                     fitted lines is applied. Lines outside the zone
*                     are rejected.
*              4      Test on values of errors
*              5      Lines with a S/N worse than a predefined level
*                     (S/N being measured in terms of the fit
*                     tolerances) are rejected
*              6      Tests on shape
*              7      A test for the separation of two features in a
*                     multiple gaussian fit.
*   The value of each element of REJECT has the value 1 or 0.
*   and controlS whether or not fits are to be rejected
*   if they fail the corresponding tolerance limits .
*   A zero value implies no tolerance test is made for the corresponding
*   parameter.
*
* Arguments:
*    STATUS = INTEGER (Given and returned)
*        Error status, 0=ok
* Global variables:
*    REJECT(SZREJT) = LOGICAL ARRAY (Given and returned)
*        Rejection criteria being used (include file arc_dims)

* Authors:
*   TNW: T.N.Wilkins, Cambridge until 9/92, the Durham

* History:
*   Prompting re-written and dict passed as argument, TNW 15-20/8/91
*   LOGFAIL set here, TNW, 11/2/94
*   LOGFAIL incorporated into REJECT and QCHECK used TNW 15/3/94
*-
      implicit none
      include 'arc_dims'
      integer status
* local
      integer m
      integer NDICT
      parameter (NDICT = SZREJT+1)
      real dumr
      character dumc
      character*39 dict(NDICT)

* rejection criteria

      data dict/
     :     'L HEIGHT      : Test on HEIGHT',
     :     'L CENTRE      : Test on CENTRE',
     :     'L WIDTH       : Test on WIDTH',
     :     'L ERRORS      : Test on ERRORS',
     :     'L S/N         : Test on S/N',
     :     'L SHAPE       : Test on SHAPE',
     :     'L SEPARATIONS : Test on SEPARATIONS',
     :     'L LOG         : Log failures',
     :     'Q APPLY       : Apply tolerances now'/

* alter rejection criteria -if so which?

      call qcheck('Set Rejection Criteria',dict,NDICT,dumr,dumc,reject,m
     :     ,status)
      end
