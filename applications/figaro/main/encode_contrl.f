      subroutine encode_contrl(deccntr,ncntrl,control)
*+
* Name:
*    ENCODE_CONTRL

* Invocation:
*    CALL ENCODE_CONTRL(DECCNTR,NCNTRL,CONTROL)

* Purpose:
*    To encode the control elements.

* Description:
*    To encode the control elements.
*
* Arguments:
*      DECCNTR(MAX_DECODE_CONTROL) = INTEGER ARRAY (Given)
*        Profile control elements
*      NCNTRL = INTEGER (Given)
*        Number of elements in control
*      CONTROL(NCNTRL) = INTEGER ARRAY (Returned)
*        Control encoded
*  Subroutines/functions referenced:
*
* Author:
*    T.N.Wilkins, Cambridge,  2-MAY-1991
* History:
*          "          "       8-JUL-1991 Only support new format
*-
      implicit none
      include 'status_inc'
      integer ncntrl
      integer control(ncntrl)


* CONTROL element 1:

      control(1) = deccntr(1) * 10000000
     :           + deccntr(2) * 100000
     :           + deccntr(3) * 1000
     :           + deccntr(4) * 100
     :           + deccntr(5) * 10
     :           + deccntr(6)

* CONTROL element 2:

      control(2) = deccntr(7) * 1000000
     :           + deccntr(8) * 10000
     :           + deccntr(9) * 1000
     :           + deccntr(10) * 100
     :           + deccntr(11) * 10
     :           + deccntr(12)

* CONTROL element 3:

      control(3) = deccntr(max_profile_control+1) * 100000000
     :           + deccntr(max_profile_control+2) * 1000000
     :           + deccntr(max_profile_control+3) * 10000
     :           + deccntr(max_profile_control+4) * 1000
     :           + deccntr(max_profile_control+5) * 100
     :           + deccntr(max_profile_control+6) * 10
     :           + deccntr(max_profile_control+7)

      end
