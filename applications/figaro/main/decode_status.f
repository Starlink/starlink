      subroutine decode_status(ncntrlt,control,deccntr)
*+
* Name:
*    DECODE_STATUS

* Invocation:
*    CALL DECODE_STATUS(NCNTRLT,CONTROL,DECCNTR)

* Purpose:
*    Decode control elements into form for fitting etc.

* Description:
*    To decode the control elements. This handles both the old and new
*    formats, but note that some of the decoding of old formats is
*    guesswork, since less information was stored!
*
* Arguments:
*      NCNTRLT = INTEGER (Given)
*        Number of elements in control
*      CONTROL(NCNTRLT) = INTEGER ARRAY (Given)
*        Control encoded
*      DECCNTR(MAX_DECODE_CONTROL) = INTEGER ARRAY (Returned)
*        Profile control elements
*  Subroutines/functions referenced:
*
* Author:
*    T.N.Wilkins, Cambridge,  2-MAY-1991
* History:
*         "          "        1 to 8-JUL-1991 Various
*-
      implicit none
      include 'status_inc'
      include 'arc_dims'
      include 'opt_cmn'
      integer ncntrlt
      integer control(ncntrlt)
      integer curval,type,model,cstat

      if(ncntrlt.eq.3) then

* CONTROL element 1:
* Note that deccntr(FIT_ABS) can be 0 or 1 only
* and deccntr(FIT_MODEL) can be up to 99

        curval = control(1)
        deccntr(1) = curval/10000000
        curval = mod(curval,10000000)
        deccntr(2) = curval/100000
        curval = mod(curval,100000)
        deccntr(3) = curval/1000
        curval = mod(curval,1000)
        deccntr(4) = curval/100
        curval = mod(curval,100)
        deccntr(5) = curval/10
        curval = mod(curval,10)
        deccntr(6) = curval

* CONTROL element 2:

        curval = control(2)
        deccntr(7) = curval/1000000
        curval = mod(curval,1000000)
        deccntr(8) = curval/10000
        curval = mod(curval,10000)
        deccntr(9) = curval/1000
        curval = mod(curval,1000)
        deccntr(10) = curval/100
        curval = mod(curval,100)
        deccntr(11) = curval/10
        curval = mod(curval,10)
        deccntr(12) = curval

* CONTROL element 3 (base):

        curval = control(3)
        deccntr(MAX_PROFILE_CONTROL+1) = curval/100000000
        curval = mod(curval,100000000)
        deccntr(MAX_PROFILE_CONTROL+2) = curval/1000000
        curval = mod(curval,1000000)
        deccntr(MAX_PROFILE_CONTROL+3) = curval/10000
        curval = mod(curval,10000)
        deccntr(MAX_PROFILE_CONTROL+4) = curval/1000
        curval = mod(curval,1000)
        deccntr(MAX_PROFILE_CONTROL+5) = curval/100
        curval = mod(curval,100)
        deccntr(MAX_PROFILE_CONTROL+6) = curval/10
        curval = mod(curval,10)
        deccntr(MAX_PROFILE_CONTROL+7) = curval

      else

*   Old-style coding

        call zero_int(deccntr,max_decode_control)
        curval = control(1)

*   Fit status

        cstat = curval/1000
        if(cstat.ge.2) cstat = cstat + 1
        deccntr(FIT_STAT) = cstat
        curval = mod(curval,1000)

*   Number of components

        type = curval/100
        deccntr(FIT_ABS) = mod((type+1),2)
        curval = mod(curval,100)
        model = curval/10
        if(type.eq.0) then
        else if(type.le.2) then
          if(model.gt.1) model = model - 1
          deccntr(FIT_MODEL) = model
          deccntr(FIT_TYPE) = 1
          deccntr(FIT_NCMP) = 1
          if(usepeak) then
            deccntr(FIT_GUES) = 2
          else
            deccntr(FIT_GUES) = 1
          end if
          deccntr(FIT_OPT) = 1
        else
          deccntr(FIT_MODEL) = 1
          if(type.le.4) then
            deccntr(FIT_TYPE) = model - 4
            deccntr(FIT_NCMP) = 2
            deccntr(FIT_GUES) = 3
            deccntr(FIT_OPT) = 1
          else if(type.le.6) then
            deccntr(FIT_TYPE) = 6
            deccntr(FIT_NCMP) = model
            deccntr(FIT_GUES) = 2
            deccntr(FIT_OPT) = 2
          end if
        end if
        if(errpre) then
          deccntr(FIT_WEIGH) = 1
        else
          deccntr(FIT_WEIGH) = 0
        end if
        deccntr(BACK_MODEL) = mod(curval,10)
      end if
      end
