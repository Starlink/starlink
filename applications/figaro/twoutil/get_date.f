      subroutine get_date(date)
*+
* Name:
*    GET_DATE

* Invocation:
*    CALL GET_DATE(DATE)

* Purpose:
*  Return the current date/time as a string.

* Description:
*  Return the current date/time as a string.

* Arguments:
*   Returned:
*     DATE  (c*(*)): The current date/time (up to 46 characters)
* Subroutine called:
*     GEN_TIME     : Obtain current date, time and day.
* History:
*   T.N.Wilkins Manchester, altered 10/11/88 to use GEN_TIME
*-
      implicit none
      character*(*) date
*
      integer lday,ldate,lhour
      character day*9,tdate*20,hour*12
      call gen_time(2,day,lday,tdate,ldate,hour,lhour)
      date = day(:lday)//' '//tdate(:ldate)//' at '//hour(:lhour)
      end
