      subroutine underscore
*+
* Name:
*    UNDERSCORE

* Invocation:
*    CALL UNDERSCORE
* Purpose:
*    Write row of dashes to terminal

* Description:
*    Write row of dashes to terminal
*-
      integer status
      call par_wruser(
     :  '-----------------------------------------------'/
     : /'-------------------------', status)
      end
