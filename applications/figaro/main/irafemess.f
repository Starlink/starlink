      subroutine irafemess(code)
*+
* Name:
*    IRAFEMESS

* Invocation:
*    CALL IRAFEMESS(CODE)

* Purpose:
*   To output an IRAF (IMFORT) error message, given the code.
*
* Arguments:
*    CODE = INTEGER (Given)
*        IRAF error code
* Description:
*   To output an IRAF (IMFORT) error message, given the code.
*-
      implicit none
      integer code,status,chr_len
      character*80 emess
      call imemsg(code,emess)
      call par_wruser(emess(:chr_len(emess)),status)
      end
