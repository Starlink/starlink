      subroutine opt_wruser(string,status)
*+
* Name:
*    OPT_WRUSER

* Invocation:
*    CALL OPT_WRUSER(STRING,STATUS)
* Description:
*  To output a string to the log file/terminal, dependant upon the
*  value of the flag PRFITS in the common block OPT_COMMANDS.
*
* Purpose:
*  To output a string to the log file/terminal, dependant upon the
*  value of the flag PRFITS in the common block OPT_COMMANDS.
*
* Arguments:
*      STRING = CHARACTER*(*) (Given)
*        String to output
*      STATUS = INTEGER (Given and returned)
*        Error status, 0=ok
*    Subroutines/functions referenced:
*      PAR_WRUSER

* Author:
*   T.N.Wilkins, Cambridge, 29-MAR-1990
*-
      implicit none
      character*(*) string
      integer status
      include 'opt_cmn'
*

      if(prfits) call par_wruser(string,status)
      end
