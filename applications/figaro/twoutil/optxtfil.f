      subroutine optxtfil(name,lu,status)
*+
* Name:
*    OPTXTFIL

* Invocation:
*    CALL OPTXTFIL(NAME,LU,STATUS)

* Purpose:
*  Open text file for output

* Description:
*  To open a text file with carriagecontrol='list',status='new' on a
*  GIVEN unit number. This is used instead of DSA_OPEN_TEXT_FILE when
*  the unit is already allocated. Note that the carriagecontrol specifier
*  is only used under VMS.
*
* Arguments:
*      NAME = CHARACTER*(*) (Given)
*        File name
*      LU = INTEGER (Given)
*        Logical unit number
*      STATUS = INTEGER (Given and returned)
*        Error status, 0=ok
*    Subroutines/functions referenced:
*
*   T.N.Wilkins, Cambridge,  6-JUN-1990
*-
      implicit none
      character*(*) name
      integer lu
      integer status
      include 'SAE_PAR'

*

      if(status.eq.SAI__OK) then
        open(lu,file=name,status='new',iostat=status)
      end if
      end
