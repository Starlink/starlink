      subroutine tnw_dtaerr(status,action,structure)
*+
* Name:
*    TNW_DTAERR

* Invocation:
*    CALL TNW_DTAERR(STATUS,ACTION,STRUCTURE)

* Purpose:
*  To output an error message for a DTA error.

* Description:
*  To output an error message for a DTA error.
*
* Arguments:
*      STATUS = INTEGER (Given)
*        Error status from DTA routine
*      ACTION = CHARACTER*(*) (Given)
*        Action in which error occurred (e.g. reading)
*      STRUCTURE = CHARACTER*(*) (Given)
*        Structure for which error occurred
* Subroutines/functions referenced:
*      DSA_WRUSER, DSA_WRNAME, DTA_ERROR
* History:
*   T.N.Wilkins, Cambridge, 28-JUN-1990
*   A.J.Holloway, Manchester 10-MAR-1998
*-
      implicit none
      integer status
      character*(*) action
      character*(*) structure
      integer chr_len
*      character*80 chars
      character*2 bss,bsn
      data bss/'\\'/
      bsn = bss(1:1)//'n'
*
      call dsa_wruser('Error ')
      call dsa_wruser(action)
      call dsa_wruser(' ')
*      call dsa_wrname(structure(:chr_len(structure)))
      call dsa_wruser(structure(:chr_len(structure)))
      call dsa_wruser(bsn)
*      call dta_error(status,chars)
*      call dsa_wruser(chars(:chr_len(chars)))
*      call dsa_wruser(bsn)
      end
