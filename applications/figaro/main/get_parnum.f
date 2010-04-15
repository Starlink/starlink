      integer function get_parnum(param)
*+
* Name:
*    GET_PARNUM

* Invocation:
*   (INTEGER) = GET_PARNUM(PARAM)
*
* Description:
*    To locate a parameter in the results structure using the
*   parameter identification array. Note that PARAMS and PARAM are
*   case-sensitive. PARAM is padded to 10 characters long.
*
* Arguments:
*      PARAM = CHARACTER*(*) (Given)
*        Parameter to find
*   (pointers via common) Parameter names array
*      GET_PARNUM = INTEGER (Returned)
*        Location of parameter in "cube"

* History:
*   T.N.Wilkins, Cambridge,  7-8-JUN-1989
*        "           "       17-18-JUL-1989 Bug fixes, also to update
*                            .params structure if required
*        "           "       15-SEP-1989 New (more portable) method of
*                            referencing params array
*        "           "       30-NOV-1989 Bug fix to prevent writing
*                            beyond array limits.
*        "           "       14-MAY-1990 Checking of array moved to
*                            MAP_RES
*        "           "       30-MAY-1991 DSA_WRUSER replaced PAR_WRUSER
*        "        Durham,    7-APR-1993 Use tnw_index so works on
*                            DECStation
*        "        Durham,    30-APR-1993 Use chr_index
*-
      implicit none
      include 'arc_dims'
      character*(*) param
      integer ind, chr_index
      character*10 tmpp
      character*1 bss
      character*2 bsn
      data bss/'\\'/


*
      bsn = bss//'n'
      get_parnum = 0

      tmpp = param

*      ind = chr_index(dynamic_chars(parptr:parend),tmpp)
      ind = chr_index(parval,tmpp)
      if(ind.eq.0) then
        get_parnum = 0
        call dsa_wruser('Error, parameter "')
        call dsa_wruser(param)
        call dsa_wruser('" not found')
        call dsa_wruser(bsn)
      else
        get_parnum = ind/10 + 1
      end if
      end

