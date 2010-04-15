*+
*   C H E C K _ F I T S
*
      subroutine check_fits(refnam)
*
*  Description:
*
*
*  Arguments:
*    Given:
*      REFNAM (c*4): ref name
*    Altered:
*    Returned:
*    Workspace:
*  Subroutines/functions referenced:
*
*  Author:
*    T.N.Wilkins, Cambridge, 25-JUL-1991
*    A.J.Holloway, Manchester 4-Dec-1997
*  History:
*    AJH - Set REFNAM to 4 characters
*-
      implicit none
      include 'SAE_PAR'
      include 'DAT_PAR'
      include 'DAT_ERR'
      include 'CMP_ERR'
      character*(DAT__SZLOC) nloc
      character*4 refnam,chars*80
      integer status


* Old DTA library call
*      call dta_tyvar(refnam//'.fits',chars,status)
*      call cmp_type(nloc, refnam//'.fits' , chars, status)

             call dta_loc(refnam//'.fits',nloc,status)
             call dat_type(nloc,chars,status)

      if(status.ne.SAI__OK) call dta_crvar(refnam//'.fits','struct',
     :     status)

             call dta_annul(nloc,status)

*      call dta_tyvar(refnam//'.comments',chars,status)

             call dta_loc(refnam//'.comments',nloc,status)
             call dat_type(nloc,chars,status)

      if(status.ne.SAI__OK) call dta_crvar(refnam//'.comments','struct'
     :     ,status)

             call dta_annul(nloc,status)

      end




