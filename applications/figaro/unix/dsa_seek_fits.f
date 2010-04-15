*+
*   D S A _ S E E K _ F I T S
*
      subroutine dsa_seek_fits(refnam,item,exist,acc,el,slen,status)
*
*  Description:
*
*
*  Purpose:
*
*
*  Arguments:
*      REFNAM = CHARACTER*(*) (Given)
*        ref name
*      ITEM = CHARACTER*(*) (Given)
*        item
*      STATUS = INTEGER (Given and returned)
*        status
*      EXIST = LOGICAL (Returned)
*        if exists
*      ACC = CHARACTER*(*) (Returned)
*        access
*      EL = INTEGER (Returned)
*        elements
*      SLEN = INTEGER (Returned)
*        string lenght
*  Subroutines/functions referenced:
*
*  Author:
*    T.N.Wilkins, Cambridge, 25-JUL-1991
*  History:
*-
      implicit none
      include 'SAE_PAR'
      include 'DAT_PAR'
      include 'DAT_ERR'
      include 'CMP_ERR'
      character*(*) refnam
      character*(*) item
      logical exist
      character*(*) acc
      integer el
      integer slen
      integer status,i
      character*80 name,type
      character*(DAT__SZLOC) nloc
*

      name = refnam//'.fits.'//item

* Old dta library call
*      call dta_tyvar(name,type,status)
*      call cmp_type(nloc, name, type, status)

             call dta_loc(name,nloc,status)
             call dat_type(nloc,type,status)

      exist = status.eq.SAI__OK
             call dta_annul(nloc,status)


      if(exist) then
        acc = type(1:1)
        el = 1
        if(acc.eq.'C') then
          call dta_szvar(name,1,i,slen,status)
        end if
      end if
      end
