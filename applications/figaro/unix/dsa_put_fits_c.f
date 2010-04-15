*+
*   D S A _ P U T _ F I T S _ C
*
      subroutine dsa_put_fits_c(refnam,item,val,comment,status)
*
*  Description:
*
*
*  Arguments:
*      REFNAM = CHARACTER*4 (Given)
*        ref name
*      ITEM = CHARACTER*8 (Given)
*        item
*      STATUS = INTEGER (Given and returned)
*        status
*      VAL = CHARACTER*(*) (Returned)
*        value
*      COMMENT = CHARACTER*(*) (Returned)
*        comment
*  Subroutines/functions referenced:
*
*  Author:
*    T.N.Wilkins, Cambridge, 25-JUL-1991
*    A.J.Holloway, Manchester, 4-Dec-1997
*  History:
*    AJH Set refnam to be 4 characters, item to 8
*    AJH Removed use of dta_wrvarc
*-
      implicit none
      include 'SAE_PAR'
      include 'DAT_PAR'
      include 'DAT_ERR'
      include 'CMP_ERR'
      CHARACTER*(DAT__SZLOC) NLOC, PLOC
      character*4 refnam
      character*8 item
      character*80 chars
      character*(*) comment,val
      integer status,chr_len,ilen

*

      call check_fits(refnam)
      call dta_crnam(refnam//'.comments',item,1,chr_len(val),chars,
     :            status)
      call dta_crvar(chars,'double',status)

* Old dta
*      call dta_wrvarc(refnam//'.fits.'//item,chr_len(val),val,status)
*      call cmp_put0c(NLOC,refnam//'.fits.'//item, val, status)

           call dta_loc(refnam//'.fits.'//item,nloc,status)
           call dat_put0c(nloc,val,status)
           call dta_annul(nloc,status)


      ilen = chr_len(comment)
      if(ilen.gt.0) then
        call dta_crnam(refnam//'.comments',item,1,ilen,chars,status)
        call dta_crvar(chars,'char',status)

* Old dta
*        call dta_wrvarc(refnam//'.comments.'//item,ilen,comment,status)
*      call cmp_put0c(PLOC,refnam//'.comments.'//item, comment, status)

           call dta_loc(refnam//'.comments.'//item,ploc,status)
           call dat_put0c(ploc,comment,status)

           call dta_annul(ploc,status)


      end if
      end

