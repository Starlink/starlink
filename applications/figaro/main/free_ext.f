      subroutine free_ext(resslot,errslot,xslot,yslot,status)
*+
* Name:
*    FREE_EXT

* Invocation:
*    CALL FREE_EXT(RESSLOT,ERRSLOT,XSLOT,YSLOT,STATUS)

* Purpose:
* Description:
* To unmap the EXSTATIC file and close it.
*
* Arguments:
*    RESSLOT = INTEGER (Given)
*        Results slot
*    ERRSLOT = INTEGER (Given)
*        Errors slot
*    XSLOT = INTEGER (Given)
*        X slot
*    YSLOT = INTEGER (Given)
*        Y slot
*    STATUS = INTEGER (Given and returned)
*        Error status

* History:
* Altered to user DSA, X.UNITS added TNW 27/1/89
*
      implicit none
      include 'SAE_PAR'
      integer resslot,errslot,xslot,yslot,status
*-
      integer NSTR,len1,i
      parameter (NSTR=6)
      character*8 substructs(NSTR),extfil,chars*16
      data substructs/'.miss','.varnam','.samnam','.vmin','.vmax',
     :      '.x.units'/
      data extfil/'exstatic'/

      call dsa_unmap(resslot,status)

      call dsa_unmap(errslot,status)

      call dsa_unmap(xslot,status)

      call dsa_unmap(yslot,status)

      do i = 1, nstr
        len1 = 0
        call chr_putc(extfil,chars,len1)
        call chr_appnd(substructs(i),chars,len1)
        call dta_frvar(chars(:len1),status)
        if(status.ne.SAI__OK) then
          call tnw_dtaerr(status,'unmapping',chars)
          status = SAI__OK
        end if
      end do
      call dsa_close_structure(extfil,status)
      end
