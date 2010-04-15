      subroutine map_ext(dims,resptr,errptr,missptr,varptr,eidsptr,
     :    samptr,vminptr,vmaxptr,status,xptr,yptr,resslot,errslot,
     :    xslot,yslot,unitsptr)
*+
* Name:
*    MAP_EXT

* Invocation:
*    CALL MAP_EXT(DIMS,RESPTR,ERRPTR,MISSPTR,VARPTR,EIDSPTR,
*         SAMPTR,VMINPTR,VMAXPTR,STATUS,XPTR,YPTR,RESSLOT,ERRSLOT,
*         XSLOT,YSLOT,UNITSPTR)

* Purpose:
*   To map the EXTATIC file

* Description:
*   To map the EXTATIC file

* Arguments:
*    DIMS(2) = INTEGER ARRAY (Given)
*
*    RESPTR = INTEGER (Given)
*        Pointer to results
*    ERRPTR = INTEGER (Given)
*        Pointer to errors
*    MISSPTR = INTEGER (Given)
*        Pointer to missing values array
*    VARPTR = INTEGER (Given)
*
*    EIDSPTR = INTEGER (Given)
*
*    SAMPTR = INTEGER (Given)
*
*    VMINPTR = INTEGER (Given)
*
*    VMAXPTR = INTEGER (Given)
*
*    STATUS = INTEGER (Given)
*        Error status, 0=ok
*    XPTR = INTEGER (Given)
*
*    YPTR = INTEGER (Given)
*
*    RESSLOT = INTEGER (Given)
*
*    ERRSLOT = INTEGER (Given)
*
*    XSLOT = INTEGER (Given)
*
*    YSLOT = INTEGER (Given)
*
*    UNITSPTR = INTEGER (Given)
*
* History:
*   Use of DSA routines, addition of mapping .x.units TNW 27/1/89
*   Change to map "write", rather than "update", TNW 23/7/90
*-
      implicit none
      include 'SAE_PAR'
      integer status
      integer dims(2)
      integer resptr,errptr,missptr,varptr,eidsptr,samptr
      integer vmaxptr,vminptr,xptr,yptr,dyn_element,unitsptr
      integer resslot,errslot,xslot,yslot

      call dsa_map_data('exstatic','WRITE','float',resptr,resslot,
     :     status)
      call dsa_map_axis_data('exstatic',1,'WRITE','float',xptr,xslot,
     :     status)
      call dsa_map_axis_data('exstatic',2,'WRITE','float',yptr,yslot,
     :     status)
      call dsa_map_errors('exstatic','WRITE','float',errptr,errslot,
     :     status)
      if(status.ne.SAI__OK) return

      call dta_muvarf('exstatic.vmin',dims(1),vminptr,status)
      if(status.ne.SAI__OK) then
        call tnw_dtaerr(status,'mapping','exstatic.vmin')
        return
      end if
      call dta_muvarf('exstatic.vmax',dims(1),vmaxptr,status)
      if(status.ne.SAI__OK) then
        call tnw_dtaerr(status,'mapping','exstatic.vmax')
        return
      end if
      call dta_muvars('exstatic.miss',dims(1)*dims(2),missptr,status)
      if(status.ne.SAI__OK) then
        call tnw_dtaerr(status,'mapping','exstatic.miss')
        return
      end if
      call dta_muvarc('exstatic.varnam',dims(1)*32,varptr,status)
      if(status.ne.SAI__OK) then
        call tnw_dtaerr(status,'mapping','exstatic.varnam')
        return
      end if
      call dta_muvarc('exstatic.ids',dims(2)*32,eidsptr,status)
      if(status.ne.SAI__OK) then
        call tnw_dtaerr(status,'mapping','exstatic.ids')
        return
      end if
      call dta_muvarc('exstatic.samnam',dims(2)*32,samptr,status)
      if(status.ne.SAI__OK) then
        call tnw_dtaerr(status,'mapping','exstatic.samnam')
        return
      end if
      call dta_muvarc('exstatic.x.units',dims(1)*32,unitsptr,status)
      if(status.ne.SAI__OK) then
        call tnw_dtaerr(status,'mapping','exstatic.x.units')
      end if

* Convert to array elements in dynamic_mem

      resptr = dyn_element(resptr)
      errptr = dyn_element(errptr)
      missptr = dyn_element(missptr)
      vminptr = dyn_element(vminptr)
      vmaxptr = dyn_element(vmaxptr)
      xptr = dyn_element(xptr)
      yptr = dyn_element(yptr)
      eidsptr = dyn_element(eidsptr)
      unitsptr = dyn_element(unitsptr)
      varptr = dyn_element(varptr)
      samptr = dyn_element(samptr)
      end
