      subroutine mapcube(ftype,wrtacr,wrtacd,status)
*+
* Name:
*    MAPCUBE

* Invocation:
*    CALL MAPCUBE(FTYPE,WRTACR,WRTACD,STATUS)
* Purpose:
*  To map the data file arrays
*
* Description:
*  To map the data file arrays
*
* Arguments:
*     WRTACR = LOGICAL (Given)
*        If write access required to .RES.TOTAL_INTENSITY etc.
*     WRTACD = LOGICAL (Given)
*        If write access required to .Z.DATA, etc.
*     STATUS = INTEGER (Given and returned)
*        0 if no error occurs
*     FTYPE = CHARACTER*10 (Returned)
*        Type of array
* Global variables:
*     XPTR = INTEGER (Returned)
*        Pointer to X (spatial) array
*     XDPTR = INTEGER (Returned)
*        Pointer to X (spatial) displacement array
*     YPTR = INTEGER (Returned)
*        Pointer to Y (spatial) array
*     TOTPTR = INTEGER (Returned)
*        Pointer to total intensity array
*     D_MPTR = INTEGER (Returned)
*        Pointer to mask array
*     D_CPTR = INTEGER (Returned)
*        Pointer to fit control array
*     NYP = INTEGER (Returned)
*        Max number of lines allowed for
*     PARPTR = INTEGER (Returned)
*        Pointer to parameter names array
*     D_SPTR = INTEGER (Returned)
*        Pointer to intensity array
*     D_XPTR = INTEGER (Returned)
*        Pointer to wavelengths array
*     SPDIM1 = INTEGER (Returned)
*        X (spatial) dimension of array
*     SPDIM2 = INTEGER (Returned)
*        Y (spatial) dimension of array
*     WAVDIM = INTEGER (Returned)
*        Wavelength dimension of array
*     IDSPTR = INTEGER (Returned)
*        Pointer to IDS array
*     IDSEND = INTEGER (Returned)
*        End of IDS array
*     NZP = INTEGER (Returned)
*        1st dimension of results block
*     D_WPTR = INTEGER (Returned)
*        Pointer to rest wavelengths array
*
* Subroutines referenced:
*     ACCRES      : General access for application-specific structures
*     PAR_WRUSER  : Write string to user

* History:
*    T.N.Wilkins Manchester Current version 22/8/88
*    Altered to use ACCRES, TNW 18/10/88
*    TNW 19/12/88 Changed to pass status, rather than logical variable
*    error
*    TNW/IOA Cambridge, 28/11/89 Now to map parameter names array
*       "         "      6/2/90 Made to call map_res. Also no longer
*    allow data to have sort = .false.
*    AJH 1/99 Changed map_data mode from r,w to READ, WRITE
*-
      implicit none
      include 'SAE_PAR'
      integer status
      include 'arc_dims'
      logical wrtacr,wrtacd,nocube
      integer pstat
      character ftype*10
      character*1 mode,amode

      if(wrtacd) then
        mode = 'WRITE'
      else
        mode = 'READ'
      end if
      if(wrtacr) then
        amode = 'U'
      else
        amode = 'R'
      end if
      nxp = spdim1

      call dsa_use_flagged_values('data',status)

* Locate and map results structure. Most of this is dealt with by map_res

      refine = .false. ! We don't allow prompting for new or clone
      call map_res(.false.,.false.,nocube,status)
      if(nocube) then
        status = SAI__ERROR
      end if

* Map data and axis arrays

      call map_data(.false.,mode,status)

      if(status.ne.SAI__OK) return

* Check type of file

      call accres(' ','more.twodspec.variant','rc',10,0,ftype,status)
      if(status.ne.SAI__OK) then
        status = SAI__OK
        call accres(' ','more.twodspec.type','rc',10,0,ftype,status)
      end if
      if((status.ne.SAI__OK).or.(ftype.eq.'          ')) then
        status = SAI__OK
        call par_wruser('Will assume type "RECT"',pstat)
        ftype = 'RECT'
      else if(ftype(1:3).eq.'HEX') then
        call accres(' ','more.twodspec.xdisp','f'//amode,spdim2,xdptr,
     :            ' ',status)
      end if

      end
