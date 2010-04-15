      subroutine rdinf(filnam,name,diminf,inf,nfnd,status)
*+
* Name:
*    RDINF

* Invocation:
*    CALL RDINF(FILNAM,NAME,DIMINF,INF,NFND,STATUS)

* Purpose:
*   To read numerical information from the .FITS (or .OBS) structure.

* Description:
*  To read numerical information from the .FITS (or .OBS) structure,
* checking the type e.t.c. prior to doing so. If of type character, then
* an attempt is made to read numbers from the string.
*
* Arguments:-
*   FILNAM = CHARACTER*(*) (Given)
*        Logical name (eg. 'image') for file (no trailing
*                     blanks)
*   NAME = CHARACTER*8 (Given)
*        Name of element of .FITS (or .OBS) structure
*   DIMINF = INTEGER (Given)
*        Dimension of INF below, number of elements to fill
*   INF(DIMINF) = REAL ARRAY (Returned)
*        Information requested
*   NFND = INTEGER (Returned)
*        Number of elements found.
*   STATUS = INTEGER (Given and returned)
*        Global status - 0 = ok
*                        1 = Best to access as character string, didn't
*                            get any values.
*                        Anything else-a worse error!
*
* History:
*   T.N.Wilkins Manchester 12/5/87
*   TNW Introduction of FILNAM argument, 7/7/88
*   TNW (IOA) 13/7/89
*   TNW: 8-FEB-1994, Return bad status if no items read.
*   A.J.Holloway Manchester 4/12/97
*   AJH set name to be 8 chars.
*   AJH using HDS rather than dta_rdvar 16/3/98
*-
      implicit none
      include 'SAE_PAR'
      include 'DAT_PAR'
      include 'DAT_ERR'
      include 'CMP_ERR'
      CHARACTER*(DAT__SZLOC) NLOC, mloc
      integer diminf,status,nfnd
      real inf(diminf)
      character*(*) filnam
      character*8 name
      logical fits ,exist
      integer dims(3),ndim,size,i,icodes(5),next,strlen
      character*80 string,iobs*5,curnam,type*20,access*1
      character*80 string2
      data iobs/'.OBS.'/

* If status>0, then take it as required number of values, i.e. number
* of values found must be at least the value of status on input

      status = SAI__OK

* find out if in .FITS or .OBS structure

      call dsa_seek_fits(filnam,name,exist,access,size,strlen,status)
      fits = (status.eq.SAI__OK).and.exist
      if(fits) then
         size = min(size,diminf)
         if(access.eq.'C') then
            call dsa_get_fits_c(filnam,name,0,string,string2,status)
            type = 'CHAR'
         else
            type = access
            do i = 1, size
               call dsa_get_fits_f(filnam,name,i,inf(i),string2,status)
            end do
         end if
      else
         status = SAI__OK
         curnam = filnam//iobs//name

* Old dta call
*         call dta_tyvar(curnam,type,status)

         call dta_loc(curnam,mloc,status)
         call dat_type(mloc,type,status)
         call dta_annul(mloc,status)


*         call cmp_type(mloc, curnam, type, status)

         if(status.ne.SAI__OK) return
         call dta_szvar(curnam,3,ndim,dims,status)
         size = 1
         do i = 1, ndim
            size = size*dims(i)
         end do

* See if of type character

         if(type.eq.'CHAR') then
            size = min(size,80)
* Switch dta for HDS call
*            call dta_rdvarc(curnam,size,string,status)

            call dta_loc(curnam,nloc,status)
            call dat_get0c(nloc,string,status)
            call dta_annul(nloc,status)


*            CALL CMP_GET0C(NLOC, curnam,string,STATUS)
         else

* Assume of type real, HDS will see to any conversion.

            size = min(size,diminf)
            call dta_rdvarf(curnam,size,inf,status)
         end if
      end if
      nfnd = size
      if(type.eq.'CHAR') then
         size = min(diminf,5)
         call ich_numgt(string,1,' /,;:L','L',size,inf,icodes,next)

*  If any number found set status to 0 (icodes will be 0 for a value
* found)

         do i = size,1,-1
            if(icodes(i).ne.0) nfnd = i - 1
         end do
         if(nfnd.le.0) status = 1
      end if
      do i = size+1,diminf
         inf(i) = 0.0
      enddo
      end
