      subroutine clone_it(clopen,ifcomb,status)
*+
* Name:
*    CLONE_IT

* Invocation:
*    CALL CLONE_IT(CLOPEN,IFCOMB,STATUS)

* Purpose:
*   Setup for cloning

* Description:
*   Assign and map the pointers to the data and results files
*   for the case of cloned data
*
* Arguments:
*     COPY = LOGICAL (Given)
*        if to copy entire .RES structure wavelength values
*                 using current fit (passed in arc_dims)
*     IFCOMB = LOGICAL (Given)
*        If called from COMB
*     STATUS = INTEGER (Given and returned)
*        Error status, 0=okay
*     CLOPEN = LOGICAL (Returned)
*        if clone file open
*
* History:
*  Tidied T.N.Wilkins Manchester 6/7/88
*  TNW 10/10/88 IFCOMB added to call
*  TNW 13/10/88 DSA Version primary
*  TNW 18/10/88 More use of DSA, and use of ACCRES
*  TNW 4/11/88 Minor changes
*  TNW 20/6/89 IOA Minor changes
*  TNW 2/1/91 Now uses DSA_OPEN and DSA_SAME_DATA, rather than
*  PAR_RDCHAR and DSA_NAMED_INPUT. Also GET_DATE used.
*  TNW 8/91 Changes for new results structure
*  A.J. Holloway, Manchester. Remove dta_cyvar
*  ACD: 28/9/00 Remove local unused variables.

*- --------------------------------------------------------------------
      implicit none
      include 'SAE_PAR'
      include 'DAT_PAR'
      include 'DAT_ERR'
      include 'CMP_ERR'
      CHARACTER*(DAT__SZLOC) tloc, cloc, dloc, chars1p
      logical ifcomb
      integer ncomp
*
*  integer
*

* dimensions of .DST structures

      integer dims(4)
      integer ndim
      integer nli
      logical clopen
      integer pstat
      character*72 chars,chars1
      character*32 tname
      integer len1
      integer i

* external references

      logical dsa_same_data
      integer status
      include 'arc_dims'
* --------------------------------------------------------------

      clopen = .false.

*   Get name of the file to be cloned and open it

      call dsa_input('clone','clfile',status)
      if(dsa_same_data('data','clone',status)) then
        call par_wruser('Error: Clone file is input image!',pstat)
        status = SAI__ERROR
      end if

*
*  Get dimensions of clone data and check if the Crossection
*  dimensions are the same as those of IMAGE. Actually we check
*  the results array, which means it also works for COMB. This is only
*  needed for COPY mode.
*
      call accres('clone','results','fi',0,0,' ',status)
      ndim = 4
      call accres(' ','data_array','si',ndim,dims,' ',status)
      if(status.ne.SAI__OK) then
        return
      end if
      clopen=.true.
      if(ifcomb) then
        nli = wavdim
      else
        nli = spdim1
      end if
      if(copy) then
        if(dims(3).ne.nli) then
          len1 = 0
          call chr_putc('CLONE and IMAGE have different ',chars,len1)
          if(ifcomb) then
            call chr_putc('1st (X)',chars,len1)
          else
            call chr_putc('2nd (Y)',chars,len1)
          end if
          call chr_putc(' dimensions',chars,len1)
          call par_wruser(chars(:len1),pstat)
          call par_wruser('For COPY mode these must be the same',pstat)
          status = SAI__ERROR
        else
          call dsa_specific_structure('data','results','w',chars1,
     :            status)
          call dsa_specific_structure('clone','results','r',chars,
     :            status)
          if(status.ne.SAI__OK) return

* Old DTA CALL
*          call dta_cyvar(chars,chars1,status)

          call dta_loc(chars1,dloc,status)
          call dta_loc(chars,cloc,status)


          call DAT_PAREN(dloc, chars1p, status)


          call dat_ncomp(cloc, ncomp, status)
          do 1 i=1, ncomp
             call dat_index(cloc, i, tloc, status)
             call dat_name(tloc, tname, status)
             call dat_copy(tloc, chars1p, 'results', status)
             call dat_annul(tloc, status)
 1        continue

          call dat_annul(chars1p, status)
          call dat_annul(cloc, status)
          call dat_annul(dloc, status)

*     call DAT_COPY(chars,chars1p,'results',status)


* Replaced with HDS library DAT_ call for this data


          if(status.ne.SAI__OK) then
            call tnw_dtaerr(status,'copying',chars)
            return
          end if
        end if
      end if
*
*   Print out some diagnostics
*
      len1 = 0
      call chr_putc(' Output spectrum : ',chars,len1)
      call chr_appnd(datafile,chars,len1)
      call par_wruser(chars(:len1),pstat)

      len1 = 0
      call chr_putc(' Dimensions      : ',chars,len1)
      call chr_puti(wavdim,chars,len1)
      call chr_putc(',',chars,len1)
      call chr_puti(spdim1,chars,len1)
      if(spdim2.gt.1) then
        call chr_putc(',',chars,len1)
        call chr_puti(spdim2,chars,len1)
      end if
      call par_wruser(chars(:len1),pstat)

      len1 = 0
      call chr_putc(' Mask Threshold  : ',chars,len1)
      call chr_puti(int(iteration),chars,len1)
      call par_wruser(chars(:len1),pstat)
*
*   Get date and time
*
      call get_date(chars1)

      len1 = 0
      call chr_putc(' Date            : ',chars,len1)
      call chr_appnd(chars1,chars,len1)
      call par_wruser(chars(:len1),pstat)

*  Point now to current results structure

      call accres('data','results','fi',0,0,' ',status)
      end
