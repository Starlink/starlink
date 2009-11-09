      subroutine read_defaults(status)
*+
* Name:
*    READ_DEFAULTS

* Invocation:
*    CALL READ_DEFAULTS(STATUS)
* Purpose:
*  To read in default settings for TWODSPEC.
*
* Description:
*  To read in default settings for TWODSPEC.
*
* Arguments:
*      STATUS = INTEGER (Given and returned)
*        Error status, 0=ok
* Global variables:
*        TOLERANCE() = REAL ARRAY (Given)
*        (include file arc_dims)
*        CONDHAND = LOGICAL (Given)
*        (include file opt_cmn)
*        STEPRAT = REAL (Given)
*        (include file opt_cmn)
*        USEPEAK = LOGICAL (Given)
*        (include file opt_cmn)
* Variables
*        CONTEXT = INTEGER 
*          From find_file
* Subroutines/functions referenced:
*
* Author:
*   T.N.Wilkins, Cambridge until 9/92, then Durham
*   A.J.Holloway, Manchester
*   T.D.C.Ash, RAL, Starlink
*   A.C. Davenhall, Starlink, Edinburgh.
*   Malcolm J Currie, RAL, Starlink
*
* History:
*   TNW: 4-OCT-1990 Original version
*   TNW: 26-JAN-2-FEB-1994 Add bits for FITS keywords etc.
*   AJH: 15-DEC-1997 Fixed call to find_file - how it worked 
*        before I do not know. Included CONTEXT variable and
*        initialising before use to zero
*   AJH: 18-DEC-1997 Revert to using .dst
*   AJH: 10-MAR-1998 Remove dta_rdvarc, replace with HDS 
*        lib call
*   TDCA:25-JUN-1999 Changed enviroment variable in 
*        PSX_GETENV call to FIG_DIR
*   ACD: Fixed call to DAT_GET1C to prevent passing a scalar argument
*        where a vector was required and also fixed accessing a DO LOOP
*        variable outside its loop, which caused array bounds to be
*        exceeded.
*   ACD: 28/9/00 Remove local unused variables.
*   MJC: 2004 July 23 Initialised READVARS array.
*-
      implicit none
      integer ival
      integer context
      character*100 name
      integer status,pstat,i,j,ndim,dims(2)
C      integer len1
C      integer find_file
      integer spacepos
      include 'SAE_PAR'
      include 'arc_dims'
      include 'opt_cmn'
      include 'DAT_PAR'
      include 'DAT_ERR'
      include 'CMP_ERR'
      CHARACTER*(DAT__SZLOC) NLOC
      character*8 fits_twodnames(NFITSTAB)
      character readvar(NFITSVAR)*8
      integer elem
      character*100 binpath,newpath
      data fits_twodnames/'DEC','RA','EQUINOX','UT','TELESCOP'
     :     ,'LONG_OBS','LAT_OBS'/
*

      status = 0
      context = 0

      CALL PSX_GETENV( 'FIG_DIR', BINPATH, STATUS)
      
      i=1
 
      do while(binpath(i:i).NE.' ') 
       i=i+1
      enddo

      spacepos=i-1

* Old .dst version
*      newpath = binpath(1:spacepos)//'/twodspec_defaults.dst'
      newpath = binpath(1:spacepos)//'/twodspec_defaults'


*     Returns 1 if o.k.            
*      status = find_file(newpath,name,context)
*      i=1
*      do while(name(i:i).NE.' ') 
*       i=i+1
*      enddo
*      spacepos=i-1-4

      name=newpath
      status = 0
      call dsa_named_input('defaults',name,status)
*      call dsa_named_input('defaults',name(1:spacepos),status)

      if(status.ne.SAI__OK) then
        call par_wruser('Error opening defaults file',pstat)
        return
      endif

*  Do we use FIT_HANDLER (yes if value is 1)?

      if(status.ne.SAI__OK) return
      condhand = ival.eq.1

*  Default value for usepeak

      call dta_rdvari('defaults.more.figaro.usepeak',1,ival,status)
      if(status.ne.SAI__OK) return
      usepeak = ival.eq.1

*  Size of step in optimisation

      call dta_rdvarf('defaults.more.figaro.opt_step',1,steprat,status)
      if(status.ne.SAI__OK) return

*  Tolerance defaults

      call dta_rdvarf('defaults.more.figaro.tols',maxtol,tolerance
     :     ,status)
      if(status.ne.SAI__OK) return

      call dta_rdvari('defaults.more.figaro.relativistic',1,ival
     :           ,status)
      if(status.ne.SAI__OK) then
         status = SAI__OK
         ifrelativistic = .true.
      else
         ifrelativistic = ival.eq.1
      endif


      do i = 1, NFITSTAB
         do j = 1, NFITSVAR
            call chr_fill(' ',fits_unames(j,i))
            call chr_fill(' ',READVAR(j))
         enddo
         call dta_szvar('defaults.more.figaro.'//fits_twodnames(i),
     :        2,ndim,dims,status)

         if(status.eq.SAI__OK) then
            dims(2) = min(dims(2),NFITSVAR)
            
*     call chr_fill(' ',name)
*     len1 = 0
*     call chr_putc('defaults.',name,len1)
*     call chr_appnd(fits_twodnames(i),name,len1)
*     call chr_putc('[1,',name,len1)
*     call chr_puti(j,name,len1)
*     call chr_putc(']',name,len1)
*     call dta_rdvarc(name,8,fits_unames(j,i),status)
*     enddo
            
            call dta_loc('defaults.more.figaro.'//
     :           fits_twodnames(i),nloc,status)


            
*            call dat_get0c(nloc,readvar,status)
            call dat_get1c(nloc,NFITSVAR,readvar,elem,status)
            call dta_annul(nloc,status)

*            print2999, elem
* 2999       format(1x, 'elem ', i4)

            if(status.eq.SAI__OK) then
               do j = 1, NFITSVAR
*                  print3000, j,i, READVAR(j)
* 3000             format(1x, 'j,i, READVAR(j): ', i5, i5, a8)
                  fits_unames(j,i)=READVAR(j)
               end do

               call dta_rdvarf(
     :     'defaults.more.figaro.cor_'//fits_twodnames(i),dims(2
     :              ),fits_corrtn(1,i),status)
               if(status.ne.SAI__OK) then

*       We'll assume that the element isn't present, and take a default
*       of 1

                  status = SAI__OK
                  do j = 1, dims(2)
                     fits_corrtn(j,i) = 1.0
                  enddo
               endif
            endif
         endif
         if(status.ne.SAI__OK) then
            status = SAI__OK
            fits_unames(1,i) = fits_twodnames(i)
            fits_corrtn(1,i) = 1.0
         endif
      enddo
      call dsa_close_structure('defaults',status)
      end

