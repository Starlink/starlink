      subroutine two_open(parnam,maxdim,status)
*+
* Name:
*    TWO_OPEN

* Invocation:
*    CALL TWO_OPEN(PARNAM,MAXDIM,STATUS)
* Purpose:
*    Open data file

* Description:
*  To open DSA and a data file, and return the name of the data file
*  (less directory name) and its dimensions.

* Arguments:
*      PARNAM = CHARACTER*(*) (Given)
*        Name of parameter for data file
*      MAXDIM = INTEGER (Given)
*        Maximum number of dimensions for data (up to 3)
*      STATUS = INTEGER (Given and returned)
*        Error status, 0=okay
*      DATAFILE = CHARACTER*(*) (Returned)
*        Data file name (in common)
* Global variables:
*   SPDIM1 = INTEGER (Returned)
*        Number of cross-sections (include file arcdims)
*   WAVDIM = INTEGER (Returned)
*        Number of channels (include file arcdims)
*   NEWX = LOGICAL (Returned)
*        If a new X structure created (include file arcdims)
*   NELM = INTEGER (Returned)
*        Total size of input image (SPDIM1*WAVDIM) (include file arcdims)
* Subroutines/functions referenced:
*      DSA_...   : Access data file

* History:
*   T.N.Wilkins, Cambridge, 22-23-NOV-1990, mostly copying from
*                             GET_SPECTRUM
*       "            "      17-APR-1991 Output info to terminal etc.
*       "        Durham     19-MAR-1993 Remove directory from file name
*                            on UNIX
*  A.J.Holloway, Manchester 9/6/98 Changed dsa_input to dsa_input_update
*                                  for FDA version.
*-
      implicit none
      integer status
      include 'SAE_PAR'
      include 'arc_dims'
      character*(*) parnam
      integer maxdim

*

      integer len1,ind,i,chr_len,dims(3),specdim,pstat,len2
      character*30 work(2)
      character*79 chars,chars2
      double precision dummy
      integer istatus

*   Open dsa system

      call dsa_open(status)
*
*   Open input spectrum, and get its name
*
      call dsa_input_update('data',parnam,status)

      call dsa_get_actual_name('data',datafile,status)
      if(status.ne.SAI__OK) return
      call chr_clean(datafile)

* Vax-specific, remove directory from file name

      ind = index(datafile,']')
      if(ind.eq.0) ind = index(datafile,':')
      if(ind.ne.0) then
        len1 = chr_len(datafile)
        do i = 1, len1 - ind
          datafile(i:i) = datafile(i+ind:i+ind)
        end do
        call chr_fill(' ',datafile((len1-ind+1):))
      end if

* Same for UNIX

      ind = index(datafile,'/')
      do while(ind.ne.0)
        len1 = chr_len(datafile)
        do i = 1, len1 - ind
          datafile(i:i) = datafile(i+ind:i+ind)
        end do
        call chr_fill(' ',datafile((len1-ind+1):))
        ind = index(datafile,'/')
      end do
*
*  Get dimensions of input data
*
      call dsa_data_size('data',maxdim,specdim,dims,nelm,status)
      if(status.ne.SAI__OK) return

* Save the number of channels and crossections in the data, return via
* common

      if (specdim .lt. 2) then
        spdim1 = 1
      else
        spdim1 = dims(2)
      end if
      if (specdim.lt.3) then
        spdim2 = 1
      else
        spdim2 = dims(3)
      end if
      wavdim = dims(1)

*  Now output some basic information about the file to the terminal

      len1 = 0
      call chr_fill(' ',chars)

*  Format name of file

      call chr_appnd(datafile,chars,len1)

*  ...and dimensions

      call chr_putc('[',chars,len1)
      do i = 1, specdim
        call chr_puti(dims(i),chars,len1)
        if(i.ne.specdim) call chr_putc(',',chars,len1)
      end do
      call chr_putc(']',chars,len1)

*  Now for the title

      call dsa_object_name('data',chars2,status)
      len2 = chr_len(chars2)
      if((len1+len2).lt.79) then
        len1 = len1 + 1
        call chr_appnd(chars2,chars,len1)
        call par_wruser(chars(:len1),pstat)
      else

*   Since len1<80, we can assume len2>0

        call par_wruser(chars(:len1),pstat)
        call par_wruser(chars2(:len2),pstat)
      end if

* enquire for X Units

      call dsa_get_axis_info('data',1,2,work,1,dummy,status)
      xlabel = work(2)
      xunits = work(1)
      call chr_clean(xlabel)
      call chr_clean(xunits)

*  Get Z data information (units and label)

      call dsa_get_data_info('data',2,work,1,dummy,status)
      zlabel = work(2)
      zunits = work(1)
      call chr_clean(zlabel)
      call chr_clean(zunits)

*  See if any results structure is present

      istatus = SAI__OK
      call accres('data','results','fi',2,0,' ',istatus)
      if(istatus.eq.SAI__OK) call par_wruser(
     :     'Results structure present',pstat)
      istatus = SAI__OK
      call accres('data','res','fi',2,0,' ',istatus)
      if(istatus.eq.SAI__OK) call par_wruser(
     :     'Old results structure present',pstat)
      istatus = SAI__OK
      call accres('data','fib','fi',2,0,' ',istatus)
      if(istatus.eq.SAI__OK) call par_wruser(
     :     'Old fibre results structure present',pstat)

      end
