      subroutine vig( STATUS )
*+
* Name:
*    VIG

* Invocation:
*    CALL VIG( STATUS )
*
* Purpose:
*   Correct for vignetting.

* Description:
*   This routine accepts a sky or flat field IMAGE obtained at
*   the same filter for imaging , or at the same grating angle as a
*   series of spectra which need to be corrected for vignetting
*   in 2D.
*   Two orthogonal one dimensional cuts through the 2D
*   IMAGE are formed and fitted with Chebyshev polynomials to form
*   flattening functions on the respective axes. The correction
*   applied to the data is formed from the product of the terms of
*   each of these two series.
*   Because the correction IMAGES may include unwanted signals, such as
*   sky lines the users can specify regions of the data which may
*   be excluded from the fits. In practice rather actaully restructure
*   the template cuts formed in the two directions,by deleteing these
*   data this is achieved by ascribing them a very low weight in
*   the fitting process. At the current time it has been found that a
*   wright of 1E-6 is an effective way of elimanting such points.

* Parameters:
*    IMAGE = FILE (Read)
*        Name of image for input
*    OLD = LOGICAL (Read)
*        old coefficients are to be used for correction
*    OUTPUT = FILE (Write)
*        OUTput Name of output file
*            OUTPUT is the name of the resulting image. If OUTPUT is the
*            same as INPUT the data in the input file will be modified in
*            situ.Otherwise a new file will be created.
*    YSTART = INTEGER (Read)
*        start value to extract in channel direction
*            The data between the limits ystart and yend is extracted
*            and the resultant spectrum is used to find the vignetting in
*            the channel direction.
*    YEND = INTEGER (Read)
*        end value to extract in channel direction
*            The data between the limits ystart and yend is extracted
*            and the resultant spectrum is used to find the vignetting in
*            the channel direction.
*    XSTART = INTEGER (Read)
*        start value to extract in x-sect direction
*            The data between the limits xstart and xend is extracted
*            and the resultant spectrum is used to find the vignetting in
*            the cross-section direction.
*    XEND = INTEGER (Read)
*        end value to extract in x-sect direction
*            The data between the limits xstart and xend is extracted
*            and the resultant spectrum is used to find the vignetting in
*            the cross-section direction.

* History:
*  Bug fix 6,20/2/89 TNW/Cambridge
*  Comments added 23/2/89 DJA/MAnchester
*  AJH Changed dsa_map access mode from 'r' to 'READ' for FDA
*-
      implicit none

* image dimensions for axes 1 and 2

      integer dims(2)

* number of dimensions in data

      integer ndim

* axis size on "i" axis = DIMS(2)

      integer ni

* axis size on "L" axis = DIMS(1)

      integer nl

* Total number of pixels in the IMAGE

      integer nelm

* TEMPLATE array to be fitted  "I" axis

      integer inti

* TEMPLATE array to be fitted  "L" axis

      integer intl

* fit limirs on "I" axis

      double precision xlimi(2)

* fit limits on "L" axis

      double precision xliml(2)

* Control of application of Old correction

      logical old

* Chebyshev Polynomial


* maximum allowed order of polynomial

      integer max_ord

* set at 20 which should be enough

      parameter (max_ord = 20)

* coefficeints in the "i" direction

      double precision coeffi(max_ord)

* Coefficients  in the "L" direcyition

      double precision coeffl(max_ord)

* Variable equvalent to PARAMETER masx_ord

      integer mord

* Order to be used for "L" direction

      integer kp1l

* Order to be used for "i" direction

      integer kp1i

* work space handling


* pointer to the IMAGE.Z.DATA

      integer iptr

* pointer to "I" axis infomation

      integer iposptr

* pointer to "L" axis infomation

      integer lposptr

* pointers to VM

      integer ptr1,ptr2,ptr3,ptr4,ptr5

* amount of work space to grab

      integer nels

* FIGARO handles to VM

      integer slot

* bytes required for r*8 array length nl

      integer enl

* bytes required for r*8 array length ni

      integer eni

* MAximum of NL and NI

      integer max_size

*                            [ simplifies work space  Handling ]



* pointer to WEIGHTS for "L" axis

      integer wl

* pointer to WEIGHTS array for "I" axis

      integer wi

* Status variables

      integer status
      integer lu
      integer iostatus

* Plot Labelling

      character*41 chars

* Plot Label "I" direction

      character*30 labeli

* Plot Label "L" direction

      character*30 labell
      character*30 axlabi
      character*30 axlabl


      integer mwork
      parameter (mwork=20)
      real tram1(mwork),tram2(mwork)

* User I/O


* prompt for weighting definition

      character*25 prompt

* FIGARO Dynamic Memory Handling Facilities


* FIGARO VM handling and Mapping

      integer dyn_element
      include 'DYNAMIC_MEMORY'
      include 'SAE_PAR'
      include 'PRM_PAR'

* set up character prompts and Labels

      data prompt,axlabl,labell,axlabi,labeli/
     :   'put tramlines around data','channels',
     :   'channel direction cut','x-sections','x-sec direction cut'/
*  ---------------------------------------------------------------------
* initialize the max cheby order and the status variables for Faults
* and the DSA_routines
      mord   = max_ord
      status = SAI__OK
*
*   Get name of input file
*
      call dsa_open(status)
      call dsa_input('image','image',status)
*
*     Get dimensions of input data
*
      call dsa_data_size('image', 2 , ndim, dims, nelm, status)
      nl = dims(1)
      ni = dims(2)
*
*  Get name of output file
*
      call dsa_output('output','output','image',0,0,status)
*
*  Get the parameter and/or the keyword
*
      call par_rdkey('old',.false.,old)
*
*  Map the data and the axis information
*
      call dsa_map_data('output','UPDATE','float',iptr,slot,status)
      iptr = dyn_element(iptr)

      call dsa_map_axis_data('output',1,'READ','double',lposptr,slot,
     :   status)
      lposptr = dyn_element(lposptr)

      call dsa_map_axis_data('output',2,'READ','double',iposptr,slot,
     :   status)
      iposptr = dyn_element(iposptr)

*
*  Get workspace:
*   PTR1   Max_Size (d)
*   INTI   NI  (d)  = PTR2   NI  (d)
*   INTL   NL  (d)  = PTR3   NL  (d)
*   WL     NL  (d)           WEIGHTS "L" axis
*   WI     NI  (d)           WEIGHTS "I" axis
*   PTR4   400 (d)
*   PTR5   (NL or NI)*3+60 (d)

      eni         = val__nbd*ni
      enl         = val__nbd*nl
      max_size    = max(nl,ni)
      nels        = (4 * (ni+nl)+ 460 + max_size*2)

      call getwork(nels,'double',ptr1,slot,status)

* if failed to get WORKSPACE then abort

      if(status.ne.SAI__OK) goto 500

      ptr2 = ptr1 + max_size*val__nbd
      ptr3 = ptr2 + eni
      wl   = ptr3 + enl
      wi   = wl + enl
      ptr4 = wi + eni
      ptr5 = ptr4 + 400*val__nbd
      inti = ptr2
      intl = ptr3
*
*  Check for presence of 'old' keyword. if there then pick up
*  stored variables and do the interpolation using them

      if(old) then
        call par_wruser(
     :         'Correction will be based on existing coefficients'
     :                               ,iostatus)

        call dsa_open_text_file('vig.cor',' ','old',.false.,lu,chars,
     :            status)

        if(status.ne.SAI__OK) goto 500
        read(lu,*)xliml,xlimi,coeffi,coeffl,ni,nl,mord,kp1i,kp1l
        call dsa_free_lu(lu,status)
      else
*
*   Get the user to define the regions that need to be co-added
* along the "I" and "L" axes to define 1D arrays which we
* will fit with polynomials and thereby create the corrections
*
        call find_vig(dynamic_mem(iptr),ni,nl,dynamic_mem(inti),
     :     dynamic_mem(intl))
*
* open graphics
*
        call gr_soft(status)

* plot the template data for the "L" direction and ask
* the user to define regions which should be excluded from
* the fit.

        call plot_data(dynamic_mem(lposptr),dynamic_mem(intl),nl,
     :     axlabl,labell,dynamic_mem(wl),0,' ')

        call weight_fit(0.0,nl,dynamic_mem(wl),.false.)

        call reject_data(dynamic_mem(lposptr),nl,dynamic_mem(wl),axlabl
     :            ,'data',prompt,tram1,tram2,mwork)

* plot the template data for the "I" direction and ask
* the user to define regions which should be excluded from
* the fit.

        call pgpage
        call plot_data(dynamic_mem(iposptr),dynamic_mem(inti),ni,
     :     axlabi,labeli,dynamic_mem(wi),0,' ')

        call weight_fit(0.0,ni,dynamic_mem(wi),.false.)

        call reject_data(dynamic_mem(iposptr),ni,dynamic_mem(wi),axlabi
     :            ,'data',prompt,tram1,tram2,mwork)
*
*   Fit vignetting  in the "I" direction
*
        chars = 'working on '//labeli
        call par_wruser(chars,status)

        call contrl_cpoly2(dynamic_mem(iposptr),dynamic_mem(inti),ni,
     :       dynamic_mem(wi),coeffi,mord,kp1i,axlabi,labeli,1
     :       ,dynamic_mem(ptr4),.true.,.true.,dynamic_mem(ptr5))

        call par_wruser('Type <return> for next plot',status)
        call par_rduser(chars,status)

* clear the graphics screen

        call pgpage

* and calculate the correction for the "L" direction

        chars = 'working on '//labell
        call par_wruser(chars,status)

        call contrl_cpoly2(dynamic_mem(lposptr),dynamic_mem(intl),nl,
     :       dynamic_mem(wl),coeffl,mord,kp1l,axlabl,labell,1
     :       ,dynamic_mem(ptr4),.true.,.true.,dynamic_mem(ptr5))

* define the fit limits in the "L" and "I" directions
* these are important as the Chebyshev polynomials are normalized
* the the ranges given by these data.

        xliml(1) = 1
        xliml(2) = nl
        xlimi(1) = 1
        xlimi(2) = ni

* record the ansers on the FILe VIG.COR for future use.

        call dsa_open_text_file('vig.cor',' ','new',.true.,lu,chars,
     :            status)
        write(lu,*)xliml,xlimi,coeffi,coeffl,ni,nl,mord,kp1i,kp1l
        call dsa_free_lu(lu,status)

      end if

* get work space for the correction

      ptr2 = ptr1 + 8*max_size
      ptr3 = ptr2 + eni

* apply the correction to the data

      call correct(dynamic_mem(iptr),coeffi,coeffl,ni,nl,mord,kp1i,kp1l
     :      ,dynamic_mem(ptr1),dynamic_mem(ptr2),dynamic_mem(ptr3),
     :      max_size)

* close down graphics and DSA

      call clgrap
  500 continue
      call dsa_close(status)
      end
