      SUBROUTINE PLOTS
C+
C
C   ---------
C   P L O T S
C   ---------
C
C   Description
C   -----------
C   Displays spectra extracted from the first (spectral) dimension of a
C   ZXY-sorted 3-D image.
C
C
C   Scope of program
C   ----------------
C   - The image from which spectra are extracted must be 3-D and the
C     image used as a map must be 2-D.
C   - Data array types SHORT and FLOAT supported; others converted to FLOAT.
C   - Subsetting supported for the 2-D image.
C   - Magic values supported for the 3-D image.
C   - Quality and variance arrays not supported.
C   - Batch execution not supported.
C
C
C   Environment
C   -----------
C   FIGARO
C
C
C   Parameters (read or written)
C   ----------------------------
C
C   The following are prompted for in the preamble before the main menu:
C
C   IMAGE     Name of the structure containing the 3-D image from which
C             spectra are extracted. (character)(prompted for).
C
C   IMAGE1    Name of the structure containing the 2-D image to be used as
C             a map. (character)(prompted for).
C
C   SLOW      Lowest value which will be plotted in the spectra extracted
C             from the 3-D image (real)(prompted for).
C
C   SHIGH     Highest value which will be plotted in the spectra extracted
C             from the 3-D image (real)(prompted for).
C
C   START     Start coordinate in each dimension of the 2-D image subset to
C             be displayed. (real, array)(prompted for).
C
C   END       End coordinate in each dimension of the 2-D image subset to
C             be displayed. (real, array)(prompted for).
C
C   LOW       Value in the 2-D compacted image which is plotted in the
C             lowest colour index or as black. (real)(prompted for).
C
C   HIGH      Value in the 2-D compacted image which is plotted in the
C             highest colour index or as white. (real)(prompted for).
C
C   TABLE     Name of the lookup table. (character)(prompted for).
C
C   SLABEL    Text string placed below the spectrum plots. (character)
C             (prompted for).
C
C   The following are prompted for when main menu options are selected:
C
C   MAG       Magnification of the image plot relative to the full display
C             surface. (real)(prompted for).
C
C   XPIX      Pixel number in the X dimension of each point of extraction.
C             (integer, array)(prompted for).
C
C   YPIX      Pixel number in the Y dimension of each point of extraction.
C             (integer, array)(prompted for).
C
C   BIN       Binning interval used for averaging the spectra within a
C             defined rectangle. (integer)(prompted for).
C
C   SPECTRUM  Name of the structure to which an extracted spectrum is to be
C             written. (character)(prompted for).
C
C   SOFTDEV   Current screen device name. (character)(read from file).
C
C   HARDDEV   Current hardcopy device name. (character)(read from file).
C
C
C   Keywords
C   --------
C   SCALE     Instructs the program to scale all the plots of extracted
C             spectra according to a supplied data range. Otherwise, each
C             plot will be scaled according to its own range.
C
C   WHOLE     Instructs the program to display the whole 2-D image.
C             Otherwise, a subset of the image may be selected.
C
C   AXES      Instructs the program to plot axes and a label for each
C             spectrum. Otherwise, each spectrum will appear in a plain box.
C
C   WRITE     Instructs the program to write each extracted spectrum to a
C             data structure.
C
C
C   Propagation of data structure
C   -----------------------------
C   Not relevant.
C
C
C   Method
C   ------
C   - A 2-D image, which is usually produced by compacting the 3-D image in
C     the Z direction (although any image with the same X and Y dimensions
C     as the 3-D image may be used), or a subset thereof, is plotted in
C     colour or grey scale, depending on TABLE, on the current SOFT device.
C     The data range is selected via LOW and HIGH. The magnification of the
C     plot relative to the full display surface may be selected via MAG.
C   - The image plot is used as a map for selecting the locations of the
C     spectra to be extracted from the ZXY-sorted 3-D image. Two cursor
C     points are indicated on the plot, at the diagonal corners of a
C     rectangle. The points may also be entered through the keyboard. The
C     coordinates of the points are used to address the second and third
C     dimensions of the 3-D image, so that spectra are extracted within
C     the rectangle, being averaged at the binning interval BIN. The average
C     spectrum in one bin is displayed as one plot. If SCALE is true, all
C     the spectra are plotted at the same vertical scale, defined by SLOW
C     and SHIGH. Otherwise each is scaled according to its own data range.
C   - The spectrum plots are all displayed together in a rectangular array,
C     using the multiple virtual device feature of PGPLOT. The number of
C     plots in each dimension of the display is determined by the size of
C     the rectangle selected and by BIN. If a magic value pixel is found in
C     a spectrum, the line string is broken at that point. If the keyword
C     WRITE is true, each spectrum is written to a data structure named via
C     SPECTRUM and the current plot number.
C   - A hard copy of the spectrum plots may be produced. The spectra defined
C     by the most recent pair of points or single point are plotted on the
C     current HARD device.
C
C
C   External functions & subroutines called
C   ---------------------------------------
C   Library DSA:
C     DSA_CLOSE
C     DSA_CLOSE_STRUCTURE
C     DSA_CREATE_STRUCTURE
C     DSA_DATA_SIZE
C     DSA_GET_RANGE
C     DSA_GET_WORKSPACE
C     DSA_INPUT
C     DSA_MAP_AXIS_DATA
C     DSA_MAP_DATA
C     DSA_MAP_QUALITY
C     DSA_OPEN
C     DSA_SEEK_RANGE
C     DSA_SPECIFIC_STRUCTURE
C     DSA_TYPESIZE
C     DSA_UNMAP
C     DSA_USE_FLAGGED_VALUES
C     DSA_USE_QUALITY
C     DSA_WRUSER
C
C   Library DTA:
C     DTA_CRNAM
C     DTA_CRVAR
C     DTA_WRVARC
C     DTA_WRVARI
C
C   Library DYN:
C     DYN_ELEMENT
C
C   Library FIG:
C     FIG_HELP
C
C   Library GEN:
C     GEN_MOVE
C     GEN_RANGEF
C
C   Library ICH:
C     ICH_CI
C     ICH_ENCODE
C     ICH_FOLD
C     ICH_KEY
C     ICH_LEN
C
C   Library NDP:
C     NDP_ADD
C     NDP_AXIS_RANGE
C     NDP_GET_IMAGE_INFO
C     NDP_IMAGE_CURSOR
C     NDP_IMAGE_INDEX
C     NDP_IMAGE_LUT
C     NDP_IMAGE_PLOT
C     NDP_IMAGE_VIEWPORT
C     NDP_MATCH_SIZES
C     NDP_PAR_RDARY
C     NDP_PGBIN
C     NDP_RANGE
C     NDP_SET_BAD_PIXEL
C
C   Library PAR:
C     PAR_CNPAR
C     PAR_RDCHAR
C     PAR_RDKEY
C     PAR_RDUSER
C     PAR_RDVAL
C
C   Library VAR:
C     VAR_GETCHR
C     VAR_GETNUM
C
C   Starlink PGPLOT:
C     PGASK
C     PGADVANCE
C     PGBEGIN
C     PGBOX
C     PGDRAW
C     PGEND
C     PGLABEL
C     PGMOVE
C     PGSLW
C     PGVSIZE
C     PGWINDOW
C
C
C   Internal subroutines called
C   ---------------------------
C   GET_POLYGON_MASK
C   MAGIC_ZAP
C   PLOTS_SELECT
C   PLOTS_SPLOT_W
C   PLOTS_SPLOT_WQ
C   PLOTS_SPLOT_R
C   PLOTS_SPLOT_RQ
C   PLOTS_WRITE
C
C
C   INCLUDE statements
C   ------------------
C   INCLUDE 'DYNAMIC_MEMORY'
C   INCLUDE 'MAGIC_VALUES'
C   INCLUDE 'NUMERIC_RANGES'
C
C
C   Extensions to FORTRAN77
C   -----------------------
C   DO WHILE / END DO / IMPLICIT NONE / INCLUDE / Names > 6 characters
C
C
C   Possible future upgrades
C   ------------------------
C   - Batch version of program, with pixel numbers input and hardcopy
C     output.
C   - Use a build file instead of direct hardcopy. This would make the
C     hardcopy option much faster, but would require major modifications
C     to the DSK_ system common blocks so as to enable multiple plot
C     divisions.
C
C
C   Author/s
C   --------
C   Nick Fuller  RGO  (RGVAD::NMJF or NMJF@UK.AC.RGO.STAR)
C   Jim Lewis    RGO  (CAVAD::JRL or JRL@UK.AC.CAM.AST-STAR)
C   Julian Gold  RGO  (CAVAD::GOLDJIL or GOLDJIL@UK.AC.CAM.AST-STAR)
C
C   History
C   -------
C   01-FEB-1989   - Original program
C   21-MAR-1989   - Corrected bug which produced hardcopy instead of screen
C                   plot.
C   14-JUN-1990   - Parameters in call to NDP_IMAGE_PLOT changed.  Also replaced
C                   NDP_IMAGE_LUT with IMAGE_LUT as a temporary measure since
C                   the former no longer works.  Also fixed arguments in
C                   subroutine calls so that arrays aren't dimensioned with
C                   elements of other array.
C   29-AUG-1990   - New version of NDP_IMAGE_LUT added in. (JRL)
C   18-SEP-1990   - The program now deals with magic values in the 2-d array by
C                   replacing the flagged pixels with a value which is below
C                   the lower threshold for the plot.  To do this a quality
C                   array is used so that it can be remembered which elements
C                   were magic if the user decides to change the plot limits.
C                   Formerly the values were all replaced by zeros which won't
C                   do if there is data with a value less than zero (which
C                   is always possible). The data were also changed straight
C                   into the mapped array, which then changed the values in
C                   the disk file if the array was mapped and not type
C                   converted.  Using a copy of the data array in such a case
C                   solves the problem.  (JRL)
C   20-NOV-1991   - Now accepts polygonal regions to average spectra over.
C                   (GOLDJIL)
C   10-MAR-1992   - PGPLOT common block removed. (GOLDJIL)
C   24-MAR-1992   - The output spectra are now written to sequential files
C                   with names given by appending the spectrum number to
C                   the parameter name. Eg PLOT1.DST, PLOT2.DST etc. The
C                   output contains extra header information to locate the
C                   spectrum:
C                     FILE {
C                       .PLOTS
C                         .X    INT    x-coord of extraction
C                         .Y    INT    y-coord of extraction
C                         .BIN  INT    bin size
C                         .MODE STRING Either 'rectangle' or 'polygon'
C                      .
C                      .
C                      .
C                    }
C                    In addition, a file FILE_DAT.DST is created containing
C                    common information about the extraction, such as the
C                    polygon mask (if polygonal extraction was performed) or
C                    the size of the rectangle (if rectangular extraction was
C                    performed).
C
C                    (GOLDJIL)
C   03-DEC-1992    - Unix version (GOLDJIL)
C   06-OCT-1994    - Removed lots of unused variables. (GJP)
C
C+-----------------------------------------------------------------------------
C
      implicit none
c
c   Functions
c
      integer   dsa_typesize,dyn_element,ich_encode,ich_fold,ich_key,
     &          ich_len,pgbegin
c
c   Local variables
c
      integer   address
      logical   axes
      logical   badpix
      logical   badpix1
      integer   bin
      integer   bytes
      logical   cursor
      logical   default
      character device*16
      integer   dims(10)
      integer   dims1(10)
      integer   dumint
      real      dumreal
      integer*2 dumshort
      integer   elem
      real      end(6)
      integer   endpix(6)
      integer   gfxptr
      integer   gfxslot
      logical   hard
      character harddev*16
      real      high
      integer   i
      logical   image
      integer   imptr
      integer   im1ptr
      integer   iopt
      integer   islot
      integer   i1slot
      real      mag
      integer   maskptr
      integer   maskslot
      real      low
      integer   ndim
      integer   ndim1
      integer   nelm
      integer   nelm1
      integer   next
      logical   points
      logical   polygon
      integer   qptr
      integer   qslot
      logical   range_exist
      character response*10
      character slabel*128
      logical   scale
      logical   sdopen
      real      shigh
      real      slow
      character softdev*16
      integer   spptr
      integer   spslot
      real      sqvp
      integer   stapix(6)
      real      start(6)
      integer   status
      character string*80
      character table*32
      integer   tptr
      integer   tslot
      character type*8
      character type1*8
      integer   valptr
      integer   valslot
      real      vmax
      real      vmin
      logical   write
      integer   wkptr1
      integer   wkptr2
      integer   wkptr3
      integer   wkptr4
      integer   wkslot1
      integer   wkslot2
      integer   wkslot3
      integer   wkslot4
      integer   xptr
      real      xpix(2)
      integer   xslot
      real      ximv(2)
      integer   xyaxes(2)
      integer   yptr
      real      ypix(2)
      integer   yslot
      real      yimv(2)
c
      include 'DYNAMIC_MEMORY'
      include 'MAGIC_VALUES'
      include 'NUMERIC_RANGES'
c
c   Initialize
c
      status=0
      xyaxes(1)=2
      xyaxes(2)=3
      image=.false.
      points=.false.
c
c   Initial settings of option flags
c
      axes=.false.
      write=.false.
      hard=.false.
c
c   Open DSA system
c
      call dsa_open(status)
      if(status.ne.0)go to 500
c
c   Get current PGPLOT screen device
c
      call var_getchr('softdev',0,0,softdev,status)
      if(status.ne.0)then
        call dsa_wruser
     &    ('No screen device selected - use SOFT command.\\N')
        go to 500
      end if
c
c   Get current PGPLOT hardcopy device
c
      call var_getchr('harddev',0,0,harddev,status)
      if(status.ne.0)then
        call dsa_wruser
     &    ('No hardcopy device selected - use HARD command.\\N')
        go to 500
      end if
c
c   Get 3-D image, which must be ZXY-sorted
c   The user must verify this by looking at the output from NDP_GET_IMAGE_INFO
c
      call dsa_input('image','image',status)
      if(status.ne.0)go to 500
      call ndp_get_image_info('image',.true.,.false.,type,badpix,status)
      if(status.ne.0)go to 500
      call dsa_data_size('image',3,ndim,dims,nelm,status)
      if(status.ne.0)go to 500
      if(ndim.lt.3)then
        call dsa_wruser('This is not a 3-D image.\\N')
        go to 500
      end if
c
c   Get 2-D image
c
      call dsa_input('image1','image1',status)
      if(status.ne.0)go to 500
      call ndp_get_image_info
     &  ('image1',.true.,.false.,type1,badpix1,status)
      if(status.ne.0)go to 500
      call dsa_data_size('image1',2,ndim1,dims1,nelm1,status)
      if(status.ne.0)go to 500
      if(ndim1.lt.2)then
        call dsa_wruser('This is not a 2-D image.\\N')
        go to 500
      end if
      do i=1,2
        stapix(i)=1
        endpix(i)=dims1(i)
      end do
c
c   Compare the X and Y dimensions of the two images
c
      call ndp_match_sizes('image1','image',xyaxes,status)
      if(status.ne.0)go to 500
c
c   Magic values are not to be removed from the data arrays.
c
      call dsa_use_flagged_values('image',status)
      if(status.ne.0)go to 500
      call dsa_use_quality('image1',status)
      if(status.ne.0)go to 500
c
c   Map 2-D data array as 'FLOAT' as PGPLOT wants it that way.  Also map
c   the quality array (which doesn't exist, but DSA will create one and
c   fill it with the appropriate values according to which values in the
c   data array are set to magic values).
c
      call dsa_map_data('image1','read','FLOAT',address,tslot,status)
      tptr = dyn_element(address)
      call dsa_map_quality('image1','read','byte',address,qslot,status)
      qptr = dyn_element(address)
      if(status.ne.0)go to 500
c
c   If the 2-d data was originally not FLOAT then just make the pointers equal
c
      if (type1 .ne. 'FLOAT') then
        im1ptr = tptr
        i1slot = tslot
c
c   Otherwise get some workspace and copy the original 2-d data over to it
c
      else
        bytes = nelm1*dsa_typesize('float',status)
        call dsa_get_workspace(bytes,address,i1slot,status)
        im1ptr = dyn_element(address)
        call gen_move(bytes,dynamic_mem(tptr),dynamic_mem(im1ptr))
      end if
c
c   Allocate polygon mask memory
c
      bytes = nelm1*dsa_typesize('byte',status)
      call dsa_get_workspace(bytes,address,maskslot,status)
      if (status .ne. 0) go to 500
      maskptr = dyn_element(address)
c
c   Allocate graphics buffer memory
c
      bytes = nelm1*dsa_typesize('int',status)
      call dsa_get_workspace(bytes,address,gfxslot,status)
      if (status .ne. 0) go to 500
      gfxptr = dyn_element(address)
c
c   Map 3-D data array
c
      if(type.eq.'SHORT')then
        call dsa_map_data('image','read','SHORT',address,islot,status)
      else
        call dsa_map_data('image','read','FLOAT',address,islot,status)
      end if
      if(status.ne.0)go to 500
      imptr=dyn_element(address)
c
c   Map axes of 2-D image
c
      call dsa_map_axis_data
     &  ('image1',1,'read','FLOAT',address,xslot,status)
      if(status.ne.0)go to 500
      xptr=dyn_element(address)
c
      call dsa_map_axis_data
     &  ('image1',2,'read','FLOAT',address,yslot,status)
      if(status.ne.0)go to 500
      yptr=dyn_element(address)
c
c   Map spectral axis of 3-D image
c
      call dsa_map_axis_data
     &  ('image',1,'read','FLOAT',address,spslot,status)
      if(status.ne.0)go to 500
      spptr=dyn_element(address)
c
c   Get workspace for spectra
c
      elem=dsa_typesize('float',status)
      call dsa_get_workspace(dims(1)*elem,address,wkslot1,status)
      if(status.ne.0)go to 500
      wkptr1=dyn_element(address)
      call dsa_get_workspace(dims(1)*elem,address,wkslot2,status)
      if(status.ne.0)go to 500
      wkptr2=dyn_element(address)
      call dsa_get_workspace(dims(1)*elem,address,wkslot3,status)
      if(status.ne.0)go to 500
      wkptr3=dyn_element(address)
      call dsa_get_workspace(dims(1)*elem,address,wkslot4,status)
      if(status.ne.0)go to 500
      wkptr4=dyn_element(address)
c
c   Get workspace for validity array to be used in computing average spectrum
c
      elem=dsa_typesize('short',status)
      call dsa_get_workspace(dims(1)*elem,address,valslot,status)
      if(status.ne.0)go to 500
      valptr=dyn_element(address)
c
c   Ask whether spectrum plots are to be scaled
c
      call par_rdkey('scale',.false.,scale)
c
c   - if so, get 3-D image data range
c
      if(scale)then
        call dsa_seek_range('image',range_exist,status)
        if(status.ne.0)go to 500
        if(range_exist)then
          call dsa_get_range('image',vmin,vmax,status)
          if(status.ne.0)go to 500
        else
          if(type.eq.'SHORT')then
            vmin=real(min_short)
            vmax=real(max_short)
          else
            vmin=min_float
            vmax=max_float
          end if
        end if
        call par_rdval('slow',vmin,vmax,0.0,' ',slow)
        call par_rdval('shigh',vmin,vmax,0.0,' ',shigh)
      end if
c
c   Get 2-D image window
c
      call ndp_axis_range
     &  ('image1',dims1,ndim1,start,end,stapix,endpix,status)
      if(status.ne.0)go to 500
c
c   Get range for plot
c
      if(type.eq.'SHORT')then
        vmin=real(min_short)
        vmax=real(max_short)
      else
        vmin=min_float
        vmax=max_float
      end if
      call par_rdval('low',vmin,vmax,0.0,' ',low)
      call par_rdval('high',vmin,vmax,0.0,' ',high)
c
c   Get name of LUT
c
      call par_rdchar('table',' ',table)
c
c   Get other initial values from parameter file and display
c
      call dsa_wruser(' \\N')
      call dsa_wruser('Values read from parameter file:\\N')
c
      call var_getnum('mag',0,0,mag,status)
      if(mag.eq.0.0)mag=1.0
      string='MAG = '
      dumint=ich_encode(string,mag,7,1,next)
      call dsa_wruser(string(:ich_len(string))//'\\N')
c
      call var_getnum('bin',0,0,dumreal,status)
      bin=int(dumreal)
      if(bin.eq.0)bin=1
      string='BIN = '
      dumint=ich_encode(string,real(bin),7,0,next)
      call dsa_wruser(string(:ich_len(string))//'\\N')
c
c   OPTIONS MENU
c
      do while (iopt.ne.15)
        call dsa_wruser(' \\N')
        call dsa_wruser('$Command (? to list) > \\N')
        call par_rduser(response,status)
        dumint=ich_fold(response)
        iopt=ich_key
     &    (response,1,' ','I:W:R:M:L:C:K:P:B:E:D:S:A:O:H:Q:?:','A',next)
c
c   OPTION 'I': plot 2-D image
c
        if(iopt.eq.1)then
c
c   - advance screen device if already open for image plot, otherwise
c     open it and reload LUT
c
          if(sdopen)then
            call pgadvance
          else
            status=pgbegin(0,softdev,1,1)
            if(status.ne.1)go to 500
            status=0
            call pgask(.false.)
            sdopen=.true.
            call ndp_image_lut(table,status)
            if(status.ne.0)go to 500
          end if
c
c   - replace magic values...
c
          call magic_zap(dynamic_mem(qptr),dynamic_mem(im1ptr),dims1(1),
     &    dims1(2),low-1.0)
c
c   - define viewport for image plot
c
          call ndp_image_viewport(stapix,endpix,mag,'C',ximv,yimv,sqvp)
c
c   - plot image
c
          call ndp_image_index(dims1(1)*dims1(2),low,high,
     &                         dynamic_mem(im1ptr),badpix,
     &                         dynamic_mem(gfxptr),status)
          call ndp_image_plot
     &      (dynamic_mem(gfxptr),dims1(1),dims1(2),stapix,endpix,
     &       start,end,high,low,' ','AR',ximv,yimv)
          image=.true.
c
c   OPTION 'W': change window for 2-D image plot
c
        else if(iopt.eq.2)then
          call ndp_axis_range
     &      ('image1',dims1,ndim1,start,end,stapix,endpix,status)
          if(status.ne.0)go to 500
c
c   OPTION 'R': change data range for 2-D image plot
c
        else if(iopt.eq.3)then
          call par_cnpar('low')
          call par_rdval('low',vmin,vmax,0.0,' ',low)
          call par_cnpar('high')
          call par_rdval('high',vmin,vmax,0.0,' ',high)
c
c   OPTION 'M': change magnification of 2-D image plot
c
        else if(iopt.eq.4)then
          call par_cnpar('mag')
          call par_rdval('mag',0.01,1.0,1.0,' ',mag)
c
c   OPTION 'L': change LUT for 2-D image plot
c
        else if(iopt.eq.5)then
          call par_cnpar('table')
          call par_rdchar('table',' ',table)
c
c   - unset LUT loaded flag and load LUT
c
          call ndp_image_lut(table,status)
          if (status .ne. 0) go to 500
c
c   OPTION 'C','K' or 'P': select points on 2-D image plot
c
        else if(iopt.eq.6 .or. iopt.eq.7 .or. iopt.eq.8)then
          cursor = (iopt .eq. 6)
          polygon = (iopt .eq. 8)
          call plots_select
     &      (dynamic_mem(xptr),dynamic_mem(yptr),dims1(1),dims1(2),
     &       stapix,endpix,start,end,mag,cursor,image,points,xpix,ypix,
     &       dims1(1),dims1(2),polygon,dynamic_mem(maskptr))
c
c   OPTION 'B': change binning interval for summation of spectra
c
        else if(iopt.eq.9)then
          call par_cnpar('bin')
          call par_rdval
     &      ('bin',1.0,real(min(dims1(1),dims1(2))),1.0,' ',dumreal)
          bin=int(dumreal)
c
c   OPTION 'E' or 'H': extract spectra from 3-D image and plot
c
        else if(iopt.eq.10 .or. iopt.eq.15)then
          sdopen=.false.
          if(iopt.eq.10)then
            device=softdev
            hard=.false.
          else
            device=harddev
            hard=.true.
          end if
          if(type.eq.'SHORT')then
            if(.not.badpix)then
              call plots_splot_w
     &          (dynamic_mem(imptr),dynamic_mem(spptr),
     &           dynamic_mem(wkptr1),dynamic_mem(wkptr2),
     &           dynamic_mem(wkptr3),dynamic_mem(wkptr4),
     &           dynamic_mem(valptr),dims(1),dims(2),dims(3),
     &           xpix,ypix,scale,slow,shigh,bin,device,slabel,axes,
     &           write,hard,points,badpix,dumshort,
     &           dims1(1),dims1(2),polygon,dynamic_mem(maskptr),
     &           status)
            else
              call plots_splot_wq
     &          (dynamic_mem(imptr),dynamic_mem(spptr),
     &           dynamic_mem(wkptr1),dynamic_mem(wkptr2),
     &           dynamic_mem(wkptr3),dynamic_mem(wkptr4),
     &           dynamic_mem(valptr),dims(1),dims(2),dims(3),
     &           xpix,ypix,scale,slow,shigh,bin,device,slabel,axes,
     &           write,hard,points,badpix,magic_short,
     &           dims1(1),dims1(2),polygon,dynamic_mem(maskptr),
     &           status)
            end if
          else
            if(.not.badpix)then
              call plots_splot_r
     &          (dynamic_mem(imptr),dynamic_mem(spptr),
     &           dynamic_mem(wkptr1),dynamic_mem(wkptr2),
     &           dynamic_mem(wkptr3),dynamic_mem(wkptr4),
     &           dynamic_mem(valptr),dims(1),dims(2),dims(3),
     &           xpix,ypix,scale,slow,shigh,bin,device,slabel,axes,
     &           write,hard,points,badpix,dumreal,
     &           dims1(1),dims1(2),polygon,dynamic_mem(maskptr),
     &           status)
            else
              call plots_splot_rq
     &          (dynamic_mem(imptr),dynamic_mem(spptr),
     &           dynamic_mem(wkptr1),dynamic_mem(wkptr2),
     &           dynamic_mem(wkptr3),dynamic_mem(wkptr4),
     &           dynamic_mem(valptr),dims(1),dims(2),dims(3),
     &           xpix,ypix,scale,slow,shigh,bin,device,slabel,axes,
     &           write,hard,points,badpix,magic_float,
     &           dims1(1),dims1(2),polygon,dynamic_mem(maskptr),
     &           status)
            end if
          end if
c
c   OPTION 'D': change data range for spectrum plots
c
        else if(iopt.eq.11)then
          call par_cnpar('slow')
          call par_rdval('slow',slow,shigh,slow,' ',slow)
          call par_cnpar('shigh')
          call par_rdval('shigh',slow,shigh,shigh,' ',shigh)
          if(.not.scale)then
            call dsa_wruser('Note SCALE is not set, so SLOW and SHIGH ')
            call dsa_wruser('are ignored.\\N')
          end if
c
c   OPTION 'S': set/unset common plot scale flag
c
        else if(iopt.eq.12)then
          default=.not.scale
          call par_cnpar('scale')
          call par_rdkey('scale',default,scale)
c
c   OPTION 'A': set/unset axes flag
c
        else if(iopt.eq.13)then
          default=.not.axes
          call par_cnpar('axes')
          call par_rdkey('axes',default,axes)
c
c   OPTION 'O': set/unset spectrum output flag
c
        else if(iopt.eq.14)then
          default=.not.write
          call par_cnpar('write')
          call par_rdkey('write',default,write)
c
c   OPTION 'H': hardcopy when iopt=15
c
c   OPTION 'Q': quit when iopt=16
c
        else if (iopt .eq. 16) then
          call dsa_wruser('Bye\\n')
          go to 500
c
c   OPTION '?': type help text file
c
        else if(iopt.eq.17)then
          call fig_help('plots',status)
          if(status.ne.0)then
            call dsa_wruser('Unable to access help text file.\\N')
            status=0
          end if
c
c   Unknown option
c
        else if(iopt.ne.16)then
          call dsa_wruser('Unknown option.\\N')
        end if
      end do
c
c   Tidy and exit
c
  500 continue
      call pgend
      call dsa_close(status)
      end


      subroutine plots_select
     &  (xaxis,yaxis,nx,ny,stapix,endpix,start,end,mag,cursor,image,
     &   points,xpix,ypix,nx1,ny1,polygon,mask)
      implicit none
c
      integer   nx,ny,stapix(2),endpix(2)
      integer   nx1,ny1
      logical   cursor,image,points,polygon
      real      xaxis(nx),yaxis(ny),
     &          start(2),end(2),mag,xpix(2),ypix(2)
      byte      mask(nx1,ny1)
c
      integer   i
      integer   nid
      logical   quit
      real      sqvp
      real      xlast
      real      xpmn(2)
      real      xpmx(2)
      real      xtemp(2)
      real      ximv(2)
      real      ylast
      real      ypmn(2)
      real      ypmx(2)
      real      ytemp(2)
      real      yimv(2)
c
c   Return immediately if image has not been plotted
c
      if(.not.image)then
        call dsa_wruser('Image must be plotted first.\\N')
        quit=.true.
        go to 500
      end if
c
c   Initialize
c
      quit=.false.
      do i=1,2
        xpmn(i)=real(stapix(1))
        xpmx(i)=real(endpix(1))
        ypmn(i)=real(stapix(2))
        ypmx(i)=real(endpix(2))
      end do
c
c   Define image viewport
c
      call ndp_image_viewport(stapix,endpix,mag,'C',ximv,yimv,sqvp)
c
c   Get cursor point(s) on image plot
c
      if(cursor)then
        call dsa_wruser('Select 2 cursor points, or Q to quit.\\N')
        call ndp_image_cursor
     &    (xaxis,yaxis,nx,ny,start,end,stapix,endpix,2,'CDPS',
     &     ximv,yimv,xtemp,ytemp,nid,xlast,ylast,quit)
        if(quit)go to 500
c
c   Get a polygon
c
      else if (polygon) then
        call get_polygon_mask(xtemp,ytemp,mask,nx,ny,
     &                        stapix,endpix,xaxis,yaxis,
     &                        start,end,ximv,yimv,quit,polygon)
c
c   Get points from keyboard
c
      else
        call pgvsize(ximv(1),ximv(2),yimv(1),yimv(2))
        call par_cnpar('xpix')
        call ndp_par_rdary('xpix',xpmn,xpmx,'n',' ',2,2,xtemp)
        call par_cnpar('ypix')
        call ndp_par_rdary('ypix',ypmn,ypmx,'n',' ',2,2,ytemp)
      end if
c
c   Store points
c
      xpix(1)=min(xtemp(1),xtemp(2))
      xpix(2)=max(xtemp(1),xtemp(2))
      ypix(1)=min(ytemp(1),ytemp(2))
      ypix(2)=max(ytemp(1),ytemp(2))
      if(xpix(1).lt.real(stapix(1)))xpix(1)=real(stapix(1))
      if(xpix(2).gt.real(endpix(1)))xpix(2)=real(endpix(1))
      if(ypix(1).lt.real(stapix(2)))ypix(1)=real(stapix(2))
      if(ypix(2).gt.real(endpix(2)))ypix(2)=real(endpix(2))
c
c   Draw rectangle
c
      call pgwindow
     &  (real(stapix(1)),real(endpix(1)),
     &   real(stapix(2)),real(endpix(2)))
      call pgmove(xpix(1),ypix(1))
      call pgdraw(xpix(2),ypix(1))
      call pgdraw(xpix(2),ypix(2))
      call pgdraw(xpix(1),ypix(2))
      call pgdraw(xpix(1),ypix(1))
c
      points=.true.
c
  500 continue
      end



      SUBROUTINE PLOTS_WRITE
     &  (ARRAY,SPAXIS,NELM,BADPIX,NPLOT,BASENAME,
     &   X,Y,BIN,MODE,STATUS)
C
      IMPLICIT NONE
C
C     Functions used
C
      CHARACTER ICH_CI*8
      INTEGER   DYN_ELEMENT,ICH_LEN
C
C     Parameters
C
      INTEGER       NELM,STATUS,NPLOT
      LOGICAL       BADPIX
      REAL          ARRAY(NELM),SPAXIS(NELM)
      CHARACTER     BASENAME*(*)
      INTEGER       X,Y
      INTEGER       BIN
      CHARACTER     MODE*(*)
C
C     Local variables
C
      INTEGER       ADDRESS
      INTEGER       AXSLOT
      INTEGER       AXELM
      INTEGER       DATSLOT
      INTEGER       DATELM
      CHARACTER     DTA_NAME*64
      CHARACTER     DTA_X_NAME*64
      CHARACTER     DTA_Y_NAME*64
      CHARACTER     DTA_BIN_NAME*64
      CHARACTER     DTA_MODE_NAME*64
      INTEGER       DTA_STATUS
      CHARACTER     ERRMSG*128
      CHARACTER     SPECNAME*64
C
      INCLUDE 'DYNAMIC_MEMORY'
C
C     Create new structure
C
      SPECNAME = BASENAME(:ICH_LEN(BASENAME)) // ICH_CI(NPLOT)
      CALL DSA_NAMED_OUTPUT('SPECTRUM',SPECNAME,' ',1,1,STATUS)
      CALL DSA_SIMPLE_OUTPUT('SPECTRUM','D,A1','FLOAT',1,
     &                       NELM,STATUS)
      IF(STATUS.NE.0)GO TO 500
C
C     Now add the extra header information
C
      CALL DSA_SPECIFIC_STRUCTURE('SPECTRUM','PLOTS','WRITE',
     &                            DTA_NAME,STATUS)
      IF (STATUS.NE.0)GO TO 500
      DTA_STATUS = 0
C
      CALL DTA_CRNAM(DTA_NAME,'X',0,1,DTA_X_NAME,DTA_STATUS)
      CALL DTA_CRVAR(DTA_X_NAME,'INT',DTA_STATUS)
      CALL DTA_WRVARI(DTA_X_NAME,1,X,DTA_STATUS)
      IF (DTA_STATUS .NE. 0) THEN
        CALL DTA_ERROR(DTA_STATUS,ERRMSG)
        CALL DSA_WRUSER(ERRMSG(:ICH_LEN(ERRMSG))//'\\N')
        GO TO 500
      END IF
C
      CALL DTA_CRNAM(DTA_NAME,'Y',0,1,DTA_Y_NAME,DTA_STATUS)
      CALL DTA_CRVAR(DTA_Y_NAME,'INT',DTA_STATUS)
      CALL DTA_WRVARI(DTA_Y_NAME,1,Y,DTA_STATUS)
      IF (DTA_STATUS .NE. 0) THEN
        CALL DTA_ERROR(DTA_STATUS,ERRMSG)
        CALL DSA_WRUSER(ERRMSG(:ICH_LEN(ERRMSG))//'\\N')
        GO TO 500
      END IF
C
      CALL DTA_CRNAM(DTA_NAME,'BIN',0,1,DTA_BIN_NAME,DTA_STATUS)
      CALL DTA_CRVAR(DTA_BIN_NAME,'INT',DTA_STATUS)
      CALL DTA_WRVARI(DTA_BIN_NAME,1,BIN,DTA_STATUS)
      IF (DTA_STATUS .NE. 0) THEN
        CALL DTA_ERROR(DTA_STATUS,ERRMSG)
        CALL DSA_WRUSER(ERRMSG(:ICH_LEN(ERRMSG))//'\\N')
        GO TO 500
      END IF
C
      CALL DTA_CRNAM(DTA_NAME,'MODE',1,16,
     &               DTA_MODE_NAME,DTA_STATUS)
      CALL DTA_CRVAR(DTA_MODE_NAME,'CHAR',DTA_STATUS)
      CALL DTA_CRNAM(DTA_NAME,'MODE',0,0,
     &               DTA_MODE_NAME,DTA_STATUS)
      CALL DTA_WRVARC(DTA_MODE_NAME,16,MODE,
     &                DTA_STATUS)
      IF (DTA_STATUS .NE. 0) THEN
        CALL DTA_ERROR(DTA_STATUS,ERRMSG)
        CALL DSA_WRUSER(ERRMSG(:ICH_LEN(ERRMSG))//'\\N')
        GO TO 500
      END IF
C
C     Map axis
C
      CALL DSA_MAP_AXIS_DATA
     &  ('SPECTRUM',1,'WRITE','FLOAT',ADDRESS,AXSLOT,STATUS)
      IF(STATUS.NE.0)GO TO 500
      AXELM=DYN_ELEMENT(ADDRESS)
C
C     Set axis values
C
      CALL GEN_MOVE(NELM*4,SPAXIS,DYNAMIC_MEM(AXELM))
C
C     Map data array
C
      CALL DSA_MAP_DATA
     &  ('SPECTRUM','WRITE','FLOAT',ADDRESS,DATSLOT,STATUS)
      IF(STATUS.NE.0)GO TO 500
      DATELM=DYN_ELEMENT(ADDRESS)
C
C     Set data array values
C
      CALL GEN_MOVE(NELM*4,ARRAY,DYNAMIC_MEM(DATELM))
C
C     Set bad pixel flag if appropriate
C
      CALL NDP_SET_BAD_PIXEL('SPECTRUM',.FALSE.,BADPIX,STATUS)
C
C     Unmap arrays and close structure
C
      CALL DSA_UNMAP(AXSLOT,STATUS)
      CALL DSA_UNMAP(DATSLOT,STATUS)
      CALL DSA_CLOSE_STRUCTURE('SPECTRUM',STATUS)
C
500   CONTINUE
      END


      subroutine plots_write_polygon_data(basename,nx,ny,mask,x,y,
     &                                    bin,status)
c
      character basename*(*)
      integer   nx,ny
      byte      mask(nx,ny)
      integer   x,y
      integer   bin
      integer   status
c
      character dataname*64
      integer dims(2)
      character dta_bin_name*64
      character dta_name*64
      character dta_mask_name*64
      integer   dta_status
      character dta_x_name*64
      character dta_y_name*64
      character errmsg*128
      integer   nelm
c
      integer   ich_len
c
c  Initialise
c
      dta_status = 0
      dims(1) = nx
      dims(2) = ny
      nelm = nx*ny
c
c  Open data file
c
      dataname = basename(:ich_len(basename)) // '_dat'
      call dsa_named_output('spectrum',dataname,' ',1,1,status)
      if (status .ne. 0) go to 999
c
c  Get structure component base name
c
      call dsa_specific_structure('spectrum','polygon','write',
     &                            dta_name,status)
      if (status .ne. 0) go to 999
c
c  Create necessary entries
c
      call dta_crnam(dta_name,'mask',2,dims,dta_mask_name,dta_status)
      call dta_crvar(dta_mask_name,'byte',dta_status)
      call dta_crnam(dta_name,'mask',0,0,dta_mask_name,dta_status)
      call dta_wrvarb(dta_mask_name,nelm,mask,dta_status)
      if (dta_status .ne. 0) then
        call dta_error(dta_status,errmsg)
        call dsa_wruser(errmsg(:ich_len(errmsg))//'\\n')
        go to 999
      end if
c
      call dta_crnam(dta_name,'x',0,1,dta_x_name,dta_status)
      call dta_crvar(dta_x_name,'int',dta_status)
      call dta_wrvari(dta_x_name,1,x,dta_status)
      if (dta_status .ne. 0) then
        call dta_error(dta_status,errmsg)
        call dsa_wruser(errmsg(:ich_len(errmsg))//'\\n')
        go to 999
      end if
c
      call dta_crnam(dta_name,'y',0,1,dta_y_name,dta_status)
      call dta_crvar(dta_y_name,'int',dta_status)
      call dta_wrvari(dta_y_name,1,y,dta_status)
      if (dta_status .ne. 0) then
        call dta_error(dta_status,errmsg)
        call dsa_wruser(errmsg(:ich_len(errmsg))//'\\n')
        go to 999
      end if
c
      call dta_crnam(dta_name,'bin',0,1,dta_bin_name,dta_status)
      call dta_crvar(dta_bin_name,'int',dta_status)
      call dta_wrvari(dta_bin_name,1,bin,dta_status)
      if (dta_status .ne. 0) then
        call dta_error(dta_status,errmsg)
        call dsa_wruser(errmsg(:ich_len(errmsg))//'\\n')
        go to 999
      end if
c
c  Clean up
c
      call dsa_close_structure('spectrum',status)
c
999   return
      end



      subroutine plots_write_rect_data(basename,x1,y1,
     &                                 x2,y2,bin,status)
c
      character basename*(*)
      integer   x1,y1,x2,y2
      integer   bin
      integer   status
c
      character dataname*64
      character dta_bin_name*64
      character dta_name*64
      character dta_width_name*64
      character dta_height_name*64
      integer   dta_status
      character dta_x_name*64
      character dta_y_name*64
      character errmsg*128
      integer   width,height
c
      integer   ich_len
c
c  Initialise
c
      dta_status = 0
      width = abs(x2 - x1)
      height = abs(y2 - y1)
c
c  Open data file
c
      dataname = basename(:ich_len(basename)) // '_dat'
      call dsa_named_output('spectrum',dataname,' ',1,1,status)
      if (status .ne. 0) go to 999
c
c  Get structure component base name
c
      call dsa_specific_structure('spectrum','rectangle','write',
     &                            dta_name,status)
      if (status .ne. 0) go to 999
c
c  Create necessary entries
c
c
      call dta_crnam(dta_name,'x',0,1,dta_x_name,dta_status)
      call dta_crvar(dta_x_name,'int',dta_status)
      call dta_wrvari(dta_x_name,1,x1,dta_status)
      if (dta_status .ne. 0) then
        call dta_error(dta_status,errmsg)
        call dsa_wruser(errmsg(:ich_len(errmsg))//'\\n')
        go to 999
      end if
c
      call dta_crnam(dta_name,'y',0,1,dta_y_name,dta_status)
      call dta_crvar(dta_y_name,'int',dta_status)
      call dta_wrvari(dta_y_name,1,y1,dta_status)
      if (dta_status .ne. 0) then
        call dta_error(dta_status,errmsg)
        call dsa_wruser(errmsg(:ich_len(errmsg))//'\\n')
        go to 999
      end if
c
      call dta_crnam(dta_name,'width',0,1,dta_width_name,dta_status)
      call dta_crvar(dta_width_name,'int',dta_status)
      call dta_wrvari(dta_width_name,1,width,dta_status)
      if (dta_status .ne. 0) then
        call dta_error(dta_status,errmsg)
        call dsa_wruser(errmsg(:ich_len(errmsg))//'\\n')
        go to 999
      end if
c
      call dta_crnam(dta_name,'height',0,1,dta_height_name,dta_status)
      call dta_crvar(dta_height_name,'int',dta_status)
      call dta_wrvari(dta_height_name,1,height,dta_status)
      if (dta_status .ne. 0) then
        call dta_error(dta_status,errmsg)
        call dsa_wruser(errmsg(:ich_len(errmsg))//'\\n')
        go to 999
      end if
c
      call dta_crnam(dta_name,'bin',0,1,dta_bin_name,dta_status)
      call dta_crvar(dta_bin_name,'int',dta_status)
      call dta_wrvari(dta_bin_name,1,bin,dta_status)
      if (dta_status .ne. 0) then
        call dta_error(dta_status,errmsg)
        call dsa_wruser(errmsg(:ich_len(errmsg))//'\\n')
        go to 999
      end if
c
c
c  Clean up
c
      call dsa_close_structure('spectrum',status)
c
999   return
      end







