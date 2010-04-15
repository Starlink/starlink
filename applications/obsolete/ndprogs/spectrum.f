
      SUBROUTINE SPECTRUM
C+
C
C   ---------------
C   S P E C T R U M
C   ---------------
C
C   Description
C   -----------
C   Interactive display of spectra extracted from the first (spectral)
C   dimension of a ZXY-sorted 3-D image.
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
C   IMAGE1    Name of the structure containing the 2-D image to be used as a
C             map. (character)(prompted for).
C
C   SLOW      Lowest value which will be plotted in the spectra extracted
C             from the 3-D image. (real)(prompted for).
C
C   SHIGH     Highest value which will be plotted in the spectra extracted
C             from the 3-D image. (real)(prompted for).
C
C   START     Start coordinate in each dimension of the 2-D image subset to
C             be displayed. (real, array)(prompted for).
C
C   END       End coordinate in each dimension of the 2-D image subset to be
C             displayed. (real, array)(prompted for).
C
C   LOW       Value in the 2-D compacted image which is plotted in the
C             lowest colour index or as black. (real)(prompted for).
C
C   HIGH      Value in the 2-D compacted image which is plotted in the
C             highest colour index or as white. (real)(prompted for).
C
C   TABLE     Name of the lookup table. (character)(prompted for).
C
C   LABEL     Text string placed above the image plot. (character)
C             (prompted for).
C
C   SLABEL    Text string placed below the spectrum plots. (character)
C             (prompted for).
C
C   The following are prompted for when main menu options are selected:
C
C   XPIX      Pixel number in the X dimension of each point of extraction.
C             (integer, array)(prompted for).
C
C   YPIX      Pixel number in the Y dimension of each point of extraction.
C             (integer, array)(prompted for).
C
C   MAG       Magnification of the image plot relative to the full display
C             surface. (real)(prompted for).
C
C   NPLOTS    Number of spectrum plots to be displayed to the right of the
C             image plot. (integer)(prompted for).
C
C   BIN       Side length, in pixels, of a square area around a single point
C             within which spectra will be extracted and averaged. Ignored
C             if TWOCUR is true. (integer)(prompted for).
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
C   TWOCUR    Instructs the program to accept two points to define the area
C             within which spectra will be extracted and averaged. If TWOCUR
C             is true, BIN is ignored.
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
C     spectra to be extracted from the ZXY-sorted 3-D image. Depending on
C     the value of the keyword TWOCUR, either one or two cursor points may
C     indicated on the plot. The points may also be entered through the
C     keyboard. The coordinates of the points are used to address the second
C     and third dimensions of the 3-D image. If TWOCUR is true, two points
C     define a rectangle within which the extracted spectra are averaged. If
C     TWOCUR is false, a single point may address either a single spectrum
C     or the centre of a square of pixels defined by BIN, within which the
C     extracted spectra are averaged. In addition, the user can specify a
c     polygonal region of the image over which to average the spectra.
C   - The spectra are plotted to the right of the image. The number of
C     spectrum plots to be displayed may be selected via NPLOTS. If a
C     further point is selected beyond this number, one of the displayed
C     plots is erased and replaced. If a magic value pixel is encountered in
C     the spectrum array, the line string is broken at that point. If the
C     keyword WRITE is true, each spectrum is written to a data structure
C     named via SPECTRUM. If SCALE is true, all the spectra are plotted at
C     the same vertical scale, defined by SLOW and SHIGH. Otherwise each is
C     scaled according to its own data range.
C   - A hard copy of the image and spectrum plots may be produced. The
C     image defined by LOW, HIGH, and MAG, and the spectra defined by the
C     NPLOTS most recent pairs of points or single points are plotted on the
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
C     PGADVANCE
C     PGASK
C     PGBEGIN
C     PGBOX
C     PGDRAW
C     PGEND
C     PGLABEL
C     PGMOVE
C     PGQVP
C     PGSCI
C     PGSLW
C     PGVSIZE
C     PGVSTAND
C     PGWINDOW
C
C
C   Internal subroutines called
C   ---------------------------
C   MAGIC_ZAP
C   SPECTRUM_HARDCOPY_W
C   SPECTRUM_HARDCOPY_WQ
C   SPECTRUM_HARDCOPY_R
C   SPECTRUM_HARDCOPY_RQ
C   SPECTRUM_SLABEL
C   SPECTRUM_SPLOT_W
C   SPECTRUM_SPLOT_WQ
C   SPECTRUM_SPLOT_R
C   SPECTRUM_SPLOT_RQ
C   SPECTRUM_WRITE
C   GET_POLYGON_MASK
C
C
C   INCLUDE statements
C   ------------------
C   INCLUDE 'DYNAMIC_MEMORY'
C   INCLUDE 'MAGIC_VALUES'
C   INCLUDE 'NUMERIC_RANGES'
C   INCLUDE 'DCV_FUN'
C
C
C   Extensions to FORTRAN77
C   -----------------------
C   DO WHILE / END DO / IMPLICIT NONE / INCLUDE / Names > 6 characters
C
C
C   Possible future upgrades
C   ------------------------
C   - Use a build file instead of direct hardcopy. This would make the
C     hardcopy option much faster, but would require major modifications
C     to the DSK_ system common blocks so as to enable viewport dimensioning
C     via PGQVP and PGVSIZE. The drawing commands PGMOVE, PGDRAW, and PGGRAY
C     would also have to be added.
C   - Improve subroutine structure so that a single routine controls both
C     screen and hardcopy plotting.
C
C
C   Author/s
C   --------
C   Nick Fuller  RGO  (RGVAD::NMJF or NMJF@UK.AC.RGO.STAR)
C   Jim Lewis    RGO  (CAVAD::JRL or JRL@UK.AC.CAM.AST-STAR)
C   Julian Gold  RGO  (CAVAD::GOLDJIL or GOLDJIL@UK.AC.CAM.AST-STAR
C
C
C   History
C   -------
C   01-FEB-1989   - Original program
C   14-JUN-1990   - Some parameters in some subroutine calls have been changed
C                   so that arrays aren't dimensioned with elements of other
C                   arrays Also replaced NDP_IMAGE_LUT with IMAGE_LUT as a
C                   temporary measure as the former no longer works. (JRL).
C   29-AUG-1990   - Included new version of NDP_IMAGE_LUT.  (JRL)
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
C   30-JAN-1991   - Typo such that magic value line plotting routine wasn't
C                   called when needed for real data. (JRL)
C   20-JUN-1991   - The user can now average spectra over a polygonal region of
C                   the image (GOLDJIL). Also, the source is now in GENERIC
C                   format, split into 2 files: SPECTRUM1.FOR and SPECTRUM2.GEN.
C                   The file SPECTRUM.COM builds the executable.
C   02-MAR-1992   - Fixed to use new, improved Ariel Ultra, no, sorry, the
C                   new-ish PGPLOT routines. Also prompts for a file name
C                   for each spectrum written to disk. (GOLDJIL)
C   24-MAR-1992   - As if the last addition wasn't good enough, SPECTRUM
C                   now writes extra information into the output files:
C                   the co-ords of the spectrum; the bin size; whether
C                   it was extracted by rectangular or polygonal averaging.
C                   If the averaging was over a polygon, the polygon mask is
C                   added to the structure. If the data were averaged over a
C                   rectangle, the width and height will be stored in the
C                   structure.
C                   The output structure is now
C                     FILE {
C                       .Z
C                         .DATA     1D FLOAT spectrum data
C                         .FLAGGED  INT      Bad pixel flag, always 0
C                       .X
C                         .DATA[NX] FLOAT    axis data
C                       .SPECTRUM
C                         .X        INT      x-coord of extraction
C                         .Y        INT      y-coord of extraction
C                         .BIN      INT      bin size
C                         .MODE[16] CHAR     'rectangle','polygon' or 'pixel'
C                         .MASK     2D BYTE  mask array
C                         .WIDTH    INT      Width of averaging rectangle
C                         .HEIGHT   INT      Height "     "         "
C                     }
C                   (GOLDJIL)
C
C   25-NOV-1992   - Unix version. (GOLDJIL)
C   02-OCT-1994   - Correct the initialisation of the mask array in
C                   get_polygon_mask. The wrongbounds were used (GJP).
C   06-OCT-1994   - Removed lots of unused variables. (GJP)
C
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
      logical   badpix
      logical   badpix1
      integer   bin
      integer   bytes
      logical   cursor
      logical   default
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
      character harddev*16
      real      high
      integer   i
      logical   image
      integer   imptr
      integer   im1ptr
      integer   iopt
      integer   iplot
      integer   islot
      integer   i1slot
      integer   j
      character label*128
      real      low
      real      mag
      integer   maskptr
      integer   maskslot
      integer   ndim
      integer   ndim1
      integer   nelm
      integer   nelm1
      integer   next
      integer   nplots
      integer   oldbi
      integer   oldnp
      integer   oldsh
      integer   oldsl
      logical   polygon
      integer   qptr
      integer   qslot
      logical   quit
      logical   range_exist
      character response*10
      logical   scale
      logical   sdopen
      real      shigh
      character slabel*128
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
      logical   twocur
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
      real      xpix(2,10)
      integer   xslot
      real      ximv(2)
      integer   xyaxes(2)
      integer   yptr
      real      ypix(2,10)
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
      do j=1,10
        do i=1,2
          xpix(i,j)=0.0
          ypix(i,j)=0.0
        end do
      end do
      xyaxes(1)=2
      xyaxes(2)=3
c
c   Initial settings of option flags
c
      twocur=.false.
      write=.false.
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
c   Magic values are not to be removed from the cube data array.
c   However, quality values are to replace the bad pixels in IMAGE1
c
      call dsa_use_flagged_values('image',status)
      if(status.ne.0)go to 500
      call dsa_use_quality('image1',status)
      if(status.ne.0)go to 500
c
c   Map 2-D data array as 'FLOAT' as PGPLOT wants it that way.  Also map
c   the quality array (which may not exist, but DSA will create one and
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
c   Allocate memory for the polygon mask
c
      bytes = nelm1*dsa_typesize('byte',status)
      call dsa_get_workspace(bytes,address,maskslot,status)
      if (status .ne. 0) go to 500
      maskptr = dyn_element(address)
c
c   Allocate graphics workspace
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
        oldsl=slow
        oldsh=shigh
      end if
c
c   Get 2-D image window
c
      call ndp_axis_range
     &  ('image1',dims1,ndim1,start,end,stapix,endpix,status)
      if(status.ne.0)go to 500
c
c   Get 2-D image data range
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
c   Get label for image
c
      call par_rdchar('label',' ',label)
c
c   Get label for spectra
c
      call par_rdchar('slabel',' ',slabel)
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
      call var_getnum('nplots',0,0,dumreal,status)
      nplots=int(dumreal)
      oldnp=nplots
      if(nplots.eq.0)nplots=3
      string='NPLOTS = '
      dumint=ich_encode(string,real(nplots),10,0,next)
      call dsa_wruser(string(:ich_len(string))//'\\N')
c
      call var_getnum('bin',0,0,dumreal,status)
      bin=int(dumreal)
      oldbi=bin
      if(bin.eq.0)bin=1
      string='BIN = '
      dumint=ich_encode(string,real(bin),7,0,next)
      call dsa_wruser(string(:ich_len(string))//'\\N')
c
c   OPTIONS MENU
c
      do while (iopt.ne.16)
        call dsa_wruser(' \\N')
        call dsa_wruser('$Command (? to list) > \\N')
        call par_rduser(response,status)
        dumint=ich_fold(response)
        iopt=ich_key
     &    (response,1,' ','I:W:R:M:L:C:K:P:N:B:D:S:T:O:H:Q:?:','A',next)
c
c   OPTION 'I': plot 2-D image
c
        if(iopt.eq.1)then
          iplot=0
          image=.false.
          polygon = .false.
c
c   - advance screen device if already open, otherwise open it and reload LUT
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
c   - compute viewport for image plot
c
          call ndp_image_viewport(stapix,endpix,mag,'LC',ximv,yimv,sqvp)
c
c   - plot image
c
          call ndp_image_index(dims1(1)*dims1(2),low,high,
     &                         dynamic_mem(im1ptr),badpix,
     &                         dynamic_mem(gfxptr),status)
          if (status .ne. 0) go to 500
          call ndp_image_plot
     &      (dynamic_mem(gfxptr),dims1(1),dims1(2),stapix,endpix,
     &       start,end,high,low,label,'A',ximv,yimv)
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
c   -  load LUT
c
          call ndp_image_lut(table,status)
          if(status.ne.0)go to 500
c
c   OPTION 'C', 'K' or 'P': extract spectra from 3-D image and plot
c
        else if(iopt.eq.6 .or. iopt.eq.7 .or. iopt .eq. 8)then
          if(iopt.eq.6)then
            cursor=.true.
          else
            cursor=.false.
          end if
          polygon = iopt .eq. 8
c
c   - plot label below spectrum viewports
c
          call spectrum_slabel(ximv,yimv,nplots,slabel)
c
c   - plot one spectrum and optionally write it to a data structure
c
          quit=.false.
          do while (.not.quit)
            if(type.eq.'SHORT')then
              if(.not.badpix)then
                call spectrum_splot_w
     &            (dynamic_mem(imptr),dynamic_mem(spptr),
     &             dynamic_mem(xptr),dynamic_mem(yptr),
     &             dynamic_mem(wkptr1),dynamic_mem(wkptr2),
     &             dynamic_mem(wkptr3),dynamic_mem(wkptr4),
     &             dynamic_mem(valptr),
     &             dims(1),dims(2),dims(3),dims1(1),dims1(2),stapix,
     &             endpix,start,end,mag,nplots,iplot,
     &             bin,twocur,xpix,ypix,scale,slow,shigh,cursor,
     &             write,image,badpix,dumshort,quit,
     &             polygon,dynamic_mem(maskptr),status)
              else
                call spectrum_splot_wq
     &            (dynamic_mem(imptr),dynamic_mem(spptr),
     &             dynamic_mem(xptr),dynamic_mem(yptr),
     &             dynamic_mem(wkptr1),dynamic_mem(wkptr2),
     &             dynamic_mem(wkptr3),dynamic_mem(wkptr4),
     &             dynamic_mem(valptr),
     &             dims(1),dims(2),dims(3),dims1(1),dims1(2),stapix,
     &             endpix,start,end,mag,nplots,iplot,
     &             bin,twocur,xpix,ypix,scale,slow,shigh,cursor,
     &             write,image,badpix,magic_short,quit,
     &             polygon,dynamic_mem(maskptr),status)
              end if
            else
              if(.not.badpix)then
                call spectrum_splot_r
     &            (dynamic_mem(imptr),dynamic_mem(spptr),
     &             dynamic_mem(xptr),dynamic_mem(yptr),
     &             dynamic_mem(wkptr1),dynamic_mem(wkptr2),
     &             dynamic_mem(wkptr3),dynamic_mem(wkptr4),
     &             dynamic_mem(valptr),
     &             dims(1),dims(2),dims(3),dims1(1),dims1(2),stapix,
     &             endpix,start,end,mag,nplots,iplot,
     &             bin,twocur,xpix,ypix,scale,slow,shigh,cursor,
     &             write,image,badpix,dumreal,quit,
     &             polygon,dynamic_mem(maskptr),status)
              else
                call spectrum_splot_rq
     &            (dynamic_mem(imptr),dynamic_mem(spptr),
     &             dynamic_mem(xptr),dynamic_mem(yptr),
     &             dynamic_mem(wkptr1),dynamic_mem(wkptr2),
     &             dynamic_mem(wkptr3),dynamic_mem(wkptr4),
     &             dynamic_mem(valptr),
     &             dims(1),dims(2),dims(3),dims1(1),dims1(2),stapix,
     &             endpix,start,end,mag,nplots,iplot,
     &             bin,twocur,xpix,ypix,scale,slow,shigh,cursor,
     &             write,image,badpix,magic_float,quit,
     &             polygon,dynamic_mem(maskptr),status)
              end if
            end if
          end do
c
c   OPTION 'N': change number of spectrum plots
c
        else if(iopt.eq.9)then
          call par_cnpar('nplots')
          call par_rdval('nplots',1.0,10.0,3.0,' ',dumreal)
          nplots=int(dumreal)
          if(nplots.ne.oldnp .and. iplot.gt.0)then
            call dsa_wruser('Use option I to clear current spectra.\\N')
            oldnp=nplots
          end if
c
c   OPTION 'B': change binning interval for summation of spectra
c
        else if(iopt.eq.10)then
  10      call par_cnpar('bin')
          call par_rdval
     &      ('bin',1.0,real(min(dims1(1),dims1(2))),1.0,' ',dumreal)
          bin=int(dumreal)
          if(twocur)then
            call dsa_wruser('Note TWOCUR is set, so BIN is ignored.\\N')
          else
            if(mod(bin,2).eq.0)then
              call dsa_wruser('Note BIN is even, so binning is not ')
              call dsa_wruser('exactly centred on cursor point.\\N')
            end if
          end if
          if(bin.ne.oldbi .and. iplot.gt.0)then
            call dsa_wruser('Use option I to clear current spectra.\\N')
            oldbi=bin
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
          if(slow.ne.oldsl .or. shigh.ne.oldsh)then
            call dsa_wruser('Use option I to clear current spectra.\\N')
            oldsl=slow
            oldsh=shigh
          end if
c
c   OPTION 'S': set/unset common plot scale flag
c
        else if(iopt.eq.12)then
          default=.not.scale
          call par_cnpar('scale')
          call par_rdkey('scale',default,scale)
          if(scale.ne.default)then
            call dsa_wruser('Use option I to clear current spectra.\\N')
          end if
c
c   OPTION 'T': set/unset two point flag
c
        else if(iopt.eq.13)then
          default=.not.twocur
          call par_cnpar('twocur')
          call par_rdkey('twocur',default,twocur)
          polygon = .not.twocur
c
c   OPTION 'O': set/unset spectrum output flag
c
        else if(iopt.eq.14)then
          default=.not.write
          call par_cnpar('write')
          call par_rdkey('write',default,write)
c
c   OPTION 'H': produce hard copy plot
c
        else if(iopt.eq.15)then
c
c   - close screen device and open hardcopy device
c
          call pgend
          sdopen=.false.
          status=pgbegin(0,harddev,1,1)
          if(status.ne.1)go to 500
          status=0
          call pgask(.false.)
c
c   - unset LUT loaded flag so that PGGRAY can load its own
c
          if(type.eq.'SHORT')then
            if(.not.badpix)then
              call spectrum_hardcopy_w
     &          (dynamic_mem(imptr),dynamic_mem(spptr),
     &           dynamic_mem(im1ptr),
     &           dynamic_mem(xptr),dynamic_mem(yptr),
     &           dynamic_mem(wkptr1),dynamic_mem(wkptr2),
     &           dynamic_mem(wkptr3),dynamic_mem(wkptr4),
     &           dynamic_mem(valptr),
     &           dims(1),dims(2),dims(3),dims1(1),dims1(2),
     &           stapix,endpix,start,end,mag,high,low,nplots,
     &           bin,twocur,xpix,ypix,scale,slow,shigh,label,slabel,
     &           badpix,dumshort,
     &           polygon,dynamic_mem(maskptr),
     &           dynamic_mem(gfxptr),status)
            else
              call spectrum_hardcopy_wq
     &          (dynamic_mem(imptr),dynamic_mem(spptr),
     &           dynamic_mem(im1ptr),
     &           dynamic_mem(xptr),dynamic_mem(yptr),
     &           dynamic_mem(wkptr1),dynamic_mem(wkptr2),
     &           dynamic_mem(wkptr3),dynamic_mem(wkptr4),
     &           dynamic_mem(valptr),
     &           dims(1),dims(2),dims(3),dims1(1),dims1(2),
     &           stapix,endpix,start,end,mag,high,low,nplots,
     &           bin,twocur,xpix,ypix,scale,slow,shigh,label,slabel,
     &           badpix,magic_short,
     &           polygon,dynamic_mem(maskptr),
     &           dynamic_mem(gfxptr),status)
            end if
          else
            if(.not.badpix)then
              call spectrum_hardcopy_r
     &          (dynamic_mem(imptr),dynamic_mem(spptr),
     &           dynamic_mem(im1ptr),
     &           dynamic_mem(xptr),dynamic_mem(yptr),
     &           dynamic_mem(wkptr1),dynamic_mem(wkptr2),
     &           dynamic_mem(wkptr3),dynamic_mem(wkptr4),
     &           dynamic_mem(valptr),
     &           dims(1),dims(2),dims(3),dims1(1),dims1(2),
     &           stapix,endpix,start,end,mag,high,low,nplots,
     &           bin,twocur,xpix,ypix,scale,slow,shigh,label,slabel,
     &           badpix,dumreal,
     &           polygon,dynamic_mem(maskptr),
     &           dynamic_mem(gfxptr),status)
            else
              call spectrum_hardcopy_rq
     &          (dynamic_mem(imptr),dynamic_mem(spptr),
     &           dynamic_mem(im1ptr),
     &           dynamic_mem(xptr),dynamic_mem(yptr),
     &           dynamic_mem(wkptr1),dynamic_mem(wkptr2),
     &           dynamic_mem(wkptr3),dynamic_mem(wkptr4),
     &           dynamic_mem(valptr),
     &           dims(1),dims(2),dims(3),dims1(1),dims1(2),
     &           stapix,endpix,start,end,mag,high,low,nplots,
     &           bin,twocur,xpix,ypix,scale,slow,shigh,label,slabel,
     &           badpix,magic_float,
     &           polygon,dynamic_mem(maskptr),
     &           dynamic_mem(gfxptr),status)
            end if
          end if
c
c   OPTION 'Q': quit when iopt=16
c
        else if (iopt.eq.16) then
          call dsa_wruser('Bye.\\n')
          go to 500
c
c   OPTION '?': type help text file
c
        else if(iopt.eq.17)then
          call fig_help('spectrum',status)
          if(status.ne.0)then
            call dsa_wruser('Unable to access help text file.\\N')
            status=0
          end if
c
c   Unknown option
c
        else
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



      subroutine magic_zap(qual,data,nx,ny,val)
      implicit none
c
c     Parameters
c
      integer nx,ny
      byte qual(nx,ny)
      real data(nx,ny),val
c
c     Local variables
c
      integer i,j
c
      include 'QUALITY_MASK'
c
c     If the data is flagged in the quality array then replace it
c     with 'val'
c
      do j = 1,ny
         do i = 1,nx
            if (qual(i,j) .ne. q_good) data(i,j) = val
         end do
      end do
      end



      subroutine spectrum_slabel
     &  (ximv,yimv,nplots,slabel)
c
      implicit none
c
      character*(*) slabel
      integer       nplots
      real          ximv(2),yimv(2)
c
      real      xmax
      real      xmin
      real      xspv(2)
      real      ymax
      real      ymin
      real      ysiz
      real      yspv(2)
c
c   Plot label below bottom spectrum viewport
c   - set viewport to full screen
c   - set world coords to inches
c
      call pgvstand
      call pgqvp(1,xmin,xmax,ymin,ymax)
      call pgwindow(xmin,xmax,ymin,ymax)
      xspv(1)=ximv(2)+(xmax-ximv(2))*0.2
      xspv(2)=xmax
      ysiz=(ymax-ymin)/real(nplots)
      yspv(1)=ymin+ysiz*0.2
      yspv(2)=yspv(1)+ysiz*0.8
c
c   - set viewport to spectrum plot location
c
      call pgvsize(xspv(1),xspv(2),yspv(1),yspv(2))
      call pglabel(slabel,' ',' ')
c
      end



      SUBROUTINE SPECTRUM_WRITE
     &  (ARRAY,SPAXIS,NELM,BADPIX,SPECNAME,
     &   X,Y,BIN,MODE,POLYGON,NX,NY,MASK,TWOCUR,
     &   WIDTH,HEIGHT,STATUS)
C
      IMPLICIT NONE
C
C     Functions used
C
      INTEGER   DYN_ELEMENT,ICH_LEN
C
C     Parameters
C
      INTEGER       NELM,STATUS
      LOGICAL       BADPIX
      REAL          ARRAY(NELM),SPAXIS(NELM)
      CHARACTER     SPECNAME*(*)
      INTEGER       X,Y
      INTEGER       BIN
      CHARACTER     MODE*(*)
      INTEGER       NX,NY
      BYTE          MASK(NX,NY)
      INTEGER       WIDTH,HEIGHT
      LOGICAL       POLYGON,TWOCUR
C
C     Local variables
C
      INTEGER       ADDRESS
      INTEGER       AXSLOT
      INTEGER       AXELM
      INTEGER       DATSLOT
      INTEGER       DATELM
      INTEGER       DIMS(2)
      CHARACTER     DTA_NAME*64
      CHARACTER     DTA_X_NAME*64
      CHARACTER     DTA_Y_NAME*64
      CHARACTER     DTA_BIN_NAME*64
      CHARACTER     DTA_HEIGHT_NAME*64
      CHARACTER     DTA_MASK_NAME*64
      CHARACTER     DTA_MODE_NAME*64
      INTEGER       DTA_STATUS
      CHARACTER     DTA_WIDTH_NAME*64
      CHARACTER     ERRMSG*128
      INTEGER       NELM1
C
      INCLUDE 'DYNAMIC_MEMORY'
C
C     Initialise
C
      DTA_STATUS = 0
      DIMS(1) = NX
      DIMS(2) = NY
      NELM1 = NX*NY
C
C     Create new structure
C
      CALL DSA_NAMED_OUTPUT('SPECTRUM',SPECNAME,' ',1,1,STATUS)
      CALL DSA_SIMPLE_OUTPUT('SPECTRUM','D,A1','FLOAT',1,
     &                       NELM,STATUS)
      IF(STATUS.NE.0)GO TO 500
C
C     Now add the extra header information
C
      CALL DSA_SPECIFIC_STRUCTURE('SPECTRUM','SPECTRUM','WRITE',
     &                            DTA_NAME,STATUS)
      IF (STATUS.NE.0)GO TO 500
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
      IF (POLYGON) THEN
        CALL DTA_CRNAM(DTA_NAME,'MASK',2,DIMS,DTA_MASK_NAME,DTA_STATUS)
        CALL DTA_CRVAR(DTA_MASK_NAME,'BYTE',DTA_STATUS)
        CALL DTA_CRNAM(DTA_NAME,'MASK',0,0,DTA_MASK_NAME,DTA_STATUS)
        CALL DTA_WRVARB(DTA_MASK_NAME,NELM1,MASK,DTA_STATUS)
        IF (DTA_STATUS .NE. 0) THEN
          CALL DTA_ERROR(DTA_STATUS,ERRMSG)
          CALL DSA_WRUSER(ERRMSG(:ICH_LEN(ERRMSG))//'\\N')
          GO TO 500
        END IF
      ELSE IF (TWOCUR) THEN
        CALL DTA_CRNAM(DTA_NAME,'WIDTH',0,1,DTA_WIDTH_NAME,DTA_STATUS)
        CALL DTA_CRVAR(DTA_WIDTH_NAME,'INT',DTA_STATUS)
        CALL DTA_WRVARI(DTA_WIDTH_NAME,1,WIDTH,DTA_STATUS)
        IF (DTA_STATUS .NE. 0) THEN
          CALL DTA_ERROR(DTA_STATUS,ERRMSG)
          CALL DSA_WRUSER(ERRMSG(:ICH_LEN(ERRMSG))//'\\N')
          GO TO 500
        END IF
C
        CALL DTA_CRNAM(DTA_NAME,'HEIGHT',0,1,DTA_HEIGHT_NAME,DTA_STATUS)
        CALL DTA_CRVAR(DTA_HEIGHT_NAME,'INT',DTA_STATUS)
        CALL DTA_WRVARI(DTA_HEIGHT_NAME,1,HEIGHT,DTA_STATUS)
        IF (DTA_STATUS .NE. 0) THEN
          CALL DTA_ERROR(DTA_STATUS,ERRMSG)
          CALL DSA_WRUSER(ERRMSG(:ICH_LEN(ERRMSG))//'\\N')
          GO TO 500
        END IF
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




      subroutine get_polygon_mask(xrec,yrec,mask,nx,ny,
     &                            stapix,endpix,
     &                            xaxis,yaxis,start,
     &                            end,ximv,yimv,quit,
     &                            polygon)
c
c     Allows the user to enter a polygonal region from which to extract
c     a spectrum, and builds a mask from it.
c
c   Arguments
c
      real 	xrec(2),yrec(2)   ! define rectangle surrounding polygon
      integer 	nx,ny             ! y & x sizes of displayed image
      byte 	mask(nx,ny)       ! mask array
      integer   stapix(2)         ! start pixels of region
      integer   endpix(2)         ! end pixels of region
      real      xaxis(nx)         ! x-axis calibrations
      real      yaxis(ny)         ! y-axis calibrations
      real      start(2),end(2)   ! start and end axis values
      real      ximv(2),yimv(2)   ! viewport size in inches
      logical   quit              ! exit flag
      logical   polygon           ! polygon selected flag
c
      integer	MAXPTS
      parameter (MAXPTS = 32)	  ! 32-sided polygon maximum!
      integer   xpts(MAXPTS)      ! x-values of points
      integer   ypts(MAXPTS)      ! y-values of points
      integer   npts              ! point counter
      integer   xmax,ymax         ! max values of x,y
      integer   xmin,ymin         ! min values of x,y
      real      s,t               ! line parameters
      real      scale             ! scaling factor
      integer   i,j,k,l           ! loop variables
      integer   nid               ! number of pts selected by ndp_image_cursor
      real      xr,yr             ! co-ords of selected point
      integer   xi,yi             !          ditto
      logical   closed            ! flags curve being manually closed by user
      integer   dx,dy             ! d? = difference in ?-coords of line endpts
      integer   count             ! = k * winding number of region wrt point
      integer   pen               ! pgplot pen number
c
c   initialise mask
c
      do j=1,ny
        do i=1,nx
          mask(i,j) = 0
        end do
      end do
      quit = .false.
c
c   prompt user
c
      call dsa_wruser('Use the cursor to define a polygon.\\n')
      call dsa_wruser('Press Q to close the region.\\n')
c
c   get the points
c
      npts = 1
      ncur = 1
      xmax = -1
      ymax = -1
      xmin = 32767
      ymin = 32767
      closed = .false.
      call pgqci(pen)              ! Save current pen colour...
      call pgsci(1)                ! ...and set to white
      do while ((npts .le. MAXPTS).and.(.not.closed))
        if (npts .eq. 1) then
          call ndp_image_cursor(xaxis,yaxis,nx,ny,
     &                           start,end,stapix,endpix,ncur,
     &                           'CDPS',ximv,yimv,
     &                           xr,yr,nid,xlast,ylast,quit)
        else
          call ndp_image_cursor(xaxis,yaxis,nx,ny,
     &                           start,end,stapix,endpix,ncur,
     &                           'DPS',ximv,yimv,xr,yr,
     &                           nid,xlast,ylast,quit)
        end if
        if (quit) then             ! Close the curve
          call dsa_wruser('Curve closed.\\n')
          xpts(npts) = xpts(1)
          ypts(npts) = ypts(1)
          call pgdraw(real(xpts(1)),real(ypts(1)))
          closed = .true.
          go to 123
        end if
        xi = int(xr)
        yi = int(yr)
c
c   check extrema
c
        if (xi .gt. xmax) xmax = xi
        if (yi .gt. ymax) ymax = yi
        if (xi .lt. xmin) xmin = xi
        if (yi .lt. ymin) ymin = yi
        xpts(npts) = xi
        ypts(npts) = yi
c
c   Draw a line and check for the polygon closing
c
        if (npts .gt. 1) then
          call pgmove(real(xpts(npts-1)),real(ypts(npts-1)))
          call pgdraw(xr,yr)
          closed = ((xi .eq. xpts(1)).and.(yi .eq. ypts(1)))
          if (closed) call dsa_wruser('Curve closed!\\n')
        else
c
c    Plot a point
c
          call pgpoint(1,xr,yr,-1)
        end if
        npts = npts + 1
      end do ! while(...)
c
c   If the curve is not closed, close it!
c
      if ((npts .gt. MAXPTS).and.(.not.closed)) then
        xpts(MAXPTS) = xpts(1)
        ypts(MAXPTS) = ypts(1)
        call pgdraw(real(xpts(1)),real(ypts(1)))
        call dsa_wruser('*** Maximum number of points has been')
        call dsa_wruser(' exceeded - the polygon has been')
        call dsa_wruser(' closed ***\\n')
      end if
123   continue
      npts = npts - 1
c
c   Reject duff polygons
c
      if (npts .lt. 3) then
        call dsa_wruser('You need at least three')
        call dsa_wruser(' points to define  a polygon\\n')
        polygon = .false.
        go to 500
      end if
c
c   Now we can begin building the mask by checking if points are inside the
c   polygon or not. This is achieved by counting the number of intersections
c   between a line joining the point to the origin and each of the straight
c   lines that form the polygon. To do this, each segment of the polygon is
c   parameterised wrt s in [0,1) and the line joining (x,y) to the origin
c   by t in [0,1]. The values of s and t can be evaluated by solving a 2x2
c   linear system explicitly for the point of intersection. If s & t are
c   within the above intervals, then the 2 lines intersect. An odd number
c   of intersections with the polygon means the point is inside
c
      do j=ymin,ymax
        do i=xmin,xmax
          count = 0
          do k=1,npts
            if (k .eq. npts) then
              l = 1
            else
              l = k+1
            end if
            dx = xpts(l) - xpts(k)
            dy = ypts(l) - ypts(k)
            scale = real(j*dx - i*dy)
c
c    Check for parallel lines!
c
            if (scale .eq. 0.0) go to 333
c
c    Compute parameters corresponding to intersection point
c
            s = real(ypts(k)*i - xpts(k)*j)/scale
            t = real(ypts(k)*dx - xpts(k)*dy)/scale
c
c    The lines intersect if 0<=t<=1 and 0<=s<1
c
            if (s .ge. 0.0) then
              if (t .ge. 0.0) then
                if (s .lt. 1.0) then
                  if (t .le. 1.0) then
                    count = count + 1
                  end if
                end if
              end if
            end if
333         continue
          end do ! k
c
c   If there is an odd number of intersections, the point is inside
c

          mask(i,j) = mod(count,2)
c
c   Test!
c
c          if (mask(i,j) .eq. 1) then
c            call pgpoint(1,real(i),real(j),-1)
c          end if
c
c
c
        end do ! i
      end do ! j
c
c   Return the corners of the rectangle enclosing the polygon
c
      xrec(1) = xmin
      xrec(2) = xmax
      yrec(1) = ymin
      yrec(2) = ymax
500   continue
      call pgsci(pen)                                 ! Reset the pen colour
      end
