      SUBROUTINE MOVIE
C+
C
C   ---------
C   M O V I E
C   ---------
C
C   Description
C   -----------
C   Plots successive planes (in dimensions 1 and 2) of a 3-D image in
C   greyscale or colour on an image display device. The speed is acceptable
C   on the Ikon but very slow on the ARGS.
C
C
C   Scope of program
C   ----------------
C   - Handles 2-D images only.
C   - Data array types SHORT and FLOAT supported; others converted to FLOAT.
C   - Subsetting supported.
C   - Magic values and quality arrays supported.
C   - Variance arrays not supported.
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
C   IMAGE    Name of the structure containing the image to be displayed.
C            (character)(prompted for).
C
C   START    Coordinate in each dimension of IMAGE at which the display is
C            to start. (real, array)(prompted for).
C
C   END      Coordinate in each dimension of IMAGE at which the display is
C            to end. (real, array)(prompted for).
C
C   STEP     Number of planes to increment between each display. (integer)
C            (prompted for).
C
C   LOW      Data value which is plotted in the lowest colour index or as
C            black. (real)(prompted for).
C
C   HIGH     Data value which is plotted in the highest colour index or as
C            white. (real)(prompted for).
C
C   PLACE    Code for one of nine possible locations on the display surface,
C            being a combination of T, C, or B, and L, C, or R. (character)
C            (prompted for).
C
C   MAG      Magnification of the plot. Magnification 1 fits the plot to the
C            whole display surface. (real)(prompted for).
C
C   LABEL    Text placed centrally above the plot. (character)(prompted
C            for).
C
C   TABLE    Name of colour or grey scale lookup table. (character)
C            (prompted for).
C
C   SOFTDEV  Current screen device name (character)(read from file).
C
C
C   Keywords
C   --------
C   WHOLE    Instruction to display the whole image. Otherwise, a subset of
C            each dimension may be selected.
C
C   AXES     Instruction to plot calibrated axes. Otherwise, the image is
C            framed with a plain box.
C
C   RAMP     Instruction to plot a calibrated bar of the colour or grey
C            scale to the right of the image.
C
C   ERASE    Instruction to erase screen before plotting. Otherwise, a
C            number of images may be displayed at different places on the
C            same screen.
C
C
C
C   Propagation of data structure
C   -----------------------------
C   Not relevant.
C
C
C   Method
C   ------
C   - The current screen device name is read from the parameter file.
C   - The IMAGE structure is tested for the bad data flag. If it is found
C     and non-zero, magic values are assumed to be present and are left in
C     the data.
C   - The start and end coordinates in each dimension of the image are
C     prompted for. The coordinates in dimension 3 give the range of planes
C     to be displayed. The parameter STEP allows planes to to be skipped.
C   - The required lookup table is read and stored. This is done by the
C     routine NDP_IMAGE_LUT.
C   - A subroutine appropriate to the data type is called to display the
C     data. The method is to extract planes from the 3-D array into a
C     2-D floating point work array, which is processed into colour index
C     values by NDP_IMAGE_INDEX and then plotted using PGPIXL.
C   - If magic values are present, they are replaced by a value below the
C     low plotting threshold.
C   - As the planes are displayed on the image device, the frame numbers are
C     output to the alphanumeric device. This enables the movie to be paused
C     and restarted with CTRL-S and CTRL-Q.
C
C
C   External functions & subroutines called
C   ---------------------------------------
C   Library DSA:
C     DSA_CLOSE
C     DSA_DATA_SIZE
C     DSA_GET_WORKSPACE
C     DSA_INPUT
C     DSA_MAP_DATA
C     DSA_MAP_QUALITY
C     DSA_OPEN
C     DSA_TYPESIZE
C     DSA_SEEK_QUALITY
C     DSA_USE_FLAGGED_VALUES
C     DSA_WRUSER
C
C   Library DYN:
C     DYN_ELEMENT
C
C   Library ICH:
C     ICH_ENCODE
C     ICH_LEN
C
C   Library NDP:
C     NDP_AXIS_RANGE
C     NDP_GET_IMAGE_INFO
C     NDP_IMAGE_LUT
C     NDP_IMAGE_RAMP
C     NDP_IMAGE_VIEWPORT
C     NDP_REPLACE_MAGIC_R
C     NDP_REPLACE_QUAL_R
C
C   Library PAR:
C     PAR_RDCHAR
C     PAR_RDKEY
C     PAR_RDVAL
C
C   Library VAR:
C     VAR_GETCHR
C
C   Starlink PGPLOT:
C     PGBEGIN
C     PGBOX
C     PGEND
C     PGLABEL
C     PGMTEXT
C     PGPIXL
C     PGSCH
C     PGSCI
C     PGVSIZE
C     PGWINDOW
C
C
C   Internal subroutines called
C   ---------------------------
C   MOVIE_DISPLAY_W
C   MOVIE_DISPLAY_R
C
C
C   INCLUDE statements
C   ------------------
C   INCLUDE 'DYNAMIC_MEMORY'
C   INCLUDE 'NUMERIC_RANGES'
C   INCLUDE 'MAGIC_VALUES'
C   INCLUDE 'DCV_FUN'
C
C
C   Extensions to FORTRAN77
C   -----------------------
C   END DO / IMPLICIT NONE / INCLUDE / Names > 6 characters
C
C
C   Possible future upgrades
C   ------------------------
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
C   10-JUL-1990   - Parameters in some subroutine calls changed to avoid
C                   dimensioning arrays with elements of another array.  Also
C                   included call to new LUT routine.  (Temporary measure as
C                   NDP_IMAGE_LUT no longer works...) (JRL)
C   29-JUL-1990   - New version of NDP_IMAGE_LUT included. (JRL)
C   18-SEP-1990   - No longer replaces magic values with zeros.  This assumes
C                   that the data are always going to be greater than zero
C                   which in certain cases (e.g. a velocity map) it may not be.
C                   They are now replaced by a value which is below the lower
C                   threshold requested for the plot.  The magic values are
C                   now also replaced plane by plane in the subroutine
C                   MOVIE_DISPLAY. This subroutine has a one plane workspace
C                   where these changes can be made without any danger
C                   of the changes being written directly to disk. (JRL)
C   14-OCT-1991   - Quality arrays now implemeted and code written in GENERIC
C                   form. (GOLDJIL)
C
C   07-FEB-1992   - 2D gfx calls updated (GOLDJIL)
C   03-DEC-1992   - Unix version (GOLDJIL)
C   06-OCT-1994   - Removed unused variables. (GJP)
C
C+-----------------------------------------------------------------------------
      implicit none
c
c   Functions.
c
      integer   dsa_typesize,dyn_element,ich_len,pgbegin
c
c   Local variables.
c
      integer   address
      logical   axes
      logical   badpix
      integer   bytes_float
      integer   bytes_int
      integer   dims(10)
      real      dumreal
      real      end(6)
      integer   endpix(6)
      integer   endpix2d(2)
      logical   erase
      real      high
      integer   i
      integer   imptr
      integer   islot
      character label*128
      real      low
      real      mag
      integer   ndim
      integer   nelm
      character place*2
      logical   qual
      integer   qptr
      integer   qslot
      logical   ramp
      character softdev*16
      real      sqvp
      integer   stapix(6)
      integer   stapix2d(2)
      real      start(6)
      integer   status
      integer   step
      character table*32
      integer   totspace
      character type*8
      real      vmax
      real      vmin
      integer   wkptr
      integer   wkptr2
      integer   wkslot
      real      ximv(2)
      real      yimv(2)
c
      include 'DYNAMIC_MEMORY'
      include 'NUMERIC_RANGES'
      include 'MAGIC_VALUES'
c
c   Initialize.
c
      status=0
c
c   Open DSA system.
c
      call dsa_open(status)
      if(status.ne.0)go to 500
c
c   Get current PGPLOT screen device.
c
      call var_getchr('softdev',0,0,softdev,status)
      if(status.ne.0)then
        call dsa_wruser
     &    ('No screen device selected - use SOFT command.\\N')
        go to 500
      end if
c
c   Get name of image.
c
      call dsa_input('image','image',status)
      if(status.ne.0)go to 500
      call ndp_get_image_info('image',.true.,.false.,type,badpix,status)
      if(status.ne.0)go to 500
c
c   Get dimensions of image.
c
      call dsa_data_size('image',3,ndim,dims,nelm,status)
      if(status.ne.0)go to 500
      if(ndim.lt.3)then
        call dsa_wruser('This is not a 3-D image.\\N')
        go to 500
      end if
c
c   Get coordinate range to be displayed.
c
      call ndp_axis_range
     &  ('image',dims,ndim,start,end,stapix,endpix,status)
      if(status.ne.0)go to 500
c
c   Compute dimensions of subset.
c
      do i=1,2
        stapix2d(i)=stapix(i)
        endpix2d(i)=endpix(i)
      end do
c
c   Get frame step and make sure it has the correct sign.
c
      call par_rdval
     &  ('step',-real(dims(3)),real(dims(3)),1.0,' ',dumreal)
      step=int(dumreal)
      if(stapix(3).le.endpix(3))then
        if(step.lt.0)step=-step
      else
        if(step.gt.0)step=-step
      end if
c
c   Get range of data for plot
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
c   Get image viewport location.
c
      call par_rdchar('place',' ',place)
c
c   Get magnification factor.
c
      call par_rdval('mag',0.1,1.0,1.0,' ',mag)
c
c   Get label for image.
c
      call par_rdchar('label',' ',label)
c
c   Get instruction to plot axes.
c
      call par_rdkey('axes',.true.,axes)
c
c   Get instruction to plot ramp.
c
      call par_rdkey('ramp',.true.,ramp)
c
c   Get name of LUT.
c
      call par_rdchar('table',' ',table)
c
c   Get instruction to erase screen.
c
      call par_rdkey('erase',.true.,erase)
c
c   See if there is a quality array kicking around...
c
      call dsa_seek_quality('image',qual,status)
      if (status.ne.0) go to 500
c
c   Magic values are not to be removed from the data array unless
c   a quality array exists
c
      if (.not.qual) then
        call dsa_use_flagged_values('image',status)
        if(status.ne.0)go to 500
      end if
c
c   Map data array.
c
      if(type.eq.'SHORT')then
        call dsa_map_data('image','read','SHORT',address,islot,status)
      else
        call dsa_map_data('image','read','FLOAT',address,islot,status)
      end if
      if(status.ne.0)go to 500
      imptr=dyn_element(address)
c
c   Map quality array if present
c
      if (qual) then
        call dsa_map_quality('image','read','byte',address,qslot,status)
        if (status.ne.0) go to 500
        qptr=dyn_element(address)
      end if
c
c   Get workspace for XY plane.
c
      bytes_float=dsa_typesize('float',status)
      bytes_int=dsa_typesize('int',status)
      totspace=dims(1)*dims(2)*(bytes_float+bytes_int)
      call dsa_get_workspace(totspace,address,wkslot,status)
      if(status.ne.0)go to 500
      wkptr=dyn_element(address)
      wkptr2=wkptr+dims(1)*dims(2)*bytes_int
c
c   Get workspace for plotting routine
c

c
c   Open screen device.
c
      if(erase)then
        status=pgbegin(0,softdev,1,1)
      else
        status=pgbegin(0,softdev(:ich_len(softdev))//'/APPEND',1,1)
      end if
      if(status.ne.1)go to 500
      status=0
c
c   Load LUT
c
      call ndp_image_lut(table,status)
      if (status .ne. 0) go to 500
c
c   Make viewport for image.
c
      call ndp_image_viewport
     &  (stapix2d,endpix2d,mag,place,ximv,yimv,sqvp)
c
c   Plot ramp if requested.
c
      if(ramp)then
        call ndp_image_ramp(ximv,yimv,low,high)
      end if
c
c   Display movie.
c
      if(type.eq.'SHORT')then
        call movie_display_w
     &    (dynamic_mem(imptr),dynamic_mem(wkptr),dynamic_mem(wkptr2),
     &     table,dims(1),dims(2),dims(3),stapix,endpix,step,
     &     start,end,high,low,label,axes,badpix,
     &     magic_short,qual,dynamic_mem(qptr),ximv,yimv,status)
      else
        call movie_display_r
     &    (dynamic_mem(imptr),dynamic_mem(wkptr),dynamic_mem(wkptr2),
     &     table,dims(1),dims(2),dims(3),stapix,endpix,step,
     &     start,end,high,low,label,axes,badpix,
     &     magic_float,qual,dynamic_mem(qptr),ximv,yimv,status)
      end if
c
c   Tidy and exit.
c
  500 continue
      call pgend
      call dsa_close(status)
      end
