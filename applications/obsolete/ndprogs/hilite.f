      SUBROUTINE HILITE
C+
C
C   -----------
C   H I L I T E
C   -----------
C
C   Description
C   -----------
C   Plots a 2-D image on an image display device, progressively highlighting
C   pixels through a selected data range. This is accomplished with a
C   rotating lookup table which is primarily black, but contains a small
C   region of colour. As the program runs, all the image pixels of one
C   particular value become visible at the same time and then disappear.
C   The intention is to show with one program the locations of data values
C   which might be obscured if a large data range were mapped onto a static
C   lookup table in a standard image display program.
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
C   SHOWS    Number of cycles through the data. (integer)(prompted for).
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
C            scale to the right of the image. This actually produces an
C            interesting thermometer-like pointer which moves up the ramp
C            as the program runs.
C
C   DATA     Instruction to display the data value currently being
C            highlighted.
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
C   - The current screen and hardcopy device names are read from the
C     parameter file.
C   - The IMAGE structure is tested for the bad data flag. If it is found
C     and non-zero, magic values are assumed to be present and are left in
C     tha data.
C   - The data array is mapped as FLOAT regardless of its type, because
C     PGPLOT (and ultimately GKS) only handle floating point data.
C   - If magic values are present, they are replaced by a value which is
C     is lower than the lower threshold supplied by the user.
C   - A small lookup table based on one period of a sine wave is computed.
C   - The mapped array is passed to NDP_IMAGE_INDEX, which processes the
C     data values directly into integer color indices.
C   - The index array is passed to NDP_IMAGE_PLOT, which calls PGPIXL to
C     plot the image. At first nothing is seen because the GKS colour
C     indices are all initialized with black.
C   - The small lookup table is progressively cycled through the colour
C     indices. As it moves to higher indices it is replaced by black. The
C     effect on the image is to illuminate the pixels in a small part of the
C     data range for a brief time, and then to move on slightly in the data
C     range and repeat the process.
C   - As the pixels are highlighted on the image device, their data values
C     are output to the alphanumeric device. This enables the display to be
C     paused and restarted with CTRL-S and CTRL-Q. The data value shown is
C     approximate, being the middle of the data interval mapped on to the
C     current colour index.
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
C     DSA_SEEK_QUALITY
C     DSA_TYPESIZE
C     DSA_USE_FLAGGED_VALUES
C     DSA_WRUSER
C
C   Library DYN:
C     DYN_ELEMENT
C
C   Library GEN:
C     GEN_MOVE
C
C   Library ICH:
C     ICH_ENCODE
C     ICH_LEN
C
C   Library NDP:
C     NDP_AXIS_RANGE
C     NDP_DEVICE_INDEX
C     NDP_GET_IMAGE_INFO
C     NDP_IMAGE_INDEX
C     NDP_IMAGE_PLOT
C     NDP_IMAGE_VIEWPORT
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
C     PGEND
C     PGMTEXT
C     PGSCH
C     PGSCI
C     PGSCR
C
C   Internal subroutines called
C   ---------------------------
C   None.
C
C
C   INCLUDE statements
C   ------------------
C   INCLUDE 'DYNAMIC_MEMORY'
C   INCLUDE '$NDP_SOURCE/NDP_NUMERIC_RANGES.INC'
C   INCLUDE '$NDP_SOURCE/NDP_MAGIC_VALUES.INC'
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
C
C   History
C   -------
C   01-FEB-1989   - Original program
C   14-JUN-1990   - Parameters in call to NDP_IMAGE_PLOT changed.  (JRL)
C   17-SEP-1990   - No longer replaces magic values with zeros.  This assumes
C                   that the data is always going to be greater than zero which
C                   in certain cases (e.g. a velocity map) it may not be.
C                   They are now replaced by a value which is below the lower
C                   threshold requested for the plot and/or the contour map.
C                   The magic values are now also replaced only in a direct
C                   copy of the original file.  This is because if the data
C                   are mapped directly, then any changes will be copied to the
C                   disk, even if it is mapped READ.  The data pointer for
C                   non-FLOAT data which has been mapped FLOAT (e.g. type
C                   converted) will be the pointer for a workspace anyway,
C                   so the copying only has to be done for FLOAT data which
C                   has been mapped FLOAT.  (JRL)
C   15-OCT-1991   - Quality arrays implemented. (GOLDJIL)
C   02-MAR-1992   - Removed GKS stuff and PGPLOT common block references.
C                   Added NDP_IMAGE_INDEX call.
C                   (GOLDJIL)
C   24-NOV-1992   - Unix version. (GOLDJIL)
C   02-OCT-1994   - Removed variable c0 since it was being used before
C                   being defined. Have now defined the colour blue
C                   to be red(i) - should generate shades of grey. (GJP)
C   06-OCT-1994   - Removed unused variables. (GJP)
CC
C
C+-----------------------------------------------------------------------------
      implicit none
c
c   Functions.
c
      integer   dyn_element,ich_encode,ich_len,pgbegin,dsa_typesize
c
c   Local variables.
c
      integer   address
      real      angle
      logical   axes
      logical   badpix
      real      blue(256)
      integer   bytes
      integer   ciend
      integer   cispan
      integer   cistart
      character control*8
      logical   data
      integer   dims(10)
      integer   dims2d(2)
      integer   dumint
      real      dumreal
      real      end(2)
      integer   endpix2d(2)
      logical   erase
      real      green(256)
      real      high
      integer   i
      integer   imptr
      integer   index
      integer   islot
      integer   j
      integer   k
      character label*128
      real      low
      real      mag
      integer   maxind
      integer   minind
      integer   ndim
      integer   nelm
      character newval*8
      integer   next
      character oldval*8
      character place*2
      logical   qual
      integer   qptr
      integer   qslot
      logical   ramp
      real      red(256)
      integer   shows
      character softdev*16
      real      sqvp
      integer   stapix2d(2)
      real      start(2)
      integer   status
      character string*80
      integer   tptr
      integer   tslot
      character type*8
      real      value
      real      vmax
      real      vmin
      integer   wslot
      integer   wptr
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
      control='        '
      newval='        '
      oldval='        '
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
     &    ('No screen device selected - use SOFT command\\N')
        go to 500
      end if
c
c   Get name of image.
c
      call dsa_input('image','image',status)
      if(status.ne.0)go to 500
      call ndp_get_image_info('image',.true.,.false.,type,badpix,status)
      if(status.ne.0)go to 500
      call dsa_data_size('image',2,ndim,dims,nelm,status)
      if(status.ne.0)go to 500
      if(ndim.lt.2)then
        call dsa_wruser('This program handles 2-D images only.\\N')
        go to 500
      end if
      do i=1,2
        dims2d(i)=dims(i)
        stapix2d(i)=1
        endpix2d(i)=dims(i)
      end do
c
c   Get coordinate range to be displayed.
c
      call ndp_axis_range
     &  ('image',dims,ndim,start,end,stapix2d,endpix2d,status)
      if(status.ne.0)go to 500
c
c   Get image data range.
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
      if(axes)control(ich_len(control)+1:)='A'
c
c   Get instruction to plot ramp.
c
      call par_rdkey('ramp',.true.,ramp)
      if(ramp)control(ich_len(control)+1:)='R'
c
c   Get instruction to display data values.
c
      call par_rdkey('data',.false.,data)
c
c   Get number of shows.
c
      call par_rdval('shows',1.0,100.0,1.0,' ',dumreal)
      shows=int(dumreal)
c
c   Get instruction to erase screen.
c
      call par_rdkey('erase',.false.,erase)
c
c   Compute small table based on one period of a cosine function.
c
      cistart=1
      ciend=9
      do i=cistart,ciend
        angle=((2.0*real(i-cistart)/real(ciend-cistart))-1.0)*3.14159
        red(i)=  0.5*(cos(angle)+1.0)
        green(i)=red(i)
        blue(i)= red(i)
      end do
      cispan=ciend-cistart+1
c
c   Any quality arrays in town?
c
      call dsa_seek_quality('image',qual,status)
      if (status.ne.0) go to 500
c
c   Magic values are not to be removed from the data array unless quality
c   array is present!
c
      if (.not.qual) then
        call dsa_use_flagged_values('image',status)
        if(status.ne.0)go to 500
      end if
c
c   Map data array as FLOAT (this is required by PGPLOT).  If the data
c   are already FLOAT then we need to copy the original data over to
c   a workspace.  This is because the pointer to such data will point
c   to the actual disk file and thus when the magic values are changed,
c   (temporarily) it will change them on disk.
c
      if (type .ne. 'FLOAT') then
        call dsa_map_data('image','read','FLOAT',address,islot,status)
        if(status.ne.0)go to 500
        imptr=dyn_element(address)
      else
        bytes = nelm*dsa_typesize('float',status)
        call dsa_get_workspace(bytes,address,islot,status)
        imptr = dyn_element(address)
        call dsa_map_data('image','read','float',address,tslot,status)
        if (status .ne. 0) go to 500
        tptr = dyn_element(address)
        call gen_move(bytes,dynamic_mem(tptr),dynamic_mem(imptr))
      end if
c
c   Map quality if necessary
c
      if (qual) then
        call dsa_map_quality('image','read','byte',address,qslot,
     &                      status)
        if (status .ne. 0) go to 500
        qptr=dyn_element(address)
      end if
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
c   Initialize all colour indices to black
c
      call ndp_device_index(minind,maxind,status)
      if(status.ne.0)go to 500
      do i=1,maxind-minind+1
        call pgscr(i+minind-1,0.0,0.0,0.0)
      end do
c
c   Make viewport for image.
c
      call ndp_image_viewport
     &  (stapix2d,endpix2d,mag,place,ximv,yimv,sqvp)
c
c   Allocate image workspace for NDP_IMAGE_INDEX
c
      bytes = nelm * dsa_typesize('int',status)
      call dsa_get_workspace(bytes,address,wslot,status)
      if (status .ne. 0) go to 500
      wptr = dyn_element(address)
c
c   Plot image, temporarily replacing bad pixels if need be
c
      if (qual) then
        call dsa_wruser
     &    ('Temporarily replacing bad pixels...\\n')
        call ndp_replace_qual_r(dynamic_mem(imptr),
     &                          dynamic_mem(qptr),
     &                          nelm,low-1.0)
      end if
      call ndp_image_index(nelm,low,high,dynamic_mem(imptr),
     &                     badpix,dynamic_mem(wptr),status)
      call ndp_image_plot
     &  (dynamic_mem(wptr),dims(1),dims(2),stapix2d,endpix2d,
     &   start,end,high,low,label,control,ximv,yimv)
c
c   Set text size for data values.
c
      call pgsch(1.5)
c
c   Run the display.
c
      do k=1,shows
        string='Starting show '
        dumint=ich_encode(string,real(k),15,0,next)
        string(next:)='...'
        call dsa_wruser(string(:ich_len(string))//'\\n')
c
        do j=minind,maxind+cispan-1
c
c   - write data value to terminal so show can be paused with CTRL-S.
c
          value=low+(real(j-minind)/real(maxind-minind+1))*(high-low)
          dumint=ich_encode(newval,value,1,0,dumint)
          call dsa_wruser('Data '//newval//'\\N')
c
c   - erase previously plotted data value.
c
          if(data)then
            call pgsci(0)
            call pgmtext('B',2.0,0.5,0.5,oldval)
c
c   - plot current data value.
c
            call pgsci(1)
            call pgmtext('B',2.0,0.5,0.5,newval)
            oldval=newval
          end if
c
c   - advance the whole small colour table by one index.
c
          do i=1,cispan
            index=j+i-cispan
            if(index.ge.minind .and. index.le.maxind)then
              call pgscr(index,red(i),green(i),blue(i))
            end if
          end do
c
       end do
      end do
c
c   Tidy and exit.
c
  500 continue
      call pgend
      call dsa_close(status)
      end
