      SUBROUTINE DEPICT
C+
C
C   -----------
C   D E P I C T
C   -----------
C
C   Description
C   -----------
C   Plots a 2-D image in greyscale or colour on an image display device,
C   with optional overplotted contours of the same or another 2-D image, and
C   optional hardcopy.
C
C
C   Scope of program
C   ----------------
C   - Handles 2-D images only.
C   - Data array types SHORT and FLOAT supported; others converted to FLOAT.
C   - Subsetting supported.
C   - Magic values and quality arrays supported.
C   - Variance arrays not supported.
C   - Batch execution supported.
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
C   IMAGE1   Name of the structure containing the image to be contoured, if
C            CONTOUR is true. (character)(prompted for).
C
C   LOW1     Lowest data value to be contoured. (real)(prompted for).
C
C   HIGH1    Highest data value to be contoured. (real)(prompted for).
C
C   LEVELS   Number of contour levels. The first contour is at LOW and the
C            last is at HIGH. The data range HIGH-LOW divided by LEVELS-1
C            gives the contour interval. (integer)(prompted for).
C
C   TABLE    Name of colour or grey scale lookup table. (character)
C            (prompted for).
C
C   SOFTDEV  Current screen device name (character)(read from file).
C
C   HARDDEV  Current hardcopy device name (character)(read from file).
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
C   CONTOUR  Instruction to overplot with contours of the same or another
C            2-D image.
C
C   HARDCOPY Instruction to plot on the current hardcopy device.
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
C   - If magic values are present, they are replaced by a value below the
C     lower threshold as supplied by the user.
C   - The required lookup table is read and stored. This is done by the
C     routine NDP_IMAGE_LUT.
C   - The mapped array is processed into integer colour indices by
C     NDP_IAMGE_INDEX.
C   - The index array is passed to NDP_IMAGE_PLOT, which calls PGPIXL to
C     plot the image.
C   - If overplotted contours of a different image are required, the data
C     array is mapped as FLOAT and any magic values are replaced by values
C   - below the user supplied threshold for the contour map.
C   - The mapped array to be contoured is passed to PGCONT without clearing
C     the screen or changing the PGPLOT viewport, so that the contours are
C     registered with the image.
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
C   Library GEN:
C     GEN_MOVE
C
C   Library ICH:
C     ICH_LEN
C
C   Library NDP:
C     NDP_AXIS_RANGE
C     NDP_GET_IMAGE_INFO
C     NDP_IMAGE_INDEX
C     NDP_IMAGE_LUT
C     NDP_IMAGE_PLOT
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
C     PGCONT
C     PGEND
C     PGVSIZE
C     PGWINDOW
C
C
C   Internal subroutines called
C   ---------------------------
C   DEPICT_CONTOUR
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
C   01-FEB-1989   - Original program (NMJF)
C   14-JUN-1990   - Parameters in some subroutine calls changed to avoid
C                   dimensioning arrays with elements of another array.  Also
C                   included call to new LUT routine.  (Temporary measure as
C                   NDP_IMAGE_LUT no longer works...) (JRL)
C   29-AUG-1990   - New version of NDP_IMAGE_LUT now included.  (JRL)
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
C   15-OCT-1991   - Quality arrays implemeted. (GOLDJIL)
C   07-FEB-1992   - Changed to new NDP 2D gfx routines. (GOLDJIL)
C   24-NOV-1992   - Unix version (GOLDJIL)
C+-----------------------------------------------------------------------------
c
      implicit none
c
c   Functions.
c
      integer   dyn_element,ich_len,pgbegin,dsa_typesize
c
c   Local variables.
c
      integer   address
      logical   axes
      logical   badpix
      logical   badpix1
      integer   bytes
      integer   bytes1
      logical   contour
      character control*8
      integer   dims(10)
      integer   dims1(10)
      integer   dim2d(2)
      integer   dim2d1(2)
      real      dumreal
      real      end(6)
      real      end2d(2)
      real      end2d1(2)
      integer   endpix(6)
      integer   epx2d(2)
      integer   epx2d1(2)
      logical   erase
      logical   hard
      character harddev*16
      real      high
      real      high1
      integer   i
      integer   imptr
      integer   im1ptr
      integer   islot
      integer   i1slot
      character label*128
      integer   levels
      real      levinc
      real      low
      real      low1
      real      mag
      integer   ndim
      integer   ndim1
      integer   nelm
      integer   nelm1
      character place*2
      logical   qual,qual1
      integer   qptr,q1ptr
      integer   qslot,q1slot
      logical   ramp
      character softdev*16
      real      sqvp
      integer   stapix(6)
      integer   spx2d(2)
      integer   spx2d1(2)
      real      start(6)
      real      sta2d(2)
      real      sta2d1(2)
      integer   status
      character table*32
      integer   tptr
      integer   tslot
      character type*8
      character type1*8
      integer   t1ptr
      integer   t1slot
      real      values(30)
      real      vmax
      real      vmin
      integer   wptr
      integer   wslot
      real      ximv(2)
      real      yimv(2)
c
      INCLUDE 'DYNAMIC_MEMORY'
      INCLUDE 'MAGIC_VALUES'
      INCLUDE 'NUMERIC_RANGES'
c
c   Initialize.
c
      status=0
      control='        '
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
c   Get current PGPLOT hardcopy device.
c
      call var_getchr('harddev',0,0,harddev,status)
      if(status.ne.0)then
        call dsa_wruser
     &    ('No hardcopy device selected - use HARD command.\\N')
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
      call dsa_data_size('image',2,ndim,dims,nelm,status)
      if(status.ne.0)go to 500
      if(ndim.lt.2)then
        call dsa_wruser('This is not a 2-D image.\\N')
        go to 500
      end if
c
c   Get coordinate range to be displayed.
c
      call ndp_axis_range
     &  ('image',dims,ndim,start,end,stapix,endpix,status)
      if(status.ne.0)go to 500
c
c   Set up dimensions and start and end coordinates in 2-D.
c
      do i=1,2
        dim2d(i)=dims(i)
        spx2d(i)=stapix(i)
        epx2d(i)=endpix(i)
        sta2d(i)=start(i)
        end2d(i)=end(i)
      end do
c
c   Quality, quality, quality street?
c
      call dsa_seek_quality('image',qual,status)
      if (status.ne.0) go to 500
c
c   Magic values are not to be removed from the data array if no quality
c   array is present
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
c   Like, map the quality array, man... if you *really* have to.
c
      if (qual) then
        call dsa_map_quality('image','read','byte',address,qslot,status)
        if (status.ne.0) go to 500
        qptr=dyn_element(address)
      end if
c
c   Set up data ranges according to type...
c
      if(type.eq.'SHORT')then
        vmin=real(min_short)
        vmax=real(max_short)
      else
        vmin=min_float
        vmax=max_float
      end if
c
c   Get range for plot
c
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
c   Allocate workspace for processing the pixel values
c
        bytes = nelm*dsa_typesize('int',status)
        call dsa_get_workspace(bytes,address,wslot,status)
        if (status .ne. 0) go to 500
        wptr = dyn_element(address)
c
c   Get instructions to plot a contour map of the same or another image.
c
      call par_rdkey('contour',.false.,contour)
      if(contour)then
c
c   - get name of image.
c
        call dsa_input('image1','image1',status)
        if(status.ne.0)go to 500
        call ndp_get_image_info
     &    ('image1',.true.,.false.,type1,badpix1,status)
        if(status.ne.0)go to 500
c
c   Any quality data in image1 ?
c
        call dsa_seek_quality('image1',qual1,status)
        if(status.ne.0)go to 500
        if (.not.qual1) then
          call dsa_use_flagged_values('image1',status)
          if(status.ne.0)go to 500
        end if
c
c   - get image dimensions.
c
        call dsa_data_size('image1',2,ndim1,dims1,nelm1,status)
        if(status.ne.0)go to 500
        if(ndim1.lt.2)then
          call dsa_wruser('This is not a 2-D image.\\N')
          go to 500
        end if
c
c   - get coordinate range to be displayed.
c
        call ndp_axis_range
     &    ('image1',dims1,ndim1,start,end,stapix,endpix,status)
        if(status.ne.0)go to 500
c
c   - set up dimensions and start and end coordinates in 2-D.
c
        do i=1,2
          dim2d1(i)=dims(i)
          spx2d1(i)=stapix(i)
          epx2d1(i)=endpix(i)
          sta2d1(i)=start(i)
          end2d1(i)=end(i)
        end do
c
c   - make sure the two sets of dimensions match.
c
        do i=1,2
          if((epx2d1(i)-spx2d1(i)).ne.(epx2d(i)-stapix(i)))then
            call dsa_wruser('The two sets of image dimensions ')
            call dsa_wruser('do not match.\\N')
            go to 500
          end if
        end do
c
c   Map data array as FLOAT (this is required by PGPLOT).  If the data
c   are already FLOAT then we need to copy the original data over to
c   a workspace.  I've explained why before...
c
        if (type1 .ne. 'FLOAT') then
          call dsa_map_data('image1','read','FLOAT',address,i1slot,
     :    status)
          if(status.ne.0)go to 500
          im1ptr=dyn_element(address)
        else
          bytes1 = nelm1*dsa_typesize('float',status)
          call dsa_get_workspace(bytes1,address,i1slot,status)
          im1ptr = dyn_element(address)
          call dsa_map_data('image1','read','float',address,t1slot,
     :    status)
          if (status .ne. 0) go to 500
          t1ptr = dyn_element(address)
          call gen_move(bytes1,dynamic_mem(t1ptr),dynamic_mem(im1ptr))
        end if
c
c   Map any quality data present
c
        if (qual1) then
          call dsa_map_quality('image1','read','byte',
     :                         address,q1slot,status)
          if (status.ne.0) go to 500
          q1ptr=dyn_element(address)
        end if
c
c   - Set up ranges according to type
c
        if(type.eq.'SHORT')then
          vmin=real(min_short)
          vmax=real(max_short)
        else
          vmin=min_float
          vmax=max_float
        end if
c
c   - get range for plot
c
        call par_rdval('low1',vmin,vmax,0.0,' ',low1)
        call par_rdval('high1',vmin,vmax,0.0,' ',high1)
c
c   - get number of contour levels and compute their values.
c
        call par_rdval('levels',1.0,30.0,10.0,' ',dumreal)
        levels=int(dumreal)
        if(levels.gt.1)then
          levinc=(high1-low1)/(real(levels)-1.0)
        else
          levinc=0.0
        end if
        dumreal=low1
        do i=1,levels
          values(i)=dumreal
          dumreal=dumreal+levinc
        end do
      end if
c
c   Get instruction to plot hardcopy.
c
      call par_rdkey('hardcopy',.false.,hard)
c
c   If using screen device -
c   - get name of LUT.
c
      if(.not.hard)then
        call par_rdchar('table',' ',table)
c
c   - get instruction to erase screen.
c
        call par_rdkey('erase',.false.,erase)
      end if
c
c   Open required plot device.
c
      if(.not.hard)then
        if(erase)then
          status=pgbegin(0,softdev,1,1)
        else
          status=pgbegin(0,softdev(:ich_len(softdev))//'/APPEND',1,1)
        end if
      else
        status=pgbegin(0,harddev,1,1)
      end if
      if(status.ne.1)go to 500
      status=0
c
c   If using screen device, load LUT.
c
      if(.not.hard)then
        call ndp_image_lut(table,status)
        if(status.ne.0)go to 500
      end if
c
c   Compute viewport for image.
c
      call ndp_image_viewport(spx2d,epx2d,mag,place,ximv,yimv,sqvp)
c
c   Display image, temporarily replacing magic values or bad quality
c   pixels if necessary.
c
      if (qual) then
        call dsa_wruser
     &    ('Temporarily replacing bad pixels in IMAGE...\\n')
        call ndp_replace_qual_r(dynamic_mem(imptr),dynamic_mem(qptr),
     &                          nelm,low-1.0)
      end if
      call ndp_image_index(dim2d(1)*dim2d(2),low,high,
     &                     dynamic_mem(imptr),badpix,
     &                     dynamic_mem(wptr),status)
      if (status .ne. 0) go to 500
      call ndp_image_plot
     &  (dynamic_mem(wptr),dim2d(1),dim2d(2),spx2d,epx2d,sta2d,end2d,
     &   high,low,label,control,ximv,yimv)
c
c   Contour image, temporarily replacing magic values if necessary.
c
      if(contour)then
        if(badpix1)then
          call dsa_wruser
     &      ('Temporarily replacing magic values in IMAGE1...\\N')
          call ndp_replace_magic_r(dynamic_mem(im1ptr),nelm1,
     &                             magic_float,low1-1.0)
        end if
        if (qual1) then
          call dsa_wruser
     &      ('Temporarily replacing bad pixels in IMAGE1...\\n')
          call ndp_replace_qual_r(dynamic_mem(im1ptr),
     &                            dynamic_mem(q1ptr),
     &                            nelm1,low1-1.0)
        end if
        call depict_contour
     &    (dynamic_mem(im1ptr),dim2d1(1),dim2d1(2),spx2d1,epx2d1,
     &     sta2d1,end2d1,values,levels,ximv,yimv)
      end if
c
c   Tidy and exit
c
  500 continue
      call pgend
      call dsa_close(status)
      end





      subroutine depict_contour
     &  (array,nx,ny,stapix,endpix,start,end,values,levels,ximv,yimv)
c
      implicit none
c
c   Parameters.
c
      integer       nx,ny,stapix(2),endpix(2),levels
      real          array(nx,ny),start(2),end(2),values(30),
     &              ximv(2),yimv(2)
c
c   Local variables.
c
      real          tran(6)
c
c   Define contour map viewport.
c
      call pgvsize(ximv(1),ximv(2),yimv(1),yimv(2))
c
c   Set world coordinates to axis units.
c
      call pgwindow(start(1),end(1),start(2),end(2))
c
c   Compute transformation array for contour plot. The world coordinates of the
c   array point ARRAY(I,J) are are given by:
c   X = TRAN(1) + TRAN(2)*I + TRAN(3)*J
c   Y = TRAN(4) + TRAN(5)*I + TRAN(6)*J
c
      tran(2)=(end(1)-start(1))/real(endpix(1)-stapix(1))
      tran(1)=start(1)-tran(2)*real(stapix(1))
      tran(3)=0.0
c
      tran(6)=(end(2)-start(2))/real(endpix(2)-stapix(2))
      tran(4)=start(2)-tran(6)*real(stapix(2))
      tran(5)=0.0
c
c   Contour image.
c
      call pgcont
     &  (array,nx,ny,stapix(1),endpix(1),stapix(2),endpix(2),
     &   values,levels,tran)
c
      end
