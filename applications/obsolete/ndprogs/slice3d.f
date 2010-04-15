      SUBROUTINE SLICE3D
C+
C
C   -------------
C   S L I C E 3 D
C   -------------
C
C   Description
C   -----------
C   SLICE3D will take a 3-D datacube and extract/display any rectangular subset
C   thereof. It has a simple graphic interface to simplify the choice of
C   extraction plane. The extracted data (plus quality and errors) can be
C   optionally written to a new structure.
C
C   Scope of program
C   ----------------
C   - Handles 3-D images only.
C   - Data array types SHORT and FLOAT supported - others mapped to FLOAT.
C   - Subsetting irrelevant.
C   - Magic values, error and quality arrays supported.
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
C   OUTPUT   Name of file the slice is to be written to. (character)
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
C   TABLE    Name of colour or grey scale lookup table. (character)
C            (prompted for).
C
C   SOFTDEV  Current screen device name (character)(read from file).
C
C   VIEW     Axis to view cube along. (integer)
C
C   ACTION   Allows user to try another slice or exit at will (character).
C
C   Keywords
C   --------
C   AXES     Instruction to plot calibrated axes. Otherwise, the image is
C            framed with a plain box.
C   RAMP     Instruction to plot a calibrated bar of the colour or grey
C            scale to the right of the image.
C
C   ERASE    Instruction to erase screen before plotting.
C
C   VERBOSE  Make the program garrulous in the extreme. (Hidden)
C
C   WRITE    Instruction to write the slice to disk.
C
C   Propagation of data structure
C   -----------------------------
C   Not relevant.
C
C
C   Method
C   ------
C   - The program obtains the name of an image. This MUST be a 3D image.
C   - Information about data type, error arrays etc is obtained.
C   - The view axis is obtained from the user. The cube is then collapsed
C     (summed) over that axis.
C   - The collapsed cube is then plotted. The plotting is handled similarly
C     to (cf) DEPICT.
C   - The user can then select 2 points using the cursor which define the
C     slice.
C   - The display is cleared and the slice is plotted.
C   - If the user is happy with this, he/she can save the slice to a file.
C   - The user then has the option of quitting or slicing again. If the
C     latter, all output and slice arrays are unmapped.
C
C   External functions & subroutines called
C   ---------------------------------------
C   Library DSA:
C     DSA_CLOSE
C     DSA_DATA_SIZE
C     DSA_FREE_WORKSPACE
C     DSA_GET_WORKSPACE
C     DSA_GET_WORK_ARRAY
C     DSA_INPUT
C     DSA_MAP_DATA
C     DSA_MAP_ERRORS
C     DSA_MAP_QUALITY
C     DSA_OPEN
C     DSA_TYPESIZE
C     DSA_SEEK_ERRORS
C     DSA_SEEK_QUALITY
C     DSA_UNMAP
C     DSA_USE_FLAGGED_VALUES
C     DSA_WRUSER
C
C   Library DYN:
C     DYN_ELEMENT
C
C   Library GEN:
C     GEN_ELEMF
C     GEN_MOVE
C
C   Library ICH:
C     ICH_CI
C     ICH_FOLD
C     ICH_LEN
C
C   Library NDP:
C     NDP_GET_IMAGE_INFO
C     NDP_IMAGE_INDEX
C     NDP_IMAGE_LUT
C     NDP_IMAGE_PLOT
C     NDP_IMAGE_VIEWPORT
C
C   Library PAR:
C     PAR_CNPAR
C     PAR_RDCHAR
C     PAR_RDKEY
C     PAR_RDVAL
C
C   Library VAR:
C     VAR_GETCHR
C
C   Starlink PGPLOT:
C     PGADVANCE
C     PGBEGIN
C     PGCONT
C     PGEND
C     PGPOINT
C     PGVSIZE
C     PGWINDOW
C
C
C   Internal subroutines called
C   ---------------------------
C   CRUSH_AXIS1
C   CRUSH_AXIS2
C   CRUSH_AXIS3
C   CRUSH_AXIS1Q
C   CRUSH_AXIS2Q
C   CRUSH_AXIS3Q
C   SLICE_AXIS1
C   SLICE_AXIS2
C   SLICE_AXIS3
C   SLICE_COPY_W
C   SLICE_COPY_R
C
C   INCLUDE statements
C   ------------------
C   INCLUDE 'DYNAMIC_MEMORY'
C   INCLUDE 'NUMERIC_RANGES'
C   INCLUDE 'MAGIC_VALUES'
C
C
C   Extensions to FORTRAN77
C   -----------------------
C   END DO / IMPLICIT NONE / INCLUDE / Names > 6 characters  / WHILE
C
C
C   Possible future upgrades
C   ------------------------
C   Have the slice and original view displayed simultaneously.
C
C   Author/s
C   --------
C   Julian Gold  RGO  (CAVAD::GOLDJIL or GOLDJIL@UK.AC.CAM.AST-STAR)
C
C
C   History
C   -------
C   22-JUN-1992   - Original program (GOLDJIL)
C   04-DEC-1992   - Unix version.
C   06-OCT-1994   - Removed lots of unused variables. (GJP)
C
C
C+-----------------------------------------------------------------------------
c
      implicit none
c
c   Functions.
c
      integer   dyn_element,ich_len,pgbegin,dsa_typesize
      real      gen_elemf
      character ich_ci*3
c
c   Local variables.
c
      character action*16
      integer   address
      integer   ax1,ax2
      logical   axes
      integer   axptr(3)
      integer   axslot(3)
      logical   badpix
      integer   bytes
      character control*8
      integer   dims(10)
      logical   done
      integer   dumint
      real      dumreal
      integer   dx,dy
      real      end(6)
      integer   epix(6)
      logical   erase
      logical   err
      real      high
      integer   i
      integer   ieptr
      integer   ieslot
      integer   imptr
      integer   ind1,ind2
      character info*16
      integer   iqptr
      integer   iqslot
      integer   islot
      character label*128
      real      low
      real      mag
      integer   ndim
      integer   nelm
      integer   nelm1
      integer   nelm2
      integer   nxpix,nypix
      integer   oaxptr(2)
      integer   oaxslot(2)
      integer   odims(2)
      integer   oeptr
      integer   oeslot
      integer   optr
      integer   oqptr
      integer   oqslot
      integer   oslot
      character place*2
      logical   qual
      logical   quit
      logical   ramp
      integer   sldptr
      integer   sldslot
      integer   sleptr
      integer   sleslot
      integer   slqptr
      integer   slqslot
      real      smax(6)
      real      smin(6)
      character softdev*16
      real      sqvp
      integer   spix(6)
      real      start(6)
      integer   status
      character table*32
      character type*8
      integer   t1ptr
      integer   t1slot
      integer   vaxis
      logical   verbose
      real      vmax
      real      vmin
      integer   wptr
      logical   write
      integer   wslot
      integer   x1,x2
      real      ximv(2)
      real      xlast
      real      xr
      integer   y1,y2
      real      yimv(2)
      real      ylast
      real      yr
c
      include 'DYNAMIC_MEMORY'
      include 'NUMERIC_RANGES'
      include 'MAGIC_VALUES'
c
c  Initialize.
c
      status=0
      control='        '
      done = .false.
c
c  Open DSA system.
c
      call dsa_open(status)
      if(status.ne.0)go to 999
c
c  Get hidden parameters(s)
c
      call par_rdkey('verbose',.false.,verbose)
c
c  Get current PGPLOT screen device.
c
      call var_getchr('softdev',0,0,softdev,status)
      if(status.ne.0)then
        call dsa_wruser('%SLICE3D-E-NOSOFT  ')
        call dsa_wruser
     &    ('No screen device selected - use SOFT command.\\N')
        go to 999
      end if
c
c  Get name of image.
c
      call dsa_input('image','image',status)
      if (status .ne. 0) go to 999
      call ndp_get_image_info('image',.true.,.false.,type,badpix,status)
      if (status .ne. 0) go to 999
c
c  Get dimensions of image.
c
      call dsa_data_size('image',3,ndim,dims,nelm,status)
      if (status .ne. 0) go to 999
      if(ndim .ne. 3)then
        call dsa_wruser('%SLICE3D-E-NOT3D  ')
        call dsa_wruser('This is not a 3-D image.\\N')
        go to 999
      end if
c
c  Get co-ord ranges
c
      do i = 1,3
        call dsa_map_axis_data('image',i,'read','float',address,
     &                          axslot(i),status)
        if (status .ne. 0) go to 999
        axptr(i) = dyn_element(address)
        smin(i) = gen_elemf(dynamic_mem(axptr(i)),1)
        smax(i) = gen_elemf(dynamic_mem(axptr(i)),dims(i))
      end do
c
c  Quality, quality, quality street?
c
      call dsa_seek_quality('image',qual,status)
      if (status .ne. 0) go to 999
c
c  Might as well have a look for error arrays while we're at it (oo-er missus)
c
      call dsa_seek_errors('image',err,status)
      if (status .ne. 0) go to 999
c
c  Map data array as FLOAT (this is required by PGPLOT).
c
        call dsa_map_data('image','read','FLOAT',address,islot,status)
        if ( status .ne. 0) go to 999
        imptr=dyn_element(address)
c
c  Like, map the quality array, man... if you *really* have to.
c
      if (qual) then
        call dsa_map_quality('image','read','byte',
     &                        address,iqslot,status)
        if (status.ne.0) go to 999
        iqptr=dyn_element(address)
      end if
c
c  Oh, and the error array I suppose...
c
      if (err) then
        call dsa_map_errors('image','read',type,
     &                       address,ieslot,status)
        if (status .ne. 0) go to 999
        ieptr = dyn_element(address)
      end if
c
c  Set up data ranges according to type...
c
      if (type .eq. 'SHORT') then
        vmin=real(min_short)
        vmax=real(max_short)
      else
        vmin=min_float
        vmax=max_float
      end if
c
c  Main loop
c
      do while (.not. done)
c
c  Get 'redundant' axis to view along
C
        call par_cnpar('view')
        call par_rdval('view',1.0,3.0,3.0,' ',dumreal)
        vaxis = int(dumreal)
c
c  Allocate workspace
c
        if (vaxis .eq. 1) then
          nelm1 = dims(2) * dims(3)
        else if (vaxis .eq. 2) then
          nelm1 = dims(1) * dims(3)
        else
          nelm1 = dims(1) * dims(2)
        end if
        call dsa_get_work_array(nelm1,'float',address,t1slot,status)
        if (status .ne. 0) go to 999
        t1ptr = dyn_element(address)
c
c  Get range for plot
c
        call par_cnpar('low')
        call par_rdval('low',vmin,vmax,0.0,' ',low)
        call par_cnpar('high')
        call par_rdval('high',vmin,vmax,0.0,' ',high)
c
c  Collapse array
c
        if (verbose) then
          call dsa_wruser('%SLICE3D-I-VAXIS  ')
          call dsa_wruser('Re-orienting view axis...\\n')
        end if

        if (badpix) then
          if (vaxis .eq. 1) then
            call crush_axis1q(dynamic_mem(imptr),dims(1),dims(2),
     &                        dims(3),dynamic_mem(t1ptr),
     &                        magic_float,low)
          else if (vaxis .eq. 2) then
            call crush_axis2q(dynamic_mem(imptr),dims(1),dims(2),
     &                        dims(3),dynamic_mem(t1ptr),
     &                        magic_float,low)
          else
            call crush_axis3q(dynamic_mem(imptr),dims(1),dims(2),
     &                        dims(3),dynamic_mem(t1ptr),
     &                        magic_float,low)
          end if
        else
          if (vaxis .eq. 1) then
            call crush_axis1(dynamic_mem(imptr),dims(1),dims(2),
     &                       dims(3),dynamic_mem(t1ptr),
     &                       qual,dynamic_mem(iqptr),low)
          else if (vaxis .eq. 2) then
            call crush_axis2(dynamic_mem(imptr),dims(1),dims(2),
     &                       dims(3),dynamic_mem(t1ptr),
     &                       qual,dynamic_mem(iqptr),low)
          else
            call crush_axis3(dynamic_mem(imptr),dims(1),dims(2),
     &                       dims(3),dynamic_mem(t1ptr),
     &                       qual,dynamic_mem(iqptr),low)
          end if
        end if
c
c  Get image viewport location.
c
        call par_cnpar('place')
        call par_rdchar('place',' ',place)
c
c  Get magnification factor.
c
        call par_cnpar('mag')
        call par_rdval('mag',0.1,1.0,1.0,' ',mag)
c
c  Set label for image.
c
        label = 'View along axis '//ich_ci(vaxis)
c
c  Get instruction to plot axes.
c
        call par_cnpar('axes')
        call par_rdkey('axes',.true.,axes)
        if (axes) control(ich_len(control)+1:)='A'
c
c  Get instruction to plot ramp.
c
        call par_cnpar('ramp')
        call par_rdkey('ramp',.true.,ramp)
        if (ramp) control(ich_len(control)+1:)='R'
c
c  Allocate workspace for processing the pixel values
c
        bytes = nelm1*dsa_typesize('int',status)
        call dsa_get_workspace(bytes,address,wslot,status)
        if (status .ne. 0) go to 999
        wptr = dyn_element(address)
c
c
c  Get name of LUT.
c
        call par_cnpar('table')
        call par_rdchar('table',' ',table)
c
c  Get instruction to erase screen.
c
        call par_cnpar('erase')
        call par_rdkey('erase',.false.,erase)
c
c  Open required plot device.
c
        if(erase)then
          status=pgbegin(0,softdev,1,1)
        else
          status=pgbegin(0,softdev(:ich_len(softdev))//'/APPEND',1,1)
        end if
        if (status .ne. 1) go to 999
        status=0
c
c  If using screen device, load LUT.
c
        call ndp_image_lut(table,status)
        if(status.ne.0)go to 999
c
c  Orient axes and compute viewport for image.
c
        do i = 1,2
          spix(i) = 1
        end do

        if (vaxis .eq. 1) then
          ind1 = 2
          ind2 = 3
        else if (vaxis .eq. 2) then
          ind1 = 1
          ind2 = 3
        else
          ind1 = 1
          ind2 = 2
        end if

        epix(1) = dims(ind1)
        epix(2) = dims(ind2)
        start(1) = smin(ind1)
        start(2) = smin(ind2)
        end(1) = smax(ind1)
        end(2) = smax(ind2)
        ax1 = axptr(ind1)
        ax2 = axptr(ind2)

        call ndp_image_viewport(spix,epix,mag,place,ximv,yimv,sqvp)
c
c  Display image
c
        call ndp_image_index(epix(1)*epix(2),low,high,
     &                       dynamic_mem(t1ptr),badpix,
     &                       dynamic_mem(wptr),status)
        if (status .ne. 0) go to 999
        call ndp_image_plot
     &    (dynamic_mem(wptr),epix(1),epix(2),spix,epix,start,end,
     &     high,low,label,control,ximv,yimv)
c
c  Now get the user to specify the slice points (returned as PIXELS)
c
        call dsa_wruser
     &   ('Define the slice by selecting two points.\\n')
        call ndp_image_cursor(dynamic_mem(ax1),dynamic_mem(ax2),
     &                        dims(ind1),dims(ind2),
     &                        start,end,spix,epix,
     &                        1,'CSDP',ximv,yimv,xr,yr,dumint,
     &                        xlast,ylast,quit)
        x1 = int(xr)
        y1 = int(yr)
        call pgpoint(1,gen_elemf(dynamic_mem(ax1),x1),
     &                 gen_elemf(dynamic_mem(ax2),y1),5)
        call ndp_image_cursor(dynamic_mem(ax1),dynamic_mem(ax2),
     &                        dims(ind1),dims(ind2),
     &                        start,end,spix,epix,
     &                        1,'SDP',ximv,yimv,xr,yr,dumint,
     &                        xlast,ylast,quit)
        x2 = int(xr)
        y2 = int(yr)
        call pgpoint(1,gen_elemf(dynamic_mem(ax1),x2),
     &                 gen_elemf(dynamic_mem(ax2),y2),5)
c
c  Compute slice details and perform the operation (will he live, doc?)
c
        dx = x2 - x1
        dy = y2 - y1
        nxpix = max(dx,dy)+1
        nypix = dims(vaxis)
        nelm2 = nxpix * nypix

        if (verbose) then
          call dsa_wruser('%SLICE3D-I-SLICING  ')
          call dsa_wruser('Performing slice.\\n')
          call dsa_wruser('Slice dimensions: ')
          call dsa_wruser(ich_ci(nxpix)//' x '//ich_ci(nypix)//'\\n')
        end if

        call dsa_get_work_array(nelm2,'float',address,sldslot,status)
        if (status .ne. 0) go to 999
        sldptr = dyn_element(address)

        if (err) then
          call dsa_get_work_array(nelm2,'float',address,
     &                            sleslot,status)
          if (status .ne. 0) go to 999
          sleptr = dyn_element(address)
        end if

        if (qual) then
          call dsa_get_work_array(nelm2,'byte',address,
     &                            slqslot,status)
          if (status .ne. 0) go to 999
          slqptr = dyn_element(address)
        end if

        if (vaxis .eq. 1) then
          call slice_axis1(dynamic_mem(imptr),dims(1),dims(2),
     &                     dims(3),dynamic_mem(sldptr),
     &                     err,dynamic_mem(ieptr),dynamic_mem(sleptr),
     &                     qual,dynamic_mem(iqptr),dynamic_mem(slqptr),
     &                     x1,y1,x2,y2,nxpix,nypix)
        else if (vaxis .eq. 2) then
          call slice_axis2(dynamic_mem(imptr),dims(1),dims(2),
     &                     dims(3),dynamic_mem(sldptr),
     &                     err,dynamic_mem(ieptr),dynamic_mem(sleptr),
     &                     qual,dynamic_mem(iqptr),dynamic_mem(slqptr),
     &                     x1,y1,x2,y2,nxpix,nypix)
        else
          call slice_axis3(dynamic_mem(imptr),dims(1),dims(2),
     &                     dims(3),dynamic_mem(sldptr),
     &                     err,dynamic_mem(ieptr),dynamic_mem(sleptr),
     &                     qual,dynamic_mem(iqptr),dynamic_mem(slqptr),
     &                     x1,y1,x2,y2,nxpix)
        end if
c
c  Unmap temporary stuff and re-allocate workspace
c
        call dsa_free_workspace(wslot,status)
        call dsa_get_work_array(nelm2,'int',address,wslot,status)
        if (status .ne. 0) go to 999
c
c  Plot slice and free up the temporary slot
c
        epix(1) = nxpix
        epix(2) = nypix
        start(1) = 1.0
        start(2) = gen_elemf(dynamic_mem(axptr(vaxis)),1)
        end(1) = real(nxpix)
        end(2) = gen_elemf(dynamic_mem(axptr(vaxis)),dims(vaxis))

        label = 'Slice'

        call ndp_image_viewport(spix,epix,mag,place,ximv,yimv,sqvp)
        call ndp_image_index(epix(1)*epix(2),low,high,
     &                       dynamic_mem(sldptr),badpix,
     &                       dynamic_mem(wptr),status)
        if (status .ne. 0) go to 999
        call pgadvance ! Clear the screen
        call ndp_image_plot(dynamic_mem(wptr),epix(1),epix(2),
     &                      spix,epix,start,end,high,low,label,
     &                      control,ximv,yimv)
        call dsa_free_workspace(wslot,status)
c
c  What to do next?
c
        call par_cnpar('write')
        call par_rdkey('write',.true.,write)
        if (write) then
          call par_cnpar('output')
          call dsa_output('output','output',' ',1,1,status)
          if (status .ne. 0) go to 999

          info = 'd,a1,a2'
          if (qual) info = info // ',q'
          if (err) info = info // ',e'
          odims(1) = nxpix
          odims(2) = nypix
          call dsa_simple_output('output',info,type,2,odims,status)
          if (status .ne. 0) go to 999

          call dsa_map_data('output','write',type,
     &                       address,oslot,status)
          if (status .ne. 0) go to 999
          optr = dyn_element(address)

          if (err) then
            call dsa_map_errors('output','write',type,
     &                           address,oeslot,status)
            if (status .ne. 0) go to 999
            oeptr = dyn_element(address)
          end if

          if (qual) then
            call dsa_map_quality('output','write','byte',
     &                            address,oqslot,status)
            if (status .ne. 0) go to 999
            oqptr = dyn_element(address)
          end if

          do i = 1,2
            call dsa_map_axis_data('output',i,'write','float',
     &                              address,oaxslot(i),status)
            if (status .ne. 0) go to 999
            oaxptr(i) = dyn_element(address)
          end do
c
c  Copy the required information to the output structure.
c
          if (verbose) then
            call dsa_wruser('%SLICE3D-I-COPYING  ')
            call dsa_wruser('Copying slice data to file...\\n')
          end if

          if (type .eq. 'SHORT') then
            call slice_copy_w(
     &                 dynamic_mem(sldptr),nxpix,nypix,
     &                 dynamic_mem(optr),
     &                 err,dynamic_mem(sleptr),dynamic_mem(oeptr),
     &                 qual,dynamic_mem(slqptr),dynamic_mem(oqptr),
     &                 dynamic_mem(oaxptr(1)),dynamic_mem(oaxptr(2)),
     &                 start,end)
          else
            call slice_copy_r(
     &                 dynamic_mem(sldptr),nxpix,nypix,
     &                 dynamic_mem(optr),
     &                 err,dynamic_mem(sleptr),dynamic_mem(oeptr),
     &                 qual,dynamic_mem(slqptr),dynamic_mem(oqptr),
     &                 dynamic_mem(oaxptr(1)),dynamic_mem(oaxptr(2)),
     &                 start,end)
          end if ! (type...)
        end if ! (write)

c
c   Find out what to do now - try again or exit?
c
        call par_cnpar('action')
        call par_rdchar('action','Q',action)
        call ich_fold(action)
        if (action(1:1) .eq. 'Q') then
          done = .true.
        else
          if (write) then
            call dsa_unmap(oaxslot(1),status)
            call dsa_unmap(oaxslot(2),status)
            call dsa_unmap(oslot,status)
            if (err) then
              call dsa_unmap(oeslot,status)
              call dsa_unmap(sleslot,status)
            end if
            if (qual) then
              call dsa_unmap(oqslot,status)
              call dsa_unmap(slqslot,status)
            end if
            if (status .ne. 0) go to 999
          end if
        end if
      end do ! while
c
c   Tidy and exit
c
  999 continue
      call pgend
      call dsa_close(status)
      end

*******************************************************************************

      subroutine crush_axis1(array3d,nx,ny,nz,array2d,qual,qarray,low)
c------------------------------------------------------------------------------
c     Description:
c       Crush cube along axis 1.
c     Parameters:
c       > array3d              The original cube (real 3D array)
c       > nx,ny,nz             Cube dimensions (integers)
c       < array2d              Collapsed cube (real 2D array)
c       > qual                 Quality array flag (logical)
c       > qarray               Quality array (3D byte array)
c       > low                  Default lowest plot value to insert (real)
c------------------------------------------------------------------------------
      integer    nx,ny,nz
      real       array3d(nx,ny,nz)
      real       array2d(ny,nz)
      logical    qual
      byte       qarray(nx,ny,nz)
c
      integer    i,j,k
      real       sum
      integer    valid
c
      do k = 1,nz
        do j = 1,ny
          sum = 0.0
          valid = 0
          do i = 1,nx
            if (qual) then
              if (qarray(i,j,k) .eq. 0) then
                sum = sum + array3d(i,j,k)
                valid = valid + 1
              end if
            else
              sum = sum + array3d(i,j,k)
              valid = valid + 1
            end if
          end do
          if (valid .gt. 0) then
            array2d(j,k) = sum / real(valid)
          else
            array2d(j,k) = low-1.0
          end if
        end do
      end do
      return
      end

*******************************************************************************

      subroutine crush_axis1q(array3d,nx,ny,nz,array2d,magic,low)
c------------------------------------------------------------------------------
c     Description:
c       Crush cube along axis 1. Magic value version
c     Parameters:
c       > array3d              The original cube (real 3D array)
c       > nx,ny,nz             Cube dimensions (integers)
c       < array2d              Collapsed cube (real 2D array)
c       > magic                Magic value (real)
c       > low                  Default lowest plot value to insert (real)
c------------------------------------------------------------------------------
      integer    nx,ny,nz
      real       array3d(nx,ny,nz)
      real       array2d(ny,nz)
      real       magic
c
      integer    i,j,k
      real       sum
      integer    valid
c
      do k = 1,nz
        do j = 1,ny
          sum = 0.0
          valid = 0
          do i = 1,nx
            if (array3d(i,j,k) .gt. magic) then
              sum = sum + array3d(i,j,k)
              valid = valid + 1
            end if
          end do
          if (valid .gt. 0) then
            array2d(j,k) = sum / real(valid)
          else
            array2d(j,k) = low-1.0
          end if
        end do
      end do
      return
      end

*******************************************************************************

      subroutine crush_axis2(array3d,nx,ny,nz,array2d,qual,qarray,low)
c------------------------------------------------------------------------------
c     Description:
c       Crush cube along axis 2.
c     Parameters:
c       > array3d              The original cube (real 3D array)
c       > nx,ny,nz             Cube dimensions (integers)
c       < array2d              Collapsed cube (real 2D array)
c       > qual                 Quality array flag (logical)
c       > qarray               Quality array (3D byte array)
c       > low                  Default lowest plot value to insert (real)
c------------------------------------------------------------------------------
      integer    nx,ny,nz
      real       array3d(nx,ny,nz)
      real       array2d(nx,nz)
      logical    qual
      byte       qarray(nx,ny,nz)
c
      integer    i,j,k
      real       sum
      integer    valid
c
      do k = 1,nz
        do i = 1,nx
          sum = 0.0
          valid = 0
          do j = 1,ny
            if (qual) then
              if (qarray(i,j,k) .eq. 0) then
                sum = sum + array3d(i,j,k)
                valid = valid + 1
              end if
            else
              sum = sum + array3d(i,j,k)
              valid = valid + 1
            end if
          end do
          if (valid .gt. 0) then
            array2d(i,k) = sum / real(valid)
          else
            array2d(i,k) = low-1.0
          end if
        end do
      end do
      return
      end

*******************************************************************************

      subroutine crush_axis2q(array3d,nx,ny,nz,array2d,magic,low)
c------------------------------------------------------------------------------
c     Description:
c       Crush cube along axis 2. Magic value version
c     Parameters:
c       > array3d              The original cube (real 3D array)
c       > nx,ny,nz             Cube dimensions (integers)
c       < array2d              Collapsed cube (real 2D array)
c       > magic                Magic value (real)
c       > low                  Default lowest plot value to insert (real)
c------------------------------------------------------------------------------
      integer    nx,ny,nz
      real       array3d(nx,ny,nz)
      real       array2d(nx,nz)
      real       magic
c
      integer    i,j,k
      real       sum
      integer    valid
c
      do k = 1,nz
        do i = 1,nx
          sum = 0.0
          valid = 0
          do j = 1,ny
            if (array3d(i,j,k) .gt. magic) then
              sum = sum + array3d(i,j,k)
              valid = valid + 1
            end if
          end do
          if (valid .gt. 0) then
            array2d(i,k) = sum / real(valid)
          else
            array2d(i,k) = low-1.0
          end if
        end do
      end do
      return
      end

*******************************************************************************

      subroutine crush_axis3(array3d,nx,ny,nz,array2d,qual,qarray,low)
c------------------------------------------------------------------------------
c     Description:
c       Crush cube along axis 3.
c     Parameters:
c       > array3d              The original cube (real 3D array)
c       > nx,ny,nz             Cube dimensions (integers)
c       < array2d              Collapsed cube (real 2D array)
c       > qual                 Quality array flag (logical)
c       > qarray               Quality array (3D byte array)
c       > low                  Default lowest plot value to insert (real)
c------------------------------------------------------------------------------
      integer    nx,ny,nz
      real       array3d(nx,ny,nz)
      real       array2d(nx,ny)
      logical    qual
      byte       qarray(nx,ny,nz)
c
      integer    i,j,k
      real       sum
      integer    valid
c
      do j = 1,ny
        do i = 1,nx
          sum = 0.0
          valid = 0
          do k = 1,nz
            if (qual) then
              if (qarray(i,j,k) .eq. 0) then
                sum = sum + array3d(i,j,k)
                valid = valid + 1
              end if
            else
              sum = sum + array3d(i,j,k)
              valid = valid + 1
            end if
          end do
          if (valid .gt. 0) then
            array2d(i,j) = sum / real(valid)
          else
            array2d(i,j) = low-1.0
          end if
        end do
      end do
      return
      end

*******************************************************************************

      subroutine crush_axis3q(array3d,nx,ny,nz,array2d,magic,low)
c------------------------------------------------------------------------------
c     Description:
c       Crush cube along axis 3. Magic value version
c     Parameters:
c       > array3d              The original cube (real 3D array)
c       > nx,ny,nz             Cube dimensions (integers)
c       < array2d              Collapsed cube (real 2D array)
c       > magic                Magic value (real)
c       > low                  Default lowest plot value to insert (real)
c------------------------------------------------------------------------------
      integer    nx,ny,nz
      real       array3d(nx,ny,nz)
      real       array2d(nx,ny)
      real       magic
c
      integer    i,j,k
      real       sum
      integer    valid
c
      do j = 1,ny
        do i = 1,nx
          sum = 0.0
          valid = 0
          do k = 1,nz
            if (array3d(i,j,k) .gt. magic) then
              sum = sum + array3d(i,j,k)
              valid = valid + 1
            end if
          end do
          if (valid .gt. 0) then
            array2d(i,j) = sum / real(valid)
          else
            array2d(i,j) = low-1.0
          end if
        end do
      end do
      return
      end

*******************************************************************************

      subroutine slice_axis1(array3d,nx,ny,nz,slice,
     &                       err,error3d,error2d,
     &                       qual,qual3d,qual2d,
     &                       y1,z1,y2,z2,
     &                       nyzpix,nxpix)
c------------------------------------------------------------------------------
c     Description:
c       Slices the 3-D array as requested by the user. This is done by
c       computing the pixels lying between the specified start and end points
c       in the viewed (y-z) plane.
c     Parameters:
c       > array3d           3-D datacube (real array)
c       > nx,ny,nz          Datacube dimensions (integers)
c       < slice             2-D slice (real array)
c       > err               Error array flag (logical)
c       > error3d           3-D error array (real array)
c       < error2d           2-D slice error array (real array)
c       > qual              Quality array flag (logical)
c       > qual3d            3-D quality array (byte array)
c       < qual2d            Slice quality array (2-D byte array)
c       > y1,z1,y2,z2       Pixel co-ords of slice start and end points
c       > nyzpix,nxpix      Dimensions of slice
c------------------------------------------------------------------------------
      integer    nx,ny,nz,nyzpix,nxpix
      real       array3d(nx,ny,nz),error3d(nx,ny,nz)
      real       slice(nxpix,nyzpix),error2d(nxpix,nyzpix)
      integer    y1,z1,y2,z2
      logical    err,qual
      byte       qual3d(nx,ny,nz),qual2d(nxpix,nyzpix)
c
      real       dy,dz
      real       yr,zr
      real       y1r,z1r
      integer    i,j
      real       s
c
      dy = real(y2 - y1)
      dz = real(z2 - z1)
      y1r = real(y1)
      z1r = real(z1)

      do j = 1,nxpix
        do i = 1,nyzpix
          s = real(i-1) / real(nyzpix-1)
          yr = y1r + s*dy
          zr = z1r + s*dz
          slice(j,i) = array3d(j,int(yr),int(zr))
          if (err) error2d(j,i) = error3d(j,int(yr),int(zr))
          if (qual) qual2d(j,i) = qual3d(j,int(yr),int(zr))
        end do
      end do

      return
      end

*******************************************************************************

      subroutine slice_axis2(array3d,nx,ny,nz,slice,
     &                       err,error3d,error2d,
     &                       qual,qual3d,qual2d,
     &                       x1,z1,x2,z2,
     &                       nxzpix,nypix)
c------------------------------------------------------------------------------
c     Description:
c       Slices the 3-D array as requested by the user. This is done by
c       computing the pixels lying between the specified start and end points
c       in the viewed (x-z) plane.
c     Parameters:
c       > array3d           3-D datacube (real array)
c       > nx,ny,nz          Datacube dimensions (integers)
c       < slice             2-D slice (real array)
c       > err               Error array flag (logical)
c       > error3d           3-D error array (real array)
c       < error2d           2-D slice error array (real array)
c       > qual              Quality array flag (logical)
c       > qual3d            3-D quality array (byte array)
c       < qual2d            Slice quality array (2-D byte array)
c       > y1,z1,y2,z2       Pixel co-ords of slice start and end points
c       > nxzpix,nypix      Dimensions of slice
c------------------------------------------------------------------------------
      integer    nx,ny,nz,nxzpix,nypix
      real       array3d(nx,ny,nz)
      real       slice(nxzpix,nypix)
      integer    x1,z1,x2,z2
      byte       qual3d(nx,ny,nz),qual2d(nxzpix,nypix)
      real       error3d(nx,ny,nz),error2d(nxzpix,nypix)
      logical    err,qual
c
      real       dx,dz
      real       xr,zr
      real       x1r,z1r
      integer    i,j
      real       s
c
      dx = real(x2 - x1)
      dz = real(z2 - z1)
      x1r = real(x1)
      z1r = real(z1)

      do j = 1,nypix
        do i = 1,nxzpix
          s = real(i-1) / real(nxzpix-1)
          xr = x1r + s*dx
          zr = z1r + s*dz
          slice(i,j) = array3d(int(xr),j,int(zr))
          if (err) error2d(i,j) = error3d(int(xr),j,int(zr))
          if (qual) qual2d(i,j) = qual3d(int(xr),j,int(zr))
        end do
      end do

      return
      end

*******************************************************************************

      subroutine slice_axis3(array3d,nx,ny,nz,slice,
     &                       err,error3d,error2d,
     &                       qual,qual3d,qual2d,
     &                       x1,y1,x2,y2,nxypix)
c------------------------------------------------------------------------------
c     Description:
c       Slices the 3-D array as requested by the user. This is done by
c       computing the pixels lying between the specified start and end points
c       in the viewed (x-y) plane.
c     Parameters:
c       > array3d           3-D datacube (real array)
c       > nx,ny,nz          Datacube dimensions (integers)
c       < slice             2-D slice (real array)
c       > err               Error array flag (logical)
c       > error3d           3-D error array (real array)
c       < error2d           2-D slice error array (real array)
c       > qual              Quality array flag (logical)
c       > qual3d            3-D quality array (byte array)
c       < qual2d            Slice quality array (2-D byte array)
c       > x1,y1,x2,y2       Pixel co-ords of slice start and end points
c       > nxypix            Dimension of slice in slicing plane
c------------------------------------------------------------------------------
      integer    nx,ny,nz,nxypix
      real       array3d(nx,ny,nz)
      real       slice(nxypix,nz)
      integer    x1,y1,x2,y2
      byte       qual3d(nx,ny,nz),qual2d(nxypix,nz)
      real       error3d(nx,ny,nz),error2d(nxypix,nz)
      logical    err,qual
c
      real       dy,dx
      real       yr,xr
      real       y1r,x1r
      integer    i,j
      real       s
c
      dy = real(y2 - y1)
      dx = real(x2 - x1)
      y1r = real(y1)
      x1r = real(x1)

      do j = 1,nz
        do i = 1,nxypix
          s = real(i-1) / real(nxypix-1)
          xr = x1r + s*dx
          yr = y1r + s*dy
          slice(i,j) = array3d(int(xr),int(yr),j)
          if (err) error2d(i,j) = error3d(int(xr),int(yr),j)
          if (qual) qual2d(i,j) = qual3d(int(xr),int(yr),j)
        end do
      end do

      return
      end

