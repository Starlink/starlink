      subroutine addnd
C+
c
c   ---------
c   A D D N D
c   ---------
c
c   Description
c   -----------
c   Takes several  images and adds them together, shifting each of the images
c   by a prescribed amount along each dimension.
c
c   Program Scope
c   -------------
c   - FLOAT or REAL data handled explicitly, all others mapped to FLOAT
c   - Images of up to six dimensions handled.
c   - Subsetting not appropriate.
c   - Magic values, quality and error arrays supported.
c   - Batch execution supported.
c   - Linearly scaled axis arrays only allowed if USEAXES is .TRUE.
c
c   Environment
c   -----------
c   Figaro
c
c   Parameters
c   ----------
c   FILES	Name of a .DAT file containg input file names. (file)
c   OUTPUT	Name of the structure containing the resulting cube. (file)
c   SHIFTS      Name of a .DAT file containing the pixel shifts.
c   SMALL       Value of effective floating-point 0. (real, hidden)
c
c   Keywords
c   --------
c   VERBOSE  	Signals whether messages are terse (false) or long (hidden).
c   AVERAGE     If true, cubes are averaged rather than added.
c   USEAXES     If true, use cube axes to determine the shifts.
c
c   Propagation of Data Structure
c   -----------------------------
c   See method.
c
c   Method
c   ------
c
c   - The program obtains USEAXES to determine whether shift values are to
c     be computed or read from a file.
c   - The parameter FILES is obtained. This is an ASCII file containing the
c     names of the images to be co-added.
c   - The parameter SHIFTS is obtained if USEAXES is FALSE. This is an ASCII
c     file containing the relative pixel shifts of each of the images in the
c     input file.
c   - Each image is read in in turn, and the output file's properties are
c     determined from the information gathered in this first pass. The rules
c     are as follows:
c
c     o The output file size is the maximum in each dimension of the input
c       images.
c     o The output file type is determined by the type of the first file
c       in the input list (hereafter called the BASE file).
c     o If shifts are to be read from a file rather than computed, it is up
c       to the user to ensure that they do not exceed the output array
c       boundaries. (Although checking could be added, it would significantly
c       reduce performance.) All arrays are therefore treated as "elastic"
c       and will expand to whatever size is necessary.
c     o Each input file MUST have the same dimensionality as the base file.
c     o If USEAXES is true, ALL axes must be present in ALL the images.
c       Further, logarithmic axes are disallowed and axes with units other
c       than that of the base file also cause program abortion.
c     o All axis data must increase monotonically with index (if it doesn't,
c       use AXFLIP or SETAXES to make it so).
c     o All axis data must be on the same scale as the base file (ie the ratio
c
c                        (end - start + 1) / no.of pixels
c
c       must be the same for each respective axis.)
c     o Quality or error arrays will appear in the output iff each input
c       file contains them, otherwise they will be ignored. Similarly
c       the bad pixel flag will only be set in the output structure iff
c       all the input files have it set. This avoids conflicts with magic
c       values and quality arrays.
c
c   - Each file is then opened, and the appropriate arrays mapped. Errors
c     are mapped as variances to simplify the arithmetic. The arrays are then
c     co-added by treating all arrays as 6D (using dimension of 1 for each
c     unused dimension). If AVERAGE is true, a count is maintained for each
c     pixel in the image. The file is closed after use, freeing up the slot
c     for the next file.
c
c   - If AVERAGE is true, the mean pixel values are computed in the final
c     image.
c
c   - If USEAXES is true, the output axes are scaled accordingly.
c
c   External Functions & Subroutines
c   --------------------------------
c   DSA Library:
c     DSA_CLOSE
c     DSA_CLOSE_STRUCTURE
c     DSA_DATA_SIZE
c     DSA_DATA_TYPE
c     DSA_GET_AXIS_INFO
c     DSA_GET_LU
c     DSA_GET_WORKSPACE
c     DSA_INPUT
c     DSA_MAP_AXIS_DATA
c     DSA_MAP_DATA
c     DSA_MAP_QUALITY
c     DSA_MAP_VARIANCE
c     DSA_NAMED_INPUT
c     DSA_OPEN
c     DSA_OUTPUT
c     DSA_SEEK_AXIS
c     DSA_SEEK_ERRORS
c     DSA_SEEK_FLAGGED_VALUES
c     DSA_SEEK_QUALITY
c     DSA_SIMPLE_OUTPUT
c     DSA_TYPESIZE
c     DSA_UNMAP
c     DSA_WRUSER
c
c   GEN Library:
c     GEN_FILL
c
c   ICH Library:
c     ICH_CI
c     ICH_FOLD

c   NDP Library:
c     NDP_GET_IMAGE_INFO
c     NDP_PAR_RDARY
c
c   PAR Library:
c     PAR_CNPAR
c     PAR_GIVEN
c     PAR_RDCHAR
c     PAR_RDVAL
c
c   Internal Subroutines
c   --------------------
c
c     ADDND_ADD_W
c     ADDND_ADD_R
c     ADDND_GET_AXIS_INFO
c     ADDND_INFO
c     ADDND_SETAXES
c
c   INCLUDE's
c   ---------
c   INCLUDE 'DYNAMIC_MEMORY'
c   INCLUDE 'MAGIC_VALUES'
c   INCLUDE 'NUMERIC_RANGES'
c   INCLUDE 'QUALITY_MASK'
c
c   Fortran 77 Extensions
c   ---------------------
c   END DO / IMPLICIT NONE / INCLUDE / Names > 6 characters / DO WHILE
c
c   Author
c   ------
c   Julian Gold		RGO	(CAVAD::GOLDJIL or goldjil@uk.ac.cam.ast-star)
c
c   History
c   -------
c
c   14-MAY-1992		- Original program.
c   26-NOV-1992         - Unix version
C   06-OCT-1994         - Removed lots of unused variables. (GJP)
C
C+
c----------------------------------------------------------------------------
c
      implicit none
c
c   Functions
c
      integer           dyn_element
      integer           dsa_typesize
      character         ich_ci*8
      integer           ich_len

c
c   Parameters
c
      integer           FMAX            ! Maximum number of files to add
      parameter         (FMAX=16)       !
      real              FPZERO          ! Absolute smallest REAL
      parameter         (FPZERO=1.0E-7) !
      integer           EOF             ! End Of File value
      parameter         (EOF=(-1))      !
c
c   Loadsa variables
c
      integer           address         ! Address of dynamic memory element
      logical           average         ! Add or average the cubes
      logical           axis            ! Any axes in cubes?
      real              axend(FMAX,10)  ! Axis end values
      real              axmin(10)       ! Minimum axis values
      integer           axptr           ! Pointer to i/p axis array
      integer           axslot          ! Slot number for i/p axis
      real              axstart(FMAX,10)! Axis start values
      logical           bad             ! Bad pixel flag for file
      logical           badpix          ! Bad pixel flag for o/p file
      integer           bdims(10)       ! Dimensions of 'base' file
      real              bstep(10)       ! Grid size of base axes
      character         btype*16        ! Data type of 'base' file
      character         bunits(10)*32   ! Units of 'base' file
      character         ctemp*32        ! Temporary character variable
      integer           dims(FMAX,10)   ! Dimensions of images
      logical           done            ! All files done with?
      logical           err             ! Error flag for file
      logical           error           ! Error flag for cube
      character         filenames*80    ! Name of input file
      integer           i               ! Loop counter
      integer           ieptr           ! Dynamic pointer to i/p errors
      integer           ieslot          ! Slot number for i/p errors
      character         image(FMAX)*64  ! File names
      character         info*32         ! String for DSA_SIMPLE_OUTPUT
      integer           inptr           ! Dynamic pointer to input structure
      integer           inslot          ! Slot number for input structure
      integer           iqptr           ! Dynamic pointer to i/p quality
      integer           iqslot          ! Slot number for i/p quality
      integer           j               ! Another loop counter
      double precision  logaxis         ! Logarithmically scaled axis flag
      integer           file_lu         ! Logical unit number for input data
      integer           my_status       ! My very own status variable
      integer           nbad            ! No of files with bad pixels
      integer           nbdim           ! No of dimesions in base cube
      integer           ndim(FMAX)      ! Number of dimensions in cube
      integer           nelm(FMAX)      ! No. of elements in cube
      integer           nerr            ! No of files with error arrays
      integer           nfiles          ! No of files to process
      integer           nqual           ! No of files with quality arrays
      integer           noelm           ! No. of elements in output cube
      integer           nshifts         ! No. of shift values processed
      integer           obytes          ! No. of bytes in OUTPUT data
      integer           odims(10)       ! Output dimensions
      integer           oeptr           ! Dynamic pointer to o/p errors
      integer           oeslot          ! Slot number for o/p errors
      integer           oqptr           ! Dynamic pointer to output quality
      integer           oqslot          ! Slot number for o/p quality
      integer           outptr          ! Dynamic pointer to output structure
      character         output*64       ! Output structure file name
      integer           oslot           ! Slot number for output structure
      logical           qual            ! Quality flag for file
      logical           quality         ! Quality flag for cube
      real              rtemp1,rtemp2   ! Temporary reals
      integer           shift(FMAX,10)  ! Pixel shifts for cubes
      character         shiftname*64    ! Name for shift file
      integer           shift_lu        ! Logical unit number for shift file
      real              small           ! Effective floating-point 0.
      integer           status          ! DSA status variable
      real              step            ! Grid size of an axis array
      logical           struct          ! For DSA_DATA_TYPE
      integer           tempdims(10)    ! Temporary array for dimensions
      character         type*32         ! Array data type
      character         units*32        ! Units string
      logical           useaxes         ! If true, determine shift from axes
      logical           verbose         ! Verbose mode
      integer           wptr            ! Dynamic pointer to (work) count array
      integer           wslot           ! Slot number for count array
c
      include 'DYNAMIC_MEMORY'
      include 'MAGIC_VALUES'
      include 'NUMERIC_RANGES'
c
c   Initialise
c
      status = 0
      call dsa_open(status)
      if (status .ne. 0) go to 999
c
c   Get hidden parameters
c

      call par_rdkey('verbose',.false.,verbose)
c
      call par_rdval('small',0.0,0.1,FPZERO,' ',small)
c
c   Determine if the shifts are to be computed automatically
c
      call par_rdkey('useaxes',.true.,useaxes)
c
c   Are files to be averaged or added?
c
      call par_rdkey('average',.false.,average)
c
c   Open file containing image names
c
      call dsa_get_lu(file_lu,status)
      call par_rdchar('files','files.dat',filenames)
      call ich_fold(filenames)
      open(unit=file_lu,file=filenames,status='old',iostat=status)
      if (status .ne. 0) then
        call dsa_wruser('%ADDND-E-BADFILE  ')
        call dsa_wruser('Error opening image-name file.\\n')
        go to 999
      end if
c
c   Open file containing shifts if needed
c
      if (.not.useaxes) then
        call dsa_get_lu(shift_lu,status)
        call par_rdchar('shifts','shifts.dat',shiftname)
        call ich_fold(shiftname)
        open(unit=shift_lu,file=shiftname,status='old',iostat=status)
        if (status .ne. 0) then
          call dsa_wruser('%ADDND-E-BADSHFT  ')
          call dsa_wruser('Error opening shift file.\\n')
          go to 999
        end if
      end if
c
c   Initialise some property variables
c
      nqual = 0
      nerr = 0
      nbad = 0
      nfiles = 0
c
      do j = 1,10
        do i = 1,FMAX
          dims(i,j) = 1
          shift(i,j) = 0
        end do
        odims(j) = 1
      end do
c
      done = .false.
c
      do while(.not.done)
        nfiles = nfiles + 1
        if (nfiles .gt. FMAX) then
          call dsa_wruser('%ADDND-E-BADCNT  ')
          call dsa_wruser('Too many files in data file. ')
          call dsa_wruser('Maximum is '//ich_ci(FMAX)//'.\\n')
          go to 999
        end if
c
        read(file_lu,'(A)',iostat=status) image(nfiles)
        if (status .eq. EOF) then
          done = .true.
          nfiles = nfiles - 1
          if (nfiles .eq. 1) then
            call dsa_wruser('%ADDND-E-BADCNT  ')
            call dsa_wruser('At least 2 files are needed.\\n')
            go to 999
          end if
          status = 0
c
        else if (status .ne. 0) then
          call dsa_wruser('%ADDND-E-IOERR  ')
          call dsa_wruser('I/O error on data file.\\n')
          go to 999
c
        else
c
          call dsa_named_input('image',image(nfiles),status)
          if (status.ne.0) go to 999
          call dsa_data_size('image',10,ndim(nfiles),tempdims,
     &                        nelm,status)
          do i = 1,ndim(nfiles)
            dims(nfiles,i) = tempdims(i)
          end do
c
          call dsa_data_type('image',type,struct,status)
          call ich_fold(type)

c
          call addnd_info(image(nfiles),ndim,tempdims,type)
c
c   Set initial output dimensions and data type
c
          if (nfiles .eq. 1) then
            nbdim = ndim(1)
            do i = 1,nbdim
              bdims(i) = dims(1,i)
              odims(i) = dims(1,i)
            end do
c
            btype = type
            if (btype.ne.'SHORT') then
              btype = 'FLOAT'
            else
              if (verbose) then
                call dsa_wruser('%ADDND-W-RNDERR  ')
                call dsa_wruser('Data type SHORT may incur rounding')
                call dsa_wruser(' error.\\n')
              end if
            end if
c
          else ! nfiles > 1
c
            if (ndim(nfiles) .ne. nbdim) then
              call dsa_wruser('%ADDND-E-BADDIM  ')
              call dsa_wruser('This file has the wrong number')
              call dsa_wruser(' of dimensions.\\n')
              go to 999
            end if
c
c   Update output file size
c
            do i = 1,nbdim
              odims(i) = max(odims(i),dims(nfiles,i))
            end do
c
          end if ! (nfiles...)
c
c    Get axis data if required for shifts
c
          if (useaxes) then
            do i = 1,nbdim
              call dsa_seek_axis('image',i,axis,status)
              if (.not.axis) then
                call dsa_wruser('%ADDND-E-NOAXIS  ')
                call dsa_wruser('Dimension #'//ich_ci(i))
                call dsa_wruser(' has no axis data.\\n')
                go to 999
              else
                call dsa_get_axis_info('image',i,1,units,1,
     &                                  logaxis,status)
                call ich_fold(units)
c
                if (nfiles .eq. 1) then
                  bunits(i) = units
                else
                  if (units .ne. bunits(i)) then
                    call dsa_wruser('%ADDND-E-BADUNIT  ')
                    call dsa_wruser('Inconsistent units in axis #')
                    call dsa_wruser(ich_ci(i)//'.\\n')
                    go to 999
                  end if ! (units...)
                end if ! (nfiles...)
c
                if (logaxis.ne.0.0D0) then
                  call dsa_wruser('%ADDND-E-LOGAXIS  ')
                  call dsa_wruser('No logarithmic axes allowed!\\n')
                  go to 999
                end if
c
c   Map axes and extract information
c
                call dsa_map_axis_data('image',i,'read','float',
     &                                  address,axslot,status)
                if (status .ne. 0) go to 999
                axptr = dyn_element(address)
                call addnd_get_axis_info(dynamic_mem(axptr),
     &                                   dims(nfiles,i),
     &                                   rtemp1,rtemp2,
     &                                   my_status)
                if (my_status .ne. 0) then
                  call dsa_wruser('%ADDND-E-REVAXIS  ')
                  call dsa_wruser('Axis #'//ich_ci(i))
                  call dsa_wruser(' is reversed. Use AXFLIP first.\\n')
                  go to 999
                end if
c
                step = (rtemp2 - rtemp1 + 1.0) / dims(nfiles,i)
                axstart(nfiles,i) = rtemp1
                axend(nfiles,i) = rtemp2
                if (nfiles .eq. 1) then
                  axmin(i) = rtemp1
                  bstep(i) = step
                else
                  if (abs(step-bstep(i)) .gt. small) then
                    call dsa_wruser('%ADDND-E-BADSCALE  ')
                    call dsa_wruser('Axis #'//ich_ci(i))
                    call dsa_wruser(' is scaled badly.\\n')
                    go to 999
                  end if
                  axmin(i) = min(axmin(i),rtemp1)
                end if
c
                call dsa_unmap(axslot,status)
              end if ! (.not.axis...)
            end do ! i
          end if ! (useaxes)
c
c   Get quality/error array info
c
          call dsa_seek_quality('image',qual,status)
          if (qual) then
            nqual = nqual + 1
          else
            call dsa_seek_flagged_values('image',bad,status)
            if (bad) nbad = nbad + 1
          end if
          call dsa_seek_errors('image',err,status)
          if (err) nerr = nerr + 1
c
c   Ditch the file temporarily
c
          call dsa_close_structure('image',status)
          if (status.ne.0) go to 999
c
        end if
c
      end do ! while
      close(unit=file_lu,iostat=status)
c
c   Get shift values from file if needed
c
      if (.not. useaxes) then
        nshifts = 0
        done = .false.
        do while (.not.done)
          nshifts = nshifts + 1
          read(shift_lu,*,iostat=status)
     &                        (shift(nshifts,i), i=1,nbdim)
          if (status .eq. EOF) then
            nshifts = nshifts - 1
            if (nshifts .ne. nfiles) then
              call dsa_wruser('%ADDND-E-BADSHFT ')
              call dsa_wruser
     &                 ('Wrong number of shift values in file.\\n')
              go to 999
            end if
            done = .true.
            status = 0
          else
            if (status .ne. 0) then
              call dsa_wruser('%ADDND-E-IOERR ')
              call dsa_wruser('I/O error reading shift file.\\n')
              go to 999
            end if ! (status.ne.0)
          end if ! (status.eq.EOF)
        end do ! while
        close(unit=shift_lu,iostat=status)
      end if ! (.not.useaxes)
c
c   Process the quality and error information
c
      quality = (nqual .eq. nfiles)
      if (quality.and.verbose) then
        call dsa_wruser('Output file will have a quality array\\n')
      end if
c
      error = (nerr .eq. nfiles)
      if (error.and.verbose) then
        call dsa_wruser('Output file will have an error array\\n')
      end if
c
      badpix = (nbad .eq. nfiles)
      if (badpix.and.verbose) then
        call dsa_wruser('Output file will contain magic values\\n')
      end if
c
      if ((nbad .gt. 0).and.(nbad .lt. nfiles).and.(nqual .ne. 0)) then
        call dsa_wruser('%ADDND-E-BADPIX Quality/magic value')
        call dsa_wruser(' conflict.\\n')
        call dsa_wruser('You can''t have magic values and quality ')
        call dsa_wruser('arrays in the same structure.\\n')
        call dsa_wruser('Use UNMAGIC to get rid of magic values.\\n')
        go to 999
      end if
c
      if (verbose) then
        call dsa_wruser('Output file will be of type ')
        call dsa_wruser(btype(:ich_len(btype))//'\\n')
      end if
c
c   Process the axis information into shifts
c
      if (useaxes) then
        do i = 1,nfiles
          do j = 1,nbdim
            shift(i,j) = int((axstart(i,j)-axmin(j))/bstep(j))
          end do
        end do
      end if
c
c   Compute final output dimensions
c
      do i = 1,nbdim
        do j = 1,nfiles
          odims(i) = max(odims(i),dims(j,i)+shift(j,i))
        end do
      end do
c
      if (verbose) then
        call dsa_wruser('Output file will have dimensions: ')
        do i = 1,nbdim-1
          ctemp = ich_ci(odims(i))
          call dsa_wruser(ctemp(:ich_len(ctemp))//' x ')
        end do
        ctemp = ich_ci(odims(nbdim))
        call dsa_wruser(ctemp(:ich_len(ctemp))//'\\n')
      end if
c
c   Get name of the output file. Check that it ISN'T in the input list.
c
      call par_rdchar('output',' ',output)
      call ich_fold(output)
      do i = 1,nfiles
        if (output .eq. image(i)) then
          call dsa_wruser('%ADDND-E-BADOUT  ')
          call dsa_wruser('Output file / input file conflict.\\n')
          go to 999
        end if
      end do
c
      call dsa_output('output','output',' ',1,1,status)
      if (status .ne. 0) go to 999
c
c   Now construct the output file
c
      info = 'd'
c
c   - add error, quality and axis arrays if appropriate
c
      if (quality) info = info // ',q'
      if (error)   info = info // ',e'
c
      do i = 1,nbdim
        info = info // ',a'//ich_ci(i)
      end do
c
      call dsa_simple_output('output',info,btype,nbdim,
     &                        odims,status)
      if (status .ne. 0) go to 999
c
c   - compute number of bytes in an output array
c
      noelm = 1
      do i = 1,nbdim
        noelm = noelm * odims(i)
      end do
      obytes = noelm * dsa_typesize(btype,status)
c
c   Allocate workspace for averaging if needed and clear
c
      if (average) then
        call dsa_get_workspace(noelm,address,wslot,status)
        if (status .ne. 0) go to 999
        wptr = dyn_element(address)
        call gen_fill(noelm,0,dynamic_mem(wptr))
      end if
c
c   - map output array and zero
c
      call dsa_map_data('output','write',btype,address,
     &                   oslot,status)
      if (status .ne. 0) go to 999
      outptr = dyn_element(address)
      call gen_fill(obytes,0,dynamic_mem(outptr))
c
c   - map output error array and zero
c
      if (error) then
        call dsa_map_errors('output','write',btype,address,
     &                       oeslot,status)
        if (status .ne. 0) go to 999
        oeptr = dyn_element(address)
        call gen_fill(obytes,0,dynamic_mem(oeptr))
      end if
c
c   - map output quality array and zero
c
      if (quality) then
        call dsa_map_quality('output','write','byte',
     &                        address,oqslot,status)
        if (status .ne. 0) go to 999
        oqptr = dyn_element(address)
        call gen_fill(noelm,0,dynamic_mem(oqptr))
      end if
c
c   Now we can start to add the cubes
c
      do i=1,nfiles
c
        call dsa_named_input('image',image(i),status)
        if (status.ne.0) go to 999
c
        if (verbose) then
          ctemp = image(i)
          call dsa_wruser('Processing file ')
          call dsa_wruser(ctemp(:ich_len(ctemp))//'...\\n')
        end if
c
c   Map the input file arrays
c
        call dsa_map_data('image','read',btype,address,
     &                     inslot,status)
        if (status .ne. 0) go to 999
        inptr = dyn_element(address)
        if (error) then
c
c   Mapping errors as variances simplifies the error arithmetic
c
          call dsa_map_variance('image','read',btype,address,
     &                           ieslot,status)
          if (status .ne. 0) go to 999
          ieptr = dyn_element(address)
        end if
c
        if (quality) then
          call dsa_map_quality('image','read','byte',address,
     &                          iqslot,status)
          if (status .ne. 0) go to 999
          iqptr = dyn_element(address)
        end if
c
c   Add the cube to the total
c
        if (btype .eq. 'SHORT') then
          if (badpix) then
            call addnd_wq(
     &           dynamic_mem(inptr),dims(i,1),dims(i,2),
     &           dims(i,3),dims(i,4),dims(i,5),dims(i,6),
     &           dynamic_mem(outptr),odims(1),odims(2),
     &           odims(3),odims(4),odims(5),odims(6),
     &           error,dynamic_mem(ieptr),dynamic_mem(oeptr),
     &           magic_short,shift(i,1),shift(i,2),shift(i,3),
     &           shift(i,4),shift(i,5),shift(i,6),
     &           average,dynamic_mem(wptr))
          else
            call addnd_w(
     &          dynamic_mem(inptr),dims(i,1),dims(i,2),
     &          dims(i,3),dims(i,4),dims(i,5),dims(i,6),
     &          dynamic_mem(outptr),odims(1),odims(2),
     &          odims(3),odims(4),odims(5),odims(6),
     &          error,dynamic_mem(ieptr),dynamic_mem(oeptr),
     &          quality,dynamic_mem(iqptr),dynamic_mem(oqptr),
     &          shift(i,1),shift(i,2),shift(i,3),
     &          shift(i,4),shift(i,5),shift(i,6),
     &          average,dynamic_mem(wptr))
          end if ! (badpix)
        else ! btype must be FLOAT
          if (badpix) then
            call addnd_rq(
     &           dynamic_mem(inptr),dims(i,1),dims(i,2),
     &           dims(i,3),dims(i,4),dims(i,5),dims(i,6),
     &           dynamic_mem(outptr),odims(1),odims(2),
     &           odims(3),odims(4),odims(5),odims(6),
     &           error,dynamic_mem(ieptr),dynamic_mem(oeptr),
     &           magic_float,shift(i,1),shift(i,2),shift(i,3),
     &           shift(i,4),shift(i,5),shift(i,6),
     &           average,dynamic_mem(wptr))
          else
            call addnd_r(
     &          dynamic_mem(inptr),dims(i,1),dims(i,2),
     &          dims(i,3),dims(i,4),dims(i,5),dims(i,6),
     &          dynamic_mem(outptr),odims(1),odims(2),
     &          odims(3),odims(4),odims(5),odims(6),
     &          error,dynamic_mem(ieptr),dynamic_mem(oeptr),
     &          quality,dynamic_mem(iqptr),dynamic_mem(oqptr),
     &          shift(i,1),shift(i,2),shift(i,3),
     &          shift(i,4),shift(i,5),shift(i,6),
     &          average,dynamic_mem(wptr))
            end if ! (badpix)
        end if ! (btype...)
c
c   Now we can unmap the cube and free the slot for the next one
c
        call dsa_unmap(inslot,status)
        if (quality) call dsa_unmap(iqslot,status)
        if (error) call dsa_unmap(ieslot,status)
        call dsa_close_structure('image',status)
c
      end do
c
c   Now we can compute the average if requested
c
      if (average) then
        if (verbose) call dsa_wruser('Averaging images.\\n')
        if (btype .eq. 'SHORT') then
          if (.not. badpix) then
            call addnd_mean_w(dynamic_mem(outptr),noelm,
     &                        quality,dynamic_mem(oqptr),
     &                        error,dynamic_mem(oeptr),
     &                        dynamic_mem(wptr))
          else
            call addnd_mean_wq(dynamic_mem(outptr),noelm,
     &                         magic_short,
     &                         error,dynamic_mem(oeptr),
     &                         dynamic_mem(wptr))
          end if ! (.not. badpix)
       else
          if (.not. badpix) then
            call addnd_mean_r(dynamic_mem(outptr),noelm,
     &                        quality,dynamic_mem(oqptr),
     &                        error,dynamic_mem(oeptr),
     &                        dynamic_mem(wptr))
          else
            call addnd_mean_rq(dynamic_mem(outptr),noelm,
     &                         error,dynamic_mem(oeptr),
     &                         magic_float,
     &                         dynamic_mem(wptr))
          end if ! (.not. badpix)
        end if ! (btype ...)
      end if ! (average)
c
c   Set the bad pixel flag in OUTPUT if called for
c
      if (badpix) call dsa_set_flagged_values('output',.true.,status)
c
c   Finally, set the output axes if required
c
      if (useaxes) then
        do i = 1,nbdim
          call dsa_map_axis_data('output',i,'write','float',
     &                            address,axslot,status)
          if (status .ne. 0) go to 999
          axptr = dyn_element(address)
          call addnd_setaxes(dynamic_mem(axptr),odims(i),
     &                       axmin(i),bstep(i))
          call dsa_unmap(axslot,status)
        end do
      end if
c
999   continue
      call dsa_close(status)
      end

*******************************************************************************

      subroutine addnd_info(name,ndim,dims,type)
      integer    ndim
      integer    dims(10)
      character  type*(*)
      character  name*(*)
c
      integer    i
      character  ctemp*32
c
      character  ich_ci*8
      integer    ich_len
c
      call ich_fold(name)
      call dsa_wruser(' \\n')
      call dsa_wruser('File name: ')
      call dsa_wruser(name(:ich_len(name))//'.DST\\n')
      call dsa_wruser('File dimensions: ')
      do i = 1,ndim-1
         ctemp = ich_ci(dims(i))
        call dsa_wruser(ctemp(:ich_len(ctemp))//' x ')
      end do
      ctemp = ich_ci(dims(ndim))
      call dsa_wruser(ctemp(:ich_len(ctemp))//'\\n')
      call dsa_wruser('File data type: '//type(:ich_len(type)))
      call dsa_wruser('.\\n')
      call dsa_wruser(' \\n')
      return
      end

*******************************************************************************

      subroutine addnd_get_axis_info(axis,nelm,min,max,status)
      integer    nelm
      real       axis(nelm)
      real       min,max
      integer    status             ! *NOT* DSA status.
c
      integer    ADDND_E_REVAXIS
      parameter  (ADDND_E_REVAXIS=1)
c
      status = 0
      min = axis(1)
      max = axis(nelm)
      if (min.ge.max) status = ADDND_E_REVAXIS
      return
      end

*******************************************************************************

      subroutine addnd_setaxes(axis,nelm,axmin,step)
      integer    nelm
      real       axis(nelm)
      real       step
c
      integer    i
c
      do i = 1,nelm
        axis(i) = axmin + real(i-1)*step
      end do
c
      return
      end
