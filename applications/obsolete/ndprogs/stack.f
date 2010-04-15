      subroutine stack
C+
c
c   ---------
c   S T A C K
c   ---------
c
c   Description
c   -----------
c   Takes several 2 OR 3D images and joins them along a prescribed dimension
c   either contiguously or using the axis data if present.
c
c   Program Scope
c   -------------
c   - FLOAT or REAL data handled explicitly, all others mapped to FLOAT
c   - Images of 2 or 3 dimensions handled.
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
c   JOIN        Dimension images are to be stacked along. (integer)
c   OUTPUT	Name of the structure containing the resulting cube. (file)
c   PADVAL      Value to pad arrays out with. (real)
c   SMALL       Value of effective floating-point 0. (real, hidden)
c
c   Keywords
c   --------
c   USEAXES     If true, use cube axes to determine the stacking position.
c   VERBOSE  	Signals whether messages are terse (false) or long (hidden).
c
c   Propagation of Data Structure
c   -----------------------------
c   See method.
c
c   Method
c   ------
c
c   - The program obtains USEAXES to determine how to join the cubes. If
c     this is true, the data will be placed in the output image according
c     to axis information. Otherwise, the cubes are simply butted together.
c   - The parameter FILES is obtained. This is an ASCII file containing the
c     names of the images to be stacked.
c   - The axis to join the images along is obtained.
c   - A value to pad arrays out with is obtained.
c   - Each image is read in in turn, and the output file's properties are
c     determined from the information gathered in this first pass. The rules
c     are as follows:
c
c     o Only 2 or 3 dimensions allowed.
c
c     o The output file size depends upon USEAXES: if we first consider the
c       dimension cubes are to be joined along -
c
c       if (USEAXES) then
c         output_dimension = (max_axis-min_axis) / axis_scale  + 1
c       else
c         output_dimension = sum of input_dimensions
c       end if.
c
c       (Note that this implies overlapping cubes will replace values
c       in the output obtained from previous files, so the order in the
c       list of filenames is important if USEAXES is true.)
c
c       Along other dimensions -
c
c       output_dimension = maximum of input_dimensions
c
c       Arrays will be padded out with value of PADVAL and left-justified.
c
c     o The output file type is determined by the type of the first file
c       in the input list (hereafter called the BASE file). This also
c       applies to axis units and scale.
c
c     o If USEAXES is true, ALL axes must be present in ALL the images.
c       Further, logarithmic axes are disallowed and axes with units other
c       than that of the base file also cause program abortion.
c
c     o All axis data must increase monotonically with index (if it doesn't,
c       use AXFLIP or SETAXES to make it so).
c
c     o All axis data must be on the same scale as the base file (ie the ratio
c
c                        (end - start + 1) / no.of pixels
c
c       must be the same for each respective axis.)
c
c     o Quality or error arrays will appear in the output iff each input
c       file contains them, otherwise they will be ignored. Similarly
c       the bad pixel flag will only be set in the output structure iff
c       all the input files have it set. This avoids conflicts with magic
c       values and quality arrays.
c
c     o The output file CANNOT be in the input list as well, because
c       this could lead to the program obtaining the wrong data from the
c       wrong version of the file (or an i/o error).
c
c   - The output file is mapped and filled with the padding value.
c   - Each image is then opened, and the appropriate arrays mapped. The arrays
c     are then copied to the appropriate locations in the output.
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
c     DSA_INPUT
c     DSA_MAP_AXIS_DATA
c     DSA_MAP_DATA
c     DSA_MAP_ERRORS
c     DSA_MAP_QUALITY
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
c     ICH_LEN
c
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
c     STACK_FILL_R
c     STACK_FILL_W
c     STACK_GET_AXIS_INFO
c     STACK_INFO
c     STACK_OVERLAP
c     STACK_R
c     STACK_SETAXES
c     STACK_W
c
c   INCLUDE's
c   ---------
c   INCLUDE 'DYNAMIC_MEMORY'
c   INCLUDE 'MAGIC_VALUES'
c   INCLUDE 'NUMERIC_RANGES'
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
c   21-MAY-1992		- Original program.
c   07-DEC-1992         - Unix version.
C   06-OCT-1994   - Removed lots of unused variables. (GJP)
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
      logical           stack_overlap
c
c   Parameters
c
      integer           FMAX            ! Maximum number of files to add
      parameter         (FMAX=16)       !
      real              FPZERO          ! Absolute smallest REAL
      parameter         (FPZERO=1.0E-7) !
      integer           EOF             ! End Of File value
      parameter         (EOF=(-1))      !
      real              MINPAD,MAXPAD   ! Minimum/maximum array pad values
      parameter         (MINPAD=-32767.0)
      parameter         (MAXPAD=32767.0)
c
c   Loadsa variables
c
      integer           address         ! Address of dynamic memory element
      logical           axis            ! Any axes in cubes?
      real              axend(FMAX,10)  ! Axis end values
      real              axmax(10)       ! Maximum axis values
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
      real              dumreal         ! Dummy real value
      logical           err             ! Error flag for input image
      logical           error           ! Error flag for output cube
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
      integer           jdim            ! Axis to join along
      double precision  logaxis         ! Logarithmically scaled axis flag
      integer           file_lu         ! Logical unit number for input data
      integer           my_status       ! My very own status variable
      integer           nbad            ! No of files with bad pixels
      integer           ndim(FMAX)      ! Number of dimensions in cube
      integer           nelm(FMAX)      ! No. of elements in cube
      integer           nerr            ! No of files with error arrays
      integer           nfiles          ! No of files to process
      integer           nqual           ! No of files with quality arrays
      integer           noelm           ! No. of elements in output cube
      integer           obytes          ! No. of bytes in OUTPUT data
      integer           odims(10)       ! Output dimensions
      integer           oeptr           ! Dynamic pointer to o/p errors
      integer           oeslot          ! Slot number for o/p errors
      integer           oqptr           ! Dynamic pointer to output quality
      integer           oqslot          ! Slot number for o/p quality
      integer           outptr          ! Dynamic pointer to output structure
      character         output*64       ! Output structure file name
      integer           oslot           ! Slot number for output structure
      real              padval          ! Value to pad arrays out with
      logical           qual            ! Quality flag for file
      logical           quality         ! Quality flag for output cube
      real              rtemp1,rtemp2   ! Temporary reals
      real              small           ! Effective floating-point 0.
      integer           start(FMAX)     ! Start indices for cubes along join
      integer           status          ! DSA status variable
      real              step            ! Grid size of an axis array
      logical           struct          ! For DSA_DATA_TYPE
      integer           tempdims(10)    ! Temporary array for dimensions
      character         type*32         ! Array data type
      character         units*32        ! Units string
      logical           useaxes         ! If true, determine stacking from axes
      logical           verbose         ! Verbose mode
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
c   Determine if the stacking position is to be computed automatically
c
      call par_rdkey('useaxes',.true.,useaxes)
c
c   Get axis to join images along
c
      call par_rdval('join',1.0,3.0,3.0,' ',dumreal)
      jdim = int(dumreal)
c
c   Get array padding value
c
      call par_rdval('padval',MINPAD,MAXPAD,0.0,' ',padval)
c
c   Open file containing image names
c
      call dsa_get_lu(file_lu,status)
      call par_rdchar('files','files.dat',filenames)
      call ich_fold(filenames)
      open(unit=file_lu,file=filenames,status='old',iostat=status)
      if (status .ne. 0) then
        call dsa_wruser('%STACK-E-BADFILE  ')
        call dsa_wruser('Error opening image-name file.\\n')
        go to 999
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
        end do
        odims(j) = 1
      end do
c
      done = .false.
c
      do while(.not.done)
        nfiles = nfiles + 1
        if (nfiles .gt. FMAX) then
          call dsa_wruser('%STACK-E-BADCNT  ')
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
            call dsa_wruser('%STACK-E-BADCNT  ')
            call dsa_wruser('At least 2 files are needed.\\n')
            go to 999
          end if
          status = 0
c
        else if (status .ne. 0) then
          call dsa_wruser('%STACK-E-IOERR  ')
          call dsa_wruser('I/O error on data file.\\n')
          go to 999
c
        else
c
          call dsa_named_input('image',image(nfiles),status)
          if (status.ne.0) go to 999
          call dsa_data_size('image',10,ndim(nfiles),tempdims,
     &                        nelm,status)
c
          if ((ndim(nfiles).gt.3).or.(ndim(nfiles).eq.1)) then
            call dsa_wruser('%STACK-E-BADDIM  ')
            call dsa_wruser('Images can be 2D or 3D only.\\n')
            go to 999
          end if
c
          do i = 1,ndim(nfiles)
            dims(nfiles,i) = tempdims(i)
          end do
c
          call dsa_data_type('image',type,struct,status)
          call ich_fold(type)

c
          call stack_info(image(nfiles),tempdims,type)
c
c   Set initial output dimensions and data type
c
          if (nfiles .eq. 1) then
            do i = 1,3
              bdims(i) = dims(1,i)
              odims(i) = dims(1,i)
            end do
c
            btype = type
            if (btype.ne.'SHORT') btype = 'FLOAT'
c
          else ! nfiles > 1
c
c   Update output file size
c
            do i = 1,3
              if (i .ne. jdim) then
                odims(i) = max(odims(i),dims(nfiles,i))
              else
                if (.not.useaxes) then
                  odims(i) = odims(i) + dims(nfiles,i)
                end if
              end if
            end do
c
          end if ! (nfiles...)
c
c    Get axis data if required for stacking
c
          if (useaxes) then
            do i = 1,3
              call dsa_seek_axis('image',i,axis,status)
              if (.not.axis) then
                call dsa_wruser('%STACK-E-NOAXIS  ')
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
                    call dsa_wruser('%STACK-E-BADUNIT  ')
                    call dsa_wruser('Inconsistent units in axis #')
                    call dsa_wruser(ich_ci(i)//'.\\n')
                    go to 999
                  end if ! (units...)
                end if ! (nfiles...)
c
                if (logaxis.ne.0.0D0) then
                  call dsa_wruser('%STACK-E-LOGAXIS  ')
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
                call stack_get_axis_info(dynamic_mem(axptr),
     &                                   dims(nfiles,i),
     &                                   rtemp1,rtemp2,
     &                                   my_status)
                if (my_status .ne. 0) then
                  call dsa_wruser('%STACK-E-REVAXIS  ')
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
                  axmax(i) = rtemp2
                  bstep(i) = step
                else
                  if (abs(step-bstep(i)) .gt. small) then
                    call dsa_wruser('%STACK-E-BADSCALE  ')
                    call dsa_wruser('Axis #'//ich_ci(i))
                    call dsa_wruser(' is scaled badly.\\n')
                    go to 999
                  end if
                  axmin(i) = min(axmin(i),rtemp1)
                  axmax(i) = max(axmax(i),rtemp2)
                end if
c
c   Check and warn for overlapping axes
c
                if (verbose) then
                  if (stack_overlap(jdim,nfiles,axstart,
     &                                      axend,FMAX)) then
                    call dsa_wruser('%STACK-W-OVERLAP  ')
                    call dsa_wruser('  Overlapping axis data.\\n')
                  end if
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
        call dsa_wruser('%STACK-E-BADPIX Quality/magic value')
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
c   Process the information for the common dimension into an output size
c   and start indices.
c
      if (useaxes) then
        odims(jdim) = int((axmax(jdim)-axmin(jdim))/bstep(jdim))+1
c
        do i = 1,nfiles
          start(i) = int((axstart(i,jdim)-axmin(jdim))/bstep(jdim))+1
        end do
c
      else
c
        start(1) = 1
        do i = 2,nfiles
          start(i) = start(i-1) + dims(i-1,jdim)
        end do
      end if
c
      if (verbose) then
        call dsa_wruser('Output file will have dimensions: ')
        do i = 1,2
          ctemp = ich_ci(odims(i))
          call dsa_wruser(ctemp(:ich_len(ctemp))//' x ')
        end do
        ctemp = ich_ci(odims(3))
        call dsa_wruser(ctemp(:ich_len(ctemp))//'\\n')
      end if
c
c   Get name of the output file. Check that it ISN'T in the input list.
c
      call par_rdchar('output',' ',output)
      call ich_fold(output)
      do i = 1,nfiles
        if (output .eq. image(i)) then
          call dsa_wruser('%STACK-E-BADOUT  ')
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
      do i = 1,3
        info = info // ',a'//ich_ci(i)
      end do
c
      call dsa_simple_output('output',info,btype,3,
     &                        odims,status)
      if (status .ne. 0) go to 999
c
c   - compute number of bytes in an output array
c
      noelm = 1
      do i = 1,3
        noelm = noelm * odims(i)
      end do
      obytes = noelm * dsa_typesize(btype,status)
c
c   - map output array and fill with padding values
c
      call dsa_map_data('output','write',btype,address,
     &                   oslot,status)
      if (status .ne. 0) go to 999
      outptr = dyn_element(address)
      if (btype .eq. 'SHORT') then
        call stack_fill_w(noelm,dynamic_mem(outptr),int(padval))
      else
        call stack_fill_r(noelm,dynamic_mem(outptr),padval)
      end if
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
c   Now we can start to stack the cubes
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
c
c   Map errors and quality if needed
c
        if (error) then
          call dsa_map_errors('image','read',btype,address,
     &                         ieslot,status)
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
c   Add the cube to the stack
c
        if (btype .eq. 'SHORT') then
          call stack_w(
     &         dynamic_mem(inptr),dims(i,1),dims(i,2),dims(i,3),
     &         start(i),jdim,
     &         dynamic_mem(outptr),odims(1),odims(2),odims(3),
     &         error,dynamic_mem(ieptr),dynamic_mem(oeptr),
     &         quality,dynamic_mem(iqptr),dynamic_mem(oqptr))
        else ! btype must be FLOAT
          call stack_r(
     &         dynamic_mem(inptr),dims(i,1),dims(i,2),dims(i,3),
     &         start(i),jdim,
     &         dynamic_mem(outptr),odims(1),odims(2),odims(3),
     &         error,dynamic_mem(ieptr),dynamic_mem(oeptr),
     &         quality,dynamic_mem(iqptr),dynamic_mem(oqptr))
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
c   Set the bad pixel flag in OUTPUT if called for
c
      if (badpix) call dsa_set_flagged_values('output',.true.,status)
c
c   Finally, set the output axes if required
c
      if (useaxes) then
        do i = 1,3
          call dsa_map_axis_data('output',i,'write','float',
     &                            address,axslot,status)
          if (status .ne. 0) go to 999
          axptr = dyn_element(address)
          call stack_setaxes(dynamic_mem(axptr),odims(i),
     &                       axmin(i),bstep(i))
          call dsa_unmap(axslot,status)
        end do
      end if
c
999   continue
      call dsa_close(status)
      end

*******************************************************************************

      subroutine stack_info(name,dims,type)
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
      do i = 1,2
         ctemp = ich_ci(dims(i))
        call dsa_wruser(ctemp(:ich_len(ctemp))//' x ')
      end do
      ctemp = ich_ci(dims(3))
      call dsa_wruser(ctemp(:ich_len(ctemp))//'\\n')
      call dsa_wruser('File data type: '//type(:ich_len(type)))
      call dsa_wruser('.\\n')
      call dsa_wruser(' \\n')
      return
      end

*******************************************************************************

      subroutine stack_get_axis_info(axis,nelm,min,max,status)
      integer    nelm
      real       axis(nelm)
      real       min,max
      integer    status             ! *NOT* DSA status.
c
      integer    STACK_E_REVAXIS
      parameter  (STACK_E_REVAXIS=1)
c
      status = 0
      min = axis(1)
      max = axis(nelm)
      if (min.ge.max) status = STACK_E_REVAXIS
      return
      end

*******************************************************************************

      subroutine stack_setaxes(axis,nelm,axmin,step)
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

*******************************************************************************

      logical function stack_overlap(join,nfiles,axstart,axend,maxfiles)
      integer join,nfiles,maxfiles
      real    axstart(maxfiles,10),axend(maxfiles,10)
c
      integer file
      real    s1,e1,s2,e2
c
      stack_overlap = .false.
c
      s1 = axstart(nfiles,join)
      e1 = axend(nfiles,join)
      do file = 1,nfiles-1
        s2 = axstart(file,join)
        e2 = axend(file,join)
        if (((s2 .le. e1) .and. (s2 .ge. s1)) .or.
     &       ((e2 .le. e1) .and. (e2 .ge. s1))) then
          stack_overlap = .true.
          return
        end if
      end do
      return
      end
