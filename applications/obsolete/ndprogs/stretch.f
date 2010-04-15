      subroutine stretch
C+
c
c   -------------
c   S T R E T C H
c   -------------
c
c   Description
c   -----------
c   Uses the axis arrays of one cube to re-scale the data in another cube,
c   performing any necessary interpolation.
c
c   Program Scope
c   -------------
c   - FLOAT or REAL data, other types handled as float
c   - Images of up to 3 dimensions
c   - Subsetting not appropriate
c   - Linear axis arrays necessary in at least one dimension
c   - Magic values, error and quality arrays supported.
c   - Batch mode supported
c
c   Environment
c   -----------
c   Figaro
c
c   Parameters
c   ----------
c   IMAGE1      Name of image to be scaled
c   IMAGE2      Name of image whose dimensions will determine the scaling
c   OUTPUT      Name of output structure
c   INTERP      Type of interpolation to be used.
c
c   Keywords
c   --------
c   VERBOSE     Make the program more talkative.
c
c   Method
c   ------
c   - Two data structures are prompted for and opened. They must both have
c     the same dimensionality but differing dimensions. The first is the
c     image to be stretched, the second one defines the size of the stretched
c     array.
c   - Various checks are made to see if the two images are compatible.
c   - Error and quality arrays are checked for.
c   - The output structure is created with the same dimensions as the second
c     input structure.
c   - All necessary arrays are mapped.
c   - The program can perform either piecewise constant or linear interpolation
c     of pixel data. This option is obtained.
c   - The input cube is stretched (or indeed squashed) to fit the output cube.
c   - The axes (if present) are adjusted to fit the new size.
c
c   External functions and subroutines
c   ----------------------------------
c
c   DSA Library:
c     DSA_CLOSE
c     DSA_GET_AXIS_DATA
c     DSA_GET_WORK_ARRAY
c     DSA_INPUT
c     DSA_OPEN
c     DSA_OUTPUT
c     DSA_MAP_AXIS_DATA
c     DSA_MAP_DATA
c     DSA_MAP_ERRORS
c     DSA_MAP_QUALITY
c     DSA_SEEK_ERRORS
c     DSA_SEEK_QUALITY
c     DSA_SIMPLE_OUTPUT
c     DSA_UNMAP
c     DSA_WRUSER
c
c   GEN Library:
c     GEN_ELEMF
c
c   ICH Library:
c     ICH_CI
c     ICH_FOLD
c
c   NDP Library:
c     NDP_GET_IMAGE_INFO
c
c   PAR Library:
c     PAR_RDCHAR
c     PAR_RDKEY
c
c   Internal subroutines
c   --------------------
c   STRETCH_1D_<T>
c   STRETCH_2D_<T>
c   STRETCH_3D_<T>
c   STRETCH_DO_AXIS
c
c   INCLUDE's
c   ---------
c   INCLUDE 'DYNAMIC_MEMORY'
c   INCLUDE 'MAGIC_VALUES'
c   INCLUDE 'NUMERIC_RANGES'
c   INCLUDE 'DCV_FUN'
c
c   Fortran 77 Extensions
c   ---------------------
c   END DO / IMPLICIT NONE / INCLUDE / Names > 6 characters / DO WHILE
c
c   Author
c   ------
c   Julian Gold         RGO     (CAVAD::GOLDJIL or goldjil@uk.ac.cam.ast-star)
c
c   History
c   -------
c
c   14-AUG-1992         - Original program.
c   10-DEC-1992         - Unix version.
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
      character         ich_ci*8
      real              gen_elemf
c
c   Variables
c
      integer           address         ! Address of dynamic memory element
      integer           axptr           ! Pointer to IMAGE1 axis array
      integer           axslot          ! Slot number for axis array
      logical           axes(6)         ! Axis presence flags
      logical           badpix          ! Bad pixel flag
      integer           dims(6)         ! IMAGE1 dimensions
      integer           dumint          ! Dummy integer
      real              end             ! Axis end value
      integer           eptr            ! Pointer to error array
      integer           eslot           ! Slot number for above
      logical           err             ! Error array flag
      integer           i               ! Loop variable
      integer           im1ptr          ! Pointer to IMAGE1 data
      integer           im1slot         ! Slot number for above
      character         info*64         ! For dsa_simple_output
      integer           interp          ! 0=piecewise constant, 1=linear
      character         junk*64         ! Temporary text buffer
      integer           ndim1           ! No. of dims of IMAGE1
      integer           ndim2           !  "   "  "   "  IMAGE2
      integer           nelm1           ! No of elements in IMAGE1
      integer           nelm2           !  "  "     "     " IMAGE2
      integer           oaxptr          ! Pointer to o/p axis
      integer           oaxslot         ! Slot number for above
      integer           odims(6)        ! Output image dimensions
      integer           oeptr           ! Output error array pointer
      integer           oeslot          ! Slot number for o/p errors
      integer           optr            ! Pointer to output data
      integer           oqptr           ! Pointer to output quality array
      integer           oqslot          ! Output quality slot number
      integer           oslot           ! Slot number for o/p data
      integer           qptr            ! Pointer to quality array
      integer           qslot           ! Slot number for above
      logical           qual            ! Quality array flag
      real              start           ! Axis start value
      integer           status          ! DSA status variable

      character         type*16         ! Data type of IMAGE1
      logical           verbose         ! Verbosity flag
c
      include 'DYNAMIC_MEMORY'
      include 'MAGIC_VALUES'
      include 'NUMERIC_RANGES'
c
c   Initialise
c
      status = 0
      type = '                '
      call dsa_open(status)
      if (status .ne. 0) go to 999
c
c   Get hidden parameters
c

      call par_rdkey('verbose',.false.,verbose)
c
c   Open image1
c
      call dsa_input('image1','image1',status)
      if (status .ne. 0) go to 999

c
c   Obtain stats for image1
c
      call ndp_get_image_info('image1',.true.,.true.,type,badpix,status)
      if (status .ne. 0) go to 999

      call dsa_data_size('image1',6,ndim1,dims,nelm1,status)
      if (status .ne. 0) go to 999
c
c   Open image2 and make some comparisons
c
      call dsa_input('image2','image2',status)
      if (status .ne. 0) go to 999

      call dsa_data_size('image2',6,ndim2,odims,nelm2,status)
      if (status .ne. 0) go to 999

      if (verbose) then
        call dsa_wruser('IMAGE2 dimensions: ')
        do i = 1,ndim2-1
          call dsa_wruser(ich_ci(odims(i))//' x ')
        end do
        call dsa_wruser(ich_ci(odims(ndim2))//'\\n')
      end if

      if (ndim1 .ne. ndim2) then
        call dsa_wruser('%STRETCH-E-BADDIM  ')
        call dsa_wruser('Both images must have same')
        call dsa_wruser(' dimensionality.\\n')
        go to 999
      end if

      if (ndim1 .gt. 3) then
        call dsa_wruser('%STRETCH-E-BADDIM  ')
        call dsa_wruser('Image cannot have more than 3')
        call dsa_wruser(' dimensions\\n')
        go to 999
      end if

      dumint = 0
      do i = 1,ndim1
        dumint = dumint+abs(odims(i)-dims(i))
      end do
      if (dumint .eq. 0) then
        call dsa_wruser('%STRETCH-E-NODIFF  ')
        call dsa_wruser('Images are dimensioned indentically\\n')
        go to 999
      end if
c
c   Check for quality and error arrays
c
      call dsa_seek_quality('image1',qual,status)
      call dsa_seek_errors('image1',err,status)
      if (status .ne. 0) go to 999
c
c   Check for axes
c
      do i = 1,ndim1
        call dsa_seek_axis('image1',i,axes(i),status)
      end do
      if (status .ne. 0) go to 999
c
c   Open and construct output file
c
      call dsa_output('output','output',' ',1,1,status)
      if (status .ne. 0) go to 999
      info = 'd'

      if (qual) info = info // ',q'
      if (err)  info = info // ',e'

      do i = 1,ndim2
        if (axes(i)) info = info // ',a'//ich_ci(i)
      end do

      call dsa_simple_output('output',info,type,ndim2,odims,status)
      if (status .ne. 0) go to 999
c
c   Map relevant things
c
      call dsa_map_data('image1','read',type,address,
     &                   im1slot,status)
      if (status .ne. 0) go to 999
      im1ptr = dyn_element(address)

      call dsa_map_data('output','write',type,address,
     &                   oslot,status)
      if (status .ne. 0) go to 999
      optr = dyn_element(address)

      if (qual) then
        call dsa_map_quality('image1','read','byte',
     &                        address,qslot,status)
        if (status .ne. 0) go to 999
        qptr = dyn_element(address)
        call dsa_map_quality('output','write','byte',
     &                        address,oqslot,status)
        if (status .ne. 0) go to 999
        oqptr = dyn_element(address)
      end if

      if (err) then
        call dsa_map_errors('image1','read',type,
     &                        address,eslot,status)
        if (status .ne. 0) go to 999
        eptr = dyn_element(address)
        call dsa_map_errors('output','write',type,
     &                        address,oeslot,status)
        if (status .ne. 0) go to 999
        oeptr = dyn_element(address)
      end if
c
c   Get interpolation option - constant or linear?
c
      call par_rdchar('interp','LINEAR',junk)
      call ich_fold(junk)
      if (junk(1:1) .eq. 'L') then
        interp = 1
      else
        interp = 0
      end if
c
c   Feel the stretch!
c
      if (verbose) then
        call dsa_wruser('%STRETCH-I-STRETCH  ')
        call dsa_wruser('Reshaping structure\\n')
      end if

      if (ndim1 .eq. 1) then

        if (type .eq. 'FLOAT') then

          call stretch_1d_r(dynamic_mem(im1ptr),dynamic_mem(optr),
     &                      dims(1),odims(1),interp)
          if (err) then
            call stretch_1d_r(dynamic_mem(eptr),dynamic_mem(oeptr),
     &                        dims(1),odims(1),interp)
          end if

        else

          call stretch_1d_w(dynamic_mem(im1ptr),dynamic_mem(optr),
     &                      dims(1),odims(1),interp)
          if (err) then
            call stretch_1d_w(dynamic_mem(eptr),dynamic_mem(oeptr),
     &                        dims(1),odims(1),interp)
          end if

        end if

        if (qual) then
          call stretch_1d_b(dynamic_mem(qptr),dynamic_mem(oqptr),
     &                      dims(1),odims(1),0)
        end if

      else if (ndim1 .eq. 2) then

        if (type .eq. 'FLOAT') then

          call stretch_2d_r(dynamic_mem(im1ptr),dynamic_mem(optr),
     &                      dims(1),dims(2),odims(1),odims(2),interp)
          if (err) then
            call stretch_2d_r(dynamic_mem(eptr),dynamic_mem(oeptr),
     &                        dims(1),dims(2),odims(1),odims(2),interp)
          end if

        else

          call stretch_2d_w(dynamic_mem(im1ptr),dynamic_mem(optr),
     &                      dims(1),dims(2),odims(1),odims(2),interp)
          if (err) then
            call stretch_2d_w(dynamic_mem(eptr),dynamic_mem(oeptr),
     &                        dims(1),dims(2),odims(1),odims(2),interp)
          end if

        end if

        if (qual) then
          call stretch_2d_b(dynamic_mem(qptr),dynamic_mem(oqptr),
     &                      dims(1),dims(2),odims(1),odims(2),0)
        end if

      else ! (ndim1 .eq. 3)

        if (type .eq. 'FLOAT') then

          call stretch_3d_r(dynamic_mem(im1ptr),dynamic_mem(optr),
     &                      dims(1),dims(2),dims(3),
     &                      odims(1),odims(2),odims(3),interp)
          if (err) then
            call stretch_3d_r(dynamic_mem(eptr),dynamic_mem(oeptr),
     &                        dims(1),odims(1),interp)
          end if

        else

          call stretch_3d_w(dynamic_mem(im1ptr),dynamic_mem(optr),
     &                      dims(1),dims(2),dims(3),
     &                      odims(1),odims(2),odims(3),interp)
          if (err) then
            call stretch_3d_w(dynamic_mem(eptr),dynamic_mem(oeptr),
     &                        dims(1),dims(2),dims(3),
     &                        odims(1),odims(2),odims(3),interp)
          end if

        end if

        if (qual) then
          call stretch_3d_b(dynamic_mem(qptr),dynamic_mem(oqptr),
     &                      dims(1),dims(2),dims(3),
     &                      odims(1),odims(2),odims(3),0)
        end if

      end if
c
c   Finally reshape the axes to fit the new cube size
c
      if (verbose) then
        call dsa_wruser('%STRETCH-I-DOAXES  ')
        call dsa_wruser('Reshaping axes\\n')
      end if

      do i = 1,ndim1
        if (axes(i)) then
          call dsa_map_axis_data('image1',i,'read','float',
     &                           address,axslot,status)
          if (status .ne. 0) go to 999
          axptr = dyn_element(address)
          call dsa_map_axis_data('output',i,'write','float',
     &                           address,oaxslot,status)
          if (status .ne. 0) go to 999
          oaxptr = dyn_element(address)

          start = gen_elemf(dynamic_mem(axptr),1)
          end = gen_elemf(dynamic_mem(axptr),dims(i))
          call stretch_do_axis(dynamic_mem(oaxptr),start,end,odims(i))

          call dsa_unmap(oaxslot,status)
          call dsa_unmap(axslot,status)
        end if
      end do

999   continue
      call dsa_close(status)
      end

*******************************************************************************

      subroutine stretch_do_axis(axis,start,end,size)
*******************************************************************************
* Purpose:
*   Make the axis array fit the new cube size.
*
* Arguments:
*   ! axis        (real array) The axis
*   > start       (real) starting value of axis
*   > end         (real) end value of axis
*   > size        (integer) axis dimension
*******************************************************************************
      integer     size
      real        axis(size)
      real        start
*
      integer     i
      real        h
*
      h = (end-start) / real(size-1)
      do i = 1,size
        axis(i) = start + real(i-1)*h
      end do
*
      return
      end

*******************************************************************************

      real function frac(x)
*******************************************************************************
* Purpose:
*   Return the fractional part of the argument.
* Argument:
*   > x    real
*******************************************************************************
      real x
*
      frac = x - int(x)
      return
      end

