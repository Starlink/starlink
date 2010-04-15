      subroutine fib2cube( STATUS )
*+
* Name:
*    FIB2CUBE

* Invocation:
*    CALL FIB2CUBE( STATUS )

* Purpose:
*    Arrange fibre output into 3-d data file.

* Description:
*  To convert a longslit spectrum of fibre spectra to a cube in an
* arbitrary manner. The output cube is "SORTED" in the TAURUS sense.
*
* Parameters:
*   IMAGE1,IMAGE2 etc. = FILE (Read)
*        Input images
*   CUBE = FILE (Write)
*        Output cube
*   FILE = CHARACTER (Read)
*        File with relationships defined
* Notes:
*  Note that the X arrays MUST be the same for each file.
*
* Subroutine/functions referenced:
*   WR2NEW        : Copy Z data into new file
*   CLGRAP        : Close graphics
*   CRRES         : Create fibre structure
*   FILLXDISP     : Fill X-displacement array
*   GETRANGE      : Get range of data for consideration
*   GETWORK       : Get virtual memory
*   MAPCUBE       : Map 3-d data file
*   RX2CHN        : Convert real X value to channel number
*   SET_SHORT     : Set an array of integer*2 number to a given value
*   ZERO_REAL     : Zero a real array
*   ZERO_SHORT    : Zero an integer*2 array
*
*   CNF_PVAL      : Full pointer to dynamically allocated memory
*   DSA_NAMED_OUTPUT : Open output file. This is used here rather than
*                      dsa_output since the name should not be set to
*                      that of an input file.
*   DSA_RESHAPE_AXIS : Create axis similar but of different size to
*                      previously existing axis
*   DSA_RESHAPE_DATA : Create data array similar, but of different size
*                      to previously existing data array
*   DSA_SIMPLE_OUTPUT : Put required structures into output file
*   DTA_WRVARF    : Write real*4 data to file
*   DSA_OPEN_TEXT_FILE : Open formatted file
*   GEN_NFILLF    : Fill array with their array element numbers
*   PAR_RDCHAR    : Read character string from user
*   PAR_QNUM      : Read number from user
*
* Authors:
*   TNW: T.N.Wilkins Manchester, Cambridge from 2/89
*   AJH: A.J.Holloway Manchester
* History:
*   TNW 5/4/88 Original version
*   TNW 25/10/88 DSA version. Unfortunately I can see no easy way of
*   directly replacing the one remaining direct DTA call.
*   FIG_OPFILE replaced by DSA_OPEN_TEXT_FILE, TNW 14/7/89.
*   AJH 6/4/98 Replace dsa_named_output with fda dsa_output
*   AJH 10/98 Replace dsa_map access 'r' to 'READ'
*-
      implicit none
      include 'SAE_PAR'
      include 'PRM_PAR'
      include 'CNF_PAR'          ! For CNF_PVAL function

      character*72 file,ftype*10,chars
      integer status
      include 'arc_dims'
      integer lunit,dims(3),ndims,nx,ny
      integer idims(3),rx2chn,ndims1,start,cxptr
      integer max_gauss
      parameter (max_gauss = 9)
      integer z1ptr,slit,nslit,misptr
      integer slot,slota,slotd,ml
      parameter (ml = 1)
      real tl,tr,minleft,maxright,wend
      real value
      character*1 nums(9)
      logical hex,par_qnum,qstat
      data nums/'1','2','3','4','5','6','7','8','9'/

      nyp = ml
      status = SAI__OK
      call dsa_open(status)
      call gr_init

* If mxpars is have been set to maximum value this gives a large results
* structure, so the user may prefer a smaller array than the maximum
* handled.

      call par_rdval('maxgauss',1.0,real(max_gauss),5.0,' ',value)
      mxpars = nint(value)*6 + 14
      iteration = 0

      call par_rdchar('file',' ',file)

* Open file with relation

      call dsa_open_text_file(file,'.dat','old',.false.,lunit,chars,
     :          status)
      if(status.ne.SAI__OK) then
        go to 500
      end if

* Read in "type" of array to create:
* ==================================
*   RECT    : Rectangular-normal "cube"
*   HEX     : Hexagonal array, stored as cube, but with an offset array
*   GEN     : General, stored as Z(NPTS),X(NPTS),Y(NPTS)

      read(lunit,'(a10)') ftype

* Read in dimensions of array

      if(ftype(:3).eq.'GEN') then
        read(lunit,'(2i5)')dims(2),nslit
        dims(3) = 1
        ndims = 2
      else
        read(lunit,'(3i5)')dims(2),dims(3),nslit
        ndims = 3
      end if
      hex = ftype(:3).eq.'HEX'
      minleft = VAL__MAXR
      maxright = VAL__MINR

* Get names of input files

      do slit = 1, nslit

* Open and get dimensions of input image

        call dsa_input('image','image'//nums(slit),status)
        call dsa_data_size('image',2,ndims1,idims,nelm,status)
        if(ndims1.ne.2) then
          call par_wruser('File does not have 2 dimensions!',status)
          go to 500
        end if
        ny=idims(2)
        nx=idims(1)
        call dsa_map_axis_data('image',1,'READ','float',xptr,slota,
     :                         status)

* Map input data set

        call dsa_map_data('image','READ','float',z1ptr,slotd,status)
        if(status.ne.SAI__OK) then
          go to 500
        end if
        if(slit.eq.1) then
          call getrange(%VAL(CNF_PVAL(z1ptr)),nx,ny,tl,tr,
     :                  %VAL(CNF_PVAL(xptr)))
          call clgrap
          minleft = tl
          maxright = tr
          start = rx2chn(%VAL(CNF_PVAL(xptr)),nx,minleft)
          dims(1) = rx2chn(%VAL(CNF_PVAL(xptr)),nx,maxright)
     :       - start + 1

* Create output cube, with Z array.

* Replace with dsa_output
*
*          call par_rdchar('cube',' ',file)
*          call dsa_named_output('data',file,'image',1,1,status)

          call dsa_output('data','cube','image',0,0,status)


          call dsa_simple_output('data','A2,A3','FLOAT',ndims,dims,
     :      status)
          call dsa_reshape_data('data','image',ndims,dims,status)
          call dsa_reshape_axis('data',1,'image',1,1,dims,status)

* Create fibre structure

          wavdim = dims(1)
          spdim1 = dims(2)
          if(ndims.eq.3) then
            spdim2 = dims(3)
          else
            spdim2 = 1
          end if
          call crres(ftype,status)

* Map output data set

          call mapcube(ftype,.true.,.true.,status)
          call dsa_get_work_array(dims(2)*dims(3),'short',misptr,slot,
     :                            status)
          if(status.ne.SAI__OK) goto 500

* Fill parameter names array

*          parend = parptr + mxpars*10 - 1

* Copy X array (noting that not all of input array is written to output).

          cxptr = xptr + (start - 1) * 4
          call copr2r(dims(1),%VAL(CNF_PVAL(cxptr)),
     :                %VAL(CNF_PVAL(d_xptr)))

          if(hex) then
            wend = 1.0 + (dims(3)-1)*0.8660254
            call fig_wfill(1.0,wend,.false.,dims(3),
     :                     %VAL(CNF_PVAL(yptr)))
          else
            call gen_nfillf(dims(3),%VAL(CNF_PVAL(yptr)))
          end if
          call gen_nfillf(dims(2),%VAL(CNF_PVAL(xptr)))

*  Zero output filemain data array and missing values array

          call gen_cfill(1,dims(1)*dims(2)*dims(3),val__badr,
     :                   %VAL(CNF_PVAL(d_sptr)))
          call zero_short(%VAL(CNF_PVAL(misptr)),dims(2)*dims(3))

* Read positions of data in cube, and copy it over

          qstat = par_qnum('Enter true wavelength of line',VAL__SMLR,
     :                     VAL__MAXR,6562.817,.true.,' ',
     :                     %VAL(CNF_PVAL(d_wptr)))
        end if
        call wr2new(dims(1),dims(2),dims(3),nx,ny,%VAL(CNF_PVAL(z1ptr)),
     :              %VAL(CNF_PVAL(d_sptr)),lunit,%VAL(CNF_PVAL(misptr)),
     :              minleft,maxright,%VAL(CNF_PVAL(xptr)))
        call dsa_unmap(slota,status)
        call dsa_unmap(slotd,status)
        call dsa_close_structure('image',status)
      end do
      if(hex) then

* Fill .fibre.xdisp array

        call fillxdisp(%VAL(CNF_PVAL(xdptr)),dims(3),lunit)
      end if

* Close files

      close(lunit)
      call init_res(.false.,status)
 500  continue
      call dsa_close(status)
      end
