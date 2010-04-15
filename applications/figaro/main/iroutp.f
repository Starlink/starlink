      subroutine iroutp(file,image,nx,ny,carray,order,nofits,rmsmax,
     :                  status)
*+
* Name:
*    IROUTP

* Invocation:
*    CALL IROUTP(FILE,IMAGE,NX,NY,CARRAY,ORDER,NOFITS,RMSMAX,
*                       STATUS)
* Purpose:
*     Outputs the results of the fit to the terminal, as a summary,
*     and to the results file.

* Description:
*     Outputs the results of the fit to the terminal, as a summary,
*     and to the results file.
*
* Arguments:
*      -  (">" input, "<" output)
*
*     (>) FILE   (Character) The name of the result file.
*     (>) IMAGE  (Character) The name of the image used for the fit
*     (>) NX     (Integer) The number of pixels in each row of the image
*     (>) NY     (Integer) The number of rows in the image
*     (>) CARRAY (Double precision CARRAY(11,NY)) The coefficients for
*                the fits for each row.
*     (>) ORDER  (Integer) The order for the original fit
*     (>) NOFITS (Integer) The number of rows that could not be fitted
*     (>) RMSMAX (Real) Maximum RMS error from the fits- now chi-squared
*     (<) STATUS (Integer) I/O status code. 0 if OK, non-zero => some
*                error writing to the IARC.LIS file.
*
* Common variables used -  None
*
* Subroutines / functions used -
*
*     dsa_free_lu                     Release logical unit number
*     dsa_get_lu                      Get logical unit number from pool
*     CHR_LEN      (ICH_ package) Position of last non-blank char.

* History:
*                                            KS / CIT 20th June 1984
*    Chi-squared rather than rms TNW/Man 3rd Dec 1985
*    Changed output message on fitting   AJH/Man 24 Nov 1997
*-
      implicit none
      include 'SAE_PAR'
*
*     Parameters
*
      integer nx, ny, order, nofits, status
      real    rmsmax
      double precision carray(11,ny)
      character*(*) file, image
*
*     Functions
*
      integer chr_len
*
*     Local variables
*
      logical fopen
      integer i, ignore, iy, nstat, output
      character*72 string,fulnam
      character*2 bss,bsn
      data bss/'\\'/
      bsn = bss(1:1)//'n'
*
*     Open file
*
      call dsa_open_text_file(file,' ','NEW',.true.,output,fulnam,
     :   status)
      if (status.ne.SAI__OK) then
         fopen=.false.
         go to 700
      end if
      fopen=.true.
*
*     Summary to terminal and to file
*
      call par_wruser(' ',nstat)
      call par_wruser('Summary of Image Arc Fit Results -',nstat)
      call par_wruser('-----------------------------------',nstat)
      call par_wruser(' ',nstat)
      write (output,'(2a)',iostat=status)
     :                 '2d fit to data in image ',image
      write (string,'(a,i5,a,i5)',iostat=ignore)
     :                     'image dimensions ',nx,' by ',ny
      call par_wruser(string,nstat)
      write (output,'(a)',iostat=status) string
      if (status.ne.SAI__OK) go to 600
      write (string,'(a,i5)',iostat=ignore)
     :           'number of rows that could not be fitted = ',nofits
      call par_wruser(string,nstat)
      write (output,'(a)',iostat=status) string
      if (status.ne.SAI__OK) go to 600
      write (string,'(a,f10.2)',iostat=ignore)
     :            'maximum chi-squared error = ',rmsmax
      call par_wruser(string,nstat)
      write (output,'(a)',iostat=status) string
      if (status.ne.SAI__OK) go to 600
      write (string,'(a,i3)',iostat=ignore)
     :            'maximum degree polynomial used = ',order
      call par_wruser(string,nstat)
      write (output,'(a)',iostat=status) string
      if (status.ne.SAI__OK) go to 600
      do iy=1,ny
         write (output,'(i14,10x,2d24.16,3(/3d24.16))',
*     :          iostat=status) iy,(carray(i,iy),i=1,order+1)
     :          iostat=status) iy,(carray(i,iy),i=order+1,1,-1)
         if (status.ne.SAI__OK)  go to 600
      end do
      call par_wruser(' ',status)
*      call dsa_wruser('Fit results written to file ')
*      call dsa_wruser(fulnam(:chr_len(fulnam)))
      call dsa_wruser('Fit results written to matching .iar file')
      call dsa_wruser(bsn)
      call par_wruser(' ',status)
      go to 700
*
*     I/O error from output file
*
  600 call dsa_wruser('I/O error writing to file ')
      call dsa_wruser(file(:chr_len(file)))
      call dsa_wruser(bsn)
*
*     Tidy up
*
  700 continue
      if (fopen) then
        close(unit=output,iostat=nstat)
        call dsa_free_lu(output,status)
      end if
*
      end
