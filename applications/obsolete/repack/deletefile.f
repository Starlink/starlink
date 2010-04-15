      subroutine deletefile(filename,status)

C     A simple little routine to delete a FITS file

      integer status,unit,blocksize
      character*(*) filename

C     simply return if status is greater than zero
      if (status .gt. 0)return

C     Get an unused Logical Unit Number to use to open the FITS file
 1    call ftgiou(unit,status)

C     try to open the file, so see if it exists
 2    call ftopen(unit,filename,1,blocksize,status)

      if (status .eq. 0)then
C         file was opened;  so now delete it
 3        call ftdelt(unit,status)
      else if (status .eq. 103)then
C         file doesn't exist, so just reset status to zero and clear errors
          status=0
 4        call ftcmsg
      end if

C     free the unit number for later reuse
 5    call ftfiou(unit, status)
      end
