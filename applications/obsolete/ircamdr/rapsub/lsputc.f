
*+ LSPUTC - Put out the first few values of an <string> object
      subroutine lsputc(loc, ndim, dims, size, line, leng)
*    Description :
*     A limited number of values are read from the object and coded
*     into the supplied text line in a concise manner.
*     If only some of the values can be appended to the line then
*     the elipsis notation is displayed.
*    Method :
*     Access subset of values using SDS, then code each one into a
*     local string, passing this to the LSPUTX routine to do the
*     job of planting it in the text line.
*    Deficiencies :
*     The access method will change dramatically with the next SDS.
*    Authors :
*     Jack Giddings (UCL::JRG)
*    History :
*     17-FEB-1983:  Original.  (UCL::JRG)
*    Global constants :
      include 'SAE_PAR'			! SAI Constants
      include 'DAT_PAR'                 ! Necessary for non-VMS
*    Import :
      character*(*) loc			! Object locator
      integer ndim			! Number of dimensions
      integer dims(*)			! Dimensions
      integer size			! Size of object as if vector
*    Import-Export :
      character*(*) line		! Line to receive numbers
      integer leng			! Current line length
*    External references :
      integer chr_len			! String length
      integer chr_size			! String size
*    Local Constants :
      integer SZELIP			! Size of eilpsis (...)
      parameter(SZELIP=3)
      integer MAXVAL			! maximum number of values read
      parameter(MAXVAL=80)
      integer MAXSTR			! size of temporary string for number
      parameter(MAXSTR=400)
*    Local variables :
      character*(DAT__SZLOC) vec	! Vector locator
      character*(DAT__SZLOC) slice	! Slice locator
      character*(MAXSTR) str		! String to hold coded number
      integer nchar			! Number of characters in STR
      integer inds(DAT__MXDIM)		! Array indices
      integer nvalue			! Number of values accessed
      character*(MAXSTR-2) values(MAXVAL)! integer values
      integer ivalue			! value index
      integer i				! Temporary length
      integer	nact			! Actual string value length used
      logical full			! Whether text line is full
      integer status			! local status
*-

*    read a limited part of the variable
      nvalue = min(MAXVAL, size)
      if (ndim .eq. 0) then
         call dat_get0c(loc, values, status)
      else
         call dat_vec(loc, vec, status)
         call dat_slice(vec, 1, 1, nvalue, slice, status)
         call dat_getvc(slice, MAXVAL, values, nvalue, status)
         call dat_annul(slice, status)
         call dat_annul(vec, status)
      endif
      if (status .ne. SAI__OK) then
         call err_annul(status)
         call chr_putc('<undefined>', line, leng)
      else
         do ivalue = 1, nvalue
            nchar = chr_len(values(ivalue))
            i = 0
*           call chr_putc('"', str, i)
            call chr_putc('''', str, i)
*          Find the maximum size of string that CAN be displayed
            nact = chr_size(line) - (leng + SZELIP*2 + 2)
*          Allow for prefixed ","
            if (ivalue .gt. 1) then
               nact = nact - 1
            endif
*          Plant string (or a subset)
            nact = min(nchar, nact)
            call chr_putc(values(ivalue)(1:nact), str, i)
*          Put elipsis before trailing quote if string was truncated
            if (nact .lt. nchar) then
               call chr_putc('...', str, i)
            endif
*           call chr_putc('"', str, i)
            call chr_putc('''', str, i)
            nchar = i
            call lsputx(ndim, dims, ivalue, str(1:nchar), inds,
     :        line, leng, full)
            if (full) then
               goto 1
            endif
         enddo
 1       continue

*       Append elipsis if line was full or only a subset was listed
         if (full .or. nvalue .lt. size) then
            call chr_putc('...', line, leng)
         endif
      endif

      return
      end
