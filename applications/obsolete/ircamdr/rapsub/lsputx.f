
*+ LSPUTX - Put the supplied value string in the supplied text line
      subroutine lsputx(ndim, dims, ivalue, value, inds, line, leng,
     :  full)
*    Description :
*     The supplied value string, VALUE, is planted in the text line, LINE.
*     The line length, LENG, and array indices, INDS, are updated
*     as appropriate.
*     If there is not enough room to plant the value string, then
*     FULL is returned TRUE.
*    Method :
*    Deficiencies :
*     At present, this ignores the true dimensionality, and produces
*     a list of values seperated by commas.
*    Authors :
*     Jack Giddings (UCL::JRG)
*    History :
*     17-FEB-1983:  Original.  (UCL::JRG)
*    Global constants :
      include 'SAE_PAR'			! SAI Constants
      include 'DAT_PAR'                 ! Necessary for non-VMS
*    Import :
      integer ndim			! Number of dimensions
      integer dims(*)			! Dimensions
      integer ivalue			! Value index as if object is vector
      character*(*) value		! Value string
*    Import-Export :
      integer inds(*)			! Array indices
      character*(*) line		! Line to receive numbers
      integer leng			! Current line length
*    Export :
      logical full			! Whether line was found full
*    External references :
      integer chr_len			! String length
      integer chr_size			! String size
*    Local variables :
      integer nchar			! Value string length
      integer need			! Space needed
*-

*     Space needed (prefix with comma if ivalue > 1) includes elipsis
      nchar = chr_len(value)
      need = nchar + 3
      if (ivalue .gt. 1) then
         need = need + 1
      endif

*    See if there is space in line
      full = (chr_size(line) - leng) .lt. need
      if (.not. full) then
         if (ivalue .gt. 1) then
            call chr_putc(',', line, leng)
         endif
         call chr_putc(value(1:nchar), line, leng)
      endif

      return
      end
