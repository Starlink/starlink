
*+ LSDIR - list contents of structure
      subroutine lsdir(name, indent, status)
*    Description :
*     The following items are listed for each component of the
*     structure, NAME:
*    Authors :
*     Jack Giddings (UCL::JRG)
*    History :
*     17-FEB-1983:  Original.  (UCL::JRG)
*    Global constants :
      include 'SAE_PAR'			! SAI Constants
      include 'DAT_PAR'                 ! Necessary for non-VMS
*    Import :
      character*(*) name		! Structure locator
      integer indent			! Indentation for output
*    Status return :
      integer status			! status return
*    External references :
      logical chr_simlr			! string equality test
      integer chr_len			! String length
*    Local constants :
      integer LNSIZE			! Line size
      parameter(LNSIZE=72)
*    Local variables :
      character*(DAT__SZLOC) loc	! Component locator
      character*(DAT__SZNAM) comp	! Component name
      character*(DAT__SZTYP) type	! Component type
      integer size			! Size as if vector
      integer ndim			! Number of cimponent dimensions
      integer dims(DAT__MXDIM)		! Component dimensions
      logical prim			! Whether object is primitive
      character*(LNSIZE) line		! Line string
      integer icomp			! Component index
      integer ncomp			! Number of components
      integer leng			! Line length
      integer i				! character index
*-

*    Get number of structure components
      call dat_ncomp(name, ncomp, status)
      if (status .ne. SAI__OK) then
         call dat_erdsc(name, status)
      else
*       Go through each component in turn
         do icomp = 1, ncomp

*          Get component details
            call dat_index(name, icomp, loc, status)
            call dat_name(loc, comp, status)
            call dat_prim(loc, prim, status)
            call dat_type(loc, type, status)
            call dat_size(loc, size, status)
            call dat_shape(loc, DAT__MXDIM, dims, ndim, status)
            if (status .ne. SAI__OK) then
               call dat_erdsc(loc, status)
               call dat_annul(loc, status)
               goto 1
            endif

*          initialise line (with indentation)
            line = ' '
            leng = indent

*          Append Type
            i = leng
            call chr_putc(type, line, i)
            leng = max(i, leng + DAT__SZTYP)
            call chr_putc(' ', line, leng)

*           Append Name
            i = chr_len(comp)
            call chr_putc(comp(1:i), line, leng)

*          Append Dimensions
            if (ndim .gt. 0) then
               call chr_putc('(', line, leng)
               do i = 1, ndim
                  call chr_puti(dims(i), line, leng)
                  if (i .lt. ndim) then
                     call chr_putc(',', line, leng)
                  endif
               enddo
               call chr_putc(')', line, leng)
            endif

*          pad out until 40 characters beyond initial indent
             leng = max(leng, indent+40)

*          Append values for primitive types
            if (prim) then
               if (chr_simlr(type, '_DOUBLE')) then
                  call lsputd(loc, ndim, dims, size, line, leng)
               elseif (chr_simlr(type, '_REAL')) then
                  call lsputr(loc, ndim, dims, size, line, leng)
               elseif (chr_simlr(type, '_INTEGER') .or.
     :           chr_simlr(type, '_WORD') .or.
     :           chr_simlr(type, '_UWORD') .or.
     :           chr_simlr(type, '_BYTE')  .or.
     :           chr_simlr(type, '_UBYTE')) then
                  call lsputi(loc, ndim, dims, size, line, leng)
               elseif (chr_simlr(type, '_LOGICAL')) then
                  call lsputl(loc, ndim, dims, size, line, leng)
               elseif (chr_simlr(type(1:5), '_CHAR')) then
                  call lsputc(loc, ndim, dims, size, line, leng)
               else
                  call chr_putc('<special>', line, leng)
               endif
            else
               call chr_putc('<structure>', line, leng)
            endif

*          Output line as the only token in a message
            call msg_setc('LINE', line(1:leng))
            call msg_out('LS_COMP', '^LINE', status)

            call dat_annul(loc, status)
         enddo
      endif
 1    continue

      return
      end
