*+ DAT_CCTYP - Create type string of the form _CHAR*n
      subroutine dat_cctyp(size, type)
*    Description :
*     This routine creates a type string for a specified size of
*     character string.
*    Invocation :
*     CALL DAT_CCTYP(SIZE; TYPE)
*    Parameters :
*     SIZE=INTEGER
*           Variable containing the character string size.
*     TYPE=CHARACTER*(DAT__SZTYP)
*           Variable to contain the type string.
*    Method :
*     Use string utilities to create type string.
*    Authors :
*     Sid Wright    (UCL::SLW)
*    History :
*     11-Feb-1983:  Original.  (UCL::SLW)
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
      integer size			! size of character string required
*    Export :
      character type*(*)		! type string
*    Local variables :
      integer nchar
*-

      nchar = 0
      call chr_term(0, type)
      call chr_putc('_CHAR*', type, nchar)
      call chr_puti(size, type, nchar)
      call chr_rmblk(type)

      end
