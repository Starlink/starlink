      subroutine clgrap
*+
* Name:
*    CLGRAP

* Invocation:
*    CALL CLGRAP

* Purpose:
*    Close the PGPLOT device.

* Description:
*  If device was hard-copy then copy the plot file to the device.
*    The latter assumes that logical names are set up for the queues of
*  the form GKS_(workstation type)_QUEUE, e.g. GKS_1200_QUEUE.
*
* Subroutines referenced:
*     PRINT_FILE   : Submit file to print queue
*
*     PAR_WRUSER   : Write character string to user
*
*     CHR_FILL     : Fill chacter string with one character
*     CHR_PUTC     : Format character string into character string
*     CHR_APPND    : As CHR_PUTC, but ignores trailing blanks in input
*                    string
*
*     PGEND        : Close PGPLOT
*     PGQINF       : Get information from PGPLOT

* History:
*    T.N.Wilkins Manchester., based on CLHARD.   30/6/88-1/7/88
*         "      Cambridge, 3/90 PGPLOT version
*         "          "      5/4/90 Combined CLGREY and CLHARD, and
*                           renamed CLGRAP (and used to replace CLGRAP).
*
      implicit none

* external

      include 'gr_inc'
*-
      character*25 qname,chars*20
      integer len1,len2
      external par_wruser
* --------------------------------------------------------------------

*   If nothing to close then return

      if(.not.(hardcopy.or.terminal.or.greyscale)) return

*   Close PGPLOT workstation, first checking whether it is a hardcopy
*   device

      call pgqinf('hardcopy',chars,len1)

* We can only have 1 device open. Since for greyscale plots this may
* also be the hardcopy or terminal device, we might as well say all
* devices are closed (as they are!).

      terminal = .false.
      hardcopy = .false.
      greyscale = .false.
      if(chars(:len1).eq.'YES') then

* Create logical name to point to queue

        len1 = 0
        call chr_fill(' ',qname)
        call chr_putc('GKS_',qname,len1)
        call pgqinf('type',chars,len2)
        call chr_ldblk(chars)
        call chr_appnd(chars,qname,len1)
        call chr_putc('_QUEUE',qname,len1)

* Print file

        call pgend
        call print_file(prname,qname,par_wruser)
      else
        call pgend
      end if
      end
