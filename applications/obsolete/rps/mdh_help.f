*+
*     1993 June         P. Brisco       Recompile with new com_form_files.inc
*---------------------------------------------------------------------------
      SUBROUTINE MDH_HELP( FILE , FIRST )

*  -----------
*  DESCRIPTION
*  -----------

* Provides an interface with a help library.

*  ---------
*  VARIABLES
*  ---------

*INPUT:

      CHARACTER*(*) FILE	! Library file.
     & ,            FIRST       ! First required help command.

      INCLUDE 'com_form_files.inc'

*  ------------------------------------
*  FUNCTIONS AND SUBROUTINES REFERENCED
*  ------------------------------------

      INTEGER  MDH_ENDWORD	! Finds actual string length.

* Local Variables
      INTEGER   GLUN, LFILE, LFIRST, ISTAT
      INTEGER lbuf, lret
      CHARACTER*120 TFILE
      CHARACTER*128 cbuf, cret

      call rhelp( file, first, istat)
      if (istat .ne. 0) then
         write(*,*) 'Status, exit from help: ',istat
         istat = 0
      end if

      END									! End.

