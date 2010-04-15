*+RE_EV_LIST Makes a text file of event files to be merged.
	SUBROUTINE RE_EV_LIST (STATUS)
	IMPLICIT NONE
* Input

* Output
	INTEGER	      STATUS		! Status return
* P. McGale Apr 95.
*-
* Local
	integer i, system
	character*80 unzip		! GUNZIP fits files.
	character*80 dump		! Dump merge list to a file


	IF (STATUS .NE. 0) RETURN

* Define shell commands
*       unzip = 'echo ''echo "foreach a (x*.fit.gz)\ngunzip \$a\nend"''
*     & sh | csh -f'
*       dump = 'echo ''echo "foreach a (x*.fit)\necho \$a\nend"'' | sh |
*    &csh -f > ! re_ev.list'
        unzip='gunzip x*y*.fit.gz '
        dump = 'ls -1 x*y*.fit > re_ev.list  '


* Uncompress fits files, if any.
 	i = system(unzip)

* List fits files to be merged into file re_ev.list
	i = system(dump)
	if (i .eq. 0) then
	  write(*,*)
	  write(*,*)'   List of event files to be merged in re_ev.list'
	  write(*,*)
	endif



	END

