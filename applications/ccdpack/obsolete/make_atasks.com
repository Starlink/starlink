$!+
$!  Name:
$!     MAKE_ATASKS

$!  Purpose:
$!     To extract and link all CCDPACK atasks.

$!  Language:
$!     DCL

$!  Notes:
$!     Uses LIBMAINT.
  
$!  Authors:
$!     PDRAPER: Peter Draper (STARLINK)
$!     {enter_new_authors_here}

$!  History:
$!     28-MAY-1991 (PDRAPER):
$!        Original version.
$!     {enter_changes_here}
$!-
$!
$! create a sub directory to work in and set default there
$!
$create/dir [.work]
$set def [.work]
$!
$! Start up LIBMAINT.
$!
$libmaint ccdpack_libdir:ccdpack_tasks
$switch compile off                 ! switch compilation off forcing 
$                                   ! extraction.
$!
$! extract all the modules
$!
$olbcre
$!
$! likewise for the IFL files.
$!
$deflib ccdpack_libdir:ccdpack_ifl
$olbcre
$!
$! now look for all modules of .for type and link the task.
$!
$loop:
$this_task=f$search("*.for",1)
$if this_task.eqs."" then goto next
$!
$! strip file type ..
$!
$this_task=f$parse("''this_task'",,,"NAME")
$!
$! ok got file make the .exe 
$!
$@[-]make_task 'this_task'
$!
$! compile the IFL file.
$!
$compifl 'this_task'
$!
$! copy the result and the ifc file.
$!
$rename/log 'this_task'.exe,'this_task'.ifc [-]*.*
$!
$! return for next.
$goto loop
$next:
$!
$! clean up any mess
$delete/noconf *.*;*
$set def [-]
$set prot=(O:rwed) work.dir
$delete/noconf work.dir;
$exit
$! $Id$
