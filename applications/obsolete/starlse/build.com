$!+
$!  Name:
$!     BUILD.COM
$!
$!  Purpose:
$!     Build the STARLSE system from its source files.
$!
$!  Type of Module:
$!     DCL command procedure.
$!
$!  Invocation:
$!     @BUILD
$!
$!  Parameters:
$!     None.
$!
$!  Prior Requirements:
$!     The source code files should be present in the current default
$!     directory.
$!
$!  Authors:
$!     RFWS: R.F. Warren-Smith (STARLINK)
$!
$!  History:
$!     1-JUN-1990 (RFWS):
$!        Original version.
$!     {enter_changes_here}
$!
$!  Bugs:
$!     {note_any_bugs_here}
$!
$!-
$!
$!  Compile and link the TPU$CALLUSER routine which is called from LSE.
$      FORTRAN/NOLIST []TPU$CALLUSER
$      LINK/NOMAP []TPU$CALLUSER/SHARE/OPT
$      DELETE TPU$CALLUSER.OBJ;*
$!
$!  Run LSE, supplying it with an initialisation file which builds the system,
$!  then saves the LSE context in a TPU section file and an LSE environment
$!  file. 
$      LSE JUNK.TMP -
          /NODISPLAY -
          /SECTION = SYS$LIBRARY:LSE$SECTION.TPU$SECTION -
          /NOENVIRONMENT -
          /INITIALIZATION = []BUILD.LSE
$!
$!  Make the new files readable.
$      SET PROT=(W:RE) STARLSE.ENV, STARLSE.TPU$SECTION, TPU$CALLUSER.EXE
$!
$!  Purge any duplicate files.
$      PURGE
$!
$!  Exit procedure.
$      EXIT
