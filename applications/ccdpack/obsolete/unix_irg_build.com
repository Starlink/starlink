$!+
$! Name:
$!    UNIX_IRG_BUILD

$! Purpose:
$!    Selects all the files required for producing a Unix version of IRG

$! Language:
$!    DCL

$! Invocation:
$!    @UNIX_IRG_BUILD

$! Description:
$!    This procedure produces a series of files suitable for
$!    shipping to a unix machine to produce an IRG system. Before
$!    using this procedure the current CMS library should be set to
$!    IRG with the following groups in place.
$!
$!      o GENERAL        - code which is machine non-specific
$!      o UNIX_INCLUDES  - ALL the include files required for the unix
$!                         system (regardless of machine dependent or
$!                         not)
$!      o UNIX_SPECIFIC  - unix specific routines (including and C
$!                         routines)
$!      o DOCUMENTS      - Documents describing the IRG subroutine
$!                         library. 
$!    Plus a suitable forconv.dat file which contains the FORCONV
$!    translation tables for INCLUDE statements and a makefile.

$! Notes:
$!    - The output files are all STREAM_LF format so can be directly
$!    copied to a Unix machine.
$!
$!    - The IRG system should be built on the destination machine
$!    using the fsplit command to get all the individual units
$!    then compiled and entered into an archive.
$!
$!    [ create a directory scratch
$!
$!    %cd scratch
$!    %fsplit ../irg.f
$!    %f77 -c *.f
$!    %ar cr ../libirg.a *.o
$!    %ar cr ../irg.a *.f
$!
$!    then remove all current files in scratch] any C-code will have to
$!    be delt with individually.
$!
$!    - Any new include files should be added to the forconv.dat file
$!    as appropriate. In addition the forconv.dat file will require
$!    modification to the release form before executing this procedure
$!    in earnest. (I.e. references to development directories
$!    for IRH_COM etc. should be changed to those appropriate for
$!    a complete release, whatever they may be (13-3-92 ) ).
$!
$!    - Include files should have any machine and file types removed
$!    when transfered ( I.e. IRG_PAR_SUN4.F should become
$!    irg_par, IRG_ERR.FOR becomes irg_err ).

$! Authors:
$!    PDRAPER: Peter Draper (STARLINK)
$!    {enter_new_authors_here}

$! History:
$!    13-MAR-1992 (PDRAPER):
$!       Original version.
$!    {enter_changes_here}
$!-
$!
$! get clean working directory
$!
$ if f$search("[.build]*.*") .nes. "" then del [.build]*.*;*
$ if f$search("BUILD.DIR") .eqs. "" then cre/dir [.build]
$ set def [.build]
$!
$! Get the Unix include files and IRG document set.
$!
$ cms fetch UNIX_INCLUDES "Building Unix system"
$ cms fetch DOCUMENTS     "Building Unix system"
$!
$! convert these to stream_lf format and strip dependencies from names.
$!
$ loop1:
$    file = f$search("*.*")
$    if ( file .eqs. "" ) then goto next1
$    name = f$parse( file,,,"NAME")
$    type = f$parse( file,,,"TYPE")
$    new_name = "''name'" - "_SUN4"
$    if ( name .nes. new_name ) then -
                     rename/nolog 'name''type' 'new_name''type'
$    convert/fdl=sys$input 'new_name''type' [-]'new_name''type'
IDENT	"12-MAY-1988 15:04:29   VAX-11 FDL Editor"

SYSTEM
	SOURCE			VAX/VMS

FILE
	BEST_TRY_CONTIGUOUS	no
	CLUSTER_SIZE		10
	CONTIGUOUS		no
	EXTENSION		0
	GLOBAL_BUFFER_COUNT	0
	NAME			""
	ORGANIZATION		sequential

RECORD
	BLOCK_SPAN		yes
	CARRIAGE_CONTROL	carriage_return
	FORMAT			stream_LF
	SIZE			0

$ goto loop1
$ next1:
$ delete/nolog/noconfirm *.*;*
$!
$! fetch the non-vms specific fortran and C.
$!
$ cms fetch GENERAL       "Building Unix system "
$ cms fetch UNIX_SPECIFIC "Building Unix system"
$!
$! copy all fortran code into a IRG.FOR file and use forconv.dat file to
$! change INCLUDE statements.
$!
$ copy/log *.for,*.f irg.for
$ spt
$ cms fetch forconv.dat
$ forconv -iirg.for -o[-]irg.f -sforconv.dat -q
$!
$! copy c into similar file converting to stream_lf using forconv
$!
$ copy/log *.c irg.c
$ forconv -iirg.c -o[-]find_file.c -q
$!
$! get makefile
$!
$ cms fetch irg_makefile.
$ convert/fdl=sys$input irg_makefile. [-]irg_makefile.
IDENT	"12-MAY-1988 15:04:29   VAX-11 FDL Editor"

SYSTEM
	SOURCE			VAX/VMS

FILE
	BEST_TRY_CONTIGUOUS	no
	CLUSTER_SIZE		10
	CONTIGUOUS		no
	EXTENSION		0
	GLOBAL_BUFFER_COUNT	0
	NAME			""
	ORGANIZATION		sequential

RECORD
	BLOCK_SPAN		yes
	CARRIAGE_CONTROL	carriage_return
	FORMAT			stream_LF
	SIZE			0

$!
$! delete all files.
$!
$ delete/noconfirm/nolog *.*;*
$ set def [-]
$!
$ exit
$! $Id$
