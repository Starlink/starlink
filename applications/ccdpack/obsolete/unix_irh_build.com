$!+
$! Name:
$!    UNIX_IRH_BUILD

$! Purpose:
$!    Selects all the files required for producing a Unix version of
$!    IRH

$! Language:
$!    DCL

$! Invocation:
$!    @UNIX_IRH_BUILD

$! Description:
$!    This procedure produces a series of files suitable for
$!    shipping to a unix machine to produce an IRH system. Before
$!    using this procedure the current CMS library should be set to
$!    IRH with the following groups in place.
$!
$!      o GENERAL        - code which is machine non-specific
$!      o UNIX_INCLUDES  - ALL the include files required for the unix
$!                         system (regardless of machine dependent or
$!                         not)
$!      o UNIX_EXTRAS    - External routines referenced in IRH but not
$!                         present on unix system (AIF will not be
$!                         ported - remove IRMs when library available)
$!      o DOCUMENTS      - Documents describing the IRG subroutine
$!                         library. 
$!    Plus a suitable forconv.dat file which contains the FORCONV
$!    translation tables for INCLUDE statements. 

$! Notes:
$!    - The output files are all STREAM_LF format so can be directly
$!    copied to a Unix machine.
$!
$!    - The IRH system should be built on the destination machine
$!    using the fsplit command to get all the individual units
$!    then compiled and entered into an archive.
$!
$!    [ create a directory scratch
$!
$!    %cd scratch
$!    %fsplit ../irh.f
$!    %f77 -c *.f
$!    %ar cr ../libirg.a *.o
$!    %ar cr ../irg.a *.f
$!
$!    then remove all current files in scratch]
$!
$!    - Any new include files should be added to the forconv.dat file
$!    as appropriate.
$!
$!    - Include files should have any machine and file types removed
$!    when transfered ( I.e. IRH_PAR_SUN4.F should become
$!    irh_par, IRH_ERR.FOR becomes irh_err ).

$! Authors:
$!    PDRAPER: Peter Draper (STARLINK)
$!    {enter_new_authors_here}

$! History:
$!    19-MAR-1992 (PDRAPER):
$!       Original version.
$!    {enter_changes_here}
$!-
$! go to clean sub-directory.
$!
$ if f$search("[.build]*.*") .nes. "" then del [.build]*.*;*
$ if f$search("BUILD.DIR") .eqs. "" then cre/dir [.build]
$ set default [.build]
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
$! fetch the fortran. 
$!
$ cms fetch GENERAL       "Building Unix system "
$ cms fetch UNIX_EXTRAS   "Building Unix system"
$!
$! copy all fortran code into a IRH.FOR file and use forconv.dat file to
$! change INCLUDE statements.
$!
$ copy/log *.* irh.for/exclude=irh.for
$ cms fetch forconv.dat
$ spt
$ forconv -iirh.for -o[-]irh.f -sforconv.dat
$!
$! makefile
$!
$ cms fetch irh_makefile. "Building Unix system"
$ convert/fdl=sys$input irh_makefile. [-]irh_makefile.
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
