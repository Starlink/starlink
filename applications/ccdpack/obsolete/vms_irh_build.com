$!+
$! Name:
$!    VMS_IRH_BUILD

$! Purpose:
$!    Selects all the files required for producing a VMS version of IRH

$! Language:
$!    DCL

$! Invocation:
$!    @VMS_IRH_BUILD

$! Description:
$!    This procedure produces the text and object libraries and also
$!    the include files for producing a VMS IRH system. Before
$!    using this procedure the current CMS library should be set to
$!    IRH with the following groups in place.
$!
$!      o GENERAL        - code which is machine non-specific
$!      o VMS_INCLUDES   - ALL the include files required for the VMS
$!                         system (regardless of machine dependent or
$!                         not)
$!      o DOCUMENTS      - Documents describing the IRG subroutine
$!                         library. 

$! Prior Requirements:
$!    - CMS library must be set.
$!    - Target directory must be named IRH_DIR.

$! Notes:
$!    - VMS Include files will have any machine types removed when
$!    transfered ( I.e. IRH_PAR_VMS.FOR will become IRH_PAR.FOR ).

$! Authors:
$!    PDRAPER: Peter Draper (STARLINK)
$!    {enter_new_authors_here}

$! History:
$!    19-MAR-1992 (PDRAPER):
$!       Original version.
$!    {enter_changes_here}
$!-
$! Get current default directory (work is performed in a sub-directory
$! of this).
$!
$ if f$search("[.build]*.*") .nes. "" then del [.build]*.*;*
$ if f$search("BUILD.DIR") .eqs. "" then cre/dir [.build]
$ set def [.build]
$!
$! Get the VMS include files and the IRH document set.
$!
$ cms fetch VMS_INCLUDES "Building VMS system"
$ cms fetch DOCUMENTS    "Building VMS system"
$ cms fetch VMS_OPTIONS  "Building VMS system"
$!
$! convert VMS includes to ordinary includes and set logical names to
$! point to these for compilation purposes.
$!
$ loop1:
$    file = f$search("*.*")
$    if ( file .eqs. "" ) then goto next1
$    name = f$parse( file,,,"NAME")
$    type = f$parse( file,,,"TYPE")
$    new_name = "''name'" - "_VMS"
$    copy/nolog 'name''type' IRH_DIR:'new_name''type'
$    if ( type .eqs. ".FOR" ) then define/log  'new_name' IRH_DIR:'new_name'
$ goto loop1
$ next1:
$ delete/noconfirm/nolog *.*;*
$ cms fetch GENERAL       "Building VMS system "
$!
$! Create libraries and include source and compiled object code
$!
$ library/create      irh_dir:irh.olb
$ library/create/text irh_dir:irh.tlb
$ loop2:
$    file = f$search("*.for")
$    if file.eqs."" then goto next2
$    name = f$parse( file,,,"NAME")
$    library/text irh_dir:irh  'name'.for
$    fortran 'name'
$    library irh_dir:irh 'name'
$    write sys$output "   ",name," added to IRH libraries"
$    delete/noconfirm/nolog 'name'.*;*
$ goto loop2
$ next2:
$!
$! back up one level
$!
$ set default [-]
$ set prot=(O:rwed) build.dir
$ delete build.dir;*/nolog/noconfirm
$ exit
$! $Id$
