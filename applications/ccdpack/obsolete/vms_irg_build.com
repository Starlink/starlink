$!+
$! Name:
$!    VMS_IRG_BUILD

$! Purpose:
$!    Selects all the files required for producing a VMS version of IRG

$! Language:
$!    DCL

$! Invocation:
$!    @VMS_IRG_BUILD

$! Description:
$!    This procedure produces the text and object libraries and also
$!    the include files for producing a VMS IRG system. Before
$!    using this procedure the current CMS library should be set to
$!    IRG with the following groups in place.
$!
$!      o GENERAL        - code which is machine non-specific
$!      o VMS_INCLUDES   - ALL the include files required for the VMS
$!                         system (regardless of machine dependent or
$!                         not)
$!      o VMS_SPECIFIC   - VMS specific routines
$!      o NDF_INTERNALS  - source code for the NDF routines which
$!                         cannot be accessed using the NDF shareable
$!                         transfer vector
$!      o DOCUMENTS      - Documents describing the IRG subroutine
$!                         library. 

$! Prior Requirements:
$!    - CMS library must be set.
$!    - IRH includes must be set.
$!    - NDF_CONST must be set to NDF_DIR:NDF_CONST.
$!    - Target directory must be named IRG_DIR.

$! Notes:
$!    - VMS Include files will have any machine types removed when
$!    transfered ( I.e. IRG_PAR_VMS.FOR will become IRG_PAR.FOR ).

$! Authors:
$!    PDRAPER: Peter Draper (STARLINK)
$!    {enter_new_authors_here}

$! History:
$!    13-MAR-1992 (PDRAPER):
$!       Original version.
$!    {enter_changes_here}
$!-
$! start in a clean sub-directory
$!
$ if f$search("[.build]*.*") .nes. "" then del [.build]*.*;*
$ if f$search("BUILD.DIR") .eqs. "" then cre/dir [.build]
$ set def [.build]
$!
$! Get the VMS include files and the IRG document set.
$!
$ cms fetch VMS_INCLUDES  "Building VMS system"
$ cms fetch DOCUMENTS     "Building VMS system"
$ cms fetch VMS_OPTIONS   "Building VMS system"
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
$    copy/nolog 'name''type' irg_dir:'new_name''type'
$    if ( type .eqs. ".FOR" ) then define/log  'new_name' IRG_DIR:'new_name'
$ goto loop1
$ next1:
$!
$! Fetch the VMS specific, the general and NDF internal fortran code.
$!
$ cms fetch GENERAL       "Building VMS system "
$ cms fetch VMS_SPECIFIC  "Building VMS system"
$ cms fetch NDF_INTERNALS "Building VMS system"
$!
$! Check NDF_CONST logical name, set to sensible value if not set.
$!
$ if f$trnlnm("NDF_CONST") .eqs."" then define NDF_CONST NDF_DIR:NDF_CONST
$!
$! Create libraries and include source and compiled object code
$!
$ library/create      irg_dir:irg.olb
$ library/create/text irg_dir:irg.tlb
$ loop2:
$    file = f$search("*.for")
$    if file.eqs."" then goto next2
$    name = f$parse( file,,,"NAME")
$    library/text irg_dir:irg  'name'.for
$    fortran 'name'
$    library irg_dir:irg 'name'
$    write sys$output "   ",name," added to IRG libraries"
$    delete/noconfirm/nolog 'name'.*;*
$ goto loop2
$ next2:

$!
$! back up one level
$!
$ set default [-]
$ set prot=(O:rwed)  build.dir
$ delete/noconfirm/nolog build.dir;*
$ exit
$! $Id$
