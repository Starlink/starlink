{+          
{  Name:
{     hdstools.icl

{  Purpose:
{     Set up commands for the HDSTOOLS package.

{  Type of Module:
{     ICL command file.

{  Invocation:
{     load hdstools.icl

{  Description:
{     This procedure defines an command for each HDSTOOLS command, and 
{     also defines the help commands.

{  Authors:
{     BLY: M.J. Bly (Starlink, RAL)
{     {enter_new_authors_here}

{  History:
{     12-DEC-1996 (BLY):
{       Original Version, based on `csh' model.
{     {enter_changes_here}

{-

{  Define main package help
 
defhelp hdstools $HDSTOOLS_HELP 0
defstring hdthelp help hdstools
 
{  Basic command definitions.
define hcopy $HDSTOOLS_DIR/hdstools_mon
define hcreate $HDSTOOLS_DIR/hdstools_mon
define hdelete $HDSTOOLS_DIR/hdstools_mon
define hdir $HDSTOOLS_DIR/hdstools_mon
define hdisplay $HDSTOOLS_DIR/hdstools_mon
define hfill $HDSTOOLS_DIR/hdstools_mon
define hget $HDSTOOLS_DIR/hdstools_mon
defstring hhelp help hdstools
define hmodify $HDSTOOLS_DIR/hdstools_mon
define hread $HDSTOOLS_DIR/hdstools_mon
define hrename $HDSTOOLS_DIR/hdstools_mon
define hreset $HDSTOOLS_DIR/hdstools_mon
define hreshape $HDSTOOLS_DIR/hdstools_mon
define hretype $HDSTOOLS_DIR/hdstools_mon
define htab $HDSTOOLS_DIR/hdstools_mon
define hwrite $HDSTOOLS_DIR/hdstools_mon

{ Basic help definitions }
defhelp hcopy $HDSTOOLS_HELP hcopy
defhelp hcreate $HDSTOOLS_HELP hcreate
defhelp hdelete $HDSTOOLS_HELP hdelete
defhelp hdir $HDSTOOLS_HELP hdir
defhelp hdisplay $HDSTOOLS_HELP hdisplay
defhelp hfill $HDSTOOLS_HELP hfill
defhelp hget $HDSTOOLS_HELP hget
defhelp hhelp $HDSTOOLS_HELP hhelp
defhelp hmodify $HDSTOOLS_HELP hmodify
defhelp hread $HDSTOOLS_HELP hread
defhelp hrename $HDSTOOLS_HELP hrename
defhelp hreset $HDSTOOLS_HELP hreset
defhelp hreshape $HDSTOOLS_HELP hreshape
defhelp hretype $HDSTOOLS_HELP hretype
defhelp htab $HDSTOOLS_HELP htab
defhelp hwrite $HDSTOOLS_HELP hwrite

{  Now do the same with alternative names.
define hdt_copy $HDSTOOLS_DIR/hdstools_mon
define hdt_create $HDSTOOLS_DIR/hdstools_mon
define hdt_delete $HDSTOOLS_DIR/hdstools_mon
define hdt_dir $HDSTOOLS_DIR/hdstools_mon
define hdt_display $HDSTOOLS_DIR/hdstools_mon
define hdt_fill $HDSTOOLS_DIR/hdstools_mon
define hdt_get $HDSTOOLS_DIR/hdstools_mon
defstring hdt_help help hdstools
define hdt_modify $HDSTOOLS_DIR/hdstools_mon
define hdt_read $HDSTOOLS_DIR/hdstools_mon
define hdt_rename $HDSTOOLS_DIR/hdstools_mon
define hdt_reset $HDSTOOLS_DIR/hdstools_mon
define hdt_reshape $HDSTOOLS_DIR/hdstools_mon
define hdt_retype $HDSTOOLS_DIR/hdstools_mon
define hdt_tab $HDSTOOLS_DIR/hdstools_mon
define hdt_write $HDSTOOLS_DIR/hdstools_mon

{ Alternative help definitions }
defhelp hdt_copy $HDSTOOLS_HELP hcopy
defhelp hdt_create $HDSTOOLS_HELP hcreate
defhelp hdt_delete $HDSTOOLS_HELP hdelete
defhelp hdt_dir $HDSTOOLS_HELP hdir
defhelp hdt_display $HDSTOOLS_HELP hdisplay
defhelp hdt_fill $HDSTOOLS_HELP hfill
defhelp hdt_get $HDSTOOLS_HELP hget
defhelp hdt_help $HDSTOOLS_HELP hhelp
defhelp hdt_modify $HDSTOOLS_HELP hmodify
defhelp hdt_read $HDSTOOLS_HELP hread
defhelp hdt_rename $HDSTOOLS_HELP hrename
defhelp hdt_reset $HDSTOOLS_HELP hreset
defhelp hdt_reshape $HDSTOOLS_HELP hreshape
defhelp hdt_retype $HDSTOOLS_HELP hretype
defhelp hdt_tab $HDSTOOLS_HELP htab
defhelp hdt_write $HDSTOOLS_HELP hwrite

{  Tell the user that HDSTOOLS commands are now available.

print " "
print "   HDSTOOLS commands are now available -- (Version PKG_VERS)"
print " "
print "   Type `hhelp' or `help hdstools' for help on HDSTOOLS commands"
print " "

{
{  end
{.
