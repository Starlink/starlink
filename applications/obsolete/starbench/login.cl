# LOGIN.CL -- User login file for the IRAF command language.

# Identify login.cl version (checked in images.cl).
if (defpar ("logver"))
    logver = "IRAF V2.11 May 1997"

set	home		= "U_HOME"
set	imdir		= "U_IMDIR"
set	uparm		= "home$uparm/"
set	userid		= "U_USER"

# Set the terminal type.
if (envget("TERM") == "sun") {
    if (!access (".hushiraf"))
	print "setting terminal type to gterm..."
    stty gterm
} else {
    if (!access (".hushiraf"))
	print "setting terminal type to U_TERM..."
    stty U_TERM
}

# Uncomment and edit to change the defaults.
#set	editor		= vi
#set	printer		= lw
#set	stdimage	= imt800
#set	stdimcur	= stdimage
#set	stdplot		= lw
#set	clobber		= no
#set	filewait	= yes
#set	cmbuflen	= 512000
#set	min_lenuserarea	= 24000
#set	imtype		= "imh"

# IMTOOL/XIMAGE stuff.  Set node to the name of your workstation to
# enable remote image display.  The trailing "!" is required.
#set	node		= "my_workstation!"

# CL parameters you mighth want to change.
#ehinit   = "nostandout eol noverify"
#epinit   = "standout showall"
showtype = yes

# Default USER package; extend or modify as you wish.  Note that this can
# be used to call FORTRAN programs from IRAF.

package user

task	$adb $bc $cal $cat $comm $cp $csh $date $dbx $df $diff	= "$foreign"
task	$du $find $finger $ftp $grep $lpq $lprm $ls $mail $make	= "$foreign"
task	$man $mon $mv $nm $od $ps $rcp $rlogin $rsh $ruptime	= "$foreign"
task	$rwho $sh $spell $sps $strings $su $telnet $tip $top	= "$foreign"
task	$touch $vi $emacs $w $wc $less $rusers $sync $pwd $gdb	= "$foreign"

task	$xc $mkpkg $generic $rtar $wtar $buglog			= "$foreign"
#task	$fc = "$xc -h $* -limfort -lsys -lvops -los"
task	$fc = ("$" // envget("iraf") // "unix/hlib/fc.csh" //
	    " -h $* -limfort -lsys -lvops -los")
task	$nbugs = ("$(setenv EDITOR 'buglog -e';" //
	    "less -Cqm +G " // envget ("iraf") // "local/bugs.*)")
task	$cls = "$clear;ls"

if (access ("home$loginuser.cl"))
    cl < "home$loginuser.cl"
;

keep;   clpackage

prcache directory
cache   directory page type help

# Print the message of the day.
if (access (".hushiraf"))
    menus = no
else {
    clear; type hlib$motd
}

# Delete any old MTIO lock (magtape position) files.
if (deftask ("mtclean"))
    mtclean
else
    delete uparm$mt?.lok,uparm$*.wcs verify-

# List any packages you want loaded at login time, ONE PER LINE.
images          # general image operators
plot            # graphics tasks
dataio          # data conversions, import export
lists           # list processing

# The if(deftask...) is needed for V2.9 compatibility.
if (deftask ("proto"))
    proto       # prototype or ad hoc tasks

tv              # image display
utilities       # miscellaneous utilities
noao            # optical astronomy packages

keep
