# $Id$
# color.perl by Michel Goossens <goossens@cern.ch>  01-14-96
#
# Extension to LaTeX2HTML V 96.2 to support color.sty
# which is part of standard LaTeX2e graphics bundle.
#
# Revisions by Ross Moore <ross@mpce.mq.edu.au>
#  1.  implementing named colors
#  2.  read color specifications from files
#  3.  for compatibility with the option-loading mechanism V96.2 
#  4.  \pagecolor and \color implemented, using $BODYTEXT 
#
#
# Change Log:
# ===========
# $Log$
# Revision 1.1  2004/02/20 13:13:28  nxg
# Initial import
#
# Revision 1.1  1998/08/20 16:03:32  pdraper
# *** empty log message ***
#
# Revision 1.9  1997/06/06 12:02:44  RRM
#  -  Changed some VERBOSITY levels
#  -  added some error-detection to  finding the requested color
#  -  made  \color and  \pagecolor to be of `deferred' type
#
# Revision 1.8  1997/05/19 13:39:21  RRM
#     Recognises color Names, as well as names (note capitalization).
#
# Revision 1.7  1997/05/11 03:56:21  RRM
#      ... and don't forget the "s around attribute values.
#
# Revision 1.6  1997/05/11 03:46:30  RRM
#      Oops, default segment-pagecolor was #111111, now is #FFFFFF (White).
#
# Revision 1.5  1997/05/09 12:10:51  RRM
#     Added some $VERBOSITY tracing, and cosmetic changes to messages.
#
# Revision 1.4  1997/01/03 12:17:24  L2HADMIN
# changes by RRM
#
# Revision 1.3  1996/12/25 03:03:19  JCL
# must ignore also \segmentcolor{} if colors not available
#
# Revision 1.2  1996/12/20 03:59:05  JCL
# changed reserved COLOR_HTML_VERSION to HTML_VERSION for use with 3.2
#
#
# JCL: 16 Sept 1996
#  -    introduced $PREAMBLE
#  -    introduced $COLOR_HTML to explicitly use colors,
#       and $COLOR_HTML_VERSION for future usage.
#
# RRM: 28 June 1996
#  -	added support for named colors, using
#		1. rgb, cmyk, gray  models
#		2. the 8 standard primaries and secondaries
#		3. color names and values read from  rgb.txt
#  -	implemented  \definecolor
#
# RRM: 1 July 1996
#  -	extend color-name search to include all lower-case;
#  -	recognise 6-letter Hex-strings (3 nibbles) as color-names.
#  -	when reading rgb.txt values can be integers or decimals.
#  -	read_rgb_colors  now has filename parameter (e.g. `rgb.txt').
#  -	read read_cmyk_colors  implemented similarly.
#
# RRM: 12 July 1996
#  -	recognises LaTeX's package options;
#  -	implements loading of Crayola colors, via the dvips option;
#
# RRM: 26 July 1996
#  -	\pagecolor and \color implemented, to modify $BODYTEXT;
#  -	added $RGBCOLORFILE variable for main source of named colors;
#

package main;

$html_color_version = "3.2";
$RGBCOLORFILE = "rgb.txt" unless ($RGBCOLORFILE);
$CRAYOLAFILE = "crayola.txt" unless ($CRAYOLAFILE);
$BKGSTRING = "bgcolor";

sub read_rgb_colors {
    local($file) = @_;
    local($prev) = $*;
    local($r,$g,$b,$name,$dir);
    foreach $dir (split(/:/,$LATEX2HTMLSTYLES)) {
	if (-f "$dir/$file") {
            if (open(COLORFILE,"<$dir/$file")) {
		print STDERR "\n(reading colors from $file" if $DEBUG;
		$* = 0;	# Multiline matching OFF
		while (<COLORFILE>) {
	s/^\s*(\d+)\s+(\d+)\s+(\d+)\s+(\w+(\s\w+)*)\s*/
	    ($r,$g,$b,$name)=($1,$2,$3,$4);
	    $named_color{$name} = &encode_rgbcolor($r,$g,$b);
	    print STDERR "\n$name = $named_color{$name}" if ($VERBOSITY > 5);
		/e;
		}
		close(COLORFILE);
		print STDERR ")\n" if $DEBUG;
		last
	    } else { 
		print STDERR "$file could not be opened:$dir\n";
	    }
	} 
    }
    $* = $prev;		# Restore Multiline matching
    $_[0];
}

sub read_cmyk_colors {
    local($file) = @_;
    local($prev) = $*;
    local($c,$m,$y,$k,$name,$dir);
    foreach $dir (split(/:/,$LATEX2HTMLSTYLES)) {
	if (-f "$dir/$file") {
            if (open(COLORFILE,"<$dir/$file")) {
		print STDERR "\n(reading colors from $file";
		$* = 0;			# Multiline matching OFF
		while (<COLORFILE>) {
		    s/\s*$//g;
  s/^\s*(\w+)\s+(\d|\d\.\d*)\s+(\d|\d\.\d*)\s+(\d|\d\.\d*)\s+(\d|\d\.\d*)$/
	    ($name,$c,$m,$y,$k)=($1,$2,$3,$4,$5);
	    if ($named_color{$name}) {
		print STDERR "***$name = \#$named_color{$name}\n"
		    if ($VERBOSITY > 5);}
	    $named_color{$name} = &get_cmyk_color($c,$m,$y,$k);
	    print STDERR "$name = \#$named_color{$name}\n" if ($VERBOSITY > 5);
			/oe; }
		close(COLORFILE);
		print STDERR ")\n";
		last
	    } else { 
		print STDERR "$file could not be opened:$dir\n";
	    }
	} 
    }
    $* = $prev;		# Restore Multiline matching
    $_[0];
}

sub encode_rgbcolor {
    local($r,$g,$b) = @_;
    if (($r =~ s/\./\./o)) {
	&get_rgb_color($r,$g,$b);
    } else {
	local($str)=sprintf("%2x%2x%2x",$r,$g,$b);
	$str =~ s/\s/0/g;
	$str;
#	($r,$g,$b) = map(unpack("H2",pack("S",$_)),($r,$g,$b));
#	"$r$g$b";
    }
}

# colour names are case-sensitive.
# However, if the exact name is not found, 
# then a lowercase version is tried.
# Only if this also fails is the default `black' used.
#
# Hex-strings: #<3 nibbles>  are recognised and return 
# just the nibbles. These can be of either case. 

sub get_named_color {
    local($name) = @_;
    $name =~ s/^[\s\t\n]*//o; $name =~ s/[\s\t\n]*$//o;
    if ($name =~ s/^\#//o ) {
	if (length($name)==6) {
	    $name =~ tr/A-Z/a-z/;
	    return ($name);
	}
    }
    if ($named_color{$name}) { $named_color{$name} }
    elsif ($named_color{$name."1"}) { $named_color{$name."1"} }
    else {
	local($lcname) = $name;
	$lcname =~ tr/A-Z/a-z/;
	print "no color for $name, trying $lcname";
	if ($named_color{$lcname}) { $named_color{$lcname} }
	elsif ($named_color{$lcname."1"}) { $named_color{$lcname."1"} }
	else { 
	    print STDERR "\nunknown color $name, using ";
	    ""; }
    }
}

sub get_rgb_color {
    local($r,$g,$b) = @_;
    if (!("$g$b")) {($r,$g,$b) = split(',',$r)};
    ($r,$g,$b) = (int(255*$r+.5),int(255*$g+.5),int(255*$b+.5));
    local($str)=sprintf("%2x%2x%2x",$r,$g,$b);
    $str=~s/\s/0/g;
    $str;
#    ($r,$g,$b) = map(unpack("H2",pack("S",$_)), ($r,$g,$b));
#    "$r$g$b";
}

sub get_cmyk_color {
    local($c,$m,$y,$k) = @_;
    if (!("$m$y$k")) {($c,$m,$y,$k) = split(',',$c)};
    local($r,$g,$b);
#    ($r,$g,$b) = map( 1-$_-$k, ($c,$m,$y));
    ($r,$g,$b) = (1-$c-$k,1-$m-$k,1-$y-$k);
#    ($r,$g,$b) = map( abs($_)/2+$_/2, ($r,$g,$b));
    $r = 0 unless ($r > 0);
    $g = 0 unless ($g > 0);
    $b = 0 unless ($b > 0);
    &get_rgb_color($r,$g,$b);
}

sub get_gray_color {
    local($gray) = @_;
    $gray = int(255*$gray+.5);
    local($str)=sprintf("%2x%2x%2x",$gray,$gray,$gray);
    $str=~s/\s/0/g;
    $str;
#    $gray = unpack("H2",pack("S",$gray));
#    "$gray$gray$gray";
}

sub do_cmd_DefineNamedColor {
    local($_) = @_;
    local($model,$name,$rest);
    s/$next_pair_pr_rx//o; $model =$2;
    $rest = $_;
#    local($get_string) = "get_${model}_color";
#    if (defined  &$get_string) {
#	$_ = &do_cmd_definecolor($rest);
#    } else {
#	print "\n*** Defining new Color Model \"$model\" ***\n";
#	&do_DefineColorModel($model);
#	s/$next_pair_pr_rx//o; $name =$2;
#	$_ = &do_cmd_definecolor($rest);
#	${${model}_color}{$name} = $_;
#    }
#    $_ = &do_cmd_definecolor($rest);
#    
    $_ = &do_cmd_definecolor($rest);
    $_;
}

sub do_cmd_definecolor {
    local($_) = @_;
    local($name,$model,$hex)=('','','');
    local(@data,$get_string);
    s/$next_pair_pr_rx//o; $name =$2;
    $name =~ s/^\s*//g; $name =~ s/\s*$//g;
    s/$next_pair_pr_rx//o; $model =$2;
    $model =~ s/^\s*//g; $model =~ s/\s*$//g;
    s/$next_pair_pr_rx//o; @data = split(',',$2);
    $get_string = "get_${model}_color";
    if (defined  &$get_string) {
	$hex = &$get_string(@data) ;
	if ($hex) {
	    if ($named_color{$name}) {
		print STDERR "\nredefining existing color: $name = \#$hex\n";
	    } else {
		print STDERR "new color: ($name) = \#$hex\n";
	    }
	    $named_color{$name} = "$hex";
	} else { print "\nfailed to make color: $name\n"; }
    } else { 
	print STDERR "\n$model is not a known color model\n";
    }
    $_;
}

sub initialise_colors {
    print STDERR "\n *** initialising colors ***\n";
    $named_color{'black'} = "000000";
    $named_color{'Black'} = "000000";
    $named_color{'gray'} = "808080";
    $named_color{'Gray'} = "808080";
    $named_color{'white'} = "ffffff";
    $named_color{'White'} = "ffffff";
    $named_color{'red'} = "ff0000";
    $named_color{'Red'} = "ff0000";
    $named_color{'green'} = "008000";
    $named_color{'Green'} = "008000";
    $named_color{'blue'} = "0000ff";
    $named_color{'Blue'} = "0000ff";
    $named_color{'yellow'} = "ffff00";
    $named_color{'Yellow'} = "ffff00";
    $named_color{'cyan'} = "00ffff";
    $named_color{'aqua'} = "00ffff";
    $named_color{'Aqua'} = "00ffff";
    $named_color{'magenta'} = "ff00ff";
    $named_color{'fuchsia'} = "ff00ff";
    $named_color{'Fuchsia'} = "ff00ff";
    $named_color{'lime'} = "00ff00";
    $named_color{'Lime'} = "00ff00";
    $named_color{'maroon'} = "800000";
    $named_color{'Maroon'} = "800000";
    $named_color{'navy'} = "000080";
    $named_color{'Navy'} = "000080";
    $named_color{'olive'} = "808000";
    $named_color{'Olive'} = "808000";
    $named_color{'purple'} = "800080";
    $named_color{'Purple'} = "800080";
    $named_color{'silver'} = "c0c0c0";
    $named_color{'Silver'} = "c0c0c0";
    $named_color{'teal'} = "008080";
    $named_color{'Teal'} = "008080";
    &read_rgb_colors($RGBCOLORFILE);
    ${AtBeginDocument_hook} .= "\&set_section_color; ";
}

# \textcolor is for a `local' color-change to specified text

sub do_cmd_textcolor {
    local($color,$rest) = &find_color;
    if (!($color)) {
	$color= "000000";  # default = black
	print STDERR "\ntext color = black\n";}
    $_= $rest;
    local($text);
    s/$next_pair_pr_rx//o; $text =$2;
    $_=join('',"<FONT color=\#$color>$text</FONT>",$_);
}

# \pagecolor is for a `global' color-change to the background;
#  see Lamport (2nd ed), bottom of p168.

sub do_cmd_pagecolor {
    local($color,$rest) = &find_color;
    if (!($color)) {
	$color= "ffffff";  # default = white
	print STDERR "\npage color = white\n";}
    &apply_body_options($BKGSTRING,"$color");
    $rest;
}

# colorboxes use the `blink' effect; only one color can be used.

sub do_cmd_colorbox {
    local($color,$_) = &find_color;
    s/$next_pair_pr_rx//o;
    join('',"\n<blink><FONT color=\#$color>",$2,"</FONT></blink>\n",$_);
}

sub do_cmd_fcolorbox {
    local($_) = @_;
    s/$next_pair_pr_rx//o;
    &do_cmd_colorbox($_);
}

# the result of a \color command depends upon where it is issued:
#   1.  in the preamble: change the global text-color,
#	and save the color for later (sub)sections;
#   2.  bracketed, within preamble -- ignore it;
#   3.  top-level, within the body: treat as a local color change,
#	and save the color for later (sub)sections;
#   4.  bracketed, within the body: treat as a local color change.

sub do_cmd_color {
    local($color,$rest) = &find_color;
    if (!($color)) {
	$color= "000000";  # default = black
	print STDERR "black\n";
    }
    if (($PREAMBLE)&&($NESTING_LEVEL == 0)) { 
	&apply_body_options("text","$color");
	$next_section_color = $color;
	$rest;
    } elsif ($PREAMBLE) { 
	$rest;
    } elsif ($NESTING_LEVEL == 0) { 
	$next_section_color = $color;
	join('',"\n<FONT color=\#$color>",$rest,"\n</FONT>");
    } else {
	join('',"\n<FONT color=\#$color>",$rest,"\n</FONT>");
    }
}

# use any global color from the previous section
# as the bodytext color for new (sub)sections.
# This is called for each section, from  &translate .

$next_section_color = '';
$next_section_bkgnd_color = '';

sub set_section_color {
    if ($next_section_color) {
	&apply_body_options("text","$next_section_color"); }
    if ($next_section_bkgnd_color) {
	&apply_body_options($BKGSTRING,"$next_section_bkgnd_color"); }
}

sub do_cmd_normalcolor {
    local($_) = @_;
    if ($next_section_color) {
	local($color) = &find_color($next_section_color);
	if ($color) {
	    $_ = join('',"<FONT COLOR =","\"$color\"",$_,"</FONT>");
	}
    }
    $_;
}

# \segmentcolor and \segmentpagecolor may be read
# from the .ptr file for a segment, giving global colors.

sub do_cmd_segmentcolor {
    local($color,$_) = &get_model_color(@_);
    if (!($color)) {
	$color= "000000";  # default = black
	print STDERR "black\n";}
    $next_section_color = $color;
    &set_section_color;
    $_;
}

sub do_cmd_segmentpagecolor {
    local($color,$_) = &get_model_color(@_);
    if (!($color)) {
	$color= "FFFFFF";  # default = white
	print STDERR "\nsegment page color = white\n";}
    $next_section_bkgnd_color = $color;
    &set_section_color;
    $_;
}

sub get_model_color {
    local($_) = @_;
    local($color,$model,$rest);
    s/$next_pair_pr_rx//o; $color =$2;
    $color =~ s/(\w+)\s+/$model=$1/eo;
    $color =~ s/^\s*//g;
    if ($model) { 
	$model = "[$model]";
	$color = $';
	$color =~ s/\s+/,/g;	
    }
    $rest = $_;
    local($color,$_) = &find_color("$model<\#0\#>$color<\#0\#>") if ($color);
    ($color,$rest);
}

sub find_color {
    local($_) = @_;
    local($rest,$get_string,$color);
    local ($model,$dum)=&get_next_optional_argument;
    if (!($dum)) {$model = "named";}
    $model = "named" unless ($model);
    $get_string = "get_${model}_color";
    $color = &missing_braces
	unless ((s/$next_pair_pr_rx/$color=$2;''/eo)
	        ||(s/$next_pair_rx/$color=$2;''/eo));
    $rest =$_;
    if (!(defined &$get_string)) {
	print "\nno routine for $get_string, trying named color: $color\n";
	$get_string = "get_named_color";
    }
    {&$get_string($color),$rest};
}

sub apply_body_options{
    local($which,$value)=@_;
    local($body) = $BODYTEXT;
    local($option,$test,%previous);
    $body =~ s/^\s*//o; $body =~ s/\s*$//o;
    $body =~ s/\s*\=\s*/\=/g; $body =~ s/\s+/ /g; 
    @previous = split(' ',$body); $body = '';
    local($found) = 0;
    foreach $option (@previous) {
	$option =~ s/\=/\=/o;
	$test = $`;
	$test =~ tr/A-Z/a-z/;
	if ($test eq $which) { 
	    $body .= " $which=\"\#$value\"";
	    $found = 1;
	} else { $body .= " $`=$'" }
    }
    $body .= " $which=\"\#$value\"" unless ($found);
    $BODYTEXT = $body;
}


# implement usable options from LaTeX

sub do_color_dvips {
    if (!$styles_loaded{color_dvips}) {
	&read_cmyk_colors($CRAYOLAFILE);
	$styles_loaded{color_dvips} = 1;
    }
}

sub do_color_xdvi {
    &do_color_dvips();
    &do_color_monochrome();
}

sub do_color_dvipsnames {
    &do_color_dvips();
}


# cancel redundant options from LaTeX

sub do_color_monochrome {
}

sub do_color_usenames {
}
sub do_color_dvipsnonames {
}
sub do_color_dvgt {
}
sub do_color_dvi2ps {
}
sub do_color_dvialw {
}
sub do_color_dvilaser {
}
sub do_color_dvipsone {
}
sub do_color_dviwindo {
}
sub do_color_dvitops {
}
sub do_color_emtex {
}
sub do_color_dviwin {
}
sub do_color_oztex {
}
sub do_color_psprint {
}
sub do_color_pubps {
}
sub do_color_textures {
}
sub do_color_pctexps {
}
sub do_color_pctexwin {
}
sub do_color_pctexhp {
}
sub do_color_ln {
}

#	Some declarations

    &process_commands_nowrap_in_tex ( <<_IGNORED_CMDS_);
color # [] # {}
pagecolor # [] # {}
_IGNORED_CMDS_


# Get rid of color specifications, but keep contents,
# when the html version is inappropriate.

if (($HTML_VERSION lt "$html_color_version") && !($NETSCAPE_HTML)) { do { 
    print STDERR "\n*** color is not supported with HTML version: $HTML_VERSION ***\n";
    undef &set_section_color;
    &ignore_commands( <<_IGNORED_CMDS_);
color # [] # {}
textcolor # [] # {}
pagecolor # [] # {}
colorbox # [] # {}
fcolorbox # [] # {} # [] # {}
_IGNORED_CMDS_
}} else { &initialise_colors(); }

1;	# Must be last line
