# $Id$
# frames.perl - Martin Wilck (martin@tropos.de) 22.5.96
# 
# 
# Extension to the LaTeX2HTML program by Nikos Drakos
#
# Enable LaTeX2HTML to build pages using frames
# (HTML extension for browsers that understand frames)
# 
# Change Log:
# jcl = Jens Lippmann <lippmann@cdc.informatik.tu-darmstadt.de>
# mwk = Martin Wilck
# rrm = Ross Moore <ross@mpce.mq.edu.au>
#
# $Log$
# Revision 1.1  2004/02/20 13:13:28  nxg
# Initial import
#
# Revision 1.5  1998/02/19 22:24:28  latex2html
# th-darmstadt -> tu-darmstadt
#
# Revision 1.4  1997/07/11 11:28:56  RRM
#  -  replace  (.*) patterns with something allowing \n s included
#
# Revision 1.3  1996/12/24 10:25:15  JCL
# typo
#
# Revision 1.2  1996/12/24 10:23:43  JCL
# changed &remove_markers in &replace_markers
#
# (v1.1) 20 June 1996 - rrm
# for compatibilty with segmented documents
#
# (v1.2) 4 July 1996 - rrm
# supporting easy color-changes and using backgrounds.
#
# (v1.0) 22.5.96 - mwk - created


# Different frames are used for the navigation panel buttons,
#    the main text field and the footnotes (if any).
# 
# The package redefines the following subroutines of LaTeX2HTML:
#     - process_footnote
#     - post_process
#     - make_footnotes
#     - make_file
#     - make_head_and_body
#
# This file should be put in your LATEX2HTMLSTYLES directory.
#
# The package will be loaded if the following perl code:
###############################################################################
# if ($FRAMES) {
#     foreach $dir (split(/:/,$LATEX2HTMLSTYLES)) { 
# 	print $dir;
# 	if (-f ($_ = "$dir/frames.perl")) {
# 	    print "Loading $_...\n";
# 	    require ($_);
# 	};
#     } ;
# };
###############################################################################
# is inserted into the latex2html script (uncommented, of course)
#    (I insert it directly before the call to &driver)
# and $FRAMES is set to 1 in one of your configuration files.

package main;

$html_frame_version = 3.0 ;

$frame_implementation = "Netscape";
$BACKGROUND_DIR = "http://home.netscape.com/assist/net_sites/bg/";
$BACKGROUND_DEFAULT = "marble/greenred_marble.gif";

@Netscape_colorset = ("text",'1',"alink",'1',"link",'2',"vlink",'3',"bgcolor",'4');
@Netscape_colorset_star = ("text",'4',"alink",'4',"link",'3',"vlink",'2',"bgcolor",'1');
@Netscape_colorset_star_star = ("text",'4',"alink",'1',"link",'4',"vlink",'3',"bgcolor",'2');

###############################################################################
# PACKAGE OPTIONS - set these in one of your init files !
###############################################################################
# Set $NOFRAMES=1 if you want a <noframes>...</noframes> section to be
#   inserted in your HTML documents, making the contents accessible
#   also for browsers that can't handle frames.
# If $NOFRAMES is not set, these  browsers will only find a short message
#   informing the user that he should use another browser.
$NOFRAMES = 0 unless defined ($NOFRAMES);
$NOFRAMES = 1;

# The height of the top frame containing the navigation buttons
$NAVIGATION_HEIGHT = 40 unless $NAVIGATION_HEIGHT;

# The height of the bottom frame containing footnotes (if there are any
#    on the current page)
$FOOTNOTE_HEIGHT = 80 unless $FOOTNOTE_HEIGHT;

# Additional feature: Choose colors and other options for the different 
#   frames. The layout strings will be inserted into the <BODY ...> declaration
#   of the HTML files. 
# WARNING: The default colors set here may not be what you expect / like!
#
# Text window
$TEXT_COLOR = "bgcolor=\"#ffffff\" text=\"#000000\" link=\"#9944EE\" vlink=\"#0000ff\" alink=\"#00ff00\"" unless $TEXT_COLOR;
#
# Main window (seems to have no effect in NetScape)
if (!$NOFRAMES) { $MAIN_COLOR = "bgcolor=\"#000000\" text=\"#ffffff\"" unless $MAIN_COLOR;}
else { $MAIN_COLOR = "bgcolor=\"#ffffff\" text=\"#000000\"" unless $MAIN_COLOR; }
#
# Navigation window
$NAVIG_COLOR = "bgcolor=\"#ffeee0\" text=\"#ffffff\" link=\"#9944EE\" vlink=\"#FF0000\" alink=\"#00fe00\"" unless $NAVIG_COLOR;
#
# Footnote window
$FOOT_COLOR = "bgcolor=\"#eeeee0\" text=\"#000000\" link=\"#9944EE\" vlink=\"#0000ff\" alink=\"#00fd00\"" unless $FOOT_COLOR;


sub replace_frame_markers {
    # Modifies $_
    local($frame,$frame_data)=("none","none");
    s/$frame_mark<#(.*)#><#(.*)#>/&set_frame_data("$1","$2")/geo;
    @_[0];
}

sub set_frame_data {
    local($frame,$frame_data) = @_;
    $frame_data =~ s/,$//o; $frame_data =~ s/,/ /g;
    ${$frame} = "$frame_data";
    '';    
}


# Implement some use-macros.
# The background can be set only by  \frameoptions
# Colors can be set by  \framecolor or \frameoptions

sub do_cmd_frameoptions {
    local($_) = @_;
    local($frame_data,$bkgrnd_str)=('','');
    local ($frame,$dum)=&get_next_optional_argument;
    if (!($dum)) {$frame = "TEXT";}
    s/$next_pair_pr_rx//o; $frame_data = $2;
    local($rest) = $_;
    $frame_data =~ s/background[\s\t]*\=[\s\t]*([\w\W]*)/
	if (!($1)) { "background=$BACKGROUND_DIR$BACKGROUND_DEFAULT"}
	else { "background=${BACKGROUND_DIR}$1" }/eo;
    if (!($frame_data)) 
	{ print STDERR "\nno frame options, $frame unchanged\n"; return $_;}
    join('',&apply_frame_options(1,$frame,$frame_data),$rest);
}

sub do_cmd_framecolor {
    local($_) = @_;
    local($frame,$frame_data,$frame_test);
    s/$next_pair_pr_rx//o;
    if (!($frame = $2)) 
	{ print STDERR "\nno FRAME specified\n"; return $_;}
    eval { $frame_test = ${$frame."_COLOR"} };
    if (!($frame_test))
	{ print STDERR "\nthere is no frame $frame\n"; 
	    s/$next_pair_pr_rx//o; return $_;}
    s/$next_pair_pr_rx//o;
    local($rest) = $_;
    if (!($frame_data = $2)) 
	{ print STDERR "\nno frame options, $frame unchanged\n"; return $_;}
    join('',&apply_frame_options(0,$frame,$frame_data),$rest);
}


# These are user-macros for imposing complete colorsets.

sub do_cmd_frameColorSet {
    &check_frame_colorset(0,$frame_implementation,$_[0]);
}

sub do_cmd_frameColorSetstar {
    local($_)=@_;
    if (s/^\*//o) {
	&check_frame_colorset(2,$frame_implementation,$');
    } else {
	&check_frame_colorset(1,$frame_implementation,$_[0]);
    }
}

sub do_cmd_frameColorSetstarstar {
    &check_frame_colorset(2,$frame_implementation,$_[0]);
}

sub check_frame_colorset {
    local($reverse, $which, $_) = @_;
    local($frame_data,$frame_test);
    local($frame,$dum)=&get_next_optional_argument;
    if (!($dum)) {$frame = "TEXT";}
    s/$next_pair_pr_rx//o; $frame_data = $2;
    local($rest) = $';
    eval { $frame_test = ${$frame."_COLOR"} };
    if (!($frame_test))
	{ print STDERR "\nthere is no frame $frame\n"; return($rest);}
    if (!($frame_data)) 
	{ print STDERR "\nno colorset specified, $frame unchanged\n"; return($rest);}
    local($colorset);
    if ($reverse == 0) {$colorset="${which}_colorset"}
    elsif ($reverse == 1) {$colorset="${which}_colorset_star"}
    elsif ($reverse == 2) {$colorset="${which}_colorset_star_star"}
    else {$colorset="${which}_colorset"}
    if (!(defined  @$colorset))
	{ print STDERR "\nframes for $which are not supported\n"; return($rest);}	
    local($frame_tmp)=$frame_data;
    local($key, @values);
    local($num) = $frame_tmp =~ s/,/,/g;
    if (!($num > 0)) {
	$framedata =~ s/^[\s\t\n]*//o; $framedata =~ s/[\s\t\n]*$//o; 
	local($cnt) = true;
	$frame_str = '';
	foreach $key (@$colorset) {
	    if ($cnt) { $frame_str .= "$key="; }
	    else {$frame_str .= "$frame_data$key".","}
	    $cnt = !($cnt);
	}
	$frame_str =~ s/,$//o;
    } else {
	@values = split (',',$frame_tmp);
    }
    join('',&apply_frame_options(0,$frame,$frame_str),$rest);
}


sub apply_frame_options {
    local($replace,$frame,$frame_data) = @_ ;
    local($frame_tmp,$option_str, $option) = ('','','');
    local(@previous, @options, @settings, @keys, @done);
    local(%options);
    if (!($frame_mark)) { &initialise_frames() };
    $frame = $frame."_COLOR";
    $frame_tmp = $frame."_TMP";
    # if  $replace=0, impose just the new values,
    # else use existing settings, but replacing with the new values
    if ($replace) {
	$option_str = $${frame_tmp};
	if (!($option_str)) { 
	    $option_str = $$frame; 
	    $option_str =~ s/[\s\t\n]*$//o;
	    @previous = split(' ',$option_str);
   	} else {
	    $option_str =~ s/[\s\t\n]*$//o;
	    @previous = split(' ',$option_str);
   	}
	# recover the existing settings; store in @options hash
	foreach $option (@previous) {
	    $option =~ s/^[\s\t\n]*//o; $option =~ s/[\s\t\n]*$//o;
	    $_ = $option; s/[\s\t\n]*\=[\s\t\n]*//o;
	    if ($&) { $options{$`}=$'; }
	}
	@options = sort keysort @options;
    }
    # process the new values; storing directly into $option_str
    @settings = split(',',$frame_data);
    foreach $option (@settings) {
	$option =~ s/^[\s\t\n]*//o; $option =~ s/[\s\t\n]*$//o;
	$option =~ s/[\s\t\n]*\=[\s\t\n]*//o;
	if ($&) { 
	    if ($` eq "background") {
		$options{$`}="\"$'\"";
	    }
	    elsif (defined &get_named_color) {
		$options{$`}= "\"\#".&get_named_color($')."\"";
	    } else {
		$options{$`}="\"$'\"";
	    }
	} else {print STDERR "\nno value specifed for $frame option: $option\n";}
    };
    # recover the new values from the @options hash
    @keys = keys %options;  # @keys = sort keysort @keys;
    $option_str = '';
    foreach $option (@keys) { 
	$option_str .= "$option\=@options{$option} ";
    }
    # reassign to the  $<frame>_COLOR_TMP  variable
    if ($STARTFRAMES) { 
	${$frame_tmp} = $option_str;
    } else { 
	${$frame} = $option_str ;
    }
# Uncomment next line, for a diagnostic check:
#  print STDERR "\n$frame : $option_str\n";
    "$frame_mark<#$frame#><#$option_str#>";
}



sub apply_framebody_options {
    local($frame,$which,$value) = @_;
    local($body,$option,%previous);
    $frame = "${frame}_COLOR";
    $body = $$frame;
    study $body;
    $body =~ s/^\s*//o; $body =~ s/\s*$//o;
    $body =~ s/\s*\=\s*/\=/g; $body =~ s/\s+/ /g; 
    @previous = split(' ',$body); 
    $body = '';
    foreach $option (@previous) {
	$option =~ s/\=/\=/o;
	if (lc($`) eq $which) { $body .= " $which=\#$value" }
	else { $body .= " $`=$'" }
    }
    $$frame = $body;
}

# These override definitions in color.perl

sub apply_frame_body_options{
    local($which,$value)=@_;
    if ($which eq "background") { $which="bgcolor" };
    &apply_framebody_options("TEXT",$which,$value);
    &apply_framebody_options("MAIN",$which,$value);
}

sub set_frame_section_color {
    if ($next_section_color) {
	&apply_framebody_options("TEXT","text","$next_section_color"); 
	&apply_framebody_options("MAIN","text","$next_section_color"); 
    }
    if ($next_section_bkgnd_color) {
	&apply_body_options("TEXT","bgcolor","$next_section_bkgnd_color");
	&apply_body_options("MAIN","bgcolor","$next_section_bkgnd_color");
    }
}



# Define the subroutine &frame_navigation_panel in your configuration files
#   if you don't like this definition (puts all the buttons, but only the
#   buttons, in the navigation frame).

if (! defined &frame_navigation_panel) {
    sub frame_navigation_panel {
	"$NEXT $UP $PREVIOUS $CONTENTS $INDEX $CUSTOM_BUTTONS";};};

# HINT: You may want to comment out the buttons line ("$NEXT $UP ...")
#   in the definitions of &top_navigation_panel and &bot_navigation_panel
#   in your latex2html.config file if you use this package in order to
#   avoid the buttons showing up in the text window as well. If you do that,
#   the textual links will still be there. Alternatively, you can just set 
#   $NO_NAVIGATION; in that case, no textual links will be there, but the
#   navigation window will remain.

# Here comes the main routine of the package. It is invoked by the (changed)
#    subroutine post_process. It takes over the file handling that that 
#    routine usually performed. Note that the implementation of frames takes
#    place just before the final versions of the files are written, so that
#    everything else LaTeX2html does will be preserved.

sub make_frame_header {
# Arguments: Title of the page, filename, and the whole contents of the file
    local ($title,$file,$contents) = @_;
# Get the contents of the navigation frame
#   (customizable subroutine &frame_navigation_panel !).
    local ($navig) = &frame_navigation_panel ;
# This is the same as usual. Note that the navigation panels defined by
#   $top_navigation and $bot_navigation go into the text frame,
#   not into the navigation frame (see HINT above)!
    local ($top_navigation) = &top_navigation_panel unless $NO_NAVIGATION;
    local ($bot_navigation) = &bot_navigation_panel unless $NO_NAVIGATION;
# Check if there is a reference to $footfile in the text (usually, a footnote
#   reference).
# Besides, insert a "target="footer" tag into these references in order to
#   make them point to the footnote window.
    local ($has_footref) = $contents =~ 
	s/target="footer"/$&/iog;	# RRM
    if ($has_footref) { 
# If there are footnote refs: 3 frames
	$frameset 
	    = "<FRAMESET rows=\"$NAVIGATION_HEIGHT,*,$FOOTNOTE_HEIGHT\">";
# The footnote frame is called "footer"; its contents come from $footfile.
	$footframe = 
	    "<FRAME src=\"$footfile\" name=\"footer\" scrolling=auto>";}
    else {
# Otherwise: no footnote frame required -> only 2 frames.
	$frameset = "<FRAMESET rows=\"$NAVIGATION_HEIGHT,*\">";
	$footframe = "";};
#
# Construct filenames from main name "NAME.html": 
#   Text page: "NAME_ct.html",
#   Navigation page: "NAME_hd.html".
    local ($navigfile,$contfile,$frame_def);
    ($navigfile = $file) =~ s/\.html$/_hd.html/;
    ($contfile = $file) =~ s/\.html$/_ct.html/;
# This is more or less obsolete
#   (normally these titles will never be displayed).
    local ($navigtitle)="Header of $title";
    local ($conttitle)="Contents of $title";
#
# Try to open the three files
    open (OUTFILE, ">$file") || die "Cannot open file $file $!";
    open (NAVIG,">$navigfile") || die "Cannot open file $navigfile $!";
    open (CONT,">$contfile") || die "Cannot open file $contfile $!";
#
# Construct the "<framedef>...</framedef>" section for the top file.
    $frame_def=join 
	("\n", $frameset,
	 "<FRAME src=\"$navigfile\" noresize scrolling=no marginheight=0 marginwidth=0>",
	 "<FRAME src=\"$contfile\" name=\"contents\" scrolling=auto>",
	 $footframe,
	 "</FRAMESET>", "<NOFRAMES>");
    $_ = $contents;
    &replace_frame_markers;
    $contents = $_;
# If $NOFRAMES is set, insert the whole text in the "<noframes>...</noframes>"
#   section.
    local($no_frames) = ($NOFRAMES 
	? $contents
	: "<P><B>Sorry, this can only be read with a Browser that supports frames!</B><P>");
#
# Make the text page first. Note that &make_head_and_body has been altered
#   such that it accounts for the layout string; the last argument 
#   (definitions that go between head and body) is empty.
    $_ = &make_head_and_body($conttitle,$TEXT_COLOR," ");
    $_ = join ("\n",$_,$top_navigation,$contents);
##    local ($flag) = (($BOTTOM_NAVIGATION || &auto_navigation) &&
##	     $bot_navigation);
## ... and bottom navigation panel. --- no need for this  RRM.
##    $_ = join ("\n",$_,$bot_navigation) if $flag;
# Do the usual post-processing. 
    &replace_markers;
    &post_post_process if (defined &post_post_process);
    $_ = join ("\n",$_,"<HR>",&make_address);
    print CONT $_;
    close CONT;
#
# Now go on with the navigation file.
    $_ = &make_head_and_body($navigtitle,$NAVIG_COLOR," ");
    $_ = join ("\n",$_,$navig,"</BODY>","</HTML>\n");
    &replace_markers;
    &post_post_process if (defined &post_post_process);
    print NAVIG $_;
    close NAVIG;
#
# Finally, the main file.
#  $frame_def goes between head and body 
#   (third argument of &make_head_and_body).
    local ($flag) = (($BOTTOM_NAVIGATION || &auto_navigation) &&
	     $bot_navigation);
    if ($flag) {
	$_ = join ("\n",
		&make_head_and_body($title,$MAIN_COLOR,$frame_def),
		$top_navigation, $no_frames, $bot_navigation)
    } else {
	$_ = join ("\n",
		&make_head_and_body($title,$MAIN_COLOR,$frame_def),
		$top_navigation, $no_frames)
    };
    &replace_markers;
    &post_post_process if (defined &post_post_process);
    $_ = join ("\n",$_,"<HR>",&make_noframe_address);
    print OUTFILE $_;
    close OUTFILE;
}

sub make_noframe_address {
    local($_) = $ADDRESS;
    ($_ ? "<P><ADDRESS>\n$_\n</ADDRESS>" : "")."\n</BODY>\n</NOFRAMES>\n</HTML>\n";
}
    
# Altered: &make_href
sub make_frame_href {
    local($link, $text) = @_;
    $name++;
    $text =~ s/<A .*><\/A>//go;
    if ($target) {
	if ($target eq "notarget") {
	    "<A NAME=\"tex2html$name\" HREF=\"$link\">$text</A>";
	} else {
	    "<A NAME=\"tex2html$name\" HREF=\"$link\" target=\"$target\">$text</A>";
	}
    } else {
	"<A NAME=\"tex2html$name\" HREF=\"$link\" target=\"_top\">$text</A>";
    }
}

# Altered: &make_named_href
sub make_frame_named_href {
    local($name, $link, $text) = @_;
    local($namestr) = '';
    if ($name) { $namestr = " NAME=\"$name\""; }
    $text =~ s/<A .*><\/A>//go;
    if ($target) {
	if ($target eq "notarget") {
	    "<A$namestr HREF=\"$link\">$text</A>";
	} else {
	    "<A$namestr HREF=\"$link\" target=\"$target\">$text</A>";
	}
    } else {
	"<A$namestr HREF=\"$link\" target=\"_top\">$text</A>";
    }
}

# Altered: &make_half_href
sub make_frame_half_href {
    local($link) = $_[0];
    $name++;
    if ($target) {
	if ($target eq "notarget") {
	    "<A NAME=\"tex2html$name\" HREF=\"$link\">";
	} else {
	    "<A NAME=\"tex2html$name\" HREF=\"$link\" target=\"$target\">";
	}
    } else {
	"<A NAME=\"tex2html$name\" HREF=\"$link\" target=\"_top\">";
    }
}

# Altered: &do_cmd_footnote	# RRM
sub do_cmd_frame_footnote {
    local($_) = @_;
    local($target) = 'footer';
    s/$next_pair_pr_rx//o;
    local($br_id, $footnote) = ($1, $2);
    &process_footnote($footnote);
    join('',&make_href("$footfile#$br_id",$footnote_mark),$_);
}


# Altered: &replace_cross_references
sub replace_frame_cross_references {
    # Modifies $_
    local($label,$id,$ref_label);
    s/$cross_ref_mark#(\w+)#(\w+)>$cross_ref_mark/
	do {($label,$id) = ($1,$2);
	    $ref_label = $external_labels{$label} unless
		($ref_label = $ref_files{$label});
	    '"'."$ref_label#$label\" target=\"_top".'">'.
		&get_ref_mark($label,$id)
	    }/geo;
    # This is for pagerefs, which cannot have symbolic labels
    s/$cross_ref_mark#(\w+)#\w+>/
	do {$label = $1; 
	    $ref_label = $external_labels{$label} unless
		($ref_label = $ref_files{$label});
	    '"'."$ref_files{$label}#$label\" target=\"_top".'">'
	    }/geo;
}


# Altered: &replace_external_references	# RRM
sub replace_frame_external_references {
    # Modifies $_
    local($label);
    s/$external_ref_mark#(\w+)#(\w+)>$external_ref_mark/
	do {($label,$id) = ($1,$2); 
	    '"'."$external_labels{$label}#$label\" target=\"_top".'">'.
	        &get_ref_mark("userdefined$label",$id)
	    }/geo;
}


# Altered: &process_footnote.
# The only change consists in the definition of $space -
#    those many lines with dots are no longer needed.
sub process_frame_footnote {
    # Uses $before 
    # Sets $footfile defined in translate
    # Modifies $footnotes defined in translate
    local($footnote) = @_;
    local($last_word) = &get_last_word($ref_before);
    local($space) = "\n";
    local($target) = "\n";
    if (! $NO_FOOTNODE) {	
        $footfile = "${PREFIX}footnode.html";
	$space = "\n<HR><P>\n" ;
    }
    $footnotes .= "<DT><A NAME=\"$br_id\">...$last_word</A><DD>" .
	&translate_commands($footnote) . $space;
}

# Altered: &post_process. 
# The main routine that handles section links etc.
sub frame_post_process {
    # Put hyperlinks between sections, add HTML headers and addresses,
    # do cross references and citations.
    # Uses the %section_info array created in sub translate.
    # Binds the global variables
    # $PREVIOUS, $PREVIOUS_TITLE
    # $NEXT, $NEXT_TITLE
    # $UP, $UP_TITLE
    # $CONTENTS
    # $INDEX
    # $NEXT_GROUP, $NEXT_GROUP_TITLE
    # $PREVIOUS_GROUP, $PREVIOUS_GROUP_TITLE
    # Converting to and from lists and strings is very inefficient.
    # Maybe proper lists of lists should be used (or wait for Perl5?)
    # JKR:  Now using top_navigation and bot_navigation instead of navigation
    local($_, $key, $depth, $file, $title, $header, @link, @old_link,
	  $top_navigation, $bot_navigation, @keys,
	  @tmp_keys, $flag, $child_links);
    @tmp_keys = @keys = sort numerically keys %section_info;
    print "\nDoing section links ...";
    while (@tmp_keys) {
        $key = shift @tmp_keys;
	print ".";
	($depth, $file, $title) = split($delim,$section_info{$key});
	unless ($done{$file}) {
	    $PREVIOUS = $PREVIOUS_TITLE = $NEXT = $NEXT_TITLE = $UP = $UP_TITLE =
            $CONTENTS = $INDEX = $NEXT_GROUP = $NEXT_GROUP_TITLE = 
            $PREVIOUS_GROUP = $PREVIOUS_GROUP_TITLE =
	    $_ = $top_navigation = $bot_navigation = undef;
	    @link =  split(' ',$key);
            ($PREVIOUS, $PREVIOUS_TITLE) =
		&add_link($previous_page_visible_mark,$file,@old_link);
	    @old_link = @link;

	    $link[$depth]++;
	    ($NEXT_GROUP, $NEXT_GROUP_TITLE)
		= &add_link($next_visible_mark, $file, @link);
	    	    
	    $link[$depth]--;$link[$depth]--;
	    ($PREVIOUS_GROUP, $PREVIOUS_GROUP_TITLE) =
		&add_link($previous_visible_mark, $file,@link);
	   
	    $link[$depth] = 0;
	    ($UP, $UP_TITLE) = 
		&add_link($up_visible_mark, $file, @link);
	    
	    @link = split(' ',$tmp_keys[0]);
	    ($NEXT, $NEXT_TITLE) = 
		&add_link($next_page_visible_mark, $file,@link);
	    
	    $CONTENTS = &add_special_link($contents_visible_mark, $tocfile, $file)
		if $CONTENTS_IN_NAVIGATION;
	    $INDEX = &add_special_link($index_visible_mark, $idxfile, $file)
		if $INDEX_IN_NAVIGATION;

	    rename($file, "TMP.$file");
	    open(INPUT, "<TMP.$file") || die "Cannot open file TMP.$file $!";
	    &slurp_input("TMP.$file");
#RRM:
	    if (($INDEX) && ($SHORT_INDEX) && ($SEGMENT eq 1)) { 
		&make_index_segment($title,$file); }
#/RRM
	    $child_links = &add_child_links(0,$depth, $key, @keys);
	    $_ = join('', $_, $CHILDLINE)
		if $child_links;
	    $_ = join('', $_, $child_links); 
# File operations are carried out by &make_frame_header.
	    &make_frame_header ($title,$file,$_);
	    $done{$file}++;
	    &cleanup;
	}
    }
}

# Altered: &make_footnotes.
# The only change is the call to &make_file.
sub make_frame_footnotes {
    # Uses $footnotes defined in translate and set in do_cmd_footnote
    # Also uses $footfile
    local($_) = "<DL> $footnotes <\/DL>\n";
    print "\nDoing footnotes ...";
    &replace_frame_markers;
    &replace_markers;
    if ($footfile) {
	&make_file($footfile, "Footnotes", $FOOT_COLOR); # Modifies $_;
	$_ = ""
	}
    $_;
}

# Altered: &make_file.
# It now takes a third argument specifying the layout (colors etc.)
sub make_frame_file {
    # Uses and modifies $_ defined in the caller
    local($filename, $title, $layout) = @_;
    $_ = join('',&make_head_and_body($title,$layout," "),$_,&make_address);
    &text_cleanup;
    open(FILE,">$filename") || print "Cannot open $filename $!\n";
    print FILE $_;
    close(FILE);
}

# Altered: &make_head_and_body
# Takes 2 more arguments: The layout string to be inserted into the
# <BODY ...> statement; and anything that goes between </HEAD> and <BODY>.
sub make_frame_head_and_body {
    local($title,$layout,$before_body) = @_;
    local($version,$isolanguage) = ($HTML_VERSION, 'EN');
    local(%isolanguages) = ('english',	'EN',	'USenglish', 'EN.US',
			    'original',	'EN',	'german',    'DE',
			    'austrian',	'DE.AT','french',    'FR');
    $isolanguage = $isolanguages{$default_language};
    $isolanguage = 'EN' unless $isolanguage;
    $title =~ s/<[^>]*>//g;		# Remove HTML tags
#    "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML $HTML_VERSION//$isolanguage\">\n" .
    "<!DOCTYPE HTML PUBLIC \"- NETSCAPE //$isolanguage\">\n" .
    "<!--Converted with LaTeX2HTML $TEX2HTMLVERSION by Nikos Drakos (nikos\@cbl.leeds.ac.uk), CBLU, University of Leeds -->\n" .
    "<HTML>\n<HEAD>\n<TITLE>" . $title . "</TITLE>\n" .
    &meta_information($title) .
    ($charset && $HTML_VERSION ge "2.1" ? "<META HTTP-EQUIV=\"Content-Type\" CONTENT=\"text/html; charset=$charset\">\n" : "" ) .
    "<LINK REL=STYLESHEET HREF=\"$FILE.css\">\n" .
    "</HEAD>\n$before_body\n<BODY LANG=\"$isolanguage\" $layout>\n";
}


# Settings requested in the preamble take effect immediately;
# for those in the text a frame_marker is inserted, followed
# by the frame_data, which must be applied later.
# To control this, the value of $STARTFRAMES must be changed 
# when the preamble ends.

sub initialise_frames {
    print "\n *** initialising Netscape frames ***";
    $frame_mark = '<tex2html_frame_mark>';
    &do_require_package(color);
    do {	# bind existing subroutines to the `frame' versions
	sub do_cmd_footnote { &do_cmd_frame_footnote(@_); }
	sub process_footnote { &process_frame_footnote(@_); }
	sub make_footnotes { &make_frame_footnotes(@_); }
 	sub make_file { &make_frame_file(@_); }
	sub make_head_and_body { &make_frame_head_and_body(@_); }
	sub make_half_href { &make_frame_half_href(@_); }
 	sub make_named_href { &make_frame_named_href(@_); }
 	sub make_href { &make_frame_href(@_); }
	sub replace_cross_references { &replace_frame_cross_references(@_); }
	sub replace_external_references { &replace_frame_external_references(@_); }
	sub post_process { &frame_post_process(@_); }
 	sub apply_body_options { &apply_frame_body_options(@_); }
 	sub set_section_color { &set_frame_section_color(@_); }
	${AtBeginDocument_hook} .= "\$STARTFRAMES = 1;";
    }
}

if ($HTML_VERSION lt "$html_frame_version" ) { do { 
    print STDERR "\n*** frames are not supported with HTML version: $HTML_VERSION ***\n";
    &ignore_commands( <<_IGNORED_CMDS_);
frameoptions # [] # {}
framecolor # {} # {}
frameColorSet # [] # {}
frameColorSetstar # [] # {}
frameColorSetstarstar # [] # {}
_IGNORED_CMDS_
}} else { &initialise_frames(); }

1;  # This must be the last line.
