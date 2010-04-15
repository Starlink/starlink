
### File: html3.2.pl
### Language definitions for HTML 3.2


# Note that htmlx.x.pl prior modules are *NOT*already loaded.



sub do_cmd_underline {
    local($_) = @_;
    s/$next_pair_pr_rx//o;
    &lost_argument("underline") unless ($&);
    join('',"<U>$2</U>",$_);
}


### Allow for alignment to work

sub do_env_center {
    local($_) = @_;
    "<DIV ALIGN=\"CENTER\">\n<P ALIGN=\"CENTER\">$_</P>\n</DIV>";
}
sub do_env_flushright {
    local($_) = @_;
    "<DIV ALIGN=\"RIGHT\">\n<P ALIGN=\"RIGHT\">$_</P>\n</DIV>";
}
sub do_env_flushleft {
    local($_) = @_;
    "<DIV ALIGN=\"LEFT\">\n<P ALIGN=\"LEFT\">$_</P>\n</DIV>";
}

sub do_cmd_centerline {
    local($_) = @_;
    s/$next_pair_pr_rx//o;
    &lost_argument("centerline") unless ($&);
    "<DIV ALIGN=\"CENTER\">$&<BR>\n</DIV>$_";
}

sub do_cmd_leftline {
    local($_) = @_;
    s/$next_pair_pr_rx//o;
    &lost_argument("leftline") unless ($&);
    "<DIV ALIGN=\"LEFT\">$&<BR>\n</DIV>$_";
}

sub do_cmd_rightline {
    local($_) = @_;
    s/$next_pair_pr_rx//o;
    &lost_argument("rightline") unless ($&);
    "<DIV ALIGN=\"RIGHT\">$&<BR>\n</DIV>$_";
}


# Color support
#require("$LATEX2HTMLSTYLES/color.perl");


####
####  HTML 3.2 specific extensions
####  by Ross Moore <ross@mpce.mq.edu.au>, 5 Dec 1996.
####

$num_type = "\^\\d+\$";
$percent_type = "\^\\d+(%|\\%|$percent_mark)?\$";
$color_type = "\^\#?\w{6}\$";
$coord_type = "\^(\$|\d+(,\d+)+\$)";
$string_type = $URL_type = "\^.*\$";

$halign_type = ",left,right,center,";
$valign_type = ",top,middle,bottom,";
$shape_type = ",disc,square,circle,";
$fontsize_type = ",1,2,3,4,5,6,7,";
$numstyle_type = "1,a,A,i,I,";

%closed_tags_list = ( 'A' , ''
	, 'ADDRESS' , ''
	, 'APPLET' , 'CODE,HEIGHT,WIDTH'
	, 'BIG' , ''
	, 'BLOCKQUOTE' , ''
	, 'B' , ''
	, 'CAPTION' , ''
	, 'CENTER' , ''
	, 'CITE' , ''
	, 'CODE' , ''
	, 'DD' , ''
	, 'DFN' , ''
	, 'DFN' , ''
	, 'DIR' , ''
	, 'DIV' , ''
	, 'DL' , ''
	, 'DT' , ''
	, 'EM' , ''
	, 'FONT' , ''
	, 'FORM' , ''
	, 'FORM' , ''
	, 'H1','','H2','','H3','','H4','','H5','','H6',''
	, 'HEAD' , ''
	, 'HTML' , ''
	, 'I' , ''
 	, 'KBD' , ''
  	, 'LI' , ''
  	, 'MAP' , ''
  	, 'MENU' , ''
  	, 'OL' , ''
  	, 'OPTION' , ''
  	, 'PRE' , ''
   	, 'P' , ''
   	, 'SAMP' , ''
   	, 'SCRIPT' , ''
   	, 'SELECT' , 'NAME'
    	, 'SMALL' , ''
   	, 'STRIKE' , ''
   	, 'STRONG' , ''
   	, 'STYLE' , ''
   	, 'SUB' , '', 'SUP' , ''
   	, 'TABLE' , '', 'TD', '', 'TH', '', 'TR', ''
   	, 'TEXTAREA' , 'NAME,ROWS,COLS'
   	, 'TITLE' , ''
   	, 'TT' , ''
   	, 'UL' , ''
   	, 'U' , ''
   	, 'VAR' , ''
    );

%unclosed_tags_list = (
	  'AREA' , 'ALT'
	, 'BASE' , 'HREF'
	, 'BASEFONT' , ''
	, 'BR' , ''
	, 'HR' , ''
	, 'IMG' , 'SRC'
	, 'INPUT' , ''
	, 'ISINDEX' , ''
	, 'LINK' , 'HREF'
	, 'META' , 'CONTENT'
	, 'PARAM' , 'NAME'
    );



$A_attribs_rx_list = ",HREF,NAME,REL,REV,TITLE,";
$A__HREF_rx = $URL_type;
$A__NAME_rx = $A__REL_rx = $A__REV_rx = $A__TITLE_rx = $string_type;

$APPLET_attribs = ",ALIGN,";
$APPLET_attribs_rx_list = ",CODE,CODEBASE,NAME,ALT,HEIGHT,WIDTH,HSPACE,VSPACE,";
$APPLET__CODEBASE = $URL_type;
$APPLET__CODE = $string_type;                    # required
$APPLET__NAME = $APPLET__ALT = $string_type;
$APPLET__HEIGHT = $APPLET__WIDTH = $num_type;     # required
$APPLET__HSPACE = $APPLET__VSPACE = $num_type;

$AREA_attribs = ",SHAPE,NOHREF,";
$AREA__SHAPE = ",rect,circle,poly,default,";
$AREA_attribs_rx_list = ",HREF,COORDS,ALT,";
$AREA__HREF_rx = $URL_type;
$AREA__COORDS_rx = $coord_type;
$AREA__ALT_rx = $string_type;


$BASE_attribs_rx_list = ",HREF,";
$BASE__HREF_rx = $URL_type;           # required

$BASEFONT_attribs = ",SIZE,";
$BASEFONT__SIZE = $fontsize_type;

$BODY_attribs = "";
$BODY_attribs_rx_list = ",TEXT,BGCOLOR,LINK,VLINK,ALINK,BACKGROUND,";
$BODY__BGCOLOR_rx = $BODY__LINK_rx = $BODY__VLINK_rx  = $BODY__ALINK_rx =
    $BODY__TEXT_rx = $color_type;
$BODY__BACKGROUND_rx = $string_type;

$BR_attribs = ",CLEAR,";
$BR__CLEAR = ",left,all,right,none,";


$CAPTION_attribs = ",ALIGN,";
$CAPTION__ALIGN = ",top,bottom,";

$DIR_attribs = $DL_attribs = $MENU_attribs = ",COMPACT,";

$DIV_attribs = ",ALIGN,";
$DIV__ALIGN = $halign_type;

$FONT_attribs = ",SIZE,";
$FONT__SIZE = $fontsize_type;
$FONT_attribs_rx_list = ",COLOR,";
$FONT__COLOR_rx = $fontsize_type;

$FORM_attribs = ",METHOD,ENCTYPE,";
$FORM__METHOD = ",get,post,";
$FORM__ENCTYPE = ",text\/plain,application\/x\-www\-form\-urlencoded,";
$FORM_attribs_rx_list = ",ACTION,";
$FORM__ACTION = $URL_type;               # required


$H1_attribs = $H2_attribs = $H3_attribs = $H4_attribs =
$H5_attribs = $H6_attribs = ",ALIGN,";
$H1__ALIGN = $H2__ALIGN = $H3__ALIGN = $H4__ALIGN = $H5__ALIGN = $H6__ALIGN = $halign_type;

$HR_attribs = ",ALIGN,NOSHADE,";
$HR__ALIGN = $halign_type;
$HR_attribs_rx_list = ",SIZE,WIDTH,";
$HR__SIZE_rx = $num_type;
$HR__WIDTH_rx = $percent_type;

$HTML_attribs_rx_list = ",VERSION,";
$HTML__VERSION_rx = $string_type;


$IMG_attribs = ",ALIGN,ISMAP,";
$IMG__ALIGN = $halign_type.$valign_type; $IMG__ALIGN = s/,(center)?,/,/g;
$IMG_attribs_rx_list = ",WIDTH,HEIGHT,BORDER,HSPACE,VSPACE,SRC,USEMAP,ALT,";
$IMG__WIDTH_rx = $IMG__HEIGHT_rx = $IMG__BORDER_rx = $IMG__HSPACE_rx =
 $IMG__VSPACE_rx = $num_type;
$IMG__SRC_rx = $URL_type;      # required
$IMG__USEMAP_rx = $URL_type;
$IMG__ALT_rx = $string_type;


$INPUT_attribs = ",TEXT,CHECKED,ALIGN,";
$INPUT__TEXT = ",text,password,checkbox,radio,submit,reset,hidden,image,";
$INPUT__ALIGN = $halign_type.$valign_type; $INPUT_ALIGN = s/,(center)?,/,/g;
$INPUT_attribs_rx_list = ",SIZE,MAXLENGTH,NAME,VALUE,SRC,";
$INPUT__SIZE_rx = $INPUT__MAXLENGTH_rx = $num_type;
$INPUT__NAME_rx = $INPUT__VALUE_rx = $string_type;
$INPUT__SRC_rx = $URL_type;

$ISINDEX_attribs_rx_list = ",PROMPT,";
$ISINDEX__PROMPT_rx = $string_type;


$LI_attribs = ",TYPE,";
$LI__TYPE = $shape_type . $numstyle_type; $LI_TYPE =~ s/,,/,/g;
$LI_attribs_rx_list = ",VALUE,";
$LI__VALUE_rx = $num_type;

$LINK_attribs_rx_list = ",HREF,TITLE,REL,REV,";
$LINK__HREF_rx = $URL_type;        # required
$LINK__TITLE_rx = $LINK__REL_rx = $LINK__REV_rx = $string_type;

$MAP_attribs_rx_list = ",NAME,";
$MAP__NAME_rx = $string_type;

$META_attribs_rx_list = ",CONTENT,NAME,HTTP\-EQUIV,";
$META__CONTENT_rx = $string_type;   # required
$META__NAME_rx = $string_type;
${META__HTTP-EQUIV_rx} = $string_type;

$OL_attribs = ",COMPACT,TYPE,";
$OL__TYPE = $numstyle_type;
$OL_attribs_rx_list = ",START,";
$OL__START_rx = $num_type;

$OPTION_attribs = ",SELECTED,";
$OPTION_attribs_rx_list = ",VALUE,";
$OPTION__VALUE = $string_type;

$PARAM_attribs_rx_list = ",NAME,VALUE,";
$PARAM__NAME_rx = $string_type;   # required
$PARAM__VALUE_rx = $string_type;

$PRE_attribs_rx_list = ",WIDTH,";
$PRE__WIDTH = $num_type;

$P_attribs = ",ALIGN,";
$P__ALIGN = $halign_type;

$SELECT_attribs = ",MULTIPLE,";
$SELECT_attribs_rx_list = ",SIZE,NAME,";
$SELECT__SIZE_rx = $num_type;
$SELECT__NAME_rx = $string_type;

$STYLE_attribs_rx_list = ",TYPE,";
$STYLE__TYPE_rx = $string_type;


$TABLE_attribs = ",ALIGN,";
$TABLE__ALIGN = $halign_type;
$TABLE_attribs_rx_list = ",CELLPADDING,WIDTH,CELLSPACING,BORDER,";
$TABLE__WIDTH_rx = $percent_type;
$TABLE__BORDER_rx = $TABLE__CELLSPACING_rx = $TABLE__CELLPADDING_rx = $num_type;


$TD_attribs = $TH_attribs = ",ALIGN,VALIGN,NOWRAP,";
$TR_attribs = ",ALIGN,VALIGN,";
$TD__ALIGN = $TH__ALIGN = $TR__ALIGN = $halign_type;
$TD__VALIGN = $TH__VALIGN = $TR__VALIGN = $valign_type;
$TD_attribs_rx_list = $TH_attribs_rx_list = ",COLSPAN,ROWSPAN,WIDTH,HEIGHT,";
$TD__COLSPAN_rx = $TD__ROWSPAN_rx = $TD__WIDTH_rx = $TD__HEIGHT_rx = $num_type;
$TH__COLSPAN_rx = $TH__ROWSPAN_rx = $TH__WIDTH_rx = $TH__HEIGHT_rx = $num_type;


$TEXTAREA_attribs_rx_list = ",COLS,ROWS,NAME,";
$TEXTAREA__COLS = $TEXTAREA__ROWS = $num_type;  # required
$TEXTAREA__NAME = $string_type;

$UL_attribs = ",TYPE,COMPACT,";
$UL__TYPE = $shape_type;



###   HTML3.2  tables,  based upon...
###
### File: html2.2.pl
### Language definitions for HTML 2.2 (Tables)
### Written by Marcus E. Hennecke <marcush@leland.stanford.edu>

### This file simplifies the earlier code, to be compatible with
### the simpler model used in  HTML 3.2
### Two subroutines are redefined.
###   ----  Ross Moore <ross@mpce.mq.edu.au>,  21 Feb 1997

# Translates LaTeX column specifications to HTML. Again, Netscape
# needs some extra work with its width attributes in the <td> tags.

$content_mark = "<cellcontents>";
$wrap_parbox_rx = "(\\\\begin$O\\d+${C}tex2html_deferred$O\\d+$C)?"
    . "\\\\parbox(\\s*\[[^]]*])*\\s*$O(\\d+)$C([\\w\\W]*)$O\\3$C\\s*$O(\\d+)$C([\\w\\W]*)$O\\5$C"
    . "(\\end<<\\d+>>tex2html_deferred<<\\d+>>)?";

sub translate_colspec {
    local($colspec,$celltag) = @_;
    local($cellopen) = "<$celltag ALIGN";
    local($cellclose) = "</$celltag>\n";
    local($len,$pts,@colspec,$char,$cols,$repeat,$celldata);
    local($frames, $rules, $prefix,$border)=('','','','');
    local($NOWRAP) = " NOWRAP";

    $frames  = "l" if ( $colspec =~ s/^\|+// );
    $frames .= "r" if ( $colspec =~ s/\|+$// );
    $rules = "c" if ( $colspec =~ /\|/ );
    $border = " BORDER=\"1\"" if (($frames)||($rules));
    $colspec =~ s/\\[chv]line//g;

    $cols = 0;
    while ( length($colspec) > 0 ) {
	$char = substr($colspec,0,1);
	$colspec = substr($colspec,1);
	if ( $char eq "c" ) {
	    push(@colspec,"$cellopen=\"CENTER\"$NOWRAP>$content_mark$cellclose");
	    $cols++;
	} elsif ( $char eq "l" ) {
	    push(@colspec,"$cellopen=\"LEFT\"$NOWRAP>$content_mark$cellclose");
	    $cols++;
	} elsif ( $char eq "r" ) {
	    push(@colspec,"$cellopen=\"RIGHT\"$NOWRAP>$content_mark$cellclose");
	    $cols++;
	} elsif ( $char eq "p" ) {
	    $colspec =~ s/$next_pair_rx//;
	    push(@colspec,"$cellopen=\"LEFT\">$content_mark$cellclose");
	    $cols++;
	} elsif ( $char eq "|" ) {
	} elsif ( $char eq "@" ) {
	    $colspec =~ s/$next_pair_rx//;
	    $cols++;
	    $celldata = $2;
	    $* = 1;    # multiline matching ON
	    $celldata =~ s/$wrap_parbox_rx/$6/g;
	    $* = 0;    # multiline matching OFF
	    if ( $#colspec < 0 ) {
		$prefix .= "$cellopen=\"CENTER\"$NOWRAP>" .
		    &translate_commands($celldata) . $cellclose;
	    } else {
		$colspec[$#colspec] .= "$cellopen=\"CENTER\"$NOWRAP>" .
		    &translate_commands($celldata) . $cellclose;
	    }
	} elsif ( $char eq "*" ) {
	    $colspec =~ s/$next_pair_rx//;
	    $repeat = $2;
	    $colspec =~ s/$next_pair_rx//;
	    $colspec = "$2"x$repeat . $colspec;
	};
    };

    $colspec[0] = $prefix . $colspec[0];
    ('',$frames,$rules,$cols,@colspec);
}

# convert \\s inside \parbox commands to <BR>s;
sub convert_parbox_newlines {
    local($ptext) = @_;
    $ptext =~ s/\\\\\s*/\\newline /og;
    $ptext;
}


sub do_env_tabular {
    local($_) = @_;
    &get_next_optional_argument;
    s/$next_pair_rx//;
    local($colspec) = $2;
    s/\\\\\s*\[([^]]+)\]/\\\\/g;  # TKM - get rid of [N.n pc] on end of rows...
    s/\\newline\s*\[([^]]+)\]/\\newline/g;
    s/\n\s*\n/\n/g;	# Remove empty lines (otherwise will have paragraphs!)
    local($i,@colspec,$char,$cols,$cell,$htmlcolspec,$frames,$rules);
    local(@rows,@cols,$border);
    local($colspan,$cellcount);
    local($frames);


    $border = ""; $frame = "";
    ($htmlcolspec,$frames,$rules,$cols,@colspec) =
	&translate_colspec($colspec, 'TD');

    $frames .= "t" if ( s/^\s*\\hline// );
    $frames .= "b" if ( s/\\hline\s*$// );
    $rules  .= "r" if ( s/\\[hv]line//g );
    #RRM:  retain any \cline to gobble its argument

    if ( $frames || $rules ) {
	$border = " BORDER=\"1\"";
    };

    # convert \\s inside \parbox commands to \newline s;
    # catch nestings

    while (/\\parbox/) {
        local($parlength) = length($_);
        $* = 1;    # multiline matching ON
        s/$wrap_parbox_rx/&convert_parbox_newlines($6)/eg;
	$* = 0;    # multiline matching OFF

        if ($parlength == length($_)) {
            print "\n*** \\parbox's remain in table!!\n";
	    last; # avoid looping
	}
    }

    @rows = split(/\\\\/);
    $#rows-- if ( $rows[$#rows] =~ /^\s*$/ );
    if ($captions) {
	$captions =~ s/\n+/\n/g;
	$return = join('', (($cap_anchors)? "$cap_anchors\n" : '')
                , "<TABLE CELLPADDING=3$border$_[1]"
		, ($halign ? " ALIGN=\"$halign\"" : '') , ">");
	$return .= "\n<CAPTION>$captions</CAPTION>";
	$captions = '';
	$cap_anchors = '';
    } else {
	$return = join('', "<TABLE CELLPADDING=3$border$_[1]"
		, ($halign ? " ALIGN=\"$halign\"" : '') , ">")
    }
    local($firstrow) = 1;
    local($headcell) = 0;
    foreach (@rows) {
	$return .= "\n<TR VALIGN=\"TOP\">";
	@cols = split(/$html_specials{'&'}/o);
	for ( $i = 0; $i <= $#colspec; $i++ ) {
	    $colspec = $colspec[$i];
	    $colspan = 0;
	    $cell = shift(@cols);
            # Attempt to identify title cells
            if (($firstrow || !$i) && ($cell =~ /\\(text)?(bf|it|sf)|\\large/i)) {
                $headcell = 'TH';
            } else { $headcell = '' }

	    # remove any \parbox commands, leaving the contents
	    #RRM:  actually, there shouldn't be any left  :-)
	    $cell =~ s/\\parbox[^<]*<<(\d*)>>([\w\W]*)<<\1>>/$1/g;

	    # May modify $colspec
	    $cell = &translate_environments($cell);
	    $cell = &translate_commands($cell);
	    # remove leading/trailing space
	    $cell =~ s/^\s*|\s*$//g;
	    $cell = "\&nbsp;" unless ($cell);

	    if ( $colspan ) {
		for ( $cellcount = 0; $colspan > 0; $colspan-- ) {
#PWD: removed as multicolumns break when this is applied.
#		    $colspec[$i++] =~ s/<TD/$cellcount++;"<$celltype"/ge;
                  $cellcount++; $i++;
		}
		$i--;
		$colspec =~ s/>$content_mark/ COLSPAN=$cellcount$&/;
	    };
	    $colspec =~ s/$content_mark/$cell/;
	    if ($headcell) {
		$colspec =~ s/<TD/<$headcell/g;
		$colspec =~ s/<\/TD/<\/$headcell/g;
	    }
	    $return .= $colspec;
	};
	$return .= "</TR>";
	$firstrow = 0;
    };
    $return . "\n</TABLE>";
}


## Mathematics environments

sub process_math_env {
    local($mode,$_) = @_;
    local($labels);
    ($_,$labels) = &extract_labels($_); # extract labels
    local($max_id) = ++$global{'max_id'};
    if ($failed) { return($labels, $comment, $_) };
    if ($BOLD_MATH) {
	($labels, $comment, join('',"<B>", &simple_math_env($_), "</B>"))
    } else { ($labels, $comment, &simple_math_env($_)) }
}

sub make_math_comment{
    local($_) = @_;
    local($scomm,$ecomm)=("\$","\$");
    return() if (/$image_mark/);
    do {
        $scomm = "\\begin{$env}\n";
	$ecomm = "\n\\end{$env}";
    } unless ($env =~/tex2html/);
    $_ = &revert_to_raw_tex;
    $* = 1; s/^\s+//; s/\s+$//; $* = 0;
    $_ = $scomm . $_ . $ecomm;
    return() if (length($_) < 12);
    $global{'verbatim_counter'}++;
    $verbatim{$global{'verbatim_counter'}} = $_;
    &write_mydb('verbatim_counter', $global{'verbatim_counter'}, $_ );
    join('', $verbatim_mark, '#math' , $global{'verbatim_counter'},'#')
}

sub do_env_math {
    local($_) = @_;
    local($math_mode, $failed, $labels, $comment, $img_params) = ("inline",'','');
    $failed = (/$htmlimage_rx/); # force an image
    local($attribs, $border);
    if (s/$htmlborder_rx//o) { $attribs = $2; $border = (($4)? "$4" : 1) }
    local($saved) = $_;
    ($labels, $comment, $_) = &process_math_env($math_mode,$_);
    if ($failed) {
	$_ = join ('', $comment, $labels
            , &process_undefined_environment("tex2html_wrap", $id, $saved));
    } else { $_ = join('', $comment, $labels, " ", $_ ); }
    if ($border||($attributes)) { &make_table( $border, $attribs, '', '', '', $_ ) }
    else { $_ }
}

sub do_env_tex2html_wrap {
    local($_) = @_;
    local($math_mode, $failed, $labels, $comment,$img_params) = ("inline",'','');
    $failed = (/$htmlimage_rx/); # force an image
    local($attribs, $border);
    if (s/$htmlborder_rx//o) { $attribs = $2; $border = (($4)? "$4" : 1) }
    local($saved) = $_;
    s/^\\\(//;    s/\\\)$//;
    ($labels, $comment, $_) = &process_math_env($math_mode,$_);
    if ($failed) {
	$_ = join ('', $comment, $labels
             , &process_undefined_environment("tex2html_wrap", $id, $saved));
    } else { $_ = $comment . $labels ." ".$_; }
    if ($border||($attribs)) { &make_table( $border, $attribs, '', '', '', $_ ) }
    else { $_ }
}

sub do_env_tex2html_wrap_inline {
    local($_) = @_;
    local($math_mode, $failed, $labels, $comment) = ("inline",'','');
    $failed = (/$htmlimage_rx/); # force an image
    local($attribs, $border);
    if (s/$htmlborder_rx//o) { $attribs = $2; $border = (($4)? "$4" : 1) }
    local($saved) = $_;
    s/(^\s*(\$|\\\()\s*|\s*(\$|\\\))\s*$)//g; # remove the \$ signs or \(..\)

    ($labels, $comment, $_) = &process_math_env($math_mode,$_);
    if ($failed) {
	$_ = join ('', $labels, $comment
            , &process_undefined_environment("tex2html_wrap_inline", $id, $saved));
    } else { $_ = join('', $labels, $comment, $_); }
    if ($border||($attribs)) { &make_table( $border, $attribs, '', '', '', $_ ) }
    else { $_ }
}

sub do_env_equation {
    local($_) = @_;
    local($math_mode, $failed, $labels, $comment) = ("equation",'','');
    $failed = (/$htmlimage_rx/); # force an image
    local($attribs, $border);
    if (s/$htmlborder_rx//o) { $attribs = $2; $border = (($4)? "$4" : 1) }
    local($saved) = $_;
    local($sbig,$ebig);
    ($sbig,$ebig) = ('<BIG>','</BIG>')
	if (($DISP_SCALE_FACTOR)&&($DISP_SCALE_FACTOR >= 1.2 ));
    local($math_start,$math_end)= ($sbig,$ebig);

    local($eqno) = '&nbsp;&nbsp;'; # spacer, when no numbering
    $* = 1;
    do { # include the equation number, using a <TABLE>
        $global{'eqn_number'}++;
        $eqno = &do_cmd_theequation();
    } unless ((s/(\\nonumber|\\notag)//g)||(/\\tag/));
    if (s/\\tag(\*)?//){
        # AmS-TEX line-number tags.
        local($nobrack,$before) = ($1,$`);
	$_ = $';
        s/next_pair_pr_rx//o;
	if ($nobrack) { $eqno = $2 }
        else { $eqno = "($2)" }
	$_ = $before;
    }
    $* = 0;

    if ($EQN_TAGS =~ /L/) {
        # equation number on left
        ($math_start,$math_end) =
            ("\n<TABLE WIDTH=\"100%\" ALIGN=\"CENTER\""
	      . (($border)? " BORDER=\"$border\"" : '')
	      . (($attribs)? " $attribs" : '')
	      . ">\n<TR VALIGN=\"MIDDLE\"><TD ALIGN=\"CENTER\">"
	      .$eqno."</TD>\n<TD ALIGN=\"CENTER\" NOWRAP>$sbig"
	    , "$ebig</TD>\n</TR></TABLE>");
	    $border = $attribs='';
    } else {
        # equation number on right
        ($math_start,$math_end) =
            ("\n<TABLE WIDTH=\"100%\" ALIGN=\"CENTER\""
	      . (($border)? " BORDER=\"$border\"" : '')
	      . (($attribs)? " $attribs" : '')
	      . ">\n<TR VALIGN=\"MIDDLE\"><TD></TD>"
              . "<TD ALIGN=\"CENTER\" NOWRAP>$sbig"
	    , "$ebig</TD>\n<TD ALIGN=\"CENTER\">".$eqno."</TD></TR>\n</TABLE>");
	    $border = $attribs='';
    }

    ($labels, $comment, $_) = &process_math_env($math_mode,$_);
    if ($failed) {
	$_ = join ('', $comment, $labels, $math_start
	    , &process_undefined_environment('displaymath', $id, $saved)
	    , $math_end );
    } else {
        $_ = join('', "<P ALIGN=\"CENTER\">", $labels
	       , $comment, $math_start, "\n$_\n", $math_end, "</P>" );
    }
    if ($border||($attribs)) {
	join('',"<BR>\n<DIV ALIGN=\"CENTER\">\n"
            , &make_table( $border, $attribs, '', '', '', $_ )
	    , "\n<BR CLEAR=\"ALL\">");
    } else { $_ }
}

sub do_env_displaymath {
    local($_) = @_;
    local($math_mode, $failed, $labels, $comment) = ("display",'','');
    $failed = (/$htmlimage_rx/); # force an image
    local($attribs, $border);
    if (s/$htmlborder_rx//o) { $attribs = $2; $border = (($4)? "$4" : 1) }
    local($saved) = $_;
    local($sbig,$ebig);
    ($sbig,$ebig) = ('<BIG>','</BIG>')
	if (($DISP_SCALE_FACTOR)&&($DISP_SCALE_FACTOR >= 1.2 ));
    ($labels, $comment, $_) = &process_math_env($math_mode,$_);
    if ($failed) {
	$_ = &process_undefined_environment("displaymath", $id, $saved);
	$_ = "$comment$labels\n<P ALIGN=\"CENTER\">$_</P>"
    } else {
	$_ = "$comment\n<P ALIGN=\"CENTER\">$labels\n$sbig$_$ebig\n</P>"
    }
    if ($border||($attribs)) {
	join('',"<BR>\n<DIV ALIGN=\"CENTER\">\n"
            , &make_table( $border, $attribs, '', '', '', $_ )
	    , "\n<BR CLEAR=\"ALL\">");
    } else { $_ }
}


### Multiline formulas

sub do_env_eqnarray {
    local($_) = @_;
    local($math_mode, $failed, $labels, $comment, $doimage) = ("equation",'','');
    local($attribs, $border);
    if (s/$htmlborder_rx//o) { $attribs = $2; $border = (($4)? "$4" : 1) }
    local($saved) = $_;
    local($sbig,$ebig);
    ($sbig,$ebig) = ('<BIG>','</BIG>')
	if (($DISP_SCALE_FACTOR)&&($DISP_SCALE_FACTOR >= 1.2 ));
    $failed = 1 if ($NO_SIMPLE_MATH); # simplifies the next call
    ($labels, $comment, $_) = &process_math_env($_);
    if (($failed)&&!($NO_SIMPLE_MATH)) {
        $_ = join ('', $labels, $comment
	    , &process_undefined_environment(
	        "eqnarray".(($no_eqn_numbers) ? "star" : '')
	        , $id, $saved));
	$_ = join('',"<P ALIGN=\"CENTER\">"
            , $labels, $comment, $_, "<BR CLEAR=\"ALL\">\n<P>");

    } else {
        $failed = 0;
	s/$htmlimage_rx/$doimage = $&;''/eo ; # force an image
        local($sarray, $srow, $slcell, $elcell, $srcell, $ercell, $erow, $earray);
	($sarray, $elcell, $srcell, $erow, $earray, $sempty) = (
	    "\n<TABLE CELLPADDING=\"0\" ALIGN=\"CENTER\""
	    , "</TD>\n<TD ALIGN=\"CENTER\" NOWRAP>"
	    , "</TD>\n<TD ALIGN=\"LEFT\" NOWRAP>"
	    , "</TD></TR>", "\n</TABLE>", "</TD>\n<TD>" );
	$sarray .= (($no_eqn_numbers) ? ">" :  " WIDTH=\"100%\">" );
	if ($EQN_TAGS =~ /L/) { # number on left
            ($srow, $slcell, $ercell) = (
		"\n<TR VALIGN=\"MIDDLE\"><TD ALIGN=\"LEFT\">"
	        , "</TD>\n<TD NOWRAP ALIGN=", '');
	} else { # equation number on right
            ($srow, $slcell, $ercell) = ("\n<TR VALIGN=\"MIDDLE\">"
	    , "<TD NOWRAP ALIGN="
	    , "</TD>\n<TD ALIGN=\"RIGHT\">" );
	}
	local(@rows,@cols,$eqno,$return,$thismath);
	@rows = split(/\\\\/);
	$#rows-- if ( $rows[$#rows] =~ /^\s*$/ );
	$return = join(''
            , (($border||($attribs))? '': "<BR>")
            , (($doimage)? '' : "\n<DIV ALIGN=\"CENTER\">")
            , (($labels)? $labels : "\n") , $comment, $sarray);
	foreach (@rows) { # displaymath
            $eqno = '&nbsp;&nbsp;';
            do {
	        $global{'eqn_number'}++ ;
		$eqno = &simplify(&do_cmd_theequation());
	        } unless ((s/\\nonumber//)||($no_eqn_numbers));
            $return .= $srow;
	    $return .= $eqno if ($EQN_TAGS =~ /L/);
	    $return .= $slcell;
	    if (s/\\lefteqn//) {
	        $return .= "\"LEFT\" COLSPAN=\"3\">";
		$* =1; s/(^\s*|$html_specials{'&'}|\s*$)//g; $*=0;
		if (($NO_SIMPLE_MATH)||($doimage)||($failed)) {
		    $_ = (($_)? &process_math_in_latex(
		        "indisplay" , '', '', $doimage.$thismath ):'');
	        } elsif ($_) { $_ = &simple_math_env($_) }
	        if ($_) { $return .= join('', $sbig, $_, $ebig, $erow); }
		else { $return .= join('',"\&nbsp;", $erow); }
		next;
	    }

	    # columns to be set using math-modes
	    @cols = split(/$html_specials{'&'}/o);

	    # left column, set using \displaystyle
	    $thismath = shift(@cols);
	    $* =1; $thismath =~ s/(^\s*|\s*$)//g; $*=0;
	    if (($NO_SIMPLE_MATH)||($doimage)||($failed)) {
	        $thismath = (($thismath)? &process_math_in_latex(
		    "indisplay" , '', '', $doimage.$thismath ):'');
	    } elsif ($thismath) { $thismath = &simple_math_env($thismath); }
	    if ($thismath) {
	        $return .= join('',"\"RIGHT\">$sbig",$thismath,"$ebig");
	    } else { $return .= ">\&nbsp;" }

	    # center column, set using \textstyle
	    $thismath = shift(@cols);
	    $* =1; $thismath =~ s/(^\s*|\s*$)//g; $*=0;
	    if (($NO_SIMPLE_MATH)||($doimage)||($failed)) {
	        $thismath = (($thismath)? &process_math_in_latex(
		    "indisplay" , 'text', '', $doimage.$thismath ):'');
	    } elsif ($thismath) { $thismath = &simple_math_env($thismath); }
	    if ($thismath) {
	        $return .= join('', $elcell, $sbig , $thismath, $ebig);
	    } else { $return .= join('', $sempty,"\&nbsp;\&nbsp;") }

	    # right column, set using \displaystyle
	    $thismath = shift(@cols);
	    $* =1; $thismath =~ s/(^\s*|\s*$)//g; $*=0;
	    if (($NO_SIMPLE_MATH)||($doimage)||($failed)) {
	        $thismath = (($thismath)? &process_math_in_latex(
		    "indisplay" , '', '', $doimage.$thismath ):'');
	    } elsif ($thismath) { $thismath = &simple_math_env($thismath); }
	    if ($thismath) {
	        $return .= join('', $srcell, $sbig, $thismath, $ebig, $ercell);
	    } else { $return .= join('', $sempty, "\&nbsp;", $ercell) }

	    $return .= $eqno unless ($EQN_TAGS =~ /L/);
	    $return .= $erow;
	}
        $_ = join('', $return , $earray, (($doimage)? '' : "</DIV>" ));
    }
    if ($border||($attribs)) {
	join('',"<BR>\n<DIV ALIGN=\"CENTER\">\n"
            , &make_table( $border, $attribs, '', '', '', $_ )
	    , "\n<BR CLEAR=\"ALL\">");
    } else { $_ }
}

sub do_env_eqnarraystar {
    local($_) = @_;
    local($math_mode, $failed, $labels, $comment) = ("equation",'','');
    $failed = (/$htmlimage_rx/); # force an image
    local($attribs, $border);
    local($saved) = $_;
    local($sbig,$ebig);
    ($sbig,$ebig) = ('<BIG>','</BIG>')
	if (($DISP_SCALE_FACTOR)&&($DISP_SCALE_FACTOR >= 1.2 ));

    if (($NO_SIMPLE_MATH)||($failed)) {
        local($no_eqn_numbers) = 1;
	$_ = &do_env_eqnarray($_) unless ($failed);
	if ($failed) {
            if ($saved =~ s/$htmlborder_rx//o)
	        { $attribs = $2; $border = (($4)? "$4" : 1) }
            $_ = join('', $labels
	    , &process_undefined_environment("eqnarraystar", $id, $saved));
	}
    } else {
        if (s/$htmlborder_rx//o) { $attribs = $2; $border = (($4)? "$4" : 1) }
        $saved = $_;
        ($labels, $comment, $_) = &process_math_env($_);
	if ($failed) {
            $_ = join('', $labels
	        , &process_undefined_environment("eqnarraystar", $id, $saved));
	}
    }
    if ($border||($attribs)) { $_ = &make_table( $border, $attribs, '', '', '', $_ ) }
    else { $_ }
}

#PWD: Added this. ## Define the multicolumn command
# Modifies the $colspec and $colspan variables of the tabular subroutine
sub do_cmd_multicolumn {
    local($_) = @_;
    local($dmy1,$dmy2,$dmy3,$dmy4,$spancols,$text);
    $spancols = &missing_braces unless (
        (s/$next_pair_pr_rx/$spancols=$2;''/eo)
        ||(s/$next_pair_rx/$spancols=$2;''/eo));
    $colspan = 0+$spancols;
    $colspec =~ /^<([A-Z]+)/;
    local($celltag) = $1;
    s/$next_pair_pr_rx//o;
    ($dmy1,$dmy2,$dmy3,$dmy4,$colspec) = &translate_colspec($2, $celltag);
    s/$next_pair_pr_rx/$text=$2;''/eo;
    $text = &translate_commands($text) if ($text =~ /\\/);
    $text;
}


$raw_arg_cmds{tabular} = 1;
$raw_arg_cmds{tabularstar} = 1;
$raw_arg_cmds{longtable} = 1;
$raw_arg_cmds{longtablestar} = 1;
$raw_arg_cmds{supertabular} = 1;
$raw_arg_cmds{supertabularstar} = 1;


1;
