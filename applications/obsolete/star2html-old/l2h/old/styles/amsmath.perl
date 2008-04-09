# amstex.perl by Ross Moore <ross@mpce.mq.edu.au>  9-30-96
#
# Extension to LaTeX2HTML to load features from AMS-LaTeX
#   amsfonts, amssymb, eucal, eufrak or euscript. 
#
# Change Log:
# ===========

package main;
#

# unknown environments:  alignedat, gathered, alignat, multline
#   \gather([^* ])...\endgather
#   \align([^* ])...\endalign

$abstract_name = "Abstract";
$keywords_name = "Keywords";
$subjclassname = "1991 Subject Classification";
$date_name = "Date published";
$Proof_name = "Proof";


sub do_cmd_title {
    local($_) = @_;
    if (/\\endtitle/) {
	$t_title = &translate_commands($`);
	$t_title =~ s/(^\s*|\s*$)//g;
	return($');
    }
    &get_next_optional_argument;
    local($rest) = $_;
    $rest =~ s/$next_pair_pr_rx//o;
    $_ =  &translate_commands($&);
    &extract_pure_text("liberal");
    s/([\w\W]*)(<A.*><\/A>)([\w\W]*)/$1$3/;  # HWS:  Remove embedded anchors
    ($t_title) = $_;
    $TITLE = $t_title if ($TITLE eq $default_title);
    $TITLE =~ s/<P>//g;		# Remove Newlines
    $TITLE =~ s/\s+/ /g;	# meh - remove empty lines 
    $rest;
}

sub do_cmd_author {
    local($_) = @_;
    if (/\\endauthor/) {
	$t_author = &translate_commands($`);
	$t_author =~ s/(^\s*|\s*$)//g;
	return($');
    }
    &get_next_optional_argument;
    local($rest) = $_;
    $rest =~ s/$next_pair_pr_rx//o;
    ($t_author) =  &translate_commands($&);
    $rest;
}

sub do_cmd_address {
    local($_) = @_;
    if (/\\endaddress/) {
	$t_address = &translate_commands($`);
	$t_address =~ s/(^\s*|\s*$)//g;
	return($');
    }
    &get_next_optional_argument;
    local($rest) = $_;
    $rest =~ s/$next_pair_pr_rx//o;
    ($t_address) =  &translate_commands($&);
    $rest;
}

sub do_cmd_curraddr {
    local($_) = @_;
    &get_next_optional_argument;
    local($rest) = $_;
    $rest =~ s/$next_pair_pr_rx//o;
    ($t_curraddr) =  &translate_commands($&);
    $rest;
}

sub do_cmd_affil {
    local($_) = @_;
    if (/\\endaffil/) {
	$t_affil = &translate_commands($`);
	$t_affil =~ s/(^\s*|\s*$)//g;
	return($');
    }
    &get_next_optional_argument;
    local($rest) = $_;
    $rest =~ s/$next_pair_pr_rx//o;
    ($t_curraddr) = &translate_commands($&);
    $rest;
}

sub do_cmd_dedicatory {
    local($_) = @_;
    &get_next_optional_argument;
    local($rest) = $_;
    $rest =~ s/$next_pair_pr_rx//o;
    ($t_affil) = &translate_commands($&);
    $rest;
}

sub do_cmd_date {
    local($_) = @_;
    s/$next_pair_pr_rx//o;
    ($t_date) = &translate_commands($&);
    $_;
}

sub do_cmd_email {
    local($_) = @_;
    &get_next_optional_argument;
    local($rest) = $_;
    $rest =~ s/$next_pair_pr_rx//o;
    ($t_email) = &make_href("mailto:$2","$2");
    $rest;
}

sub do_cmd_urladdr {
    local($_) = @_;
    s/$next_pair_pr_rx//o;
    ($t_authorURL) = &translate_commands($2);
    $_;
}

sub do_cmd_keywords {
    local($_) = @_;
    s/$next_pair_pr_rx//o;
    ($t_keywords) = &translate_commands($2);
    $_;
}

sub do_cmd_subjclass {
    local($_) = @_;
    s/$next_pair_pr_rx//o;
    ($t_subjclass) = &translate_commands($2);
    $_;
}

sub do_cmd_translator {
    local($_) = @_;
    s/$next_pair_pr_rx//o;
    ($t_translator) = &translate_commands($2);
    $_;
}

sub do_cmd_MR {
    local($_) = @_;
    s/$next_pair_pr_rx//o;
    ($t_math_rev) = &translate_commands($2);
    $_;
}

sub do_cmd_PII {
    local($_) = @_;
    s/$next_pair_pr_rx//o;
    ($t_publ_index) = &translate_commands($2);
    $_;
}

sub do_cmd_copyrightinfo {
    local($_) = @_;
    s/$next_pair_pr_rx//o;
    ($t_copyright_year) = &translate_commands($2);
    s/$next_pair_pr_rx//o;
    ($t_copyright_holder) = &translate_commands($2);
    $_;
}



sub do_cmd_AmS {
    local($_) = @_;
    "<i>AmS</i>".$_;
}

sub do_cmd_AmSTeX {
    local($_) = @_;
    "<i>AmS-TeX</i>" . $_;
}

sub do_cmd_maketitle {
    local($_) = @_;
    local($the_title) = '';
    if ($t_title) {
	$the_title .= "<H1 ALIGN=CENTER>$t_title</H1>\n";
    } else { &write_warnings("This document has no title."); }
    if ($t_author) {
	$the_title .= "<P ALIGN=CENTER><STRONG>$t_author</STRONG></P>\n";
    } else { &write_warnings("There is no author for this document."); }
    if ($t_translator) {
	$the_title .= "<BR><P ALIGN=CENTER>Translated by $t_translator</P>\n";}
    if ($t_affil) {
	$the_title .= "<BR><P ALIGN=CENTER><I>$t_affil</I></P>\n";}
    if ($t_date) {
	$the_title .= "<BR><P ALIGN=CENTER><I>Date:</I> $t_date</P>\n";}

    if ($t_address) {
	$the_title .= "<BR><P ALIGN=LEFT><FONT SIZE=-1>$t_address</FONT></P>\n";
    } else { $the_title .= "<P ALIGN=LEFT>"}
    if ($t_email) {
	$the_title .= "<P ALIGN=LEFT><FONT SIZE=-1>$t_email</FONT></P>\n";
    } else { $the_title .= "</P>" }
    if ($t_keywords) {
	$the_title .= "<BR><P><P ALIGN=LEFT><FONT SIZE=-1>".
	    "Key words and phrases: $t_keywords</FONT></P>\n";}
    if ($t_subjclass) {
	$the_title .= "<BR><P><P ALIGN=LEFT><FONT SIZE=-1>".
	    "1991 Mathematics Subject Classification: $t_subjclass</FONT></P>\n";}

    $the_title . $_ ;
}



sub do_cmd_boldsymbol {
    local($_) = @_;
    s/$next_pair_pr_rx//o;
    $_ = join('',"<B><I>$2</I></B>",$_);
    $_;
}

sub do_cmd_nobreakspace {
    $_ = join('',"&nbsp;",$_);
    $_;
}




# some simplifying macros that like
# to existing LaTeX constructions.

sub do_cmd_eqref {
    join('',"(",&do_cmd_ref(@_),")");
}

sub do_cmd_numberwithin {
    local(*_) = @_;
    local($ctr, $within);
    $ctr = &get_next(1);
    $within = &get_next(1);
    &addto_dependents($within,$ctr) if ($within);
    $_;
}


###   Special environments, for mathematics

# the {equation*} environment is equivalent to {displaymath}
sub do_env_equationstar {
    &do_env_displaymath(@_);
}



#  Suppress the possible options to   \usepackage[....]{amstex}
#  and  {amsmath}  {amsopn}  {amsthm}

sub do_amstex_noamsfonts {
}
sub do_amstex_psamsfonts {
}
sub do_amstex_intlimits {
}
sub do_amstex_nointlimits {
}
sub do_amstex_intlim {
}
sub do_amstex_nosumlim {
}
sub do_amstex_nonamelim {
}
sub do_amstex_nolimits {
}
sub do_amstex_sumlimits {
}
sub do_amstex_nosumlimits {
}
sub do_amstex_namelimits {
}
sub do_amstex_nonamelimits {
}
sub do_amstex_leqno {
}
sub do_amstex_reqno {
}
sub do_amstex_fleqn {
}
sub do_amstex_centereqn {
}
sub do_amstex_centertags {
}
sub do_amstex_tbtags {
}
sub do_amstex_righttag {
}
sub do_amstex_ctagsplt {
}


%AMSenvs = (
	  'cases' , 'endcases'
	, 'matrix'  , 'endmatrix'
	, 'bmatrix' , 'endbmatrix'
	, 'Bmatrix' , 'endBmatrix'
	, 'pmatrix' , 'endpmatrix'
	, 'vmatrix' , 'endvmatrix'
	, 'Vmatrix' , 'endVmatrix'
	, 'smallmatrix' , 'endsmallmatrix'
	, 'align'    , 'endalign'
	, 'alignat'  , 'endalignat'
	, 'xalignat' , 'endxalignat'
	, 'xxalignat', 'endxxalignat'
	, 'aligned'  , 'endaligned'
	, 'topaligned'  , 'endtopaligned'
	, 'botaligned'  , 'endbotaligned'
	, 'alignedat', 'endalignedat'
	, 'flalign'  , 'endflalign'
	, 'gather'   , 'endgather'
	, 'multline' , 'endmultline'
	, 'heading' , 'endheading'
	, 'proclaim' , 'endproclaim'
	, 'demo' , 'enddemo'
	, 'roster' , 'endroster'
	, 'ref' , 'endref'
)


&ignore_commands( <<_IGNORED_CMDS_);
comment # <<\\endcomment>>
displaybreak
allowdisplaybreak
allowdisplaybreaks
intertext
spreadlines
overlong
allowtthyphens
hyphenation
BlackBoxes
NoBlackBoxes
split
text
thetag
mspace # {}
smash # []
topsmash
botsmash
medspace
negmedspace
thinspace
negthinspace
thickspace
negthickspace
hdots
hdotsfor # &ignore_numeric_argument
hcorrection # &ignore_numeric_argument
vcorrection # &ignore_numeric_argument
topmatter
endtopmatter
overlong
nofrills
phantom # {}
hphantom # {}
vphantom # {}
minCDarrowwidth # {}
_IGNORED_CMDS_


&process_commands_in_tex (<<_RAW_ARG_CMDS_);
cases # <<\\endcases>>
matrix # <<\\endmatrix>>
bmatrix # <<\\endbmatrix>>
Bmatrix # <<\\endBmatrix>>
pmatrix # <<\\endpmatrix>>
vmatrix # <<\\endvmatrix>>
Vmatrix # <<\\endVmatrix>>
smallmatrix # <<\\endsmallmatrix>>
align # <<\\endalign>>
alignat # <<\\endalignat>>
xalignat # <<\\endxalignat>>
xxalignat # <<\\endxxalignat>>
aligned # <<\\endaligned>>
alignedat # <<\\endalignedat>>
flalign # <<\\endflalign>>
gather # <<\\endgather>>
multline # <<\\endmultline>>
overset # <<\\to>> # {}
underset # <<\\to>> # {}
oversetbrace # <<\\to>> # {}
undersetbrace # <<\\to>> # {}
lcfrac # <<\\endcfrac>>
rcfrac # <<\\endcfrac>>
cfrac # <<\\endcfrac>>
CD # <<\\endCD>>
fracwithdelims # &ignore_numeric_argument(); # {} # {}
thickfrac # <<\\thickness>> # &ignore_numeric_argument(); # {} # {}
thickfracwithdelims # <<\\thickness>> # &ignore_numeric_argument(); # {} # {}
boxed # {}
mathbb # {}
mathfrak # {}
_RAW_ARG_CMDS_

&process_commands_inline_in_tex (<<_RAW_ARG_CMDS_);
_RAW_ARG_CMDS_


&process_commands_nowrap_in_tex (<<_RAW_ARG_NOWRAP_CMDS_);
numberwithin # {} # {}
_RAW_ARG_NOWRAP_CMDS_


1;                              # This must be the last line

