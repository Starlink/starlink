# $Id$
# DEVNAGRI.PERL by Ross Moore <ross@mpce.mq.edu.au> 10-1-98
# Mathematics Department, Macquarie University, Sydney, Australia.
#
# Style for LaTeX2HTML v98.1 to construct images of Devanagari script
# using:
#
#  `devnag'  pre-processor and  dvng  fonts
#       by Frans J. Velthuis' <velthuis@rc.rug.nl>
#
#   and its  dev2e.sty  LaTeX-2e interface 
#       by Dominik Wujastyk <D.Wujastyk@ucl.ac.uk>
#
#  Furthermore it can be used with the transcription scheme
#  devised by Jeroen Hellingman, for his  `patc' preprocessor
#  requiring macro files:  dnmacs.tex  and  dntrmacs.tex
#
#
# These resources are *not* included with this package.
# Obtain them from CTAN:  http//ctan.tug.org/ctan
#
# ===================================================================
# This package requires the corresponding LaTeX package:  devnagri.sty .
#
# With LaTeX2HTML the options on the \usepackage line specify which
# preprocessor and transcription mode to use.
#
# Usage:
#
#  \usepackage[devnag]{devnagri}     %|  Velthuis' pre-processor only
#
#  \usepackage[patc]{devnagri}       %|  also uses Jeroen Hellingman's 
#  \usepackage[hindi]{devnagri}      %|    patc -p <option>.pat
#  \usepackage[marathi]{devnagri}    %|  with language options, and macros:
#  \usepackage[nepali]{devnagri}     %|   dnmacs.tex  dntrmacs.tex
#  \usepackage[sanskrit]{devnagri}   %|  
#
#  \usepackage[preprocess]{devnagri} %|  same as  \usepackage[patc]{devnagri}
#  \usepackage{devnagri}             %|  source already pre-processed
#
# ===================================================================
# Warning 1.
#
#  This package works BOTH with source *before* pre-processing
#  and also *after* having pre-processed.
#  The latter may create more smaller images of individual syllabes,
#  whereas the former tends to create larger images of whole lines,
#  paragraphs, sections, etc.
# ===================================================================
# Warning 2.
#
#  To use the  patc  pre-processor, set the variable $PRE_FILTERS
#  to the directory where the pre-processor's  .pat  files are found.
#  This is best done in  latex2html.config .
# ===================================================================
#
# Change Log:
# ===========
# $Log$
# Revision 1.1  2004/02/20 13:13:26  nxg
# Initial import
#
# Revision 1.2  1998/02/03 05:35:03  RRM
#  --  dev2e.sty was written by Wujastyk, not Fairbairns
#
#

package devnagri;

# Put devnagri equivalents here for headings/dates/ etc when
# latex2html starts supporting them ...

sub main'devnagri_translation {
    @_[0];
}

package main;

###  configuration variables  ###
# these may be set in .latex2html-init files

# command-name for the  devnag  pre-processor
$DEVNAG = 'devnag' unless ($DEVNAG);

# command-name for the  patc  pre-processor
$PATC = 'patc' unless ($PATC);


####  IMPORTANT: this variable *must* be set correctly ####
# directory for the  .pat  filter tables
$PRE_FILTERS = '.' unless ($PRE_FILTERS);


# max characters in an inline string
# patc:
$dn_inline = 150 unless ($dn_inline);

# devnag:
$devnag_inline = 150 unless ($devnag_inline);



# pre-processor: devnag
sub do_devnagri_devnag { &pre_process_devnag('') }

# preprocessor: patc
sub do_devnagri_preprocess { &pre_process_devnagri('') }
sub do_devnagri_patc { &pre_process_devnagri('') }
sub do_devnagri_hindi { &pre_process_devnagri('hindi') }
sub do_devnagri_marathi { &pre_process_devnagri('marathi') }
sub do_devnagri_nepali { &pre_process_devnagri('nepali') }
sub do_devnagri_sanskrit { &pre_process_devnagri('sanskrit') }



sub pre_process_devnagri {
    local($pattern) = @_; $pattern = 'dng'; # unless ($pattern);
    $preprocessor_cmds .= 
	"$PATC -p $PRE_FILTERS$dd$pattern.pat ${PREFIX}images.pre ${PREFIX}images.tex\n";
    $preprocessor_cmds .= 
	"$DEVNAG  ${PREFIX}images.pre ${PREFIX}images.tex\n"
		unless ($pattern =~ /devnagri/);

    %other_environments = ( %other_environments
		, "\$\$:\$\$", 'tr_devnagri'
		, "\$:\$", 'devnagri'
	) unless ($prelatex =~ /\@dollar/ );

    %other_environments = ( %other_environments
		, '<hindi>:</hindi>', 'devnagri'
		, '<marathi>:</marathi>', 'devnagri'
		, '<nepali>:</nepali>', 'devnagri'
		, '<sanskrit>:</sanskrit>', 'devnagri'
		, '<hindi.transcription>:</hindi>', 'tr_devnagri'
		, '<marathi.transcription>:</marathi>', 'tr_devnagri'
		, '<nepali.transcription>:</nepali>', 'tr_devnagri'
		, '<sanskrit.transcription>:</sanskrit>', 'tr_devnagri'
        );
    $PREPROCESS_IMAGES = 1;
}

sub pre_process_devnag {
    $PREPROCESS_IMAGES = 1;
    $preprocessor_cmds .= 
	"$DEVNAG  ${PREFIX}images.pre ${PREFIX}images.tex\n"
	    unless ($preprocessor_cmds =~ /devnag/);
	%other_environments = ( %other_environments
		, "\$\$:\$\$", 'tr_devnag'
		, "\$:\$", 'devnag'
	) if ($prelatex =~ /\@dollar/ );
}


sub do_env_pre_devnagri {
    local($_) = @_; 
    $_ = &revert_to_raw_tex($_);

    if (/\\par\b/m) {
	local(@paragraphs, @dn_processed, $this_par);
	local($par_start, $par_end) = ('<P', "</P>\n");
	$par_start .= (($USING_STYLE)? " CLASS=\"DEV\"":''). '>';
	@paragraphs = (split(/\\par\b/, $_ ));
	while (@paragraphs) {
	    $this_par = shift @paragraphs;
	    $this_par =~ s/\s$//;
	    if ($this_par =~ /^\s*$/) {
	        push(@dn_processed, "\n<P></P>\n");
	    } else {
	        $_ = &process_in_latex("<hindi>$this_par</hindi>");
	        push(@dn_processed
		    , &make_comment('DEVANAGARI', $this_par)
		    , $par_start , $_ , $par_end);
	    }
	}
	join('', @dn_processed );
    } else {
	local($comment);
	if (length($_) < $devnag_inline) {
	    $_ = &process_undefined_environment('tex2html_dng_inline'
	    , ++$global{'max_id'}, "<hindi>$_</hindi>");
	} else { 
	    $comment = join('', &make_comment('DEVANAGARI', $_),"\n");
	    $_ = &process_in_latex("<hindi>\n$_\n</hindi>")
	}
	if ($USING_STYLES) {
	    $env_style{'DEV'} = " " unless ($env_style{'DEV'});
	    join('', $comment, '<SPAN CLASS="DEV">', $_, '</SPAN>');
	} else { $comment . $_ }
    }
}
$begin_preprocessor{'devnagri'} = '<hindi>';
$end_preprocessor{'devnagri'} = '</hindi>';


sub do_env_pre_devnag {
    local($_) = @_; 
    $_ = &revert_to_raw_tex($_);

    if (/\\par\b/m) {
	local(@paragraphs, @dn_processed, $this_par);
	local($par_start, $par_end) = ('<P', "</P>\n");
	$par_start .= (($USING_STYLE)? " CLASS=\"DEV\"":''). '>';
	@paragraphs = (split(/\\par\b/, $_ ));
	while (@paragraphs) {
	    $this_par = shift @paragraphs;
	    $this_par =~ s/\s$//;
	    if ($this_par =~ /^\s*$/) {
	        push(@dn_processed, "\n<P></P>\n");
	    } else {
	        $_ = &process_in_latex("\{$this_par\}");
	        push(@dn_processed
		    , &make_comment('DEVANAGARI', $this_par)
		    , $par_start , $_ , $par_end);
	    }
	}
	join('', @dn_processed );
    } else  {
	local($comment);
	if (length($_) < $devnag_inline) {
	    $_ = &process_undefined_environment('tex2html_dng_inline'
	    , ++$global{'max_id'}, "\{$_\}");
	} else { 
	    $comment = join('', &make_comment('DEVANAGARI', $_),"\n");
	    $_ = &process_in_latex("\{$_\}")
	}
	if ($USING_STYLES) {
	    $env_style{'DEV'} = " " unless ($env_style{'DEV'});
	    join('', $comment, '<SPAN CLASS="DEV">', $_, '</SPAN>');
	} else { $comment . $_ }
    }
}
$begin_preprocessor{'devnag'} = "\{";
$end_preprocessor{'devnag'} = "\}";


sub do_env_pre_tr_devnagri { 
    local($_) = @_;
    open(DNTR,">dntr.tmp") || print "\n *** cannot open dntr.tmp ***" ;
    print DNTR "\$\$", &revert_to_raw_tex($_), "\$\$";
    close DNTR;
    &syswait("patc -p $PRE_FILTERS${dd}dng.pat dntr.tmp dntr.tmp1");
    &slurp_input_and_partition_and_pre_process('dntr.tmp1');
    unlink ('dntr.tmp', 'dntr.tmp1') unless $DEBUG;

    $_ = &translate_environments($_);
    $_ = &translate_commands($_);

    if ($USING_STYLES) {
        $env_style{'DEVRM'} = " " unless ($env_style{'DEVRM'});
        join('','<SPAN CLASS="DEVRM">', $_, '</SPAN>');
    } else { $_ }
}
$begin_preprocessor{'tr_devnag'} = '<hindi.transcription>';
$end_preprocessor{'tr_devnag'} = '</hindi>';


$image_switch_rx .= "|dn|(eight|nine|ten|eleven|twelve|fourteen|seventeen)dev";

$DNCURMF = '';
$DNCURRM = '';

sub do_cmd_dn { &process_dn('dn', @_ ) }

sub process_dn {
    local($dnsize, $_) = @_;
    if (($dnsize eq 'dn')&&($DNCURMF)) { $dnsize = "$DNCURRM\\dn" }
    local($devn) = &revert_to_raw_tex($_);

    if ($devn =~ /\\par\b/m) {
	local(@paragraphs, @dn_processed, $this_par);
	local($par_start, $par_end) = ('<P', "</P>\n");
	$par_start .= (($USING_STYLE)? " CLASS=\"DEV\"":''). '>';
	@paragraphs = (split(/\\par\b/, $devn ));
	while (@paragraphs) {
	    $this_par = shift @paragraphs;
	    $this_par =~ s/\s$//;
	    if ($this_par =~ /^\s*$/) {
	        push(@dn_processed, "\n<P></P>\n");
	    } else {
	        $devn = &process_in_latex("\{\\$dnsize $this_par\}");
	        push(@dn_processed
		    , &make_comment('DEVANAGARI', $this_par)
		    , $par_start , $devn , $par_end);
	    }
	}
	join('', @dn_processed );
    } else  {
	local($devn) = join('',"\{\\$dnsize\\, ", $devn, "}\\,");
	local($comment);
	if (length($devn) < $dn_inline) {
	    $devn = &process_undefined_environment('tex2html_dn_inline'
	    , ++$global{'max_id'}, $devn);
	} else { 
	    $comment = join('', &make_comment('DEVANAGARI',$devn),"\n");
	    $devn = &process_in_latex($devn)
	}
	if ($USING_STYLES) {
	    $env_style{'DEV'} = " " unless ($env_style{'DEV'});
	    join('', $comment, '<SPAN CLASS="DEV">', $devn, '</SPAN>');
	} else { $comment . $devn }
    }
}

sub do_cmd_dnx { &do_cmd_dn(@_) }
sub do_cmd_eightdev { &process_dn('eightdev', @_) }
sub do_cmd_ninedev { &process_dn('ninedev', @_) }
sub do_cmd_tendev { &process_dn('tendev', @_) }
sub do_cmd_elevendev { &process_dn('elevendev', @_) }
sub do_cmd_twelvedev { &process_dn('twelvedev', @_) }
sub do_cmd_fourteendev { &process_dn('fourteendev', @_) }
sub do_cmd_seventeendev { &process_dn('seventeendev', @_) }

# these support size changes in Jeroen Hellingman's 
# Devanagari extension to his Malayalam-TeX
sub do_cmd_dnsmall { &set_dncurrm('smalldn', 'smallcr'); @_[0] }
sub do_cmd_dnnine { &set_dncurrm('ninedn', 'ninecr'); @_[0] }
sub do_cmd_dnnormal { &set_dncurrm('dvng', 'rm'); @_[0] }
sub do_cmd_dnhalf { &set_dncurrm('halfdn', 'halfcr'); @_[0] }
sub do_cmd_dnbig { &set_dncurrm('bigdn', 'bigcr'); @_[0] }
sub do_cmd_dnlarge { &set_dncurrm('largedn', 'largecr'); @_[0] }
sub do_cmd_dnhuge { &set_dncurrm('hugedn', 'hugecr'); @_[0] }

sub set_dncurrm { ($DNCURMF, $DNCURRM) = @_ }

sub do_cmd_dntr {
    local($_)= @_;
    local($ACCENT_IMAGES) = "dntr";
    $_ =~ s/^\s*//os;
    &translate_commands($_)
};

sub do_cmd_lii { &process_dn_accent('lii') .@_[0] }
sub do_cmd_rii { &process_dn_accent('rii') .@_[0] }
sub do_cmd_Lii { &process_dn_accent('Lii') .@_[0] }
sub do_cmd_Rii { &process_dn_accent('Rii') .@_[0] }
sub do_cmd_LII { &process_dn_accent('Lii') .@_[0] }
sub do_cmd_RII { &process_dn_accent('Rii') .@_[0] }
sub do_cmd_kh { &process_dn_accent('kh') .@_[0] }
sub do_cmd_Kh { &process_dn_accent('Kh') .@_[0] }
sub do_cmd_KH { &process_dn_accent('KH') .@_[0] }
sub do_cmd_g { &process_dn_accent('g') .@_[0] }
sub do_cmd_G { &process_dn_accent('G') .@_[0] }
sub do_cmd_ltwig { 
    local($_) = @_;
    local($next);
    $next = &missing_braces unless (
        (s/$next_pair_pr_rx/$next=$2;''/e)
        ||(s/$next_pair_rx/$next=$2;''/e));
    join('', &process_dn_accent("ltwig\{$next\}"), $_)
}

sub process_dn_accent{
    local($which) = @_;
    local($afterkern); $afterkern = '\\kern.15em' if ($which =~ /[lh]/i);
    &process_undefined_environment("tex2html_accent_inline",
        , ++$global{'max_id'} , "\{\\dntr\\$which$afterkern\}"); }



sub do_cmd_rn { 
    return(@_) unless $DNNUM;
    local($num, $rsize, $_) = ('', $DNCURRM, @_);
    $rsize = "\\".$rsize if ($rsize);
    $num = &missing_braces unless (
	(s/$next_pair_pr_rx/$num=$2;''/e)
	||(s/$next_pair_rx/$num=$2;''/e));
    join('', &process_in_latex("$rsize{\\dn $num}"), $_ );
}

sub do_cmd_rsize {
    print "\n *** error: \\rsize should not occur explicitly\n"; @_[0]}


&process_commands_nowrap_in_tex (<<_RAW_ARG_NOWRAP_CMDS_);
dnnum # \$DNNUM = 1
cmnum # \$DNNUM = 0
dnnormal
dnsmall
dnnine
dnhalf
dnbig
dnlarge
dnhuge
_RAW_ARG_NOWRAP_CMDS_


1;				# Not really necessary...



