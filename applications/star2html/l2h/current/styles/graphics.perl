# graphicx.perl  by Ross Moore  <ross@mpce.mq.edu.au>  98/1/7
# same as ...
#
# graphics.perl by Herbert Swan <dprhws.edp.Arco.com>  12-22-95
#
# Extension to LaTeX2HTML V 96.1 to supply support for the "graphics"
# and "graphicx" standard LaTeX2e packages.
#
# extended and revised for LaTeX2HTML V 98.1 
# by Ross Moore <ross@mpce.mq.edu.au>  98/1/7
#
# Change Log:
# ===========

package main;

# Suppress option-warning messages:

sub do_graphics_dvips {
}
sub do_graphicx_dvips {
}



sub do_cmd_graphicspath {
    local($_) = @_;
    local($paths);
    $paths = &missing_braces unless (
	(s/$next_pair_pr_rx/$paths=$2;''/e)
	||(s/$next_pair_rx/$paths=$2;''/e));
    $paths = &revert_to_raw_tex($paths);

    #RRM: may only work correctly for Unix    
    # $dd  holds the directory-delimiter, usually / 
    $paths =~ s/\s*({|})\s*/$1/g;
    local(@paths) = split (/}/, $paths);
    if ($DESTDIR eq $FILE) {
	# given paths are relative to parent directory
	map(s|^{([^/~\.\$\\][^}]*)|{..\Q$dd\E$1|, @paths);
	map(s/^{\.\Q$dd\E/{\.\.\Q$dd\E/, @paths);
    } elsif ($DESTDIR eq '.') {
	# paths are already relative to working directory
    } else { 
	# specify full paths, by prepending source directory
	map(s|^{([^/~\.\$\\][^}]*)|{$orig_cwd\Q$dd\E$1|, @paths);
	map(s/^{\.\Q$dd\E/{$orig_cwd\Q$dd\E/, @paths);
    }
    $paths = join('}', @paths).'}';
    $latex_body .= "\n\\graphicspath{$paths}\n\n" unless ($PREAMBLE);
    $_
}

&process_commands_wrap_deferred (<<_RAW_ARG_CMDS_);
graphicspath # {}
_RAW_ARG_CMDS_

&process_commands_in_tex (<<_RAW_ARG_CMDS_);
rotatebox # [] # {} # {}
scalebox # {} # [] # {}
reflectbox # {}
resizebox # {} # {} # {}
resizeboxstar # {} # {} # {}
includegraphics # [] # [] # {}
includegraphicsstar # [] # [] # {}
_RAW_ARG_CMDS_

&process_commands_nowrap_in_tex (<<_RAW_ARG_CMDS_);
DeclareGraphicsExtensions # {}
DeclareGraphicsRule # {} # {} # {} # {}
_RAW_ARG_CMDS_


&ignore_commands( <<_IGNORED_CMDS_);
setkeys # {} # {}
_IGNORED_CMDS_


1;	# Must be last line
