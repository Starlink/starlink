# $Id$
# FRENCH.PERL by Nikos Drakos <nikos@cbl.leeds.ac.uk> 25-11-93
# Computer Based Learning Unit, University of Leeds.
#
# Extension to LaTeX2HTML to translate LaTeX french special 
# commands to equivalent HTML commands and ISO-LATIN-1 characters.
# Based on a patch to LaTeX2HTML supplied by  Franz Vojik 
# <vojik@de.tu-muenchen.informatik>. 
#
# Change Log:
# ===========
# $Log$
# Revision 1.1  2004/02/20 13:13:28  nxg
# Initial import
#
# Revision 1.1  1998/08/20 16:03:36  pdraper
# *** empty log message ***
#
# Revision 1.2  1996/12/23 01:39:54  JCL
# o added informative comments and CVS log history
# o changed usage of <date> to an OS independent construction, the
#   patch is from Piet van Oostrum.
#
#
# 11-MAR-94 Nikos Drakos - Added support for \inferieura and \superrieura

package french;

# Put french equivalents here for headings/dates/ etc when
# latex2html start supporting them ...

sub main'french_translation {
    @_[0];
}

package main;

sub do_cmd_frenchTeX {
    # Just in case we pass things to LaTeX
    $default_language = 'french';
    $latex_body .= "\\frenchTeX\n";
    @_[0];
}

sub do_cmd_originalTeX {
    # Just in case we pass things to LaTeX
    $default_language = 'original';
    $latex_body .= "\\originalTeX\n";
    @_[0];
}

sub do_cmd_inferieura {
   "&lt @_[0]"
}
 
sub do_cmd_superrieura {
   "&gt @_[0]"
}

#AYS: Prepare the french environment ...
sub french_titles {
    $toc_title = "Table des mati&egrave;res";
    $lof_title = "Liste des figures";
    $lot_title = "Liste des tableaux";
    $idx_title = "Index";
    $bib_title = "R&eacute;f&eacute;rences";
    $abs_title = "R&eacute;sum&eacute;";
    $pre_title = "Pr&eacute;face";
    $app_title = "Annexe";
    $info_title = "&Agrave;propos de ce document..."; 
    @Month = ('', 'janvier', 'f&eacute;vrier', 'mars', 'avril', 'mai',
              'juin', 'juillet', 'ao&ucirc;t', 'septembre', 'octobre',
              'novembre', 'd&eacute;cembre');
}

#AYS(JKR): Replace do_cmd_today (\today) with a nicer one, which is more
# similar to the original. 
#JCL introduced &get_date.
sub do_cmd_today {
    local($today) = &get_date;
    $today =~ s|(\d+)/0?(\d+)/|$2 $Month[$1] |;
    join('',$today,$_[0]);
}

# ... and use it.
&french_titles;
$default_language = 'french';
$TITLES_LANGUAGE = "french";

1;				# Not really necessary...



