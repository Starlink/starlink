# $Id$
#
# finnish.perl for finnish babel, inspired heavily by german.perl
# by Viljo Viitanen <viljo@iki.fi>


package finnish;

print " [finnish]";

sub main'finnish_translation {
    local($_) = @_;
s/;SPMquot;\s*('|`|;SPMlt;|;SPMgt;|\\|-|;SPMquot;|=|\|)/&get_finnish_specials($1)/geo;
    $_;
}

sub get_finnish_specials {
    $finnish_specials{@_[0]}
}

%finnish_specials = (
    '\''       => "``",
    "\`"       => ",,",
    ';SPMlt;'  => "&laquo;",
    ';SPMgt;'  => "&raquo;",
    '\\'       => "",
    '-'        => "-",
    ';SPMquot;'=> "",
    '='        => "-",
    '|'        => ""
);


package main;

sub finnish_titles {
    $toc_title = "Sis”&auml;lt–&ouml;"; # or Sis”&auml;llys
    $lof_title = "Kuvat";
    $lot_title = "Taulukot";
    $idx_title = "Hakemisto";
    $ref_title = "Viitteet";
    $bib_title = "Kirjallisuutta";
    $abs_title = "Tiivistelm”&auml;";
    $app_title = "Liite";
    $pre_title = "Esipuhe";
    $fig_name = "Kuva";
    $tab_name = "Taulukko";
    $part_name = "Osa";
    $prf_name = "Todistus";
    $child_name = "Aliluvut"; #could be better
    $info_title = "T”&auml;st&auml; dokumentista ...";
    @Month = ('', 'tammikuuta', 'helmikuuta', 'maaliskuuta', 'huhtikuuta',
              'toukokuuta', 'kes”&auml;kuuta', 'hein”&auml;kuuta', 'elokuuta',
              'syyskuuta', 'lokakuuta', 'marraskuuta', 'joulukuuta');
    $GENERIC_WORDS = "ja";
}


sub do_cmd_today {
    local($today) = &get_date();
    $today =~ s|(\d+)/0?(\d+)/|$2. $Month[$1] |;
    join('',$today,$_[0]);
}


sub do_cmd_textohtmlinfopage {
    local($_) = @_;
    ( ($INFO == 1)
     ? join('', $close_all
	, "<STRONG>$t_title</STRONG><P>\nT”&auml;m”&auml; dokumentti tehtiin ohjelmistolla\n"
	, "<A HREF=\"$TEX2HTMLADDRESS\"><STRONG>LaTeX</STRONG>2<tt>HTML</tt></A>"
	, " translator Version $TEX2HTMLVERSION\n"
	, "<P>Copyright &#169; 1993, 1994, 1995, 1996,\n"
	, "<A HREF=\"$AUTHORADDRESS\">Nikos Drakos</A>, \n"
	, "Computer Based Learning Unit, University of Leeds.\n"
	, "<BR>Copyright &#169; 1997, 1998,\n"
	, "<A HREF=\"$AUTHORADDRESS2\">Ross Moore</A>, \n"
	, "Mathematics Department, Macquarie University, Sydney.\n"
	, "<P>Komentoriviargumentit olivat: <BR>\n "
	, "<STRONG>latex2html</STRONG> <tt>$argv</tt>.\n"
	, (($INIT_FILE ne '')?
	  "\n<P>alustustiedostolla: <TT>$INIT_FILE</TT>\n$init_file_mark\n" :'')
	, "<P>Komennon ajoi $address_data[0] $address_data[1]"
	, $open_all, $_)
     : join('', $close_all, $INFO,"\n", $open_all, $_))
}

# use'em
&finnish_titles;
$default_language = 'finnish';
$TITLES_LANGUAGE = "finnish";

# $Log$
# Revision 1.1  2004/02/20 13:13:28  nxg
# Initial import
#
# Revision 1.2  1998/06/26 05:58:02  RRM
#  --  use entities instead of bare accented characters
#  --  updated the info-message
#  --  added a single item to $GENERIC_WORDS
#
# Revision 1.1  1998/06/25 02:14:46  RRM
# 	support for Finnish language
# 		by Viljo Viitanen <viljo@iki.fi>
#
#  --  based on german.perl
#

1;
