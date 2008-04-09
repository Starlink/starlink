# $Id$
#
# inputenc.perl by Ross Moore <ross@mpce.mq.edu.au>  97/10/25
#
# Extension to LaTeX2HTML V97.1 to support the "inputenc" package
# and standard LaTeX2e package options.
#
# Change Log:
# ===========
# $Log$
# Revision 1.1  2004/02/20 13:13:28  nxg
# Initial import
#
# Revision 1.4  1998/08/02 01:38:56  RRM
#  --  fixed the problem that restricted usage to latin1 and latin2 only
# 	unknown subroutine caaused segmentation error otherwise
# 	 --- thanks to H. Turgut Uyar <uyar@leylak.cs.itu.edu.tr>
#
#

package main;


sub load_language_support {
    local($enc) = @_;
    local($file) = "$LATEX2HTMLVERSIONS${dd}$enc.pl";
    if ( require($file) ) {
        print STDERR "\nLoading $file";
	if ($charset =~ /^utf\-\d$/) {
	    $PREV_CHARSET = $CHARSET;
	    $CHARSET = "iso-10646";
	    &make_unicode_map;
	};1;
    } else {
        print STDERR "\n*** could not load support for $enc encoding ***\n"; 0;
    }
}

sub no_language_support {
    print STDERR "\n*** LaTeX2HTML has no support for the @_[0] encoding yet ***\n";}


# load extension files to implement different encodings:

sub do_inputenc_latin1{
#   if (&load_language_support('latin1')) {
        $CHARSET = 'iso-8859-1';
#   }
}

sub do_inputenc_latin2{&load_language_support('latin2');}
sub do_inputenc_latin3{&load_language_support('latin3');}
sub do_inputenc_latin4{&load_language_support('latin4');}
sub do_inputenc_latin5{&load_language_support('latin5');}
sub do_inputenc_latin6{&load_language_support('latin6');}

sub do_inputenc_latin7{&no_language_support('latin7');}
sub do_inputenc_latin8{&no_language_support('latin8');}
sub do_inputenc_latin9{&no_language_support('latin9');}
sub do_inputenc_latin10{&no_language_support('latin10');}

sub do_inputenc_esperanto{&load_language_support('latin3');}
sub do_inputenc_maltese{&load_language_support('latin3');}
sub do_inputenc_estonian{&load_language_support('latin4');}
sub do_inputenc_turkish{&load_language_support('latin5');}
sub do_inputenc_nordic{&load_language_support('latin6');}


sub do_inputenc_cyrillic{&no_language_support('iso-8859-5');}
sub do_inputenc_arabic{&no_language_support('iso-8859-6');}
sub do_inputenc_greek{&no_language_support('iso-8859-7');}
sub do_inputenc_hebrew{&no_language_support('iso-8859-8');}

sub do_inputenc_thai{&no_language_support('iso-8859-11');}
sub do_inputenc_celtic{&no_language_support('latin7');}
sub do_inputenc_baltic{&no_language_support('latin8');}
sub do_inputenc_sami{&no_language_support('latin9');}

sub do_inputenc_japanese{&no_language_support('iso2022');}
sub do_inputenc_korean{&no_language_support('korean');}

sub do_inputenc_utf7{&load_language_support('unicode');}
sub do_inputenc_utf8{&load_language_support('unicode');}


1;	# Must be last line








