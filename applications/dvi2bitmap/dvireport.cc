#include <config.h>

#include <string>
#include <iostream>
#include <bitset>
#include <ctype.h>

using STD::cout;
using STD::cerr;
using STD::endl;

#include "DviFile.h"

char *progname;
void Usage();
void show_position(DviFile *dvi, const char* unitlist);


void show_position(DviFile *dvi, const char* unitlist)
{
#define CVTXY(unit)							\
    cout << " "								\
	 << DviFile::convertFromScaledPoints				\
	    (dvi->currH(DviFile::unit_sp),				\
	     DviFile::unit_##unit)					\
	 << ","								\
	 << DviFile::convertFromScaledPoints				\
	    (dvi->currV(DviFile::unit_sp),				\
	     DviFile::unit_##unit)					\
	 << #unit
    
    for (const char *p=unitlist; *p != '\0'; p++) {
	switch (*p) {
	  case 'x':
	    cout << " " << dvi->currH() << "," << dvi->currV()
		 << "px";
	    break;
	  case 'd':
	    cout << " "
		 << dvi->currH(DviFile::unit_dvi)
		 << ","
		 << dvi->currV(DviFile::unit_dvi)
		 << "dviu";
	    break;
	  case 'b':
	    CVTXY(bp);
	    break;
	  case 'p':
	    CVTXY(pt);
	    break;
	  case 'm':
	    CVTXY(mm);
	    break;
	  case 'c':
	    CVTXY(cm);
	    break;
	  case 'P':
	    CVTXY(pc);
	    break;
	  case 'D':
	    CVTXY(dd);
	    break;
	  case 'C':
	    CVTXY(cc);
	    break;
	  case 'i':
	    CVTXY(in);
	    break;
	  case 's':
	    cout << " " << dvi->currH(DviFile::unit_sp)
		 << "," << dvi->currV(DviFile::unit_sp) << "sp";
	    break;
	  default:
	    cout << "Unit(" << *p << ") unrecognised";
	    break;
	}
    }
}

int main (int argc, char **argv)
{
    string dviname;
    progname = argv[0];
    if (argc == 1)
	Usage();
    const char *unitlist = 0;
    enum { feature_characters=0, feature_rules,
	   feature_fonts, feature_specials };
    STD::bitset<8> show_features;
    bool load_fonts = false;

    for (argc--, argv++; argc>0; argc--, argv++) {
	if (**argv == '-') {
	    switch (*++*argv) {
	      case 'F':		// load fonts
		load_fonts = true;
		break;

	      case 'u':		// units
		unitlist = ++*argv;
 		break;

	      case 's':		// what to show
		for (++*argv; **argv; ++*argv)
		    switch (**argv) {
		      case 'c':
			show_features.set(feature_characters);
			break;
		      case 'f':
			show_features.set(feature_fonts);
			break;
		      case 'r':
			show_features.set(feature_rules);
			break;
		      case 's':
			show_features.set(feature_specials);
			break;
		      case 'A':
			show_features.set(); // all
			break;
		      default:
			Usage();
			break;
		    }
		break;

	      case 'h':		// help, FALL THROUGH
	      default:
		Usage();
	    }
	} else {
	    dviname = *argv;
	}
    }

    if (dviname.length() == 0)
	Usage();

    if (unitlist == 0)
	unitlist = static_cast<const char*>("d");
    if (show_features.none())
	show_features.set(feature_characters);

    DviFile* dvi;
    try {
	string mode72dpi = "mactrue"; // a mode listed as 72dpi
	PkFont::setMissingFontMode(mode72dpi);
	dvi = new DviFile(dviname, 72);
    } catch (DviError& e) {
	cerr << "Can't open DVI file " << dviname
	     << ": " << e.problem() << endl;
	exit (1);
    }
    
    DviFileEvent *ev;
    bool end_of_file = false;
    const PkFont* curr_font;

    try {
	while (!end_of_file) {
	    ev = dvi->getEvent();

	    if (DviFileSetChar *test = dynamic_cast<DviFileSetChar*>(ev)) {
		if (show_features.test(feature_characters)) {
		    DviFileSetChar& sc = *test;
		    if (load_fonts) {
			PkGlyph& glyph = *curr_font->glyph(sc.charno());
			char c = glyph.characterChar();
			cout << "c " << glyph.characterCode()
			     << "[" << (isprint(c) ? c : '?')
			     << "," << curr_font->name()
			     << "]";
		    } else {
			cout << "c " << sc.charno();
		    }
		    show_position(dvi, unitlist);
		    cout << endl;
		}

	    } else if (DviFileSetRule *test
		       = dynamic_cast<DviFileSetRule*>(ev)) {
		if (show_features.test(feature_rules)) {
		    DviFileSetRule& sr = *test;
		    cout << "r "
			 << sr.h << "x" << sr.w
			 << "px @";
		    show_position(dvi, unitlist);
		    cout << endl;
		}

	    } else if (DviFileFontChange *test =
		       dynamic_cast<DviFileFontChange*>(ev)) {
		DviFileFontChange& fc = *test;
		if (show_features.test(feature_fonts)) {
		    cout << " f " << fc.font->name()
			 << "@" << fc.font->designSize() << endl;
		}
		if (load_fonts) {
		    const PkFont *f = fc.font;
		    if (f->loaded())
			curr_font = f;
		    else {
			f = dvi->getFallbackFont(f);
			if (f != 0)
			    curr_font = f;
		    }
		    // curr_font unchanged if font-change was unsuccessful
		}
		
	    } else if (DviFileSpecial *special
		       = dynamic_cast<DviFileSpecial*>(ev)) {
		if (show_features.test(feature_specials)) {
		    cout << "s \"" << special->specialString << "\"";
                    show_position(dvi, unitlist);
                    cout << endl;
		}

	    } else if (DviFilePostamble *post
		       = dynamic_cast<DviFilePostamble*>(ev)) {
		end_of_file = true;
	    }
	
	    ev->release();
	}
    } catch (DviError& e) {
	cerr << "Error processing DVI file " << dviname 
	     << ": " << e.problem() << endl;
    }
    
    delete dvi;

    exit (0);
}

void Usage()
{
    cerr << "Usage: " << progname
	 << " [-F] [-s[cfrsA]] [-u[bcdimpx]] dvifile" << endl;
    exit (1);
}

