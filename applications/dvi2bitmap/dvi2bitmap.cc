// part of dvi2bitmap
// $Id$

#include "dvi2bitmap.h"
#include <iostream>
#include <vector>
#include <cstdlib>
#include <cstdarg>
#include <cctype>

#if VSPRINTF_IN_STDIO
#include <cstdio>
#endif

#include "DviFile.h"
#include "PkFont.h"
#include "Bitmap.h"
#include "version.h"

typedef vector<string> string_list;

// bitmap_info keeps together all the detailed information about the
// bitmap to be written.
struct bitmap_info {
    bitmap_info()
	: blur_bitmap(false), make_transparent(true), bitmap_scale_factor(1),
	  ofile_pattern(""), ofile_type("") { }
    bool blur_bitmap;
    bool make_transparent;
    int bitmap_scale_factor;
    string ofile_pattern;
    string ofile_type;
};

void process_dvi_file (DviFile *, bitmap_info&, int resolution);
string_list *tokenise_string (string s);
string get_ofn_pattern (string dviname);
void Usage (void);
char *progname;

int verbosity = 1;

main (int argc, char **argv)
{
    string dviname;
    int resolution = 72;	// in pixels-per-inch
    double magmag = 1.0;	// magnification of file magnification factor
    int show_font_list = 0;
    bitmap_info bm;
    bool do_process_file = true; // if true, then process DVI file
    bool all_fonts_present = false; // set at end of preamble

    //DviFile::verbosity(2);
    //PkFont::verbosity(2);
    //PkRasterdata::verbosity(2);
    //Bitmap::verbosity(2);
    if (char *pkpath = getenv("DVI2BITMAP_PK_PATH"))
	PkFont::setFontPath(pkpath);

    progname = argv[0];

    for (argc--, argv++; argc>0; argc--, argv++)
	if (**argv == '-')
	    switch (*++*argv)
	    {
	      case 'f':		// set PK font path
		argc--, argv++;
		if (argc <= 0)
		    Usage();
		PkFont::setFontPath(*argv);
		break;
	      case 'r':		// set resolution
		argc--, argv++;
		if (argc <= 0)
		    Usage();
		resolution = atoi (*argv);
		break;
	      case 'm':		// set magnification
		argc--, argv++;
		if (argc <= 0)
		    Usage();
		magmag = atof (*argv);
		break;
	      case 'g':		// debugging...
		for (++*argv; **argv != '\0'; ++*argv)
		    switch (**argv)
		    {
		      case 'd':	// debug DVI file
			DviFile::verbosity(2);	break;
		      case 'p':	// debug PK file
			PkFont::verbosity(2);	break;
		      case 'r':	// debug rasterdata parsing
			PkRasterdata::verbosity(2);	break;
		      default:
			Usage();
		    }
		break;
	      case 'q':		// run quietly
		DviFile::verbosity(0);
		PkFont::verbosity(0);
		PkRasterdata::verbosity(0);
		Bitmap::verbosity(0);
		verbosity = 0;
		break;
	      case 'l':		// show missing fonts
		show_font_list = 1;
		break;
	      case 'L':		// show all fonts
		show_font_list = 2;
		break;
	      case 'n':		// don't actually process the DVI file
		do_process_file = false; // ...just the pre/postamble
		break;
	      case 'P':		// process the bitmap...
		while (*++*argv != '\0')
		    switch (**argv)
		    {
		      case 'b':	// blur bitmap
			bm.blur_bitmap = !bm.blur_bitmap;
			break;
		      case 't':	// make bitmap transparent
			bm.make_transparent = !bm.make_transparent;
			break;
		      default:
			Usage();
			break;
		    }
		break;
	      case 's':		// scale down
		argc--, argv++;
		if (argc <= 0)
		    Usage();
		bm.bitmap_scale_factor = atoi (*argv);
		break;
	      case 't':		// set output file type
		argc--, argv++;
		if (argc <= 0)
		    Usage();
		bm.ofile_type = *argv;
		break;
	      case 'o':		// set output filename pattern
		argc--, argv++;
		if (argc <= 0)
		    Usage();
		bm.ofile_pattern = *argv;
		break;
	      case 'V':		// display version
		cout << version_string << '\n';
		std::exit(0);	// ...and exit

	      default:
		Usage();
	    }
	else
	{
	    if (dviname.length() != 0)
		Usage();
	    dviname = *argv;
	}

    if (dviname.length() == 0)
	Usage();

    if (bm.ofile_pattern.length() == 0)
	bm.ofile_pattern = get_ofn_pattern (dviname);
    if (bm.ofile_pattern.length() == 0)
    {
	if (verbosity > 0)
	    cerr << "Can't make output filename pattern from "
		 << dviname << '\n';
	exit(1);
    }

    try
    {
	DviFile *dvif = new DviFile(dviname, resolution, magmag);
	if (dvif->eof())
	{
	    if (verbosity > 0)
		cerr << "Can't open file " << dviname << " to read\n";
	    exit(1);
	}

	all_fonts_present = true;
	for (PkFont *f = dvif->firstFont();
	     f != 0;
	     f = dvif->nextFont())
	{
	    if (!f->loaded())	// flag at least one missing
		all_fonts_present = false;
	    if (show_font_list > 0)
		if (show_font_list > 1 || !f->loaded())
		{
		    if (f->loaded()) // show_font_list is >1
			cout << "% ";
		    // write out font name, dpi, base-dpi, mag and failed path
		    string fn = f->fontFilename();
		    cout << f->name() << ' '
			 << f->dpi()*magmag << ' '
			 << f->dpi() << ' '
			 << magmag << ' '
			 << (fn.length() == 0 ? "unknown" : fn)
			 << '\n';
		}
	}

	if (do_process_file)
	    process_dvi_file (dvif, bm, resolution);

    }
    catch (DviBug& e)
    {
	e.print();
    }
    catch (DviError& e)
    {
	e.print();
    }

    // Exit non-zero if we were just checking the pre- and postambles,
    // and we found some missing fonts
    if (!do_process_file && !all_fonts_present)
	exit (1);
    else
	exit (0);
}

void process_dvi_file (DviFile *dvif, bitmap_info& b, int resolution)
{
	DviFilePostamble *post;
	DviFileEvent *ev;
	const PkFont *curr_font;
	int pagenum = 0;
	string output_filename = "";
	do
	{
	    PkGlyph *glyph;
	    Bitmap *bitmap;

	    ev = dvif->getEvent();
	    if (verbosity > 1)
		ev->debug();
	    if (DviFilePage *page = dynamic_cast<DviFilePage*>(ev))
		if (page->isStart)
		    bitmap = new Bitmap (dvif->hSize(), dvif->vSize());
		else
		{
		    pagenum++;
		    if (bitmap->empty())
		    {
			if (verbosity > 0)
			    cerr << "Warning: page " << pagenum
				 << " empty: nothing written\n";
		    }
		    else
		    {
			bitmap->crop();
			if (b.blur_bitmap)
			    bitmap->blur();
			if (b.make_transparent)
			    bitmap->setTransparent(true);
			if (b.bitmap_scale_factor != 1)
			    bitmap->scaleDown (b.bitmap_scale_factor);
			if (output_filename.length() == 0)
			{
			    char fn[100];
			    sprintf (fn, b.ofile_pattern.c_str(), pagenum);
			    output_filename = fn;
			}
			bitmap->write (output_filename, b.ofile_type);
		    }
		    output_filename = "";

		    delete bitmap;
		    bitmap = 0;
		}
	    else if (DviFileSetChar *sc = dynamic_cast<DviFileSetChar*>(ev))
	    {
		glyph = curr_font->glyph(sc->charno);
		if (verbosity > 1)
		    cerr << "set glyph " << glyph->w() << 'x' << glyph->h()
			 << " at position ("
			 << dvif->currH() << ',' << dvif->currV()
			 << ")\n";
		// calculate glyph positions, taking into account the
		// offsets for the bitmaps, and the (1in,1in)=(72pt,72pt)
		// = (resolution px,resolution px) offset of the TeX origin.
		int x = dvif->currH() + glyph->hoff() + resolution;
		int y = dvif->currV() + glyph->voff() + resolution;
		bitmap->paint (x, y,
			       glyph->w(), glyph->h(),
			       glyph->bitmap());
	    }
	    else if (DviFileSetRule *sr = dynamic_cast<DviFileSetRule*>(ev))
	    {
		int x = dvif->currH() + resolution;
		int y = dvif->currV() + resolution;
		bitmap->rule (x,y,sr->w, sr->h);
	    }
	    else if (DviFileFontChange *fc =
		     dynamic_cast<DviFileFontChange*>(ev))
		curr_font = fc->font;
	    else if (DviFileSpecial* special =
		     dynamic_cast<DviFileSpecial*>(ev))
	    {
		string_list *l = tokenise_string (special->specialString);
		if (l->size() > 2
		    && (*l)[0] == "dvi2bitmap"
		    && (*l)[1] == "outputfile")
		{
		    output_filename = (*l)[2];
		}
		else
		    if (verbosity > 0)
			cerr << "Warning: unrecognised special: "
			     << special->specialString
			     << '\n';
		delete l;
	    }

	    delete ev;
	}
	while (!(post = dynamic_cast<DviFilePostamble*>(ev)));
}

DviError::DviError(const char *fmt,...)
{
    char *p = new char[2*strlen(fmt)];
    va_list ap;
    va_start(ap,fmt);
    vsprintf (p, fmt, ap);
    va_end(ap);
    problem_ = p;
    delete[] p;
}

void DviError::print() const { cerr << "DVI error: " << problem_ << '\n'; }
void DviBug::print() const { cerr << "BUG: " << problem_ << '\n'; }

DviBug::DviBug(const char *fmt,...)
{
    char *p = new char[2*strlen(fmt)];
    va_list ap;
    va_start(ap,fmt);
    vsprintf (p, fmt, ap);
    va_end(ap);
    problem_ = p;
    delete[] p;
}

string get_ofn_pattern (string dviname)
{
    // strip path and extension from filename
    int string_index = dviname.rfind(path_separator);
    string dvirootname;
    if (string_index < 0)
	dvirootname = dviname;
    else
	dvirootname = dviname.substr(string_index+1);
    string_index = dvirootname.rfind('.');
    if (string_index >= 0) // it's there
	dvirootname = dvirootname.substr(0,string_index);

    return dvirootname + "-page%d";
}

// Tokenise string at whitespace.  There's a more C++-ish way of doing
// this, I'm sure....
string_list *tokenise_string (string str)
{
    if (verbosity > 1)
	cerr << "tokenise_string: string=<" << str << ">\n";
    string_list *l = new string_list();
    int i=0, wstart;
    // skip leading whitespace
    while (i < str.length() && isspace(str[i]))
	i++;
    while (i < str.length())
    {
	wstart = i;
	while (i < str.length() && !isspace(str[i]))
	    i++;
	string t = str.substr(wstart,i-wstart);
	if (verbosity > 1)
	    cerr << "tokenise:" << t << ":\n";
	l->push_back(t);
	while (i < str.length() && isspace(str[i]))
	    i++;
    }
    return l;
}


void Usage (void)
{
    cerr << "Usage: " << progname << " [-lLnqV] [-f PKpath ] [-r resolution]\n\t[-P[bt]] [-s scale-factor] [-o outfile-pattern] [-m magmag ]\n\t[-t xbm|gif]\n\tdvifile" << '\n';
    exit (1);
}
