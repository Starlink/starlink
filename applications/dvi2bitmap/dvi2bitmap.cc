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

string_list *tokenise_string (string s);
string get_ofn_pattern (string dviname);
void Usage (void);
char *progname;

main (int argc, char **argv)
{
    string dviname;
    int resolution = 72;	// in pixels-per-inch
    double magmag = 1.0;	// magnification of file magnification factor
    int show_font_list = 0;
    bool blur_bitmap = false;
    bool make_transparent = true;
    string ofile_pattern = "";
    string ofile_type = "";

    bool debug_ = false;
    DviFile::debug(1);
    PkFont::debug(1);
    //DviFile::debug(2);
    //PkFont::debug(2);
    //PkRasterdata::debug(2);
    Bitmap::debug(true);
    if (char *pkpath = getenv("DVI2BITMAP_PK_PATH"))
	PkFont::setFontPath(pkpath);

    progname = argv[0];

    for (argc--, argv++; argc>0; argc--, argv++)
	if (**argv == '-')
	    switch (*++*argv)
	    {
	      case 'f':
		argc--, argv++;
		if (argc <= 0)
		    Usage();
		PkFont::setFontPath(*argv);
		break;
	      case 'r':
		argc--, argv++;
		if (argc <= 0)
		    Usage();
		resolution = atoi (*argv);
		break;
	      case 'm':
		argc--, argv++;
		if (argc <= 0)
		    Usage();
		magmag = atof (*argv);
		break;
	      case 'g':		// debugging
		for (++*argv; **argv != '\0'; ++*argv)
		    switch (**argv)
		    {
		      case 'd':
			DviFile::debug(2);	break;
		      case 'p':
			PkFont::debug(2);	break;
		      case 'r':
			PkRasterdata::debug(2);	break;
		      default:
			Usage();
		    }
		break;
	      case 'l':		// show missing fonts
		show_font_list = 1;
		break;
	      case 'L':		// show all fonts
		show_font_list = 2;
		break;
	      case 'P':		// process the bitmap
		while (*++*argv != '\0')
		    switch (**argv)
		    {
		      case 'b':
			blur_bitmap = !blur_bitmap;
			break;
		      case 't':
			make_transparent = !make_transparent;
			break;
		      default:
			Usage();
			break;
		    }
		break;
	      case 't':
		argc--, argv++;
		if (argc <= 0)
		    Usage();
		ofile_type = *argv;
		break;
	      case 'o':
		argc--, argv++;
		if (argc <= 0)
		    Usage();
		ofile_pattern = *argv;
		break;
	      case 'v':		// version
		cout << version_string << '\n';
		break;
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

    if (ofile_pattern.length() == 0)
	ofile_pattern = get_ofn_pattern (dviname);
    if (ofile_pattern.length() == 0)
    {
	cout << "Can't make output filename pattern from " << dviname << '\n';
	std::exit(0);
    }
    else
	cout << "ofile_pattern="<<ofile_pattern<<'\n';

    try
    {
	DviFile *dvif = new DviFile(dviname, resolution, magmag);
	if (dvif->eof())
	{
	    cout << "Can't open file " << dviname << " to read\n";
	    std::exit(1);
	}

	if (show_font_list > 0)
	    for (PkFont *f = dvif->firstFont();
		 f != 0;
		 f = dvif->nextFont())
	    {
		if (show_font_list > 1 || !f->loaded())
		    // write out font name, base-dpi, dpi, mag and dummy mode
		    cout << f->name() << ' '
			 << f->dpi() << ' '
			 << f->dpiScaled() << ' '
			 << f->scale() << " localfont\n";
	    }

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
	    if (debug_)
		ev->debug();
	    if (DviFilePage *page = dynamic_cast<DviFilePage*>(ev))
		if (page->isStart)
		    bitmap = new Bitmap (dvif->hSize(), dvif->vSize());
		else
		{
		    pagenum++;
		    bitmap->crop();
		    if (blur_bitmap)
			bitmap->blur();
		    if (make_transparent)
			bitmap->setTransparent(true);
		    if (output_filename.length() == 0)
		    {
			char fn[100];
			sprintf (fn, ofile_pattern.c_str(), pagenum);
			output_filename = fn;
		    }
		    bitmap->write (output_filename, ofile_type);
		    output_filename = "";

		    delete bitmap;
		    bitmap = 0;
		}
	    else if (DviFileSetChar *sc = dynamic_cast<DviFileSetChar*>(ev))
	    {
		glyph = curr_font->glyph(sc->charno);
		if (debug_)
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
		    cout << "Warning: unrecognised special: "
			 << special->specialString
			 << '\n';
		delete l;
	    }

	    delete ev;
	}
	while (!(post = dynamic_cast<DviFilePostamble*>(ev)));
    }
    catch (DviError& e)
    {
	e.print();
    }
    catch (DviBug& e)
    {
	e.print();
    }

    exit (0);
}

DviError::DviError(char *fmt,...)
{
    char *p = new char[2*strlen(fmt)];
    va_list ap;
    va_start(ap,fmt);
    vsprintf (p, fmt, ap);
    va_end(ap);
    problem_ = p;
    delete[] p;
}

DviBug::DviBug(char *fmt,...)
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
    cout << "tokenise_string: string=<" << str << ">\n";
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
	cout << "tokenise:" << t << ":\n";
	l->push_back(t);
	while (i < str.length() && isspace(str[i]))
	    i++;
    }
    cout << "tokenized!\n";
    return l;
}


void Usage (void)
{
    cout << "Usage: " << progname << " [-f PKpath ] [-r resolution] [-g[dpr]]\n\t[-lLv] [-P[bt]] [-o outfile-pattern] [-m magmag ]\n\t\n\t[-t gif]\n\tdvifile" << '\n';
    exit (1);
}
