// Part of dvi2bitmap
//
// This is merely an interface to the kpathsea library
// $Id$

#define NULL 0
#include <iostream>		// for cerr

#if NO_CSTD_INCLUDE
#include <stdlib.h>
#else
#include <cstdlib>
#endif

#include "kpathsea.h"

#ifdef ENABLE_KPATHSEA
// The Kpathsea library interface has to be isolated in this class because the
// kpathsea headers typedef a `string' type, which conflicts with C++ string.
// It would be more appropriate to put this into a namespace, but egcs
// doesn't support that, so fake it by putting them in a class with only
// static methods.
// 
// The headers here use HAVE_ASSERT_H, so that might need to be configured.
// They also test HAVE_PROTOTYPES and STDC_HEADERS, both of which should be 
// defined here, since the facilities they represent should be
// available in C++ (that's `should' as in `ought' rather than as in
// `probably are'...).
#define HAVE_PROTOTYPES
#define STDC_HEADERS
extern "C" {
#include <kpathsea/progname.h>
#include <kpathsea/debug.h>
#include <kpathsea/proginit.h>
#include <kpathsea/tex-glyph.h>
}
#undef STDC_HEADERS
#undef HAVE_PROTOTYPES
#endif

bool kpathsea::initialised_ = false;
verbosities kpathsea::verbosity_ = normal;

void kpathsea::init (const char *program_name, const int basedpi)
{
    if (initialised_ && verbosity_ > silent)
	cerr << "Warning: kpathsea doubly initialised.  Ignored\n";
#ifdef DEFAULT_TEXMFCNF
    // if the TEXMFCNF variable isn't set in the environment, set it to this
    char *texmfcnf = getenv ("TEXMFCNF");
    if (texmfcnf == NULL)
    {
	if (setenv ("TEXMFCNF", DEFAULT_TEXMFCNF, 1))
	    cerr << "Warning: couldn't set TEXMFCNF\n";
    }
    else
	cerr << "TEXMFCNF=" << texmfcnf << '\n';
#endif
    kpse_set_program_name (program_name, "dvi2bitmap");
    //kpse_init_prog ("TEX", basedpi, "localfont", "cmr10");
    kpse_init_prog ("TEX", basedpi, NULL, NULL);
    initialised_ = true;
}

const char *kpathsea::find (const char *fontname, int resolution)
{
    if (! initialised_ && verbosity_ > silent)
    {
	cerr << "Warning: kpathsea not initialised\n";
	return 0;
    }

    kpse_glyph_file_type glyph_info;
    char *fname;
    fname = kpse_find_pk (fontname, resolution, &glyph_info);

    if (verbosity_ > normal)
	if (fname)
	    cerr << "KPSE found file " << fname << '\n';
	else
	    cerr << "KPSE can't find file " << fontname
		 << " res=" << resolution << '\n';

    return fname;
}

void kpathsea::verbosity (const verbosities level)
{
    verbosity_ = level;
    if (level > debug)
	kpathsea_debug = ~0;	// all debugging
    else
	kpathsea_debug = 0;	// none
}



