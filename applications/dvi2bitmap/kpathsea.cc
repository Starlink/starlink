// Part of dvi2bitmap
//
// This is merely an interface to the kpathsea library

#define NULL 0
#include <iostream>

#include "kpathsea.h"

#ifdef ENABLE_KPATHSEA
// kpathsea headers typedef a `string' type, which conflicts with C++ string
// egcs doesn't support namespace, so hack it by defining `string' to 
// something innocuous.  Must also define HAVE_PROTOTYPES.
#define string kpse_string
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
#undef string
#endif

bool kpathsea::initialised_ = false;
int kpathsea::initRes_;
int kpathsea::verbosity_ = 1;

bool kpathsea::init (const char *argv0, const char *progname)
{
    kpse_set_program_name (argv0, "latex");
    initialised_ = false;
    return true;
}

void kpathsea::verbosity (int level)
{
    verbosity_ = level;
    if (level > 1)
	kpathsea_debug = ~0;	// all debugging
    else
	kpathsea_debug = 0;	// none
}

const char *kpathsea::find (const char *fontname, int resolution)
{
    if (! initialised_)
    {
	kpse_init_prog ("TEX", resolution, "localfont", "cmr10");
	initRes_ = resolution;
	initialised_ = true;
    }
    else
	if (resolution != initRes_)
	{
	    if (verbosity_ > 0)
		cerr << "kpathsea already initialised with resolution "
		     << initRes_ << '\n';
	    return 0;
	}

    kpse_glyph_file_type glyph_info;
    char *fname;
    fname = kpse_find_pk (fontname, resolution, &glyph_info);

    if (fname)
	cerr << "KPSE found file " << fname << '\n';
    else
	cerr << "KPSE can't find file " << fontname
	     << " res=" << resolution << '\n';

    return fname;
}

