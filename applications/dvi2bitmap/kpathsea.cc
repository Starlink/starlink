// Part of dvi2bitmap
//
// This is merely an interface to the kpathsea library

#define NULL 0
#include <iostream>		// for cerr

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
int kpathsea::initRes_;
int kpathsea::verbosity_ = 1;
const char *kpathsea::program_name_ = "dvi2bitmap";

void kpathsea::verbosity (int level)
{
    verbosity_ = level;
    if (level > 2)
	kpathsea_debug = ~0;	// all debugging
    else
	kpathsea_debug = 0;	// none
}

const char *kpathsea::find (const char *fontname, int resolution)
{
    if (! initialised_)
    {
	kpse_set_program_name (program_name_, "dvi2bitmap");
	//kpse_init_prog ("TEX", resolution, "localfont", "cmr10");
	kpse_init_prog ("TEX", resolution, NULL, NULL);
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

    if (verbosity_ > 1)
	if (fname)
	    cerr << "KPSE found file " << fname << '\n';
	else
	    cerr << "KPSE can't find file " << fontname
		 << " res=" << resolution << '\n';

    return fname;
}

