//    This file is part of dvi2bitmap.
//    Copyright 1999--2002, Council for the Central Laboratory of the Research Councils
//    
//    This program is part of the Starlink Software Distribution: see
//    http://www.starlink.ac.uk 
//
//    dvi2bitmap is free software; you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation; either version 2 of the License, or
//    (at your option) any later version.
//
//    dvi2bitmap is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with dvi2bitmap; if not, write to the Free Software
//    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//
//    The General Public License is distributed along with this
//    program in the file LICENCE.
//
//    Author: Norman Gray <norman@astro.gla.ac.uk>
//    $Id$


#include <config.h>

// Put the entire module inside #if ENABLE_KPATHSEA.
// We _shouldn't_ be compiling this is that's false, but we obviously are,
// if we get here, so make it a no-op.
#ifdef ENABLE_KPATHSEA

#define NULL 0
#include <iostream>		// for cerr

#ifdef HAVE_CSTD_INCLUDE
#include <cstdlib>
#else
#include <stdlib.h>
#endif

using STD::cerr;

#include <KarlPathSearcher.h>
#include <PkFont.h>		// for PkFont::dpiBase

// The Kpathsea library interface has to be isolated in this class because the
// kpathsea headers typedef a `string' type, which conflicts with C++ string.
//
// This WAS: ''It would be more appropriate to put this into a namespace, but egcs
// doesn't support that, so fake it by putting them in a class with only
// static methods.''  Well, we do have namespaces now (or rather, I
// don't want to support C++ compilers which don't have them), so the
// kpse stuff is now in a kpse namespace.  It might be that this
// should prompt a redesign of this class, into something more
// object-oriented.  We need to use namespacing, now, since we need to
// include PkFont.h, and that in turn includes <string.h>, which
// messes things up royally unless we finally use namespaces.
// 
// The headers here use HAVE_ASSERT_H, so that might need to be configured.
// They also test HAVE_PROTOTYPES and STDC_HEADERS, both of which should be 
// defined here, since the facilities they represent should be
// available in C++ (that's `should' as in `ought' rather than as in
// `probably are'...).
//
// We must _avoid_ including c-std.h, since that file carefully
// declares popen and pclose, but does so K&R-style, with no
// arguments.  This seems to have got past most compilers, but gcc,
// apparently in a recent burst of standards enthusiasm, seems to have
// decided to object to this.  KPATHSEA_C_STD_H is the guard
// definition which c-std.h uses to avoid rereading itself; the one
// thing we do need from that file is stdio.h, so we read that
// explicitly.
#define HAVE_PROTOTYPES
#ifndef STDC_HEADERS
#define STDC_HEADERS
#endif
#define KPATHSEA_C_STD_H 1
#include <stdio.h>
namespace kpse {
    extern "C" {
#include <kpathsea/progname.h>
#include <kpathsea/debug.h>
#include <kpathsea/proginit.h>
#include <kpathsea/tex-glyph.h>

        // Initialise kpse_format_info.  This array is elaborately
        // initialised at run-time in kpse_init_format (tex-file.c in
        // web2c sources).  If, however, it isn't initialised at
        // compile time, then it can cause linking errors on OSX.  See thread
        // <http://tug.org/pipermail/tex-k/2003-June/000717.html>,
        // where I reported what I thought was a non-initialisation
        // bug, for more details.
        kpse_format_info_type kpse_format_info[kpse_last_format] = { 0 };
    }
}
#undef HAVE_PROTOTYPES

// Declare static variables
KarlPathSearcher *KarlPathSearcher::instance_ = 0;
verbosities KarlPathSearcher::verbosity_ = normal;
const char* KarlPathSearcher::default_program_name_ = 0;

/**
 * Private constructor, does the initialisation work.
 *
 * <p>Unfortunately, this naive implementation of a Singleton pattern
 * has the problem that gcc (for example) reasonably enough warns
 * that ``class KarlPathSearcher' only defines a private destructor
 * and has no friends'.  I <em>think</em> we can address this, somehow,
 * using code:auto_ptr.  However, that doesn't work naively either.
 * In any case, this is documented to be a singleton object, so
 * nothing should attempt to delete it.  It would be nice, therefore,
 * to make the destructor private, and damn the warning.  Perhaps the
 * analogues of g++'s <code>-Wctor-dtor-privacy</code> might be useful.
 *
 * @param program_name the name to be used in kpathsea feedback
 * @param basedpi the base resolution of the PK fonts; should
 * generally be {@link PkFont#dpiBase}
 */
KarlPathSearcher::KarlPathSearcher(const char* program_name,
				   const int basedpi)
{
#ifdef DEFAULT_TEXMFCNF
    // if the TEXMFCNF variable isn't set in the environment, set it to this
    char *texmfcnf = getenv ("TEXMFCNF");
    if (texmfcnf == NULL)
    {
	if (setenv ("TEXMFCNF", DEFAULT_TEXMFCNF, 1) && verbosity_ > quiet)
	    cerr << "Warning: couldn't set TEXMFCNF\n";
    }
    else
	cerr << "TEXMFCNF=" << texmfcnf << '\n';
#endif

#ifdef FAKE_PROGNAME
    // Lie about the program name, so we can influence the definitions of
    // the SELFAUTO{LOC,DIR,PARENT} variables.
    //
    // kpse_set_program_name uses the value of program_invocation_name if
    // that's defined, and defines it, and initialises it to program_name,
    // if it's not.  The only way we can reliably intervene here is by
    // setting that variable ourselves.  Unfortunately, this means that we're
    // lying about this for the remainder of this program, but we can't do
    // better than this without independently discovering for ourselves if 
    // program_invocation_name is set in the libc library.  We can't get
    // round this by defining the variables by hand ourselves, since 
    // kpse_set_program_name does elaborate following of links and other
    // consistency checks, which we do not want to duplicate here.
    //
    // In any case, this is supposed to be not much more than a hack to avoid
    // being bitten by arguably misconfigured texmf.cnf files, so don't lose
    // sleep over it.
    program_name = kpse::program_invocation_name = FAKE_PROGNAME;
#endif

    kpse::kpse_set_program_name (program_name, // current program name
				 "dvi2bitmap");	// used in searching texmf.cnf

    kpse::kpse_init_prog ("TEX", // prefix to make TEXFONTS, TEXHEADERS, etc
			  basedpi,
			  NULL,
			  NULL);
}

KarlPathSearcher::~KarlPathSearcher()
{
    // nothing
}

/**
 * Returns an instance of the object which searches the TeX tree,
 * using Karl's path-searching algorithm (KPSE).
 *
 * <p>Note that, although it looks as if you should be able to call
 * this more than once with different parameters, you can't in fact,
 * and if you call it more than once (that's fine and sensible) you
 * get the same instance back each time.  In fact, there's no good
 * reason I can think of why you'd <em>want</em> to initialise this in
 * different ways, so this isn't a problem in fact.  The door remains
 * open to this changing in future, though.  About the only reason for
 * giving non-default parameters here is to register a different
 * <code>program_name</code> for the kpathsea library to use when
 * reporting errors or warnings.
 *
 * @param program_name the name to be used in kpathsea feedback; if
 * zero, this default to the value set by {@link #setProgramName}, and
 * if that has not been set, to <code>"tex"</code>
 * @param basedpi the base resolution of the PK fonts; if zero, it is
 * taken to be {@link PkFont.dpiBase} (a sensible default)
 */
KarlPathSearcher* KarlPathSearcher::getInstance(const char *program_name,
						const int basedpi)
{
    if (instance_ == 0) {
	instance_ = new KarlPathSearcher(program_name != 0
					 ? program_name
					 : (default_program_name_ != 0
					    ? default_program_name_
					    : "tex"),
					 basedpi != 0
					 ? basedpi
					 : PkFont::dpiBase());
    }
    return instance_;
}

/**
 * Obtains the Kpathsea version
 * @return the kpathsea version string
 */
const char *KarlPathSearcher::version_string (void)
{
    extern char *kpathsea_version_string;
    return kpathsea_version_string;
}

/**
 * Finds a font at a given resolution.
 * @param fontname the name of the font to search for
 * @param resolution the desired font resolution
 * @return the full path to the PK file which defines the font, or
 * zero if the font cannot be found
 */
const char *KarlPathSearcher::find (const char *fontname, int resolution)
{
    kpse::kpse_glyph_file_type glyph_info;
    char *fname;
    // This next line is really 
    //    fname = kpse::kpse_find_pk (fontname, resolution, &glyph_info);
    // but kpse_find_pk is really a macro (grrr), so we have to unpack it
    // if the namespacing is to work.
    fname = kpse::kpse_find_glyph(fontname,
                                  resolution,
                                  kpse::kpse_pk_format,
                                  &glyph_info);

    if (verbosity_ > normal)
	if (fname)
	    cerr << "KPSE found file " << fname << '\n';
	else
	    cerr << "KPSE can't find file " << fontname
		 << " res=" << resolution << '\n';

    return fname;
}

/**
 * Sets the verbosity level.  As well as setting the intrinsic level
 * of chatter for this class, it controls the amount of chatter from
 * the <code>libkpathsea</code> library itself: if the
 * <code>level</code> is above <code>debug</code>, then <em>all</em>
 * kpathsea debugging information is switched on.
 * @param level the desired verbosity level
 * @return the previous verbosity level
 */
verbosities KarlPathSearcher::verbosity (const verbosities level)
{
    verbosities oldv = verbosity_;
    verbosity_ = level;
    if (level > debug)
	kpse::kpathsea_debug = ~0; // all debugging
    else
	kpse::kpathsea_debug = 0; // none
    return oldv;
}



#endif // if ENABLE_KPATHSEA
