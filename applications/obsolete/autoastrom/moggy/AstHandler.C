//  This file is part of moggy.
//
//  Copyright 2001, Council for the Central Laboratory of the Research Councils
//
//  This program is part of the Starlink Software Distribution: see
//  http://www.starlink.ac.uk
//
//  moggy is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation; either version 2 of the License, or
//  (at your option) any later version.
//
//  moggy is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with moggy; if not, write to the Free Software
//  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
//
//  The General Public License is distributed along with this
//  program in the file LICENCE.
//
//  Author: Norman Gray <norman@astro.gla.ac.uk>
//  $Id$

// AstManager manages the conversion between coordinates in some 2-d
// system and SKY coordinates.  It learns about the transformation by
// being given a serialised AST frameset on construction.
//
// It always reports SKY coordinates in decimal degrees, in the FK-5
// coordinate system with equinox J2000.  At present, it demands that
// the input frameset has a SkyFrame with this System and Equinox, but
// it could pretty easily be generalised in future to silently add an
// appropriate further mapping to this system.


#include <config.h>

#include <iostream>		// for cerr, endl
#include <assert.h>

#if STD_IN_STD_NAMESPACE
using std::cerr;
using std::endl;
#endif

#include "AstHandler.h"
#include "stringstream.h"
#include "util.h"

// Class variables
AstHandler* AstHandler::channel_source_currenthandler_;
verbosities AstHandler::verbosity_ = normal;
const double AstHandler::DegreesPerRadian = 57.2957795130823208767;

// Parameter serialFrameset is a vector of strings, each of which is a
// line of an AST channel output.  Parameter fromdomain is the names
// of the domain from which it will convert coordinates: this routine
// will extract from the proffered FrameSet a mapping between these
// domains.
AstHandler::AstHandler (vector<string>serialFrameset,
			string fromdomain)
    throw (MoggyException)
    : serialFrameset_(serialFrameset),
      fromdomain_(fromdomain)
{
    Util::uppercaseString (fromdomain_);
    todomain_ = "SKY";		// always

    bool isFits;		// `frameset' is actually a set of FITS cards
    isFits = serialFrameset_[0].substr(0,6) == "SIMPLE";
    if (verbosity_ > normal)
	Util::logstream() << "AstHandler: Frameset[0]=<" << serialFrameset_[0]
	     << ">: " << (isFits ? "FITS" : "AST") << endl;

    // begin an AST context
    astBegin;

    channel_source (true);	// reset the stream, just in case
    channel_source_init (this);
    if (isFits)
    {
        AstFitsChan *channel_ = astFitsChan(0, 0, "");
        astClearStatus;
        for (vector<string>::const_iterator ci = serialFrameset_.begin();
             ci != serialFrameset_.end();
             ci++) {
            astPutFits(channel_, ci->c_str(), 0);
            if (! astOK) {
                Util::logstream()
                    << "moggy: FITS card <" << *ci
                    << "> is invalid, and will be ignored" << endl;
                astClearStatus;
            }
        }
        astClear(channel_, "Card"); // rewind the channel
        astobj_ = static_cast<AstObject*>(astRead(channel_));
        channel_ = static_cast<AstFitsChan*>(astAnnul(channel_));
    }
    else
    {
	AstChannel *channel_ = astChannel
	    (static_cast<const char *(*)()>(&channel_source_server),
	     0,
	     "");
	astobj_ = static_cast<AstObject*>(astRead (channel_));
	channel_ = static_cast<AstChannel*>(astAnnul (channel_));
    }

    // Check here that this is indeed a FrameSet.
    // Alternatively (for the future) we could accept a mapping.
    // In that case, we'd have to adjust inputSkyDomain, since
    // astobj_ would no longer, then, be a FrameSet.
    if (! astIsAFrameSet (astobj_))
	throw MoggyException
	    ("AstHandler: input AST object is not a FrameSet");

    if (verbosity_ > normal)
    {
	Util::logstream() << "AstHandler: read a " << astGetC(astobj_, "Domain") << endl;
	if (astIsAFrameSet (astobj_))
	    Util::logstream() << "    IsA FrameSet!" << endl;
    }

    fromDomainIndex_ = toDomainIndex_ = 0;
    nframes_ = astGetI (astobj_, "Nframe");
    if (verbosity_ > normal)
	Util::logstream() << "AstHandler: FrameSet has " << nframes_ << " frames" << endl;

    for (int i=1; i<=nframes_; i++)
    {
	AstFrame *f = static_cast<AstFrame*>(astGetFrame (astobj_, i));
	if (f == AST__NULL)	// error of some type
	    throw MoggyException ("failed to astGetFrame");

	const char *domain = astGetC (f, "Domain");
	astAnnul (f);		// decrement reference count,
				// incremented by astGetFrame

	if (verbosity_ > normal)
	    Util::logstream() << "AstHandler: domain " << i << "=" << domain << endl;
	if (domain == fromdomain_)
	{
	    if (verbosity_ > normal)
		Util::logstream() << "  (fromdomain)" << endl;
	    fromDomainIndex_ = i;
	}
	if (domain == todomain_)
	{
	    if (verbosity_ > normal)
		Util::logstream() << "  (todomain)" << endl;
	    toDomainIndex_ = i;
	}
    }

    // Check we did in fact find the from/input domain we were
    // promised.  We check the to/output domain below.
    if (verbosity_ > normal)
	Util::logstream() << "AstHandler::AstHandler: from "
	     << fromdomain_ << '=' << fromDomainIndex_
	     << " to "
	     << todomain_ << '=' << toDomainIndex_ << endl;
    if (fromDomainIndex_ == 0)
    {
	SSTREAM msg;
	msg << "can't find input domain " << fromdomain_;
	throw MoggyException (SS_STRING(msg));
    }

    // Check that
    //
    //   1. toDomainIndex_ is defined and does point to a SkyFrame;
    //   2. this SkyFrame has System=FK5 and Equinox=J2000.
    //
    // Fail noisily if either is false.  As noted above, it might be
    // desirable, in future, to accept a Mapping as serialised input,
    // rather than a FrameSet, but then we can't necessarily make
    // check 2 (though we could just abandon check 2 in that case, and
    // explicitly trust the caller to get things right).  It almost
    // certainly _will_ be desirable to allow SkyFrames with other
    // coordinate systems, and simply and silently add a further
    // mapping from the given coordinate system to J2000 (see section
    // `Converting between Celestial Coordinate Systems' in SUN/211).
    if (toDomainIndex_ == 0)
    {
	SSTREAM msg;
	msg << "input FrameSet has no " << todomain_;
	throw MoggyException (SS_STRING(msg));
    }
    if (astIsASkyFrame (astGetFrame(astobj_, toDomainIndex_)))
    {
	string System = astGetC(astobj_, "System");
	double Equinox = astGetD(astobj_, "Equinox");
	if (System != "FK5" || Equinox != 2000.0)
	{
	    SSTREAM msg;
	    msg << "input SkyFrame has System=" << System
		<< ", Equinox=" << Equinox
		<< " -- require FK5(J2000)";
	    throw MoggyException (SS_STRING(msg));
	}
    }
    else
	throw MoggyException ("input FrameSet does not have a SkyFrame");

    // At this point, we know that the SKY domain that's about to be
    // made the target of the mapping has a FK5(J2000) System.
    // Therefore, further, its axes are always in the order
    // (longitude, latitude) in double-precision radians (see SUN/211,
    // Sect. 8.1, `The SkyFrame Model').

    // Extract the mapping from the FrameSet, and perform any
    // simplifications.
    astmap_ = static_cast<AstMapping*>(astGetMapping (astobj_,
						      fromDomainIndex_,
						      toDomainIndex_));
    astmap_ = static_cast<AstMapping*>(astSimplify (astmap_));

    // Quick sanity-check: We _do_ have 2 input and 2 output axes,
    // don't we?
    int Nin  = astGetI (astmap_, "Nin");
    int Nout = astGetI (astmap_, "Nout");
    if (Nin != 2 || Nout != 2)
    {
	SSTREAM msg;
	msg << "mapping has unexpected dimensionality: Nin=" << Nin
	    << ", Nout=" << Nout;
	throw MoggyException (SS_STRING(msg));
    }

    // PWD: work out which axes are longitude and which are latitude
    // (note we leave at defaults if neither is a time axis).  Note
    // this comment contradicts the assertion about latitude and
    // longitude order made above.
    int astime2 = astGetI( astobj_, "astime(2)" );
    if ( astime2 ) {
        raIndex_  = 2;
        decIndex_ = 1;
    } else {
        raIndex_  = 1;
        decIndex_ = 2;
    }

    if (verbosity_ > normal)
	Util::logstream() << "AstHandler::AstHandler: successfully constructed astmap_"
	     << endl;
}

AstHandler::~AstHandler ()
{
    // End the AST context
    astEnd;
}

bool AstHandler::transToSky (const double xpix, const double ypix,
			     double& radeg, double& decdeg)
{
    astTran2 (astmap_, 1, &xpix, &ypix, 1, &radeg, &decdeg);

    // RA and Dec can be swapped. Also normalize the result into the
    // correct range.
    double point[2];
    point[0] = radeg;
    point[1] = decdeg;
    astNorm( astobj_, point );
    if ( raIndex_ == 1 ) {
        radeg = point[0];
        decdeg = point[1];
    }
    else {
        decdeg = point[0];
        radeg = point[1];
    }

    if (verbosity_ > normal)
	Util::logstream() << "AstHandler::transToSky: (" << xpix << ',' << ypix
	     << ") --> (" << radeg << ',' << decdeg
	     << ")rad = ("
	     << astFormat (astobj_, 1, radeg)
	     << ','
	     << astFormat (astobj_, 2, decdeg)
	     << ")";

    // These coordinates are in radians (by the definition of the
    // SkyFrame).  Convert them to decimal degrees.
    radeg *= DegreesPerRadian;
    decdeg *= DegreesPerRadian;

    if (verbosity_ > normal)
	Util::logstream() << " = (" << radeg << ',' << decdeg << ")deg" << endl;

    return true;		// astTran2 does not indicate errors
}



bool AstHandler::transFromSky (const double radeg, const double decdeg,
			       double& xpix, double& ypix)
{
    // Transform RA and Dec coordinates from decimal degrees to
    // radians -- no need to worry about normalisation, since AST
    // takes care of everything.
    double lradeg;
    double ldecdeg;
    if ( raIndex_ == 1 ) {
        lradeg  = radeg/DegreesPerRadian;
        ldecdeg = decdeg/DegreesPerRadian;
    } else {
        ldecdeg  = radeg/DegreesPerRadian;
        lradeg = decdeg/DegreesPerRadian;
    }

    astTran2 (astmap_, 1, &lradeg, &ldecdeg, 0, &xpix, &ypix);
    if (verbosity_ > normal)
	Util::logstream() << "AstHandler::transFromSky: (" << radeg << ',' << decdeg
	     << ")rad = (" << xpix << ',' << ypix << ')' << endl;

    return true;		// astTran2 does not indicate errors
}

// channel_source supplies lines of serialFrameset_ one at a time.
// This is the required behaviour for the function passed to
// astRead.  However, we cannot use a member function for that
// purpose, but have to use a static function instead (or use global
// data, and deal with the corresponding mess).  Call
// channel_source_init first, then use channel_source_server as the
// astRead function.
//
// This does _not_ handle multiple AstHandler objects, so you can't
// presently call this sequence more than once in a program.  However,
// it is written to _fail_ noisily in this case, so it won't fail
// without you getting to hear about it.
void AstHandler::channel_source_init (AstHandler *h)
    throw (MoggyException)
{
    if (channel_source_currenthandler_ != 0)
	throw MoggyException ("channel_source_init called more than once.  FIXME");

    channel_source_currenthandler_ = h;
}
const char *AstHandler::channel_source_server(void)
    throw (MoggyException)
{
    if (channel_source_currenthandler_ == 0)
	throw MoggyException ("channel_source_server not initialised");

    return channel_source_currenthandler_->channel_source();
}
const char *AstHandler::channel_source (bool reset)
{
    static bool more = false;	// Initial state.
				// more<-true when started;
				// then more<-false when exhausted.
    static vector<string>::const_iterator iter;

    const char *rval;

    if (reset)
    {
	// reset the stream and return immediately
	more = false;
	return 0;
    }

    if (!more)
    {
	iter = serialFrameset_.begin();
	more = true;
    }
    assert (more == true);

    if (iter == serialFrameset_.end())
    {
        more = false;
        rval = 0;
    }
    else
    {
        rval = iter->c_str();
        iter++;
    }

    return rval;
}

// Return true if the input domain is a sky frame
bool AstHandler::inputSkyDomain (void)
    throw (MoggyException)
{
    if (! astIsAFrameSet (astobj_))
	throw MoggyException ("Ast object was expected to be a FrameSet");

    return astIsASkyFrame (astGetFrame (astobj_, fromDomainIndex_));
}
