
/*
 * E.S.O. - VLT project/Archive
 * $Id: TclAstroImage.C,v 1.14 1998/08/21 10:45:59 abrighto Exp $
 *
 * StarCat.C - method definitions for class TclAstroImage
 * 
 * ------------------------------------------------------------------
 * NOTE: This class is obsolete, please use the TclAstroCat class
 *       instead.
 * ------------------------------------------------------------------
 * 
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  26 Sep 95  Created
 */
static const char* const rcsId="@(#) $Id: TclAstroImage.C,v 1.14 1998/08/21 10:45:59 abrighto Exp $";


#include <string.h>
#include <ctype.h>
#include <stdio.h>
#include <iostream.h>
#include <stdlib.h>
#include <unistd.h>
#include <strstream>
#include "TclAstroCat.h"
#include "TclAstroImage.h"


/* 
 * declare a table of tcl subcommands
 * format: name, min_args, max_args, method
 */
static class TclAstroImageSubCmds {
public:
    char* name;      // method name
    int (TclAstroImage::*fptr)(int argc, char* argv[]); 
    int min_args;    // minimum number of args
    int max_args;    // maximum number of args
} subcmds_[] = { 
    {"authorize",  &TclAstroImage::authorizeCmd,  0,  4},
    {"open",       &TclAstroImage::openCmd,       1,  1},
    {"close",      &TclAstroImage::closeCmd,      0,  0},
    {"getimage",   &TclAstroImage::getimageCmd,   6,  99},
    {"info",       &TclAstroImage::infoCmd,       1,  1},
    {"centerpos",  &TclAstroImage::centerposCmd,  0,  0},
    {"copyright",  &TclAstroImage::copyrightCmd,  0,  0},
    {"help",       &TclAstroImage::helpCmd,       0,  0},
    {"iswcs",      &TclAstroImage::iswcsCmd,      0,  0},
    {"ispix",      &TclAstroImage::ispixCmd,      0,  0},
    {"feedback",   &TclAstroImage::feedbackCmd,   1,  1},
    {"longname",   &TclAstroImage::longnameCmd,   1,  1},
    {"shortname",  &TclAstroImage::shortnameCmd,  1,  1}
};


/*
 * Call the given method in this class with the given arguments
 */
int TclAstroImage::call(const char* name, int len, int argc, char* argv[])
{
    for(unsigned int i = 0; i < sizeof(subcmds_)/sizeof(*subcmds_); i++) {
	TclAstroImageSubCmds* t = &subcmds_[i];
	if (strncmp(t->name, name, len) == 0) {
	    if (check_args(name, argc, t->min_args, t->max_args) != TCL_OK)
		return TCL_ERROR;
	    return (this->*t->fptr)(argc, argv);
	}
    }
    return TclCommand::call(name, len, argc, argv);
}


/*
 * A call to this function can be made from the tkAppInit file at startup
 * to install the starcat command
 */
extern "C"
int TclAstroImage_Init(Tcl_Interp* interp)  
{
    Tcl_CreateCommand(interp, "astroimage", TclAstroImage::astroImageCmd, NULL, NULL);
    return TCL_OK;
}

/*
 * Implementation of the tcl extended command "astroimage" -
 * usage: see man page for more details
 */
int TclAstroImage::astroImageCmd(ClientData, Tcl_Interp* interp, int argc, char* argv[])
{
    if (argc != 2) {
	Tcl_AppendResult(interp, "wrong # args:  should be \"",
			 argv[0], " instanceName catalogName\"", NULL);
	return TCL_ERROR;
    }

    TclAstroImage* cmd = new TclAstroImage(interp, argv[0], argv[1]);
    return cmd->status();
}


/*
 * Constructor -
 *
 * Create an "astroimage" object in tcl for accessing the contents of star
 * catalogs.
 *
 * Note that the tcl command for this object is created in the
 * parent class constructor.
 */
TclAstroImage::TclAstroImage(Tcl_Interp* interp, const char* cmdname, const char* instname)
: TclCommand(interp, cmdname, instname),
  im_(NULL),
  equinox_(2000.0),
  feedback_(NULL)
{
}


/*
 * destructor
 */
TclAstroImage::~TclAstroImage()
{
    if (im_)
	delete im_;
}


/*
 * Open the given astromonical catalog and refer to it in future
 * queries.
 */
int TclAstroImage::openCmd(int argc, char* argv[])
{
    if (im_)
	delete im_;
    im_ = AstroImage::open(argv[0]);
    if (!im_)
	return TCL_ERROR;
    
    // set up feedback, if requested
    if (feedback_)
	im_->feedback(feedback_);

    return TCL_OK;
}


/*
 * close the current catalog, if one was open
 */
int TclAstroImage::closeCmd(int argc, char* argv[])
{
    if (im_)
	delete im_;
    im_ = NULL;
    return TCL_OK;
}


/*
 * Request an image from the current image catalog and return (in Tcl) 
 * the name of the FITS file holding the image.
 *
 * usage: $cat getimage ?-equinox equinox? -pos $pos -width $width \
 *                       -height $height -tmpfile $file
 *
 * -pos        - should be in the WCS format: H:M:S[+-]D:M:S
 * -equinox    - equinox for position (default 2000)
 * 
 * -tmpfile    - can be used to specify the pathname of the image file
 *
 * -nameserver - name of nameserver catalog to use (simbad@eso, ned@eso,...)
 *
 * -name       - can be used instead of -pos. The name will be resolved
 *               using the value of -nameserver (default: SIMBAD)
 *
 * -width      - dimensions of image to get
 * -height      
 *
 */
int TclAstroImage::getimageCmd(int argc, char* argv[])
{
    if (!im_) 
	return error("no catalog is currently open");

    // option variables
    double width = 0.0, height = 0.0;
    char* nameServer = "simbad@eso";
    int got_pos = 0;		// flag: true if we read the position arg

    equinox_ = 2000.0;
    pos_.setNull();

   // parse options
    for (int i = 0; i < argc; i += 2) {
	char* option = argv[i];
	char* value = argv[i+1];

	if (strcmp(option, "-pos") == 0) {
	    got_pos++;
	    // get ra and dec
	    char** values = NULL;
	    int numValues = 0;
	    if (Tcl_SplitList(interp_, value, &numValues, &values) != TCL_OK) 
		return TCL_ERROR;
	    if (numValues != 2)
		return error("for -pos: expected list with 2 items");
	    if (im_->isWcs()) 
		pos_ = WorldCoords(values[0], values[1], equinox_);
	    else if (im_->isPix())
		pos_ = ImageCoords(values[0], values[1]);
	    free(values);
	    if (pos_.status())
		return TCL_ERROR;
	}
	else if (strcmp(option, "-equinox") == 0) {
	    if (got_pos) 
		return error("-equinox should precede the -pos argument");
	    while(*value && !isdigit(*value))
		value++;	// skip "J" in J 2000, or "B" in B 1950
	    if (Tcl_GetDouble(interp_, value, &equinox_) != TCL_OK)
		return error("bad value for equinox: ", interp_->result);
	}
	else if (strcmp(option, "-width") == 0) {
	    if (Tcl_GetDouble(interp_, value, &width) != TCL_OK)
		return error("bad value for width: ", interp_->result);
	}
	else if (strcmp(option, "-height") == 0) {
	    if (Tcl_GetDouble(interp_, value, &height) != TCL_OK)
		return error("bad value for height: ", interp_->result);
	}
	else if (strcmp(option, "-nameserver") == 0) {
	    nameServer = value;
	}
	else if (strcmp(option, "-name") == 0) {
	    if (AstroCatalog::nameToWorldCoords(value, pos_, nameServer, feedback_) != 0)
		return TCL_ERROR;
	}
	else if (strcmp(option, "-tmpfile") == 0) {
	    if (value) {
		unlink(im_->tmpfile());
		im_->tmpfile(value);
	    }
	}
    }

    if (pos_.isNull() || width == 0.0 || height == 0.0) 
	return error("must specify a name or position, width and height");

    // send the query
    if (im_->getImage(pos_, width, height) != 0)
	return TCL_ERROR;

    return set_result(im_->tmpfile());
}


/*
 * querypos subcommand:
 *
 * Return the world coordinate position arguments from the most recent
 * request, posibly expanded by SIMBAD. The result is a list of the form
 * {ra dec}
 */
int TclAstroImage::centerposCmd(int argc, char* argv[])
{
    char buf[126];
    std::ostrstream os(buf, sizeof(buf));

    pos_.print(os, equinox_);	// print coords in given equinox
    if (im_->isWcs())
	os << " " << equinox_ << ends;
    return set_result(buf);
}


/*
 * copyright subcommand: return the copyright info for this image server
 * from the config file.
 */
int TclAstroImage::copyrightCmd(int argc, char* argv[])
{
    if (im_) {
	return set_result(im_->copyright());
    }
    return TCL_OK;
}

/*
 * help subcommand: return the help info for this image server
 * from the config file.
 */
int TclAstroImage::helpCmd(int argc, char* argv[])
{
    if (im_) {
	return set_result(im_->help());
    }
    return TCL_OK;
}

/*
 * feedback subcommand: 
 *
 * specifies a Tcl file descriptor to use to write feedback info during
 * HTTP transfer of image.  1 Arg: file descriptor.
 */
int TclAstroImage::feedbackCmd(int argc, char* argv[])
{
    if (strlen(argv[0]) != 0) {
	if (Tcl_GetOpenFile(interp_, argv[0], 1, 1, (ClientData*)&feedback_) != TCL_OK)
	    return TCL_ERROR;
    }
    else {
	feedback_ = NULL;	
    }
    if (im_)
	im_->feedback(feedback_);
    return TCL_OK;
}


/*
 * "info" subcommand:
 *
 * usage: $im info $serv_type
 *
 * This command returns a list of servers from the config file
 * (skycat.cfg). The "serv_type" argument determines which catalogs
 * are listed (one of: catalog, namesvr, imagesvr).
 */
int TclAstroImage::infoCmd(int argc, char* argv[])
{
    const CatalogInfoEntry* e = CatalogInfo::first();
    if (!e) 
	return TCL_ERROR;
    Tcl_ResetResult(interp_);

    // get the serv_type
    for (; e != NULL; e = e->next()) {
	if (strncmp(argv[0], e->servType(), strlen(e->servType())) == 0) {
	    Tcl_AppendElement(interp_, (char*)e->longName());
	}
    }
    return TCL_OK;
}


/*
 * longname subcommand: return the long_name field from the catalog config file
 */
int TclAstroImage::longnameCmd(int argc, char* argv[])
{
    const CatalogInfoEntry* e = CatalogInfo::lookup(argv[0]);
    if (e) 
	return set_result(e->longName());
    return TCL_OK;
}

/*
 * shortname subcommand: return the short_name field from the catalog config file
 */
int TclAstroImage::shortnameCmd(int argc, char* argv[])
{
    const CatalogInfoEntry* e = CatalogInfo::lookup(argv[0]);
    if (e) 
	return set_result(e->shortName());
    return TCL_OK;
}

/* 
 * Tcl subcommand: "iswcs" returns true if the catalog is based on world
 * coordinates
 */
int TclAstroImage::iswcsCmd(int argc, char* argv[])
{
    if (!im_) 
	return error("no catalog is open");

    return set_result(im_->isWcs());
}

/* 
 * Tcl subcommand: "ispix" returns true if the catalog is based on image
 * coordinates
 */
int TclAstroImage::ispixCmd(int argc, char* argv[])
{
    if (!im_) 
	return error("no catalog is open");

    return set_result(im_->isPix());
}


/*
 * authorize subcommand: If the previous HTTP GET returned an
 * authorization error: (The HTML error text returned in Tcl contained 
 * the string: "Authorization Required"), the application can
 * ask the user to enter a username and password to use to access the
 * URL and then retry the GET after using this subcommand to set the
 * authorization information to use.
 *
 * usage: 
 *
 *     $cat authorize
 *     $cat authorize username passwd
 *
 * With no arguments, this command returns a list of the form
 *
 *   {needpasswd realm server}
 *
 * where:
 *    needpasswd is nonzero if a password is required for the URL
 *    realm is the string taken from the HTTP header (Basic realm=...).
 *    server is the name of the target server that wants the password.
 *
 * If arguments are specified, they should be the username and password.
 * These are saved and used again when the GET is retried, as well as for
 * all future GETS in this session, unless a new username and password
 * are given (as a result of an authorization error...).
 */
int TclAstroImage::authorizeCmd(int argc, char* argv[])
{
    if (!im_) 
	return error("no image server is open");
    
    if (argc == 0) {
	HTTP& http = im_->http();
	char buf[1024];
	std::ostrstream os(buf, sizeof(buf));
	os << http.authorizationRequired() 
	   << " " << http.www_auth_realm()
	   << " " << http.hostname() << ends;
	return set_result(buf);
    }

    if (argc == 2) 
	HTTP::authorize(argv[0], argv[1]);
    else if (argc == 4) 
	HTTP::authorize(argv[0], argv[1], argv[2], argv[3]);
    else
	return error("expected: astroimage authorize ?username passwd realm server?");

    return TCL_OK;
}

