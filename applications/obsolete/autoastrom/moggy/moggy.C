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


//+
// <title>moggy
//
// <description>
//   <p>A simple server which wraps the Skycat library.  It's designed
//   to be used within a two-way pipe, so its dialogue with its
//   environment is rather strictly defined (see moggy-doc.txt), it
//   communicates via status codes, and is careful to flush its output
//   appropriately.
//
//   <p>It comes with a Perl module which encapsulates the dialogue
//   with it.  See Moggy.pm.
//
// <authorlist>
//   <author id=ng affiliation='Starlink, Glasgow University'>Norman Gray
//
// <history>
//   <change author=ng date='05-Mar-2001'>Initial version.
//-

static const char RCSID[] =
        "$Id$";


#include <config.h>


#include <iostream>
#include <string>
#include <fstream>
#include <assert.h>

#if HAVE_CSTD_INCLUDE
#include <cstdlib>
#include <ctime>
#else
#include <stdlib.h>
#include <time.h>
#endif


#include <cat/CatalogInfo.h>

#include "moggy.h"
#include "CommandParse.h"
#include "CatalogueHandler.h"
#include "AstHandler.h"
#include "stringstream.h"
#include "util.h"
#include "verbosity.h"

#if STD_IN_STD_NAMESPACE
/* Following probably needed for Alphas, but there are other iostream issues on
   Alphas, including an issue with __NO_USE_STD_IOSTREAM - see section 7.1.2
   of the (Alpha) C++ Using Guide" */
using std::istream;
using std::ostream;
using std::cout;
using std::cerr;
using std::endl;
using std::exit;
#endif

#if HAVE_UNSETENV
#if !HAVE_DECL_UNSETENV
extern "C" void unsetenv(const char *name);
#endif
#elif HAVE_PUTENV
#if !HAVE_DECL_PUTENV
extern "C" int putenv(char *string);
#endif
#endif


const char *progname;

const string crlf = "\r\n";

verbosities verbosity = normal;

#if 0
// Omitted -- see routine below for explanation
void dumpCatalogue (ostream& o);
#endif
string processCommand (CommandParse *t, istream& cin, bool& keepGoing);
string processAstCommand (vector<string>& arglist, istream&, AstHandler*& ast)
    throw (MoggyException);
void Usage ();

int main (int argc, char **argv)
{
    bool errorExit = true;	// Initialise to true, and clear it if
				// we exit normally from the loop
				// below.  Exceptions don't clear it,
				// and so result in a non-zero exit status.

    progname = argv[0];

    for (argc--, argv++; argc>0; argc--, argv++)
    {
	if (**argv == '-')
	    switch (*++*argv)
	    {
              case 'l':         // log file
                argc--, argv++;
                if (argc == 0)
                    Usage();
                Util::openLogstream(*argv);
                break;
#if 0
	      case 'l':		// log file
		++*argv;
		if (**argv == '\0')
		    Usage();
		logfile.open (*argv);
		// Did we manage to open it OK?
		// If it's not open, there's little we can do about
		// it, and it won't cause problems below, since we
		// always test (logfile) before writing to it.
		break;
#endif
	      default:
		Usage();
	    }
	else
	    Usage();
    }

    // Initialisation is very simple -- delete the CATLIB_CONFIG
    // environment variable.  This program is supposed to be run as a
    // slave, so any configuration done ought to be explicit, using
    // the CONF command.
#if HAVE_SETENV
    unsetenv ("CATLIB_CONFIG");
#elif HAVE_PUTENV
    {
	// unsetenv() doesn't exist -- there seems no other way of
	// deleting a variable from the environment.  If the
	// CATLIB_CONFIG variable exists, then ensure it's an empty
	// string.  The catlib documentation doesn't say that this is
	// equivalent to the variable not existing, but let's hope for
	// the best.
	char *envval = getenv("CATLIB_CONFIG");
	if (envval != 0 && *envval != '\0')
	{
	    static char arg[] = "CATLIB_CONFIG=";
	    putenv (arg);
	}
    }
#else
#error "This is ridiculous -- we don't have either unsetenv() or putenv()"
#endif

    try
    {
	string commandline;
	bool keepGoing = true;

	// Modify the formatting of floating-point numbers on output.
	// The default output precision is 6, which isn't really
	// enough.  1arcsec is 2e-4 degrees, so we really need at least 8
	// significant figures of output, and more does no harm.
	cout.precision(10);

	// Keep reading a string from stdin until EOF or keepGoing
	// becomes true.  This will read lines terminated by '\n' (on
	// unix): this is supposed to accept lines terminated by
	// "\r\n", but will in fact accept either _because_
	// CommandParse splits strings at whitespace which includes
	// both '\r' and '\n' (ie, this is good, as it's being liberal
	// in what it accepts).
	while (keepGoing && getline (cin, commandline))
	{
	    CommandParse* cmd = new CommandParse (commandline);

	    if (verbosity > normal)
	    {
		vector<string> arglist = cmd->arguments();
		Util::logstream() << "Moggy:dialogue:Command :";
		for (vector<string>::const_iterator p=arglist.begin();
		     p != arglist.end();
		     ++p)
		    Util::logstream() << ' ' << *p;
		Util::logstream() << endl;
	    }

	    // Process the parsed command obtained from the input.  If
	    // the processing of a command produces multi-line output
	    // then that is written to cout and an empty response
	    // returned.  Otherwise, this returns a one-line response,
	    // which we write to cout below.
	    string response = processCommand (cmd, cin, keepGoing);

	    if (response.length() > 0)
	    {
		cout << response << crlf;

		if (verbosity > normal)
		    Util::logstream() << "Moggy:dialogue:Response: " << response << endl;
	    }

	    // Flush the output, since this program will be used inside a pipe.
	    cout << flush;
	}

	// Normal exit -- clear errorExit
	errorExit = false;
    }
    catch (CommandParse::BadCommandParse& e)
    {
	cout << "501 Completely unparseable command (" << e.msg << "). Exiting"
	     << crlf;
    }
    catch (MoggyException& e)
    {
	cout << "550 Error: " << e.msg << crlf;
    }
#if 0
    // Not implemented in egcs?
    catch (exception& e)
    {
	cout << "550 Internal error. Terminating ("
	     << e.what() << ")" << crlf;
    }
#endif
    catch (...)
    {
	// Is this the correct thing to do?
	cout << "550 Catastrophic internal error! Terminating" << crlf;
    }

    exit (errorExit ? 1 : 0);
}


// processCommand: given a parsed command, work through a big switch
// and process the command appropriately.
//
// Most commands generate only a one-line response, and in this case
// this is passed back as this method's return value The commands
// which generate multi-line responses (just CONF and SEARCH) do their
// own responses to cout, and return an empty string.
//
// When it is time to exit, set parameter keepGoing to false.  This is
// taken as a successful exit -- any errors should raise exceptions.
// Since this is the routine where all this program's processing
// happens, we won't give an exception-specification, but assume it
// can throw anything.
string processCommand (CommandParse *cmd, istream& instream, bool& keepGoing)
{
    vector<string> arglist = cmd->arguments();
    string response;
    static CatalogueHandler cat;
    static AstHandler *ast;
    static enum { ALL, SELECTED } columnstyle = ALL;
    static string OPseparator = " ";

    switch (cmd->type())
    {
      case CommandParse::INVALID:
	response = "500 Unknown command";
	break;

      case CommandParse::AST:
	response = processAstCommand (arglist, instream, ast);
	break;

      case CommandParse::CATCONFIG:
	if (arglist.size() == 2)
	    if (cat.setConfig (arglist[1]))
		response = "250 Parameter set successfully";
	    else
		response = "550 Error setting catlib";
	else
	    response = "501 Wrong number of parameters";
	break;

      case CommandParse::COLUMNS:
	if (arglist.size() == 2)
	{
	    if (verbosity > normal)
		Util::logstream() << "moggy: COLUMNS=<"<<arglist[1]<<">"<<endl;
	    string kwd = arglist[1];
	    Util::uppercaseString(kwd);
	    if (kwd == "ALL")
	    {
		columnstyle = ALL;
		response = "250 Parameter set successfully";
	    }
	    else if (kwd == "SIMPLE")
	    {
		columnstyle = SELECTED;
		response = "250 Parameter set successfully";
	    }
	    else
		response = "550 Error setting columns";
	}
	else
	    response = "501 Wrong number of parameters";
	break;

      case CommandParse::CONF:
	if (arglist.size() == 1)
	{
	    response = "505 CONF unimplemented";
#if 0
	    // Omitted -- see dumpCatalogue() below for explanation
	    cout << "210 Configuration file follows, "
		"terminated by <CRLF>.<CRLF>" << crlf;
	    dumpCatalogue (cout);
	    cout << '.' << crlf;
	    response = "";
#endif
	}
	else
	    response = "501 Unexpected parameter";
	break;

      case CommandParse::COORD1:
      case CommandParse::COORD2:
	{
	    int rah, ramin, decdeg, decmin;
	    double rasec=0, decsec=0; // initialise, to suppress warning

	    double equinox=2000.0; // default equinox
	    double radouble, decdouble;
	    enum { badcoords, sexagesimal, decdegrees } coordsok = badcoords;

	    // We haven't yet included the case of sky coordinates as
	    // input to an AST transformation, so object to that first
	    if (ast != 0 && ast->inputSkyDomain())
		throw MoggyException
		    ("Can't yet accept input sky coordinates plus AST");

	    // Possibilities:
	    //   if ast
	    //      x/pix y/pix
	    //   else
	    //      ra/double-deg  dec/double-deg  ?equinox/double
	    //      OR
	    //      ra/int-hrs ramin/min rasec/sec ...
	    //          ...dec/int-deg decmin/min decsec/sec ?equinox/double

	    // First, convert all the numbers in the argument list,
	    // and only then worry about the above parsing.
	    double param[8];	// param[0] unused
	    int nargs = arglist.size() - 1;
	    if (nargs > 7)
	    {
		response = "501 Wrong number of parameters";
		break;
	    }

	    bool convok = false;
	    for (int i=1; i<=nargs; i++)
		if (! (convok = Util::stringToDouble(arglist[i], param[i])))
		    break;

	    if (! convok)
	    {
		response = "501 Error parsing parameters";
		break;
	    }

	    switch (nargs)
	    {
	      case 2:
		if (ast)
		{
		    // These are pixel coordinates, which need to be
		    // converted to Sky coordinates
		    if (ast->transToSky (param[1], param[2],
					 radouble, decdouble))
			coordsok = decdegrees;

		    if (verbosity > normal)
			if (coordsok == decdegrees)
			    Util::logstream() << "moggy: transPair ("
				 << param[1] << ',' << param[2]
				 << ") --> ("
				 << radouble << ',' << decdouble
				 << ')' << endl;
			else
			    Util::logstream() << "moggy: transPair ("
				 << param[1] << ',' << param[2]
				 << ") failed!" << endl;
		}
		else
		{
		    // These must already be decimal degrees
		    radouble = param[1];
		    decdouble = param[2];
		    coordsok = decdegrees;
		}
		break;

	      case 3:
		if (ast)
		    // Nope -- can't give an equinox for device
		    // coordinates
		    response = "501 Can't give equinox for device coordinates";
		else
		{
		    radouble = param[1];
		    decdouble = param[2];
		    equinox = param[3];
		    coordsok = decdegrees;
		}
		break;

	      case 7:
		equinox = param[7];
		// FALL THROUGH
	      case 6:
		if (ast)
		    // Nope -- if there's an AST mapping, we'll only
		    // accept device coordinates
		    response = "501 AST information present -- can't accept sky coordinates";
		else
		{
		    bool convok = true;
		    convok &= Util::stringToInteger (arglist[1], rah);
		    convok &= Util::stringToInteger (arglist[2], ramin);
		    rasec =                          param[3];
		    convok &= Util::stringToInteger (arglist[4], decdeg);
		    convok &= Util::stringToInteger (arglist[5], decmin);
		    decsec =                         param[6];
		    coordsok = (convok ? sexagesimal : badcoords);
		}
		break;

	      default:
		response = "501 Wrong number of parameters";
		break;
	    }

	    if (verbosity > normal)
	    {
		Util::logstream() << "moggy: " << nargs << " arguments:" << endl;
		for (int i=1; i<=nargs; i++)
		    Util::logstream() << "    " << i << ':' << param[i] << endl;
		switch (coordsok)
		{
		  case badcoords:
		    Util::logstream() << "    type=bad" << endl;
		    break;
		  case sexagesimal:
		    Util::logstream() << "    type=sexagesimal "
			 << rah << ':' << ramin << ':' << rasec
			 << ' '
			 << decdeg << ':' << decmin << ':' << decsec
			 << endl;
		    break;
		  case decdegrees:
		    Util::logstream() << "    type=dec. degrees "
			 << radouble << ',' << decdouble
			 << endl;
		    break;
		  default:
		    Util::logstream() << "    type=IMPOSSIBLE" << endl;
		    break;
		}
	    }

	    switch (coordsok)
	    {
	      case badcoords:
		// response is already set -- nothing else to do
		break;

	      case sexagesimal:
		if (cat.setPos (cmd->type()==CommandParse::COORD1 ?1:2,
				rah, ramin, rasec,
				decdeg, decmin, decsec,
				equinox))
		    response = "250 Parameter set successfully";
		else
		    response = "501 Error setting coordinates";
		break;

	      case decdegrees:
		if (cat.setPos (cmd->type()==CommandParse::COORD1 ?1:2,
				radouble, decdouble,
				equinox))
		    response = "250 Parameter set successfully";
		else
		    response = "501 Error setting coordinates";
		break;

	      default:
		throw MoggyException ("Impossible value for coordsok");
		break;
	    }
	}

	break;

      case CommandParse::DEBUG:
	// Switch on verbose mode in selected modules.  Undocumented elsewhere.
	if (arglist.size() == 2 || arglist.size() == 3)
	{
	    int convok;
	    int flags = 0;
	    convok = Util::stringToInteger (arglist[1], flags);
	    SSTREAM msg;
	    /* moggy */ verbosity =     ( flags & 1 ? debug : normal );
	    CommandParse::verbosity     ( flags & 2 ? debug : normal );
	    AstHandler::verbosity       ( flags & 4 ? debug : normal );
	    CatalogueHandler::verbosity ( flags & 8 ? debug : normal );

	    msg << "250 Debugging:";
	    if (flags & 1) msg << " moggy";
	    if (flags & 2) msg << " CommandParse";
	    if (flags & 4) msg << " AstHandler";
	    if (flags & 8) msg << " CatalogueHandler";

            if (arglist.size() == 3) {
                if (Util::openLogstream(arglist[2].c_str()))
                    msg << " (to file " << arglist[2] << ")";
                else
                    msg << " (can't open " << arglist[2]
                        << ": no logging)";
            }

	    response = SS_STRING(msg);
	}
	else
	    response = "250 Parameter set successfully";
	break;

      case CommandParse::NAME:
	if (arglist.size() == 2)
	    if (cat.setCatname (arglist[1]))
		response = "250 Parameter set successfully";
	    else
	    {
		SSTREAM msg;
		msg << "552 Unknown catalogue (" << arglist[1];
		char *env = getenv ("CATLIB_CONFIG");
		if (env)
		    msg << ", CATLIB_CONFIG=" << env;
		msg << ")";
		response = SS_STRING(msg);
	    }
	else
	    response = "501 Wrong number or type of parameters";
	break;

      case CommandParse::NROW:
	if (arglist.size() == 2)
	{
	    int nrow;
	    if (Util::stringToInteger(arglist[1], nrow))
		if (cat.setNrows (nrow))
		    response = "250 Parameter set successfully";
		else
		    response = "501 Can't set nrows to specified value";
	    else
		response = "501 Can't convert given value to integer";
	}
	else
	    response = "501 Wrong number or type of parameters";
	break;

      case CommandParse::RADIUS:
	if (arglist.size() == 2)
	{
	    double rad;
	    if (Util::stringToDouble(arglist[1], rad))
		if (cat.setRadius (rad))
		    response = "250 Parameter set successfully";
		else
		    response = "501 Can't set radius to given value";
	    else
		response = "501 Can't convert given value to double";
	}
	else
	    response = "501 Wrong number or type of parameters";
	break;

      case CommandParse::SEARCH:
	if (arglist.size() == 1)
	{
	    int nrows = cat.doSearch();
	    if (nrows >= 0)
	    {
		if (ast)
		    // Silently override any other choice
		    columnstyle = SELECTED;

		if (columnstyle == ALL)
		{
		    vector<string> names = cat.getColnames();
		    if (names.size() == 0)
			response = "550 Can't obtain column names";
		    else
		    {
			cout << "210 Catalogue follows" << crlf << flush;
			// Write out the number of columns, followed
			// by the column names each on a separate row.
			cout << names.size() << crlf;
			for (vector<string>::const_iterator n=names.begin();
			     n != names.end();
			     ++n)
			    cout << *n << crlf;
			// Write out the number of catalogue rows,
			// followed by the catalogue.
			cout << nrows << crlf;
			for (CatalogueHandler::const_iterator p = cat.begin();
			     p != cat.end();
			     ++p)
			{
			    const CatalogueHandler::CatalogueRow& r = *p;
			    cout << r << crlf;
			}
			response = ""; // no further response required
		    }
		}
		else if (columnstyle == SELECTED)
		{
		    vector<string> colname;
		    bool got_id, got_ra, got_dec, got_mag;
		    if (got_id = cat.has_id())
			colname.push_back("ID");
		    if (got_ra = cat.has_ra())
			colname.push_back("RA");
		    if (got_dec = cat.has_dec())
			colname.push_back("DEC");
		    if (got_mag = cat.has_mag())
			colname.push_back("MAG");
		    if (ast)
		    {
			colname.push_back("X");
			colname.push_back("Y");
		    }

		    if (!(got_ra && got_dec))
		    {
			// Not prepared to deal with such catalogues, yet
			throw MoggyException
			   ("Can't yet deal with catalogues without RA & Dec");
		    }

		    cout << "210 Catalogue follows" << crlf << flush;
		    cout << colname.size() << crlf;
		    for (vector<string>::const_iterator n=colname.begin();
			 n != colname.end();
			 ++n)
			cout << *n << crlf;
		    // Write out the number of catalogue rows,
		    // followed by the catalogue.
		    cout << nrows << crlf;
		    int rowcountcheck = nrows;
		    for (CatalogueHandler::const_iterator p = cat.begin();
			 p != cat.end();
			 ++p)
		    {
			if (--rowcountcheck < 0)
			{
			    // This shouldn't happen -- we've entered
			    // this loop more times than we promised.
			    // Somehow, the number of rows reported
			    // and held in nrows is different from the
			    // number returned by the iterator.
			    throw MoggyException
				("We have more rows than we expected -- extras discarded");
			}

			const CatalogueHandler::CatalogueRow& r = *p;
			if (got_id)  cout << r.id() << OPseparator;
			if (got_ra)  cout << r.ra() << OPseparator;
			if (got_dec) cout << r.dec() << OPseparator;
			if (got_mag) cout << r.mag() << OPseparator;
			if (ast)
			{
			    double x, y;
			    ast->transFromSky (r.ra(), r.dec(), x, y);
			    cout << x << OPseparator << y << OPseparator;
			}
			cout << crlf;
		    }
		    if (rowcountcheck > 0)
		    {
			// This shouldn't happen -- the iterator ran
			// out before supplying nrows rows of values.
			// Provide fake ones, and then throw an
			// exception.
			while (rowcountcheck > 0)
			{
			    if (got_id)  cout << "XX" << OPseparator;
			    if (got_ra)  cout << 0.0 << OPseparator;
			    if (got_dec) cout << 0.0 << OPseparator;
			    if (got_mag) cout << 0.0 << OPseparator;
			    if (ast)
				cout << 0.0 << OPseparator
				     << 0.0 << OPseparator;
			    cout << crlf;
			    rowcountcheck--;
			}
			throw MoggyException
			    ("Too few rows returned from catalogue search -- last few might be bogus");
		    }
		    assert (rowcountcheck == 0);

		    response = ""; // no further response required
		}
		else
		    throw MoggyException ("Impossible columnstyle");
	    }
	    else
		response = "503 Out of order: "
		    "not all required information has been supplied";
	}
	else
	    response = "501 Wrong number or type of parameters";
	break;

      case CommandParse::STATUS:
	if (arglist.size() == 1)
	    response = "505 STATUS unimplemented";
	else
	    response = "501 Syntax error";
	break;

      case CommandParse::TYPE:
	if (arglist.size() == 2)
	    if (cat.setSearchtype(arglist[1]))
		response = "250 Parameter set successfully";
	    else
		response = "502 Unrecognised search type";
	else
	    response = "501 Wrong number or type of parameters";
	break;

      case CommandParse::MOGGYVERSION:
	  {
	      string tmp("250 ");
	      response = tmp + RCSID;
	  }
	  break;

      case CommandParse::QUIT:
	response = "220 Goodbye";
	keepGoing = false;
	break;

      default:
	response = "502 Command not implemented";
	break;
    }

    return response;
}


string processAstCommand (vector<string>& arglist,
			  istream& instream,
			  AstHandler*& ast)
    throw (MoggyException)
{
    string response;
    enum
    {
	cmd_none, cmd_frameset, cmd_convert, cmd_null
    } subcmd = cmd_null;
    string upar = arglist[1];
    Util::uppercaseString(upar);
    unsigned int reqargs = 0; // number of elements required in arglist
    if (upar == "NONE")
    {
	subcmd = cmd_none;
	reqargs = 2;
    }
    else if (upar == "FRAMESET")
    {
	subcmd = cmd_frameset;
	reqargs = 3;
    }
    else if (upar == "CONVERT")
    {
	subcmd = cmd_convert;
	reqargs = 5;
    }

    if (subcmd == cmd_null || arglist.size() != reqargs)
	response = "501 Wrong number or type of parameters";
    else
	switch (subcmd)
	{
	  case cmd_none:
	    if (ast != 0)
	    {
		delete ast;
		ast = 0;
	    }
	    response = "250 Frameset deleted";
	    break;

	  case cmd_frameset:
	      {
		  if (ast != 0)
		      delete ast;

		  // Send back the reply requesting the frameset
		  cout << "350 Command accepted -- send frameset"
		       << crlf << flush;

		  if (verbosity > normal)
		      Util::logstream() << "Moggy:dialogue:Response:"
			   << "350 Command accepted -- send frameset"
			   << endl;

		  // ... and read the frameset

		  vector<string> frameset;
		  string astlinein;
		  while (getline (instream, astlinein))
		  {
                      // Remove _trailing_ whitespace, for neatness, but don't
                      // remove leading whitespace.  If this is a
                      // sequence of FITS card images, then 8 leading
                      // blanks cound as a BLANKFIELD commentary
                      // keyword.
                      string::size_type endpos
                          = astlinein.find_last_not_of(" \t\n\r");

		      if (verbosity > normal)
			  Util::logstream() << "Moggy:dialogue:Input   :"
			       << astlinein << endl;

                      if (endpos != astlinein.npos) {
                          // The string is not solely whitespace
                          string astline = astlinein.substr(0, endpos+1);
                          if (astline == ".")
                              break;
                          frameset.push_back(astline);
                      }
		  }

		  ast = new AstHandler (frameset, arglist[2]);
		  response = "250 Frameset created successfully";
	      }
	      break;

	  case cmd_convert:
	    if (ast)
	    {
		double arg1, arg2;
		bool convok =
		    Util::stringToDouble (arglist[2], arg1)
		    && Util::stringToDouble (arglist[3], arg2);
		string direction = arglist[4];
		bool toSky = true;
		Util::uppercaseString (direction);
		if (direction == "TOSKY")
		    toSky = true;
		else if (direction == "FROMSKY")
		    toSky = false;
		else
		    convok = false;

		if (convok)
		{
		    double res1, res2;
		    if (toSky)
			ast->transToSky (arg1, arg2, res1, res2);
		    else
			ast->transFromSky (arg1, arg2, res1, res2);
		    cout << "210 Conversion follows" << crlf << flush;
		    cout << res1 << ' ' << res2 << crlf << flush;
		    response = "";
		}
		else
		    response = "501 Wrong number or type of parameters";
	    }
	    else
		response = "503 No preceding AST FRAMESET command";

	    break;

	  default:
	    throw MoggyException ("Impossible command in AST");
	}

    return response;
}


// Dump the current catalogue.
//
#if 0
// This is omitted, at present.  It does work, but the `o << *e'
// generates a link error on Tru64 Unix.  I don't know whether this is
// my fault or Tru64's, but since this routine isn't essential,
// there's no point in sweating over it just now.
void dumpCatalogue (ostream& o)
{
    const time_t nowsecs = time(NULL);
    struct tm *now = gmtime (&nowsecs);
    char timebuf[18];

    o << "# Skycat catalogue configuration file" << endl;

    // Add an ISO 8601 date/time string
    strftime (timebuf, sizeof(timebuf), "%Y-%m-%dT%H:%MZ", now);
    o << "# Generated by " << progname << ", " << timebuf << endl;

    const char *catname = getenv ("CATLIB_CONFIG");
    if (catname == NULL)
	o << "# Default configuration" << endl;
    else
	o << "# from CATLIB_CONFIG=" << catname << endl;

    o << endl;

    for (const CatalogInfoEntry *e = CatalogInfo::first();
	 e != NULL;
	 e = e->next())
    {
	o << *e << endl;
    }

    return;
}
#endif

void Usage ()
{
    //cerr << "Usage: " << progname << endl;
    cerr << "Usage: " << progname << " [-l logfilename]" << endl;
    exit (1);
}
