// Part of moggy
// Copyright 2001 Council for the Central Laboratory of the Research Councils.
// See file LICENCE for conditions.


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


#ifdef HAVE_CONFIG_H
#include "config.h"
#endif


#if HAVE_CSTD_INCLUDE
#include <cstdio>
#include <cstdlib>
#include <ctime>
//using std::exit;
#else
#include <stdio.h>              // for cout,cerr
#include <stdlib.h>
#include <time.h>
#endif

#include <string>
#include <fstream>

#include <cat/CatalogInfo.h>

#include "moggy.h"
#include "CommandParse.h"
#include "CatalogueHandler.h"
#include "AstHandler.h"
#include "stringstream.h"
#include "util.h"
#include "verbosity.h"

const char *progname;

const string crlf = "\r\n";

verbosities verbosity = normal;

void dumpCatalogue (ostream& o);
string processCommand (CommandParse *t, istream& cin, bool& keepGoing);
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
    unsetenv ("CATLIB_CONFIG");

    try
    {
	string commandline;
	bool keepGoing = true;

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
		cerr << "Moggy:dialogue:Command :";
		for (vector<string>::const_iterator p=arglist.begin();
		     p != arglist.end();
		     ++p)
		    cerr << ' ' << *p;
		cerr << endl;
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
		    cerr << "Moggy:dialogue:Response: " << response << endl;
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
    catch (exception& e)
    {
	cout << "550 Internal error. Terminating ("
	     << e.what() << ")" << crlf;
    }
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
// When it is time to exit, set parameter keepGoing to true.  This is
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
	if (arglist.size() == 2)
	{
	    string upar = arglist[1];
	    Util::uppercaseString(upar);
	    if (upar == "NONE")
	    {
		// OK
		if (ast != 0)
		{
		    delete ast;
		    ast = 0;
		}
		response = "250 Frameset deleted";
	    }
	    else
		response = "501 Wrong number or type of parameters";
	}
	else if (arglist.size() == 3)
	{
	    string upar = arglist[1];
	    Util::uppercaseString(upar);
	    if (upar == "FRAMESET")
	    {
		// OK.  First delete any old AST information
		if (ast != 0)
		    delete ast;

		// Send back the reply requesting the frameset
		cout << "350 Command accepted -- send frameset"
		     << crlf << flush;

		if (verbosity > normal)
		    cerr << "Moggy:dialogue:Response:"
			 << "350 Command accepted -- send frameset" << endl;

		// ... and read the frameset

		vector<string> frameset;
		string astlinein;
		while (getline (instream, astlinein))
		{
		    // top and tail whitespace from the string
		    string::size_type startpos
			= astlinein.find_first_not_of (" \t\r\n");
		    string::size_type endpos
			= astlinein.find_last_not_of (" \t\n\r");

		    if (verbosity > normal)
			cerr << "Moggy:dialogue:Input   :"
			     << astlinein << endl;

		    if (startpos < astlinein.npos)
		    {
			// The string is not solely whitespace
			string astline = astlinein.substr (startpos,
							   endpos-startpos+1);

			if (astline == ".")
			    break;

			frameset.push_back(astline);
		    }
		}

		ast = new AstHandler (frameset, arglist[2]);
		response = "250 Frameset created successfully";
	    }
	    else
		response = "501 Syntax error -- unrecognised keyword";
	}
	else
	    response = "501 Syntax error -- wrong number of parameters";
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
		cerr << "moggy: COLUMNS=<"<<arglist[1]<<">"<<endl;
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
	    cout << "210 Configuration file follows, "
		"terminated by <CRLF>.<CRLF>" << crlf;
	    dumpCatalogue (cout);
	    cout << '.' << crlf;
	    response = "";
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
			    cerr << "moggy: transPair ("
				 << param[1] << ',' << param[2]
				 << ") --> ("
				 << radouble << ',' << decdouble
				 << ')' << endl;
			else
			    cerr << "moggy: transPair ("
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
		cerr << "moggy: " << nargs << " arguments:" << endl;
		for (int i=1; i<=nargs; i++)
		    cerr << "    " << i << ':' << param[i] << endl;
		switch (coordsok)
		{
		  case badcoords:
		    cerr << "    type=bad" << endl;
		    break;
		  case sexagesimal:
		    cerr << "    type=sexagesimal "
			 << rah << ':' << ramin << ':' << rasec
			 << ' '
			 << decdeg << ':' << decmin << ':' << decsec
			 << endl;
		    break;
		  case decdegrees:
		    cerr << "    type=dec. degrees "
			 << radouble << ',' << decdouble
			 << endl;
		    break;
		  default:
		    cerr << "    type=IMPOSSIBLE" << endl;
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

#if 0
	    if (ast != 0)
	    {
		if (ast->inputSkyFrame())
		else
		{
		    // There must be two parameters
		    if (arglist.size() == 3)
		    {
			bool convok;
			double x, y;
			convok = Util::stringToDouble (arglist[1], x);
			if (convok)
			    convok = Util::stringToDouble (arglist[2], y);

			if (convok)
			{
			    // Convert to Sky coordinates
			}
			else
			    response = "501 Error parsing parameters";
		    }
		    else
			response = "501 Wrong number of parameters";
		}
	    }
	    else
		// Input Sky coordinates -- no AST information.
		// There must be either three or four or seven or
		// eight parameters given, either RA and Dec in
		// decimal degrees, or RA and Dec in H:M:S, D:M:S,
		// followed by an optional equinox.
		if (arglist.size() == 3 || arglist.size() == 4)
		{
		    bool convok;
		    equinox = 2000; // default

		    convok = Util::stringToDouble (arglist[1], radouble);
		    if (convok)
			convok = Util::stringToDouble (arglist[2], decdouble);
		    if (convok && arglist.size() == 4)
			convok = Util::stringToDouble (arglist[2], equinox);

		    if (convok)
			coordsok = 2;
		    else
			response = "501 Error parsing parameters";
		}
		else if (arglist.size() == 7 || arglist.size() == 8)
		{
		    bool convok;
		    equinox = 2000; // default

		    convok = Util::stringToInteger (arglist[1], rah);
		    if (convok)
			convok = Util::stringToInteger (arglist[2], ramin);
		    if (convok)
			convok = Util::stringToDouble  (arglist[3], rasec);
		    if (convok)
			convok = Util::stringToInteger (arglist[4], decdeg);
		    if (convok)
			convok = Util::stringToInteger (arglist[5], decmin);
		    if (convok)
			convok = Util::stringToDouble  (arglist[6], decsec);
		    if (convok && arglist.size() == 8)
			convok = Util::stringToDouble  (arglist[7], equinox);

		    if (convok)
			coordsok = 1;
		    else
			response = "501 Error parsing parameters";
		}
		else
		    response = "501 Wrong number of parameters";
#endif
	}

	break;

      case CommandParse::DEBUG:
	// Switch on verbose mode in selected modules.  Undocumented elsewhere.
	if (arglist.size() == 2)
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
		response = "552 Unknown catalogue";
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
		    for (CatalogueHandler::const_iterator p = cat.begin();
			 p != cat.end();
			 ++p)
		    {
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
	{
	    cout << "210 Status follows, terminated by <CRLF>.<CRLF>"
		 << crlf;
	    cat.printStatus (cout, crlf);
	    response = ".";
	}
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

      case CommandParse::VERSION:
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

void Usage ()
{
    cerr << "Usage: " << progname << endl;
    //cerr << "Usage: " << progname << " [-llogfilename]" << endl;
    exit (1);
}
