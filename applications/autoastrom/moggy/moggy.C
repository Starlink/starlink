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
#include "util.h"

const char *progname;

const string crlf = "\r\n";

void dumpCatalogue (ostream& o);
string processCommand (CommandParse *t, bool& keepGoing);
void Usage ();

int main (int argc, char **argv)
{
    ofstream logfile;		// For debugging only.
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

	    if (logfile)
	    {
		vector<string> arglist = cmd->arguments();
		logfile << "Command :";
		for (vector<string>::const_iterator p=arglist.begin();
		     p != arglist.end();
		     ++p)
		    logfile << ' ' << *p;
		logfile << endl;
	    }

	    // Process the parsed command obtained from the input.  If 
	    // the processing of a command produces multi-line output
	    // then that is written to cout and an empty response
	    // returned.  Otherwise, this returns a one-line response, 
	    // which we write to cout below.
	    string response = processCommand (cmd, keepGoing);

	    if (response.length() > 0)
	    {
		cout << response << crlf;

		if (logfile)
		    logfile << "Response: " << response << endl;
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
string processCommand (CommandParse *cmd, bool& keepGoing)
{
    vector<string> arglist = cmd->arguments();
    string response;
    static CatalogueHandler cat;

    switch (cmd->type())
    {
      case CommandParse::INVALID:
	response = "500 Unknown command";
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
	    cerr << "COLUMNS=<"<<arglist[1]<<">"<<endl;
	    if (cat.setResultCols (arglist[1]))
		response = "250 Parameter set successfully";
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
	if (arglist.size() == 7 || arglist.size() == 8)
	{
	    int rah, ramin, decdeg, decmin;
	    double rasec, decsec, equinox;

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
		convok = Util::stringToDouble (arglist[7], equinox);

	    if (convok)
		if (cat.setPos (cmd->type()==CommandParse::COORD1 ?1:2,
				rah, ramin, rasec,
				decdeg, decmin, decsec,
				equinox))
		    response = "250 Parameter set successfully";
		else
		    response = "501 Error setting coordinates";
	    else
		response = "501 Error parsing parameters";
	}
	else
	    response = "501 Wrong number of parameters";

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
		vector<string> names = cat.getColnames();
		if (names.size() == 0)
		    response = "550 Can't obtain column names";
		else
		{
		    cout << "210 Catalogue follows" << crlf << flush;
		    // Write out the number of columns, and a
		    // row for each one.
		    //
		    // Should we make this more robust, so
		    // that it _always_ returns the number of
		    // rows it promised to, even if there's
		    // some problem within the iterator,
		    // signalled by a thrown MoggyException?
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
			cout << *p << crlf;
		    response = ""; // no further response required
		}
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
    cerr << "Usage: " << progname << " [-llogfilename]" << endl;
    exit (1);
}
