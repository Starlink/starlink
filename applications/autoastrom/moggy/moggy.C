// Part of moggy
// Copyright 2001 Council for the Central Laboratory of the Research Councils.
// See file LICENCE for conditions.

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

void dumpCatalogue (ostream& o);
void Usage ();

int main (int argc, char **argv)
{
    string commandline;
    string crlf = "\r\n";
    bool keepGoing = true;
    CatalogueHandler cat;
    ofstream logfile;	// for debugging only

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
	// Keep reading a string from stdin until EOF or keepGoing
	// becomes true.  This will read lines terminated by '\n' (on
	// unix): this is supposed to accept lines terminated by
	// "\r\n", but will in fact accept either _because_ 
	// CommandParse splits strings at whitespace which includes
	// both '\r' and '\n'
	while (keepGoing && getline (cin, commandline))
	{
	    CommandParse* cmd = new CommandParse (commandline);
	    vector<string> arglist = cmd->arguments();
	    string response;

	    if (logfile)
	    {
		logfile << "Command :";
		for (vector<string>::const_iterator p=arglist.begin();
		     p != arglist.end();
		     ++p)
		    logfile << ' ' << *p;
		logfile << endl;
	    }

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
		    if (cat.setResultCols (arglist[1]))
			response = "250 Parameter set successfully";
		    else
			response = "550 Error setting columns";
		else
		    response = "501 Wrong number of parameters";
		break;

	      case CommandParse::CONF:
		if (arglist.size() == 1)
		{
		    cout << "210 Configuration file follows, "
			"terminated by <CRLF>.<CRLF>" << crlf;
		    dumpCatalogue (cout);
		    response = ".";
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

	    if (response.length() > 0)
	    {
		cout << response << crlf;

		if (logfile)
		    logfile << "Response: " << response << endl;
	    }
	    // Flush the output, since this program will be used inside a pipe.
	    cout << flush;
	}

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

    exit (0);
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
    cerr << "Usage: " << progname << " [-lfname]" << endl;
    exit (1);
}
