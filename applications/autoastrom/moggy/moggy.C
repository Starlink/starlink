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

#include <cat/CatalogInfo.h>

#include "moggy.h"
#include "CommandParse.h"
#include "CatalogueHandler.h"
#include "util.h"

const char *progname;

void dumpCatalogue (ostream& o);

int main (int argc, char **argv)
{
    string commandline;
    string crlf = "\r\n";
    bool keepGoing = true;
    CatalogueHandler cat;

    progname = argv[0];

    // Initialisation is very simple -- delete the CATLIB_CONFIG
    // environment variable.  This program is supposed to be run as a
    // slave, so any configuration done ought to be explicit, using
    // the CONF command.
    unsetenv ("CATLIB_CONFIG");

    try
    {
	while (keepGoing && getline (cin, commandline))
	{
	    CommandParse* cmd = new CommandParse (commandline);
	    vector<string> arglist = cmd->arguments();

	    switch (cmd->type())
	    {
	      case CommandParse::INVALID:
		cout << "500 Unknown command" << crlf;
		break;

	      case CommandParse::CATCONFIG:
		if (arglist.size() == 2)
		    if (cat.setConfig (arglist[1]))
			cout << "250 Parameter set successfully" << crlf;
		    else
			cout << "550 Error setting catlib" << crlf;
		else
		    cout << "501 Wrong number of parameters" << crlf;
		break;

	      case CommandParse::COLUMNS:
		if (arglist.size() == 2)
		    if (cat.setResultCols (arglist[1]))
			cout << "250 Parameter set successfully" << crlf;
		    else
			cout << "550 Error setting colums to " << arglist[1]
			     << crlf;
		else
		    cout << "501 Wrong number of parameters" << crlf;
		break;

	      case CommandParse::CONF:
		if (arglist.size() == 1)
		{
		    cout << "210 Configuration file follows, "
			"terminated by <CRLF>.<CRLF>" << crlf;
		    dumpCatalogue (cout);
		    cout << "." << crlf;
		}
		else
		    cout << "501 Unexpected parameter" << crlf;
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
			    cout << "250 Parameter set successfully" << crlf;
			else
			    cout << "501 Error setting coordinates" << crlf;
		    else
			cout << "501 Error parsing parameters" << crlf;
		}
		else
		    cout << "501 Wrong number of parameters" << crlf;

		break;

	      case CommandParse::NAME:
		if (arglist.size() == 2)
		    if (cat.setCatname (arglist[1]))
			cout << "250 Parameter set successfully" << crlf;
		    else
			cout << "552 Unknown catalogue " << arglist[1] << crlf;
		else
		    cout << "501 Wrong number or type of parameters" << crlf;
		break;

	      case CommandParse::NROW:
		if (arglist.size() == 2)
		{
		    int nrow;
		    if (Util::stringToInteger(arglist[1], nrow))
			if (cat.setNrows (nrow))
			    cout << "250 Parameter set successfully" << crlf;
			else
			    cout << "501 Can't set nrows to " << nrow << crlf;
		    else
			cout << "501 Can't convert " << arglist[1]
			     << " to integer" << crlf;
		}
		else
		    cout << "501 Wrong number or type of parameters" << crlf;
		break;

	      case CommandParse::RADIUS:
		if (arglist.size() == 2)
		{
		    double rad;
		    if (Util::stringToDouble(arglist[1], rad))
			if (cat.setRadius (rad))
			    cout << "250 Parameter set successfully" << crlf;
			else
			    cout << "501 Can't set radius to " << rad << crlf;
		    else
			cout << "501 Can't convert " << arglist[1]
			     << " to double" << crlf;
		}
		else
		    cout << "501 Wrong number or type of parameters" << crlf;
		break;

	      case CommandParse::SEARCH:
		if (arglist.size() == 1)
		{
		    int nrows = cat.doSearch();
		    if (nrows >= 0)
		    {
			vector<string> names = cat.getColnames();
			if (names.size() == 0)
			    cout << "550 Can't obtain column names" << crlf;
			else
			{
			    cout << "210 Catalogue follows" << crlf;
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
			}
		    }
		    else
			cout << "503 Out of order: "
			    "not all required information has been supplied"
			     << crlf;
		}
		else
		    cout << "501 Wrong number or type of parameters" << crlf;
		break;

	      case CommandParse::STATUS:
		if (arglist.size() == 1)
		{
		    cout << "210 Status follows, terminated by <CRLF>.<CRLF>"
			 << crlf;
		    cat.printStatus (cout, crlf);
		    cout << '.' << crlf;
		}
		else
		    cout << "501 Syntax error" << crlf;
		break;

	      case CommandParse::TYPE:
		if (arglist.size() == 2)
		    if (cat.setSearchtype(arglist[1]))
			cout << "250 Parameter set successfully" << crlf;
		    else
			cout << "502 Unrecognised search type" << crlf;
		else
		    cout << "501 Wrong number or type of parameters" << crlf;
		break;

	      case CommandParse::VERSION:
		cout << "250 " << RCSID << crlf;
		break;

	      case CommandParse::QUIT:
		cout << "220 Goodbye" << crlf;
		keepGoing = false;
		break;

	      default:
		cout << "502 Command not implemented" << crlf;
		break;
	    }
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
