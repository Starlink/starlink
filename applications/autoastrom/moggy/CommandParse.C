// Part of moggy
// Copyright 2001 Council for the Central Laboratory of the Research Councils.
// See file LICENCE for conditions.
//
// $Id$


#ifdef HAVE_CONFIG_H
#include "config.h"
#endif


#include <new>

#include "CommandParse.h"
#include "util.h"



CommandParse::CommandParse (string cmd)
    : cmdcode_(INVALID), args_(), verbosity_(normal)
{
    // Exceedingly simple at present -- just tokenise the string, then
    // search through all the legal possibilities for the command,
    // until we find a match.

    args_ = Util::tokeniseString (cmd);

#if 0
    int i=0;
    for (vector<string>::const_iterator p = args_.begin();
	 p != args_.end(); 
	 p++)
    {
	cerr << "Arg ["<<i<<"]=["<<*p<<"]"<<endl;
	i++;
    }
    cerr << "args_.size()=" << args_.size() << endl;
#endif

    if (args_.size() == 0)
    {
	cmdcode_ = INVALID;
    }
    else
    {
	string op = args_[0];
	Util::uppercaseString(op);

	if (op.compare("CONF") == 0) cmdcode_ = CONF;
	else if (op.compare("SEARCH") == 0) cmdcode_ = SEARCH;
	else if (op.compare("NAME") == 0) cmdcode_ = NAME;
	else if (op.compare("NROW") == 0) cmdcode_ = NROW;
	else if (op.compare("COORD1") == 0) cmdcode_ = COORD1;
	else if (op.compare("COORD2") == 0) cmdcode_ = COORD2;
	else if (op.compare("RADIUS") == 0) cmdcode_ = RADIUS;
	else if (op.compare("VERSION") == 0) cmdcode_ = VERSION;
	else if (op.compare("STATUS") == 0) cmdcode_ = STATUS;
	else if (op.compare("TYPE") == 0) cmdcode_ = TYPE;
	else if (op.compare("CATCONFIG") == 0) cmdcode_ = CATCONFIG;
	else if (op.compare("COLUMNS") == 0) cmdcode_ = COLUMNS;
	else if (op.compare("QUIT") == 0) cmdcode_ = QUIT;
	else cmdcode_ = INVALID;
    }
}

CommandParse::~CommandParse ()
{
}

