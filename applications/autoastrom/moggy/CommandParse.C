// Part of moggy
// Copyright 2001 Council for the Central Laboratory of the Research Councils.
// See file LICENCE for conditions.
//
// $Id$


#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <iostream>		// for cerr, endl

#if HAVE_STD_NAMESPACE
using std::cerr;
using std::endl;
#endif

#include "CommandParse.h"
#include "util.h"

// Class variables
verbosities CommandParse::verbosity_ = normal;
#if HAVE_MAP
CommandParse::CmdMap CommandParse::command_table_;
bool CommandParse::command_table_init_ = false;
#endif

#if HAVE_MAP
CommandParse::CommandParse (string cmd)
    : cmdcode_(INVALID), args_()
{
    // Exceedingly simple at present -- just tokenise the string, then
    // search in the map for the corresponding code.

    if (! command_table_init_)
    {
	if (verbosity_ > normal)
	    cerr << "CommandParse::CommandParse: initialising" << endl;
	command_table_["AST"] = AST;
	command_table_["CONF"] = CONF;
	command_table_["DEBUG"] = DEBUG;
	command_table_["SEARCH"] = SEARCH;
	command_table_["NAME"] = NAME;
	command_table_["NROW"] = NROW;
	command_table_["COORD1"] = COORD1;
	command_table_["COORD2"] = COORD2;
	command_table_["RADIUS"] = RADIUS;
	command_table_["VERSION"] = VERSION;
	command_table_["STATUS"] = STATUS;
	command_table_["TYPE"] = TYPE;
	command_table_["CATCONFIG"] = CATCONFIG;
	command_table_["COLUMNS"] = COLUMNS;
	command_table_["QUIT"] = QUIT;

	command_table_init_ = true;
    }

    args_ = Util::tokeniseString (cmd);

    if (verbosity_ > normal)
    {
	int i=0;
	cerr << "CommandParse::CommandParse: args_.size()="
	     << args_.size() << ':' << endl;
	for (vector<string>::const_iterator p = args_.begin();
	     p != args_.end(); 
	     p++)
	{
	    cerr << "    " << i << ':' << *p << endl;
	    i++;
	}
    }

    if (args_.size() == 0)
    {
	cmdcode_ = INVALID;
    }
    else
    {
	string op = args_[0];
	Util::uppercaseString(op);

	CmdMap::const_iterator p = command_table_.find(op);

	if (p == command_table_.end())
	    cmdcode_ = INVALID;
	else
	    cmdcode_ = p->second;

	if (verbosity_ > normal)
	    cerr << "CommandParse::CommandParse: " << op << "-->"
		 << static_cast<int>(cmdcode_) << endl;
    }
}



#else  /* HAVE_MAP */

CommandParse::CommandParse (string cmd)
    : cmdcode_(INVALID), args_()
{
    // Exceedingly simple at present -- just tokenise the string, then
    // search through all the legal possibilities for the command,
    // until we find a match.

    args_ = Util::tokeniseString (cmd);

    if (verbosity_ > normal)
    {
	int i=0;
	for (vector<string>::const_iterator p = args_.begin();
	     p != args_.end(); 
	     p++)
	{
	    cerr << "Arg ["<<i<<"]=["<<*p<<"]"<<endl;
	    i++;
	}
	cerr << "args_.size()=" << args_.size() << endl;
    }

    if (args_.size() == 0)
    {
	cmdcode_ = INVALID;
    }
    else
    {
	string op = args_[0];
	Util::uppercaseString(op);

	if (op.compare("AST") == 0) cmdcode_ = AST;
	else if (op.compare("CATCONFIG") == 0) cmdcode_ = CATCONFIG;
	else if (op.compare("COLUMNS") == 0) cmdcode_ = COLUMNS;
	else if (op.compare("CONF") == 0) cmdcode_ = CONF;
	else if (op.compare("COORD1") == 0) cmdcode_ = COORD1;
	else if (op.compare("COORD2") == 0) cmdcode_ = COORD2;
	else if (op.compare("DEBUG") == 0) cmdcode_ = DEBUG;
	else if (op.compare("NAME") == 0) cmdcode_ = NAME;
	else if (op.compare("NROW") == 0) cmdcode_ = NROW;
	else if (op.compare("QUIT") == 0) cmdcode_ = QUIT;
	else if (op.compare("RADIUS") == 0) cmdcode_ = RADIUS;
	else if (op.compare("SEARCH") == 0) cmdcode_ = SEARCH;
	else if (op.compare("STATUS") == 0) cmdcode_ = STATUS;
	else if (op.compare("TYPE") == 0) cmdcode_ = TYPE;
	else if (op.compare("VERSION") == 0) cmdcode_ = VERSION;
	else cmdcode_ = INVALID;
    }
}
#endif  /* HAVE_MAP */

CommandParse::~CommandParse ()
{
}

