// Part of moggy
// Copyright 2001 Council for the Central Laboratory of the Research Councils.
// See file LICENCE for conditions.
//
// $Id$


#ifdef HAVE_CONFIG_H
#include "config.h"
#endif


#if HAVE_CSTD_INCLUDE
#include <cstring>
#else
#include <string.h>
#endif
#include <new>

#include "CommandParse.h"



CommandParse::CommandParse (string cmd)
    : cmdcode_(INVALID), args_(), verbosity_(normal)
{
    // Exceedingly simple at present -- just tokenise the string, then
    // search through all the legal possibilities for the command,
    // until we find a match.

    tokenise_string_ (cmd);

#if 0
    int i=0;
    for (vector<string>::const_iterator p = args_.begin();
	 p != args_.end(); 
	 p++)
    {
	cerr << "Arg ["<<i<<"] "<<*p<<endl;
	i++;
    }
#endif

    if (args_.size() == 0)
    {
	cmdcode_ = INVALID;
    }
    else
    {
	string op = args_[0];

	if (op.compare("CONF") == 0) cmdcode_ = CONF;
	else if (op.compare("SRCH") == 0) cmdcode_ = SRCH;
	else if (op.compare("NROW") == 0) cmdcode_ = NROW;
	else if (op.compare("SRAD") == 0) cmdcode_ = SRAD;
	else if (op.compare("SBOX") == 0) cmdcode_ = SBOX;
	else if (op.compare("QUIT") == 0) cmdcode_ = QUIT;
	else cmdcode_ = INVALID;
    }
}

CommandParse::~CommandParse ()
{
}

// Tokenise string at whitespace.
// There's a more C++-ish way of doing this, I'm sure....
// Result put into args_
void CommandParse::tokenise_string_ (string str)
    throw (BadCommandParse)
{
    if (verbosity_ > normal)
	cerr << "tokenise_string: string=<" << str << ">\n";

    // Copy the string to a modifiable place
    try {
	char* strbuf = new char[str.length()+1];
	memcpy (strbuf, str.c_str(), str.length()+1);

	// Use strtok, even though the man page says not to.  Since
	// we've copied the string, and know what the delimiter is, we 
	// don't care about its deficiencies.
	char *word;
	word = strtok (strbuf, " \t");

	if (word == 0)		// No first word
	    return;		// Probably an error, but not really
				// one of tokenisation, so let the
				// caller work it out.

	args_.push_back (word);
	for (word = strtok (0, " \t");
	     word != 0;
	     word = strtok (0, " \t"))
	{
	    args_.push_back (word);
	}

	delete[] strbuf;
    } catch (bad_alloc) {
	throw BadCommandParse ("Insufficient memory");
    }

    return;
}

