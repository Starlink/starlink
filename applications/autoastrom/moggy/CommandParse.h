/* Part of moggy
 * Copyright 2001 Council for the Central Laboratory of the Research Councils.
 * See file LICENCE for conditions.
 *
 * $Id$
 */


#ifndef COMMANDPARSE_H_LOADED
#define COMMANDPARSE_H_LOADED 1

#include "config.h"

#include <vector>
#include <string>

#include "moggy.h"
#include "verbosity.h"

#if HAVE_MAP
#include <map>
#endif

#if HAVE_STD_NAMESPACE
using std::string;
using std::vector;
using std::map;
#endif

class CommandParse {
 public:
    CommandParse (string);
    ~CommandParse ();

    enum token_value {
	INVALID,
	AST, CONF, DEBUG, SEARCH, NAME, NROW, COORD1, COORD2, RADIUS, VERSION,
	STATUS, TYPE, CATCONFIG, COLUMNS, QUIT
    };

    token_value type () const { return cmdcode_; }
    bool commandOK () const { return cmdcode_ != INVALID; }
    const vector<string>& arguments () const { return args_; }

    struct BadCommandParse : MoggyException {
	BadCommandParse (string s) { msg = s; };
    };	/* exception class */

    static void verbosity (const verbosities level) { verbosity_ = level; }

 private:
    token_value cmdcode_;
    vector<string> args_;
    static verbosities verbosity_;
#if HAVE_MAP
    typedef map<string,token_value> CmdMap;
    static CmdMap command_table_;
    static bool command_table_init_;
#endif
};

#endif /* COMMANDPARSE_H_LOADED */
