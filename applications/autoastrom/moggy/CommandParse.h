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

class CommandParse {
 public:
    CommandParse (string);
    ~CommandParse ();

    enum token_value {
	INVALID,
	CONF, SEARCH, NAME, NROW, COORD1, COORD2, RADIUS, VERSION,
	STATUS, TYPE, CATCONFIG, COLUMNS, QUIT
    };

    token_value type () const { return cmdcode_; }
    bool commandOK () const { return cmdcode_ != INVALID; }
    const vector<string>& arguments () const { return args_; }

    struct BadCommandParse : MoggyException {
	BadCommandParse (string s) { msg = s; };
    };	/* exception class */

 private:
    token_value cmdcode_;
    vector<string> args_;
    verbosities verbosity_;

};

#endif /* COMMANDPARSE_H_LOADED */
