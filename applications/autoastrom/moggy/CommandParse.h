// Part of moggy
// Copyright 2001 Council for the Central Laboratory of the Research Councils.
// See file LICENCE for conditions.
//
// $Id$


#ifndef COMMANDPARSE_HEADER_READ
#define COMMANDPARSE_HEADER_READ 1

#include <vector>
#include <string>

#include "moggy.h"
#include "verbosity.h"

class CommandParse {
 public:
    CommandParse (string);
    ~CommandParse ();

    enum token_value {
	INVALID, CONF, SRCH, NROW, SRAD, SBOX, QUIT
    };

    token_value type () const { return cmdcode_; }
    bool argumentsOK () const { return cmdcode_ != INVALID; }
    const vector<string>& arguments () const { return args_; }

    struct BadCommandParse : MoggyException {
	BadCommandParse (string s) { msg = s; };
    };	/* exception class */

 private:
    token_value cmdcode_;
    vector<string> args_;
    verbosities verbosity_;

    void tokenise_string_ (string s) throw (BadCommandParse);
};

#endif /* COMMANDPARSE_HEADER_READ */
