/*
 *  This file is part of moggy.
 *
 *  Copyright 2001, Council for the Central Laboratory of the Research Councils
 *
 *  This program is part of the Starlink Software Distribution: see
 *  http://www.starlink.ac.uk
 *
 *  moggy is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  moggy is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with moggy; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 *
 *  The General Public License is distributed along with this
 *  program in the file LICENCE.
 *
 *  Author: Norman Gray <norman@astro.gla.ac.uk>
 *  $Id$
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

#if STD_IN_STD_NAMESPACE
using std::string;
using std::vector;
#define STD std
#else
#define STD
#endif

class CommandParse {
 public:
    CommandParse (string);
    ~CommandParse ();

    enum token_value {
	INVALID,
	AST, CONF, DEBUG, SEARCH, NAME, NROW, COORD1, COORD2, RADIUS,
        MOGGYVERSION, STATUS, TYPE, CATCONFIG, COLUMNS, QUIT
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
    typedef STD::map<string,token_value> CmdMap;
    static CmdMap command_table_;
    static bool command_table_init_;
#endif
};

#endif /* COMMANDPARSE_H_LOADED */
