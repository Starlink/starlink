// Part of moggy
// Copyright 2001 Council for the Central Laboratory of the Research Councils.
// See file LICENCE for conditions.
//
// $Id$

#ifndef MOGGY_H_LOADED
#define MOGGY_H_LOADED 1

#include <string>

struct MoggyException {
    string msg;
    MoggyException () { }
    MoggyException (string s) { msg = s; }
};

#endif /* MOGGY_H_LOADED */
