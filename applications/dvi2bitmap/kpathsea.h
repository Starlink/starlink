// Part of dvi2bitmap
// Copyright 1999, 2000 Council for the Central Laboratory of the Research Councils.
//
// This is merely an interface to the kpathsea library
#include "verbosity.h"

class kpathsea {
 public:
    static void init (const char *name, const int basedpi);
    static void verbosity (const verbosities level);
    static const char *find (const char *font, int resolution);

 private:
    static bool initialised_;
    static verbosities verbosity_;
};
