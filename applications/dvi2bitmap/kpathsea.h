// Part of dvi2bitmap
//
// This is merely an interface to the kpathsea library

class kpathsea {
 public:
    static bool init (const char *argv0, const char *progname);
    static void verbosity (int);
    static const char *find (const char *font, int resolution);

 private:
    static bool initialised_;
    static int initRes_;
    static int verbosity_;
};
