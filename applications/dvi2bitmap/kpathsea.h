// Part of dvi2bitmap
//
// This is merely an interface to the kpathsea library

class kpathsea {
 public:
    //static bool init (const char *argv0, const char *progname);
    static void setProgramName (const char *name)
	{ program_name_ = name; }
    static void verbosity (int);
    static const char *find (const char *font, int resolution);

 private:
    static bool initialised_;
    static int initRes_;
    static int verbosity_;
    static const char *program_name_;
};
