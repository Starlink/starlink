/*
 * In order to find init.tcl during initialization, the following script
 * is invoked by Tcladam_Init().  It looks in several different directories:
 *
 *      $startcl_library        - can specify a primary location, if set
 *                                no other locations will be checked
 *
 *      $env(STARTCL_LIBRARY)   - highest priority so user can always override
 *                                the search path unless the application has
 *                                specified an exact directory above
 *
 *      <executable directory>/../lib/startcl
 *                              - look for a lib/startcl in a sibling of
 *                                the bin directory (e.g. /usr/local)
 *
 * The first directory on this path that contains a valid init.tcl script
 * will be set ast the value of startcl_library.
 */
static char initScript[] = \
"if {[info proc startclInit]==\"\"} {  \n\
  proc startclInit {} {                \n\
    global startcl_library             \n\
    rename startclInit {}              \n\
    tcl_findLibrary startcl \"\" \"\" init.tcl STARTCL_LIBRARY startcl_library \n\
  }                                    \n\
}                                      \n\
startclInit";

