//    This file is part of dvi2bitmap.
//    Copyright 1999--2002, Council for the Central Laboratory of the Research Councils
//    
//    This program is part of the Starlink Software Distribution: see
//    http://www.starlink.ac.uk 
//
//    dvi2bitmap is free software; you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation; either version 2 of the License, or
//    (at your option) any later version.
//
//    dvi2bitmap is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with dvi2bitmap; if not, write to the Free Software
//    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//
//    The General Public License is distributed along with this
//    program in the file LICENCE.
//
//    Author: Norman Gray <norman@astro.gla.ac.uk>
//    $Id$


#include <config.h>

#include <iostream>

#ifdef HAVE_CSTD_INCLUDE
#include <cstdio>
#include <cctype>
#include <cerrno>
#else
#include <stdio.h>
#include <ctype.h>
#include <errno.h>
#endif
#include <unistd.h>		// this is standard according to single-unix, 
				// how POSIXy is that?  How C++?
#include <sys/wait.h>
#include <map>

#ifdef HAVE_STD_NAMESPACE
using std::ostream;
using std::ends;
using std::endl;
using std::cerr;
#define STD std
#else
#define STD
#endif

#include "Util.h"
#include "stringstream.h"

/**
 * Various utility functions.
 */
namespace Util
{
    verbosities verbosity_ = normal;
}


/**
 * Open a pipe with the given command, and return the first line of
 * the command's output.
 * 
 * <p>If the second argument is omitted, then a default list of
 * environment variables, namely <code>PATH HOME LOGNAME SHELL TMPDIR</code>,
 * are inherited from the current environment.  If present, it should
 * be a space-separated list of environment variables with optional
 * values: if no equal sign is present, the variable's value is
 * inherited from the current environment.  For example, if this list
 * is given as in the form <code>env1=val1 env2= env3</code>, then
 * <code>env1</code> would receive the value <code>val1</code>,
 * <code>env2</code> would receive a null value, and <code>env3</code>
 * would be inherited.
 *
 * <p>If the command exits with a non-zero exit status, this is noted
 * on <code>stderr</code>, but any output from the command is still returned.
 *
 * <p>If there's no way of doing this on a particular platform, throw
 * <code>DviError</code> always.
 *
 * @param cmd the command to be run
 *
 * @param envs if specified, this is a list of environment variables;
 * if it is an empty string, <code>""</code> (the default), then a
 * default list of variables is inherited
 *
 * @param statusp a pointer to an integer which will receive the
 * status returned from the command; it may be zero (the default) in
 * which case no status will be returned
 *
 * @return the command's output as a single string, or an empty
 * string if the command command could not be executed, or if it
 * exited on a signal, including <code>abort()</code>.  If it is
 * important to distinguish a command which fails from one
 * which returns an empty string, then the status should be obtained
 * using the third parameter.
 *
 * @throws DviError on any errors
*/
string Util::runCommandPipe (string cmd, string envs, int *statusp)
    throw (DviError)
{
    string rval = "";

#ifdef HAVE_PIPE
    /*
     * Can't use popen, because we want to control the environment
     * more.  Initialisation of kpathsea adds settings to the
     * environment which confuse an invocation of mktexpk.
     *
     * Implementation here follows useful example in
     * <http://www.erack.de/download/pipe-fork.c> 
     */
    int fd[2];

    if (pipe(fd))
	throw new DviError("runCommandPipe: failed to create pipe");

    int childpid = fork();
    if (childpid < 0) {
	throw new DviError("runCommandPipe: failed to fork");
    } else if (childpid == 0) {
	// ...in child.  We will write to the pipe, so immediately
	// close the reading end
	close(fd[0]);
	// And we want our stdout to go down the writing end
	dup2(fd[1], STDOUT_FILENO);
	close(fd[1]);

	char **argv = string_list_to_array(tokenise_string(cmd));

 	string defenvs = "PATH HOME LOGNAME SHELL TMPDIR";
	string_list envlist;
	if (envs == "") {
            envs = "+";         // triggers processing below
	}
        STD::map<string,string> env;
        for (string_list e = tokenise_string(envs);
             ! e.empty();
             e.pop_front()) {
            if (e.front() == "+") {
                e.pop_front(); // the "+"
                string_list de = tokenise_string(defenvs);
                while (! de.empty()) {
                    e.push_front(de.front());
                    de.pop_front();
                }
                if (verbosity_ > normal) {
                    cerr << "Added: now";
                    for (string_list::const_iterator ci = e.begin();
                         ci != e.end();
                         ci++) {
                        cerr << " " << *ci;
                    }
                    cerr << endl;
                }
            } 
            string& s = e.front();
            int si = s.find('=');
            if (si == string::npos) {
                // no setting found
                char *envval = getenv(s.c_str());
                if (envval != 0) {
                    //cerr << "set [" << s << "]=" << envval << endl;
                    env[s] = envval;
                }
            } else {
                // value found
                env[s.substr(0,si)] = s.substr(si+1);
                //cerr << "substr: [" << s.substr(0,si) << "]=" << s.substr(si+1) << endl;
            }
        }
        for (STD::map<string,string>::const_iterator mi = env.begin();
             mi != env.end();
             mi++) {
            string v = mi->first;
            v.append("=");
            v.append(mi->second);
            envlist.push_back(v);
        }

	if (verbosity_ > normal) {
            cerr << "Final envlist:";
            for (string_list::const_iterator si = envlist.begin();
                 si != envlist.end();
                 si++) {
                cerr << "  " << *si << endl;
            }
        }

	char **envp = string_list_to_array(envlist);

	execve(argv[0], argv, envp);

	// This is an error
        if (verbosity_ > normal)
            cerr << "Error executing " << argv[0]
                 << ": " << strerror(errno) << endl;

	exit(EXIT_FAILURE);

    } else {

	// ... in parent.  Close the writing end.
	close(fd[1]);
	
	SSTREAM resp;
	bool collectoutput = true;
	const int BUFLEN = 100;
	char buf[BUFLEN];
	int nread;
	while ((nread = read(fd[0], buf, BUFLEN)) != 0) {
	    if (nread < 0)
		throw DviError("Error reading from pipe");
	    if (collectoutput) {
		for (int i=0; i<nread; i++) {
		    if (buf[i] == '\n' || buf[i] == '\r') { // end of line
			collectoutput = false;
			break;
		    } else {
			resp << buf[i];
		    }
		}
	    }
	    // else just keep reading to the end of the file
	}
	close(fd[0]);

	int status;
	if (verbosity_ > normal)
	    cerr << "Waiting for status from " << childpid << "..." << endl;
	waitpid(childpid, &status, 0);

	if (statusp != 0)
	    *statusp = status;

	string response;
	if (WIFEXITED(status)) {
	    if (verbosity_ > normal)
		cerr << "exit status=" << WEXITSTATUS(status) << endl;
	    if (WEXITSTATUS(status) != 0)
		cerr << "Command <" << cmd << "> exited with non-zero status "
		     << WEXITSTATUS(status) << endl;
	    response = SS_STRING(resp);
	} else if (WIFSIGNALED(status)) {
	    // if the command fails we may come here
	    if (verbosity_ > normal)
		cerr << "Signalled with signal number " << WTERMSIG(status)
		     << (WCOREDUMP(status) ? " (coredump)" : " (no coredump)")
		     << endl;
	    response = "";
	} else if (WIFSTOPPED(status)) {
	    if (verbosity_ >= normal)
		cerr << "Stopped on signal " << WSTOPSIG(status) << endl;
	    response = "";
	} else {
	    DviError("Impossible status from child");
	}

	//return SS_STRING(resp);
        rval = SS_STRING(resp);
    }

#else  /* HAVE_PIPE */
    throw new DviError("Have no pipe() function");
#endif /* HAVE_PIPE */

    return rval;
}


#if 0
/**
 * Open a pipe with the given command, and read from it until EOF.
 * 
 * <p>If there's no way of doing this on a particular platform, throw
 * <code>DviError</code> always.
 *
 * @return the command's output as a single string
 * @throws DviError on any errors
*/
string Util::runCommandPipe (string cmd)
    throw (DviError)
{
    string response;

#ifdef HAVE_POPEN
    const char* envs[] = {
        "PWD",
        "MT_MKTEX_OPT",
        "MT_MKTEXDIR",
        "MT_TEXMFMAIN",
        "PAGER",
        "MT_MKTEXDIR_OPT",
        "TEXDOCVIEW_ps",
        "MT_MKTEXNAM_OPT",
        "TEXDOCVIEW_dvi",
        "KPSE_DOT",
        "TERM_PROGRAM",
        "D",
        "CLASSPATH",
        "MANPATH",
        "MT_MKTEXNAM",
        "VISUAL",
        "KPATHSEA_NAME",
        "LESS",
        "MT_MKTEXUPD",
        "USER",
        "TEXDOCVIEW_pdf",
        "BIBINPUTS",
        "CVS_RSH",
        "OLDPWD",
        "MAKETEX_BASE_DPI",
        "KPATHSEA_DPI",
        "LATEXBASE",
        "TERMCAP",
        "EDITOR",
        "USE_PATH",
        "DISPLAY",
        "Z",
        "SSH_AGENT_PID",
        "LOGNAME",
        "SHLVL",
        "TEXMFCNF",
        "TEXDOCVIEW_order",
        "MAKETEX_MODE",
        "CVSROOT",
        "DYLD_LIBRARY_PATH",
        "MT_VARTEXFONTS",
        "dept",
        "KPATHSEA_FORMAT",
        "SSH_AUTH_SOCK",
        "SSH_ASKPASS",
        "TERM",
        "TERM_PROGRAM_VERSION",
        "__CF_USER_TEXT_ENCODING",
        "TEXDOCVIEW_html",
        "MT_MKTEX_CNF",
	0
    };
    const char **p;
    for (p=envs; *p!=0; p++) {
	cerr << "unsetenv(" << *p << ")" << endl;
	unsetenv(*p);
    }

    FILE *PIN = popen (cmd.c_str(), "r");

    if (PIN == NULL)
	throw DviError ("Can't open pipe");

    SSTREAM resp;

    int ich;
    bool firstline = true;
    while ((ich = fgetc (PIN)) != EOF)
    {
	if (firstline)
	{
	    const char ch = static_cast<char>(ich);
	    if (ch == '\n' || ch == '\r') // end of line
		firstline = false;
	    else
		resp << ch;
	}
	// else just keep reading to the end of the file
    }

    int exitval = pclose(PIN);
    if (exitval == -1)
    {
	// Couldn't exit cleanly
	if (verbosity_ >= normal)
	    cerr << "runCommandPipe: command <" << cmd
		 << "> didn't exit cleanly" << endl;
    }
    else if ((exitval & 0xff) == 0)
	// Get wait4(2) exit status -- low-order 8 bits
	response = SS_STRING(resp);
    else
    {
	if (verbosity_ >= normal)
	    cerr << "runCommandPipe: command <"
		 << cmd << "> apparently failed (" << exitval << ')'
		 << endl;
	response = "";
    }

#else

    // No popen-line function available.  We probably shouldn't have got here.
    throw DviError ("Can't run subcommands on this platform");

#endif /* HAVE_POPEN */

    return response;
}
#endif

/** Tokenise string at whitespace.
 *
 * @param str the string to be tokenised
 * @return a list containing the whitespace-separated tokens in the string
 */
string_list& Util::tokenise_string (string str)
{
    static string_list *l;

    if (verbosity_ > normal)
	cerr << "tokenise_string: string=<" << str << ">" << endl;

    if (l == 0)
	l = new string_list();
    else
	l->clear();

    unsigned int i=0;

    // skip leading whitespace
    while (i < str.length() && isspace(str[i]))
	i++;
    while (i < str.length())
    {
	unsigned int wstart = i;
	while (i < str.length() && !isspace(str[i]))
	    i++;
	string t = str.substr(wstart,i-wstart);
	if (verbosity_ > normal)
	    cerr << "tokenise:" << t << ":" << endl;
	l->push_back(t);
	while (i < str.length() && isspace(str[i]))
	    i++;
    }
    return *l;
}

/**
 * Parse an RGB specification.  This is either a sequence of three integers
 * separated by slashes (or in fact any non-number character), or else
 * a string of the form <code>#RRGGBB</code>.  Set the `rgb' structure to the
 * resulting numbers.  The integers must be in the range [0,255], and
 * may be specified in decimal, octal, or hex.
 *
 * @param rgb the <code>BitmapColour</code> corresponding to the
 * <code>s</code> argument
 * @param s the RGB specification
 *
 * @return true if the parse is successful.
*/
bool Util::parseRGB (Bitmap::BitmapColour& rgb, const char* s)
{
    const char *p = s;
    unsigned long val;

    while (*p != '\0' && isspace(*p))
	p++;
    
    if (*p == '#') {
	char buf[3];
	buf[2] = '\0';
	p++;
	strncpy(buf, p, 2);
	val = strtoul(buf, 0, 16);
	if (val > 255)		// can't happen, with two hex digits
	    return false;
	rgb.red = static_cast<Byte>(val);
	p += 2;
	
	strncpy(buf, p, 2);
	val = strtoul(buf, 0, 16);
	if (val > 255) return false;
	rgb.green = static_cast<Byte>(val);
	p += 2;
	
	strncpy(buf, p, 2);
	val = strtoul(buf, 0, 16);
	if (val > 255) return false;
	rgb.blue = static_cast<Byte>(val);
    } else {
	val = strtoul (s, const_cast<char**>(&p), 0);
	if (val > 255) return false;
	rgb.red = static_cast<Byte>(val);
	if (p == s)			// no digit
	    return false;
	if (*p == '\0')		// end of string
	    return false;
	s = p;
	while (!isxdigit(*s))
	{
	    if (*s == '\0') return false;
	    s++;
	}

	val = strtoul (s, const_cast<char**>(&p), 0);
	if (val > 255) return false;
	rgb.green = static_cast<Byte>(val);
	if (p == s)			// no digit
	    return false;
	if (*p == '\0')		// end of string
	    return false;
	s = p;
	while (!isxdigit(*s))
	{
	    if (*s == '\0') return false;
	    s++;
	}

	val = strtoul (s, const_cast<char**>(&p), 0);
	if (val > 255) return false;
	rgb.blue = static_cast<Byte>(val);
	if (p == s)			// no digit
	    return false;
    }

    return true;
}

/**
 * Convert a <code>string_list</code> to a null-terminated array of
 * character pointers.  The resulting array can conveniently be
 * deleted using {@link #delete_string_array}.
 *
 * @param l a string_list
 * @return a pointer to a null-terminated array of null-terminated
 * character arrays
 */
char** Util::string_list_to_array(string_list& l)
{
    int argc = l.size();
    char **sl = new char*[argc+1];
    int i = 0;
    for (string_list::const_iterator ci = l.begin();
	 ci != l.end();
	 ci++) {
	string s = *ci;
	sl[i] = new char[s.size()+1];
	strcpy(sl[i], s.c_str());
	i++;
    }
    sl[i] = 0;
    return sl;
}

/**
 * Deletes the array of strings returned by {@link #string_list_to_array}.
 * 
 * @param sl a null-terminated array of strings
 */
void Util::delete_string_array(char** sl)
{
    for (int i=0; sl[i] != 0; i++)
	delete[] sl[i];
    delete[] sl;
}

/**
 * Sets the verbosity of the methods in this class
 *
 * @param level how verbose the class's methods should be
 */
void Util::verbosity (const verbosities level)
{
    verbosity_ = level;

    return;
}
