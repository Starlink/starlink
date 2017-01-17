import os
import stat
import sys
import re
import abc
import subprocess
import tempfile
import shutil
import glob
import inspect
import time
import datetime
import uuid

#  Provide recall and editing facilities for parameter prompts
import readline
readline.parse_and_bind('tab: complete')




#  To run the app under the pdb debugger and break at a given line,
#  copy the following line to the place where the break point is
#  required, and uncomment.
# import pdb; pdb.set_trace()




#  -------------------  misc ---------------------------

#  original adam directory
__adam_user = None

#  Function to return the name of the executing script, without any
#  trailing ".py" suffix.
__cmd = None
def cmd():
   global __cmd
   if __cmd == None:
      __cmd = os.path.basename(sys.argv[0])
   if __cmd[-3:] == ".py":
      __cmd = __cmd[:-3]
   return __cmd


#  "Protected" function to return the name of the executing script,
#  followed by a colon and a space.
def _cmd_token():
   return "{0}: ".format(cmd())


def which(program):
   """

   This command mimics the UNix 'which' command. It searches the PATH for
   a named commadn and returns the full path to the executable if found, and
   None otherwise.

   Invocation:
      which(command)

   Arguments:
      command = string
         The commmand to find

   Returned Value:
      The full path to the executable, or None of the executanble is not
      found.

   """

   def is_exe(fpath):
      return os.path.isfile(fpath) and os.access(fpath, os.X_OK)

   fpath, fname = os.path.split(program)
   if fpath:
      if is_exe(program):
         return program
   else:
      for path in os.environ["PATH"].split(os.pathsep):
          path = path.strip('"')
          exe_file = os.path.join(path, program)
          if is_exe(exe_file):
             return exe_file

   return None


#  Removes non-printable chars from ASCII String
def remove_junk(str):
   return ''.join([ch for ch in str if ord(ch) > 31 and ord(ch) < 126 or ord(ch) == 9])


#  -------------------  Logging ---------------------------

#  Constants defining increasing levels of information to display or log
NOTSET = -1
NONE = 0
CRITICAL = 1
PROGRESS = 2
ATASK = 3
DEBUG = 4

#  Level of information to display on the screen, and to log. These are
#  set to NONE here to prevent any information being displayed or logged
#  by default prior to the creation of the ParSys object.
ilevel = NOTSET
glevel = NOTSET

#  The requested log file.
logfile = None

#  File descriptor and name for the currently open log file (if any)
__logfd = None
__logfile = None

#  Funtion to ensure a log file is open
def __open_log_file():
   global logfile
   global __logfd
   global __logfile

   # If a log file is currently open, close it if the user has assigned a
   # different value to the module "logfile" variable.
   if __logfd != None:
      if logfile != __logfile:
         print( "Closing log file {0}".format(__logfile) )
         __logfd.close()
         __logfd = None
         __logfile == None

   # If no log file is currently open, open one now.
   if __logfd == None:
      if logfile == None:
         logfile = "{0}.log".format(cmd())
      print( "Logging to file {0} ".format(logfile) )

      # Rename any existing file by appending a unique version number to
      # the end of it.
      version = 0
      mvto = logfile
      while os.path.isfile(mvto):
         version += 1
         mvto = "{0}.{1}".format(logfile,version)

      if version > 0 :
         os.rename( logfile, mvto )
         print( "(existing file {0} moved to {1})\n".format(logfile,mvto) )

      __logfd = open( logfile, "w" )
      __logfile = logfile
      __logfd.write("{0} log file created at {1}\n".format(cmd(),datetime.datetime.now()))


#  Funtion to remove the current working directory from the front of a
#  file path.
def _rmcwd(path):
   cwd = os.getcwd()
   if path.find(cwd) == 0:
      return path[len(cwd)+1:]
   elif path.find("./") == 0:
      return path[2:]
   else:
      return path



def msg_out(text,level=PROGRESS):
   """

   Display a string on the screen and store in the starutil log file,
   subject to the current screen and log file information levels
   specified by the module variables "ilevel" and "glevel". The name of
   the log file is specified by the module variable "logfile".If
   "logfile" is unset, it will default to "<script>.log" if the name of
   the executing script is known, or to "starutil.log" otherwise.

   Invocation:
      msg_out(text,level=PROGRESS)

   Arguments:
      text = string
         The text to display.
      level = integer
         The message will be displayed on the screen if "level" is less
         than or equal to the current value of "starutil.ilevel". It
         will be written to the log file (if a log file is open) if
         "level" is less than or equal to the current value of
         "starutil.glevel".

   """

   global ilevel
   global glevel
   global __logfd
   if isinstance( text, bytes ):
      text = text.decode("ascii","ignore")
   if level == DEBUG:
      text = "debug> {0}".format(text)

   if level <= ilevel:
      print(text)

   if level <= glevel:
      __open_log_file()
      __logfd.write(text)


def _getLevel( level ):
   if level == "NONE":
      return NONE
   elif level == "CRITICAL":
      return CRITICAL
   elif level == "PROGRESS":
      return PROGRESS
   elif level == "ATASK":
      return ATASK
   elif level == "DEBUG":
      return DEBUG
   else:
      raise UsageError("{0}Illegal information level '{1}' supplied.".format(_cmd_token(),level))

def _findLevel( level, default  ):
   if level == NOTSET:
      return default
   elif level == NONE:
      return "NONE"
   elif level == CRITICAL:
      return "CRITICAL"
   elif level == PROGRESS:
      return "PROGRESS"
   elif level == ATASK:
      return "ATASK"
   elif level == DEBUG:
      return "DEBUG"
   else:
      raise UsageError("{0}Illegal information level '{1}' supplied.".format(_cmd_token(),level))

#  -------------------  Using ATASKS ---------------------------

def ndfExists( ndf ):
   """

   Test if an NDF exists.

   Invocation:
      value = ndfExists(ndf)

   Arguments:
      ndf = string
         The full specificiation for the NDF to test.

   Returned Value:
      True if hte NDF exists, False otherwise.

   """

   result = True
   try:
      invoke("$KAPPA_DIR/ndftrace ndf=\"{0}\" quiet".format(ndf))
   except AtaskError:
      result = False
   return result


def invoke(command,aslist=False,buffer=None,annul=False):
   """

   Invoke an ADAM atask. An AtaskError is raised if the command fails.
   The standard output from the command is returned as the function value.
   It may also be written to the screen and to the log file, depending
   on the value of the "ilevel" and "glevel" module variables.

   Note - this command should only be used to invoke ADAM tasks. It is
   not re-entrant. That is, you should not use it to invoke a command
   that may possible then use "invoke" itself (i.e. do not use it to
   invoke other python scripts, or ADAM tasks that result in other
   python scripts being run).

   Invocation:
      value = invoke(command,aslist=False,buffer=None,annul=False)

   Arguments:
      command = string
         The full command including directory and arguments. The
         shell_quote function defined in this module can be used to ensure
         that any string containing shell metacharacters is properly quoted.
      aslist = boolean
         If true, then standard output from the command is returned as a
         list of lines. If aslist is False, the standard output is
         returned as a single string with new lines separated by embedded
         newline characters.
      buffer = boolean
         If true, then standard output from the command is written to a
         disk file as the atask runs, and only read back into Python when
         the atask completes. Thus if the screen imformation level
         ("ILEVEL") is set to ATASK or above, the atask output will only
         be shown to the user when the atask has completed. If "buffer" is
         False, then the atask standard output is fed back to Python as
         the atask creates it, and will appear on the screen in real-time.
         NOTE - "buffer" should always be set to True if the atask is a
         graphics application that may cause a GWM window to be created.
         This is because any new gwmXrefresh process created by the atask
         will never terminate and so will cause this script to freeze
         whilst it waits for the gwmXrefresh process to end. Writing
         standard output to a disk file seems to prevent this for some unknown
         reason. If "buffer" is not specified, the default is always to
         buffer unless the screen information level ("ILEVEL") is set to
         ATASK or higher.
      annul = boolean
         If False, then an AtaskError exception is raised if the command
         fails for any reason. If True, then no exception is raised.

   Returned Value:
      A single string, or a list of strings, holding the standard output
      from the command (see "aslist").

   Example
      out = invoke("$KAPPA_DIR/stats ndf=$KAPPA_DIR/m31 clip=\[3,3,3\]")
         kappa:stats is run.

   """

   global __logfd
   global ATASK
   global glevel
   global ilevel

   #  Prevent atasks from prompting for a new value if a bad parameter
   #  value is supplied.
   os.environ["ADAM_NOPROMPT"] = "1"

   #  Ensure atasks set the shell status variable to indicate if the
   #  atask succeeded or failed.
   os.environ["ADAM_EXIT"] = "1"

   # Ensure that the MERS library, which atasks use to write text to
   # standard output, does not split long lines. Without this (for
   # instance) NDF names reported by KAPPA:NDFECHO can be mangled.
   os.environ["MSG_SZOUT"] = "0"

   msg_out( "\n>>> {0}\n".format(command), ATASK )

   if buffer == None:
      buffer = ( ilevel < ATASK )

   #  The original scheme used subprocess.check_output to invoke the
   #  atask. But the process hung for ever if the invoked command
   #  involved the creation of a gwm graphics window. I presume this is
   #  because subprocess.chech_call was waiting for the gwmXrefresh
   #  process (created by the atask) to die, which it never did unless
   #  the GWM window was closed. So instead we do the following, which
   #  involves writing standard output from the atask to a disk file, and
   #  then reading the disk file back into the script. Any attempt to
   #  use the stdout argument of subprocess.popen() caused the thing to
   #  freeze again. StringIO would not work either as subprocess requires
   #  the stdout file object to "have a real file number".

   if buffer:
      stdout_file = "starutil-{0}".format(uuid.uuid4())
      p = subprocess.Popen("{0} > {1} 2>&1".format(command,stdout_file), shell=True)
      status = p.wait()

      if os.path.exists( stdout_file ):
         fd = open(stdout_file,"r")
         outtxt = fd.read().strip()
         fd.close()
         os.remove(stdout_file)
         msg_out( outtxt, ATASK )
      else:
         outtxt = ""

      if status != 0:
         if not annul:
            raise AtaskError("\n\n{0}".format(outtxt))
      elif aslist:
         outtxt = outtxt.split("\n")

   # Non-buffering scheme - Does someone knows how to fix the GWM
   # freezing issue without resorting to the above buffering scheme~
   else:

      if aslist:
         outtxt = []
      else:
         outtxt = ""

      proc = subprocess.Popen(command,shell=True, stdout=subprocess.PIPE,
                              stderr=subprocess.STDOUT)
      while True:

         line = proc.stdout.readline()
         while line != None and len(line) > 0:
            if isinstance( line, bytes ):
               line = line.decode("ascii","ignore")
            line = line.rstrip()
            msg_out( line, ATASK )
            if aslist:
               outtxt.append(line)
            else:
               outtxt = "{0}\n{1}".format(outtxt,line)
            line = proc.stdout.readline()

         status = proc.poll()
         if status != None:
            break

         time.sleep(1.0)

      if status != 0 and not annul:
         if outtxt:
            if aslist:
               msg = ""
               for line in outtxt:
                  msg = "{0}\n{1}".format(msg,line)
            else:
               msg = outtxt
            raise AtaskError("\n\n{0}".format(msg))
         else:
            raise AtaskError()

   msg_out( "\n", ATASK )
   return outtxt

def get_adam_user():
   global __adam_user
   if not __adam_user:
      if "ADAM_USER" in os.environ:
         __adam_user = os.environ["ADAM_USER"]
      else:
         __adam_user = os.path.join(os.environ["HOME"],"adam")
   return __adam_user

def get_fits_header( ndf, keyword, report=False ):
   try:
      there = invoke("$KAPPA_DIR/fitsmod ndf={0} edit=exist keyword={1}".format( ndf, keyword ), False ).strip()
      if there == "TRUE":
         value = invoke("$KAPPA_DIR/fitsmod ndf={0} edit=print keyword={1}".format( ndf, keyword ), False )
      else:
         value = None

   except AtaskError:
      value = None

   if report:
      if value == "":
         raise NoValueError("{0} contains a null value for FITS header'{1}'.".format(ndf,keyword))
      elif value == None:
         raise NoValueError("{0} contains no value for FITS header'{1}'.".format(ndf,keyword))

   return value



def get_task_par( parname, taskname, **kwargs ):
   """

   Get the current value of an ATASK parameter.

   Invocation:
      value = get_task_par( parname, taskname, default=??? )

   Arguments:
      parname = string
         The name of the task parameter. This may refer to a single value
         in a vector valued parameter by appending the one-based index in
         parentheses to the end of the parameter name.
      taskname = string
         The name of the task.
      default = string
         A default value to return if the specified parameter cannot be
         accessed (e.g. if the parameter file does not exist, or does not
         contain the required parameter). If "default" is not supplied,
         an exception will be raised if the parameter cannot be accessed.

   Returned Value:
      The parameter value. This will be a single value if the task parameter
      is a scalar value, and a list if the task parameter is a vector.

   """

   cmd = "$KAPPA_DIR/parget parname={0} applic={1} vector=yes".format( shell_quote(parname),taskname)

   if 'default' in kwargs:
      try:
         text = invoke( cmd, False )
         text = text.replace('\n', '' )
         text = text.replace('TRUE', 'True' )
         text = text.replace('FALSE', 'False' )
         result = eval( text )
      except AtaskError:
         result = kwargs['default']
   else:
      text = invoke( cmd, False )
      text = text.replace('\n', '' )
      text = text.replace('TRUE', 'True' )
      text = text.replace('FALSE', 'False' )
      result = eval( text )

   return result


def put_task_par( parname, taskname, parvalue, partype ):
   """

   Stores the supplied value for an ATASK parameter.

   Invocation:
      put_task_par( parname, taskname, parvalue, partype )

   Arguments:
      parname = string
         The name of the task parameter. Must be a scalar value.
      taskname = string
         The name of the task.
      parvalue = ?
         The value to store.
      partype = string
         The HDS type of the parameter.

   """

   parfile = os.path.join( get_adam_user(), "{0}.sdf".format(taskname))

   if not os.path.isfile(parfile):
      invoke( "$HDSTOOLS_DIR/hcreate {0} struc".format(parfile) )
      exists = False
   else:
      try:
         invoke( "$HDSTOOLS_DIR/hget {0}.{1} typ".format(parfile,parname) )
         exists = True
      except AtaskError:
         exists = False

   if not exists:
      if partype:
         invoke( "$HDSTOOLS_DIR/hcreate {0}.{1} {2}".format(parfile,parname,partype) )
      else:
         raise UsageError("No HDS type supplied for new parameter {0}.{1}".
                           format(taskname,parname))

   if partype == "_LOGICAL":
      if parvalue:
         hdsval = "TRUE"
      else:
         hdsval = "FALSE"
   else:
      hdsval = "{0}".format(parvalue)

   invoke( "$HDSTOOLS_DIR/hmodify {0}.{1} {2}".format(parfile,parname,hdsval))





def shell_quote(text,remove=False):
   """

   Put single quotes around a string (and escape any embedded single
   quotes) so that it can be used as a command line argument when
   running an ATASK from the shell. Does nothing if the string is already
   enclosed in single quotes.

   Invocation:
      value = shell_quote(text)

   Arguments:
      command = string
         The full command including directory and arguments. The
      remove = boolean
         If True, remove any existing shell quotes rather than
         adding them.

   Returned Value:
      The quoted string.

   """

   if text != None:

      if remove:
         match = re.search( "^\s*\'(.*)\'\s*$",text)
         if match:
            return match.group(1)
         else:
            return text

      else:
         if not re.search( "^\s*\'(.*)\'\s*$",text):
            return "'" + text.replace("'", "'\\''") + "'"
         else:
            return text
   else:
      return None




#  -------------------  Parameter System ---------------------------

class ParSys(object):
   """

   Encapsulates all parameters used by a Python script.

   Each parameter is described by an instance of a subclass of the base
   "Parameter" class defined within this module. Different subclasses
   describe parameters of different types, and also encapsulates the
   parameter name, prompt string, default value, current value, limits,
   etc., all of which can be specified when the Parameter object is
   created, or set subsequently by assigning values to the appropriate
   properties of the Parameter object. These Parameter objects must be
   created explicitly prior to creating the ParSys object.

   A ParSys can be used as a dictionary in which the keys are the
   parameter names, and the values are the Parameter objects, as
   in:

   scalar = parsys_instance["scalar"].value
   parsys_instance["NDF"].default = Parameter.UNSET
   etc

   When a new ParSys object is created, any values supplied on the
   command line of the executing script are stored as the initial values
   for the corresponding parameters. However, such values are only
   validated when the calling script attempts to get the parameter
   value. An UnknownParameterError is raised if the command line
   includes values for which there are no corresponding Parameters in
   the ParSys. If "--help" or "-h" is found on the command line, the
   docstring from the top-level executing script is displayed. If no
   docstring is available, a string is displayed holding the script
   name followed by the names of the script parameters with any
   associated defaults, in their expected order.

   Creation of a new ParSys object also causes the creation of a new,
   empty, ADAM directory in the user's home directory. This ADAM
   directory can be deleted using function ParSys.cleanup(). It also
   sets the NDF_AUTO_HISTORY and AUTOPROV environment variables so that
   newly created NDFs will contain history and provenance information.

   Three parameters controlling the level of information to display and
   log are created automatically when the ParSys constructor is called.
   These parameters are:

      MSG_FILTER = _LITERAL (Read)
         Controls the default level of information reported by Starlink
         atasks invoked within the executing script. This default can be
         over-ridden by including a value for the msg_filter parameter
         within the command string passed to the "invoke" function. The
         accepted values are the list defined in SUN/104 ("None", "Quiet",
         "Normal", "Verbose", etc). ["Normal"]

      ILEVEL = _LITERAL (Read)
         Controls the level of information displayed on the screen by the
         script. It can take any of the following values (note, these values
         are purposefully different to the SUN/104 values to avoid confusion
         in their effects):

         "NONE" - No screen output is created

         "CRITICAL" - Only critical messages are displayed such as warnings.

         "PROGRESS" - Extra messages indicating script progress are also
            displayed.

         "ATASK" - Extra messages are also displayed describing each atask
            invocation. Lines starting with ">>>" indicate the command name
            and parameter values, and subsequent lines hold the screen output
            generated by the command. Also, the value of all script
            parameters are displayed at this level.

         "DEBUG" - Extra messages are also displayed containing unspecified
            debugging information.

         In adition, the glevel value can be changed by assigning a new
         integer value (one of starutil.NONE, starutil.CRITICAL,
         starutil.PROGRESS, starutil.ATASK or starutil.DEBUG) to the module
         variable starutil.glevel. The default value is the value of
         "starutil.ilevel" when the ParSys object is created, which is
         starutil.PROGRESS unless the caller changes it before creating
         the ParSys. []

      GLEVEL = _LITERAL (Read)
         Controls the level of information to write to a text log file.
	 Allowed values are as for "ILEVEL". The log file to create is
	 specified via parameter "LOGFILE. In adition, the glevel value
	 can be changed by assigning a new integer value (one of
	 starutil.NONE, starutil.CRITICAL, starutil.PROGRESS,
	 starutil.ATASK or starutil.DEBUG) to the module variable
	 starutil.glevel. The default value is the value of
         "starutil.glevel" when the ParSys object is created, which is
         starutil.ATASK unless the caller changes it before creating
         the ParSys. []

      LOGFILE = _LITERAL (Read)
         The name of the log file to create if GLEVEL is not NONE. The
	 default is "<command>.log", where <command> is the name of the
	 executing script (minus any trailing ".py" suffix), and will be
	 created in the current directory. Any file with the same name is
	 over-written. The script can change the logfile if necessary by
	 assign the new log file path to the module variable
	 "starutil.logfile". Any old log file will be closed befopre the
	 new one is opened. []

   To Do:
      - Add equivalents to the standard starlink "accept" and "prompt"
      keywords.
      - Add system for specifying logical parameters as "param" or
      "noparam"
      - Add a system for remembering current values?

   Constructor:
      parsys_instance = ParSys( params )
         params = list
            A list of starutil.Parameter objects. These define all parameters
            used by the Python application. The order in which they are
            stored in the list define their expected order on the command
            line when specified by position rather than keyword.

   Properties:
      cmd = string
         The name of the executing python script.
      cmdline = string
         A reconstruction of the command line that invoked the executing
         script.
      params = dict
         A dictionary holding all the supplied Parameters, indexed by
         parameter name.
      usage = string
         A string that summarises the usage of the running script. It
         contains the script name followed by the supplied parameter
         kewords with default values, if any, in the supplied order.

   Class Methods:
      ParSys.cleanup():
         Deletes the temporary ADAM directory created when the ParSys was
         constructed.

   """

   # The full path to the temporary ADAM directory.
   adamdir = None

   def __init__(self,params):
      global ilevel
      global glevel
      global logfile

      #  Ensure starlink has been initialised (i.e. the etc/login and
      #  etc/cshrc files have been sourced). Do this by checking that
      #  KAPPA_DIR is defined.
      if "KAPPA_DIR" not in os.environ:
         raise UsageError("Starlink has not been initialised. You should "
                          "source the starlink login and cshrc/profile "
                          "files before running this script. ")

      #  Store a dictionary holding references to all the supplied Parameters,
      #  indexed by parameter name. Raise an exception if the list
      #  contains msg_filter, ilevel or glevel.
      self.params = {}
      for p in params:
         if p.name in self.params:
            raise UsageError("{0}The list of starutil parameters includes "
                             "more than one occurrence of parameter name {1}.".
                             format(_cmd_token(),p.name))

         self.params[p.name] = p
         p._parsys = self
         if p.name == "MSG_FILTER" or p.name == "ILEVEL" or p.name == "GLEVEL":
            raise UsageError("{0}The list of starutil parameters includes the reserved parameter name {1} (programming error).".format(_cmd_token(),p.name))

      #  Create the extra message handling parameters, and append them to the
      #  end of the list.
      levs = [ "NONE", "QUIET", "NORMAL", "VERBOSE", "DEBUG" ]
      for i in range(1,21):
         levs.append("DEBUG{0}".format(i))

      p = ParChoice("MSG_FILTER", levs, "ATASK Message level", "NORMAL",
                    noprompt=True)
      params.append(p)
      self.params[p.name] = p

      levs = [ "NONE", "CRITICAL", "PROGRESS", "ATASK", "DEBUG" ]

      p = ParChoice("ILEVEL", levs, "Level of info. to display on screen",
                    _findLevel( ilevel, "PROGRESS"), noprompt=True)
      params.append(p)
      self.params[p.name] = p

      p = ParChoice("GLEVEL", levs, "Level of info. to store in log file",
                    _findLevel( glevel, "ATASK"), noprompt=True)
      params.append(p)
      self.params[p.name] = p

      p = Par0S("LOGFILE", "The log file to receive diagnostic messages",
                "{0}.log".format(cmd()), noprompt=True)
      params.append(p)
      self.params[p.name] = p

      #  Store a "usage" string.
      self.usage = "Usage: {0} ".format(cmd())
      for p in params:
         if p.noprompt:
            self.usage += "[({0}=){1}] ".format(p.name.lower(),p.default)
         else:
            self.usage += "({0}=)? ".format(p.name.lower())

      #  Look for "name=value" items on the command line and store them in
      #  a dictionary ("byName"). Store all other command line items in an
      #  ordered list ("byPosition"). Also build a copy of the command line.
      self.cmdline = "{0} ".format(cmd())
      byName = {}
      byPosition = []
      for item in sys.argv[1:]:

         #  Remove any non-printing characters, and trailing or leading
         # white space.
         item = remove_junk( item ).strip()

         #  Skip this item if it is now empty.
         if item == "":
            continue

         #  If "-h" or "--help" is encountered at any point, display the
         #  docstring from the top-level script, if available. Otherwise,
         #  display a simple list of the command name and parameters.
         #  Then exit the script.
         if item == "--help" or item == "-h":
            try:
               frm = inspect.stack()[ -1 ][0]
               text = inspect.getargvalues(frm)[3]["__doc__"]
            except:
               text = None;
            if not text:
               text = self.usage
            print(text)
            sys.exit(0)

         self.cmdline += "{0} ".format(item)
         m = re.search( '^(\w+)=(.*)$', item )
         if m:
            name = m.group(1).upper()
            value = m.group(2)
         else:
            name = None
            value = item

         if len(value) > 0:
            if value[0] == "\"" or value[0] == "'":
               if value[0] == value[-1]:
                  value = value[1:-1]

            if name == None:
               byPosition.append(value)
            else:
               byName[name] = value

      #  Loop round all legal parameter names, maintaining the zero-based
      #  index of the current parameter..
      pos = -1
      for p in params:
         pos += 1
         name = p.name

         #  If a value was supplied for the parameter on the command line,
         #  store it as the initial value for the Parameter and remove it
         #  from the "byName" dict.
         if name in byName:
            p._setValue( byName.pop(name) )

         #  Otherwise use the next positional value (if any).
         elif len(byPosition) > 0:
            p._setValue( byPosition.pop(0) )

      #  If the byName dict or the byPosition list is not empty, (i.e. values
      #  for unknown parameters were supplied on the command line), raise an
      #  exception.
      if len(byName) > 0:
         name = list(byName.keys())[0]
         text = "\n\n{0}: Unknown parameter '{1}' specified on the command line.".format(cmd(),name)

      elif len(byPosition) > 0:
         text = "\n\n{0}: Too many parameter values supplied on command line.".format(cmd())
      else:
         text = None

      if text != None:
         text += "\n{0}\n\n{1}\n".format(self.cmdline,self.usage)
         raise UnknownParameterError(text)

      #  Set the default MSG_FILTER value for all atasks by setting the
      #  MSG_FILTER environment variable.
      msg_filter = self.params["MSG_FILTER"].value
      if msg_filter != None:
         os.environ["MSG_FILTER"] = msg_filter

      #  Set the logfile name.
      logfile = self.params["LOGFILE"].value

      #  Set the module variables holding the screen and log file
      #  information levels.
      ilevel = _getLevel(self.params["ILEVEL"].value)
      glevel = _getLevel(self.params["GLEVEL"].value)

      #  Ensure the original ADAM directory is recorded
      get_adam_user()

      #  Create a new ADAM directory in the NDG temp directory.
      if ParSys.adamdir == None:
         ParSys.adamdir = tempfile.mkdtemp( prefix="adam_", suffix="_py",
                                            dir=NDG._gettmpdir() )
      msg_out( "Setting ADAM_USER to {0}\n".format(ParSys.adamdir), ATASK )
      os.environ["ADAM_USER"] = ParSys.adamdir

      # Ensure that KAPPA is not in verbose mode and does not report NDF
      # names as these features cause extra text to appear in standard output
      # from things like "fitsmod" and so upsets get_fits_header.
      if "KAPPA_REPORT_NAMES" in os.environ:
         del os.environ["KAPPA_REPORT_NAMES"]
      if "KAPPA_VERBOSE" in os.environ:
         del os.environ["KAPPA_VERBOSE"]

      # Ensure new NDFs contain history and provenance.
      os.environ["NDF_AUTO_HISTORY"] = "1"
      os.environ["AUTOPROV"] = "1"

   # Delete the temporary ADAM directory.
   @classmethod
   def cleanup(cls):
      if ParSys.adamdir != None:
         shutil.rmtree( ParSys.adamdir )

#  Allow the ParSys to be indexed by parameter name (returns the
#  Parameter object as the value).
   def __len__(self):
      return len(self.params)
   def __getitem__(self, key):
      return self.params[key.upper()]
   def __setitem__(self, key, value):
      raise UsageError("{0}Attempt to set a read-only item in a ParSys object (programming error).".format(_cmd_token()))
   def __delitem__(self, key):
      raise UsageError("{0}Attempt to delete a read-only item in a ParSys object (programming error).".format(_cmd_token()))
   def __iter__(self):
      self.__inext = -1
      return self
   def __next__(self):
      return self.next()
   def next(self):
      self.__inext += 1
      parnames = self.params.keys()
      if self.__inext < len( parnames ):
         return self.params[parnames[ self.__inext ]]
      else:
         raise StopIteration


#  A method to deliver an error message to the user without affecting
#  control flow
   def _error(self,msg):
      print("{0}\n".format(msg))












class Unset(object):
   ''' A singleton object used to represent unset parameter values or defaults '''
   def __repr__(self):
      return "<unset>"





class Parameter(object):
   __metaclass__ = abc.ABCMeta
   '''

   An abstract base class describing a generic parameter.

   Subclasses of Parameter provide facilities for storing and validating
   parameters of different types. The Parameter class itself should never
   be instantiated. All parameters are initially unset when created.

   Class Constants:
      Parameter.UNSET = object
         A value that can be assigned to the "value" or "default" property
         of a Parameter to indicate that the parameter has no value or
         default. Note, the Python "None" object is considered to be a
         plausable value for the "value" or "default" property.

   Properties:
      name = string
         The parameter name. Always upper case.
      prompt = string
         The prompt string.
      default = object
         The default value. This property may be set to Parameter.UNSET
         to indicate that the parameter has no default value, and can be
         compared with Parameter.UNSET to test if a default has been set
         for the parameter.
      value = object
         The current value of the parameter. This property may be set to
         Parameter.UNSET to indicate that the parameter has no value.
         This clears the parameter so that a new value will be ontained
         when the value property is next accessed. An InvalidValueError
         is raised if an invalid value is assigned to the "value" property.

         When the "value" property is read, the returned value is
         determined as follows:

         I) If the parameter already has a set value, it is returned.

         II) If the parameter was assigned a valid value on the command line,
         the value is returned.

         III) If the parameter allows prompting (see property "noprompt"),
         a valid value is obtained from the user and returned.

         IV) If the user supplies a null (!) value, or if prompting is
         not allowed, then the default value (if set) is returned.

         V) If no default has been set, a NoValueError is raised and
         Parameter.UNSET is returned.

         The string value obtained for the parameter may then be converted
         to some other data type, depending on the actions of the particular
         subclass of Parameter being used. The "raw" property can be used
         to retrieve the uninterpreted string value.
      noprompt = boolean
         If False, then the user may be prompted to obtain a parameter
         value. The full prompt string contains the parameter name, the
         "prompt" property, and the default value (if set). If the user
         enters an abort (!!) then an AbortError is raised. If the user
         enters a null (!) then the default value is accepted, if set (if
         no default has been set a NoValueError is raised). If an empty
         string is supplied, then the default is accepted, if set (if no
         default has been set then the user is re-prompted). If a single
         or double question mark is supplied, the "help" property is
         displayed, and the user is re-prompted. If an invalid value
         is supplied, an error message is displayed, and the user is
         re-prompted.
      help = string
         The help string. May be "None".
      raw = string (read-only)
         The raw string value of the parameter as obtained from the user
         before any interpretation or conversion to other data types.
         This will be None if no value has yet been obtained for the
         parameter, or if a default of None was accepted by the user.
   '''




   #  __unset is the private value used within the Parameter class
   #  to indicate an unset value or default. It is never changed. Use
   #  an instance of a custom class to ensure there is no chance of a
   #  user-supplied value being equal to __unset.
   __unset = Unset()

   #  UNSET is the public value used to indicate an unset value or
   #  default. It may conceivably be changed by the caller.
   UNSET = __unset




   #  Define the class initialiser
   @abc.abstractmethod
   def __init__(self, name, prompt=None, default=UNSET,
                noprompt=False, help=None ):

      # Initial values for all fields.
      self.__name = name.strip().upper()
      self.__prompt = None
      self.__default = Parameter.__unset
      self.__noprompt = False
      self.__help = None
      self.__value = Parameter.__unset
      self.__validated = False
      self.__raw = None
      self._parsys = None

      #  Use "protected" setter methods to set the supplied field values.
      self._setPrompt( prompt )
      self._setDefault( default )
      self._setNoPrompt( noprompt )
      self._setHelp( help )




   #  Define "protected" accessor methods for all fields
   def _getName(self):  # Read-only
      return self.__name

   def _getRaw(self):  # Read-only
      return self.__raw

   def _getPrompt(self):
      return self.__prompt
   def _setPrompt(self,val):
      self.__prompt = "{0}".format(val) if val else None

   def _getDefault(self):
      return self.__default if self._testDefault() else Parameter.UNSET
   def _setDefault(self,val):
      if val != Parameter.UNSET:
         self.__default = val
      else:
         self.__default = Parameter.__unset
   def _clearDefault(self):
      self.__default = Parameter.__unset
   def _testDefault(self):
      return self.__default != Parameter.__unset

   def _getNoPrompt(self):
      return self.__noprompt
   def _setNoPrompt(self,val):
      self.__noprompt = True if val else False

   def _getHelp(self):
      return self.__help
   def _setHelp(self,val):
      self.__help = "{0}".format(val) if val else None

   def _getValue(self):
      return self.__value if self._testValue() else Parameter.UNSET
   def _setValue(self,val):
      self.__validated = False
      if val != Parameter.UNSET:
         self.__value = val
      else:
         self.__value = Parameter.__unset
   def _clearValue(self):
      self.__validated = False
      self.__value = Parameter.__unset
   def _testValue(self):
      return self.__value != Parameter.__unset

   # Define a function to cancel a parameter value, so that the next
   # access to the "value" property forces a prompt (or the default to be
   # used if prompting is not allowed)
   def cancel(self):
      self._clearValue()

   # Define a function to get the public value of the parameter,
   # prompting the user if required.
   def _getParameterValue(self):
      name = self._getName()
      value = self._getValue()
      default = self._getDefault()

      while not self.__validated:
         defaultUsed = False
         validate = True

         if not self._testValue():
            if not self.__noprompt:
               value = self.__promptUser()
            else:
               value = "!"
            self.__raw = value

         if value == "!" :
            if self._testDefault():
               value = default
               defaultUsed = True
            else:
               raise NoValueError("\n{0}No value obtained for parameter '{1}'.".format(_cmd_token(),name))

         elif value == "!!" :
            raise AbortError("\n{0}Aborted prompt for parameter '{1}'.".format(_cmd_token(),name))

         elif value == "?" or value == "??":
            help = self._getHelp()
            if help:
               print(help)
            else:
               text = "No help available for parameter '{0}'.".format(name)
               self.__error(text)
            validate = False

         if validate:
            try:
               self._setValue( value )
               self.__validate()
               msg_out( "Parameter {0} is set to {1}\n".format(name,self.__value), ATASK )

            except InvalidParameterError as err:
               self.__error(err)
               self.__raw = None
               self.__noprompt = False

            if defaultUsed:
               self._clearDefault()

      return self._getValue()




   # Define a function to set the public value of the parameter, raising
   # an InvalidParameterError if the value is not valid.
   def _setParameterValue(self,value):
      self._setValue( value )
      self.__validate()




   #  Define public properties for all public fields
   name = property(_getName, None, None, "The parameter name")
   raw = property(_getRaw, None, None, "The raw string value of the parameter")
   prompt = property(_getPrompt, _setPrompt, None, "The user prompt string")
   default = property(_getDefault, _setDefault, None, "The default value")
   noprompt = property(_getNoPrompt, _setNoPrompt, None, "Do not prompt the user?")
   help = property(_getHelp, _setHelp, None, "The parameter help string")
   value = property(_getParameterValue, _setParameterValue, None, "The parameter value")





   #  "Protected" method to check if the current parameter value is a
   #  valid value for the parameter, raising an InvalidParameterError if
   #  not. It may modify the stored value if necessary (e.g. changing its
   #  type) as part of the check.
   @abc.abstractmethod
   def _isValid(self):
      pass




   #  "Protected" method to validate the current value of a Parameter,
   #  clearing the value and raising an InvalidParameterError if the
   #  value is not valid.
   def __validate(self):
      if not self.__validated:

         if not self._testValue():
            raise InvalidParameterError("\n{0}Unset value not valid for parameter '{1}'.".format(_cmd_token(),self._getName()))

         elif self._getValue() == None and self._getDefault() == None:
            self.__validated = True

         else:
            try:
               self._isValid()
               self.__validated = True
            except InvalidParameterError:
               self._clearValue()
               raise




   #  Private function to display an error to the user without raising an
   #  exception. Delegate it to the enclosing parameter system if possible.
   #  Do a simple print otherwise.
   def __error(self,msg):
      if self._parsys:
         self._parsys._error(msg)
      else:
         print("{0}\n".format(msg))




   #  Prompt the user for a value
   def __promptUser(self):

      if "STARUTIL_NOPROMPT" in os.environ:
         raise NoValueError("\nNo value obtained for parameter '{0}' - "
                            "prompting is disallowed.".format(self.__name))

      name = self._getName()
      pmt = "{0} ".format(name)

      prompt = self._getPrompt()
      if prompt:
         pmt += "- "+prompt+" "

      default = self._getDefault()
      if default == None:
         pmt += "/!/ "
      elif default != Parameter.UNSET:
         pmt += "/{0}/ ".format(default)

      pmt += "> "

      while True:

         if sys.hexversion > 0x03000000:
            result= input(pmt).strip()
         else:
            result = raw_input(pmt).strip()

         if result == "" or result.isspace():
            if default != Parameter.UNSET:
               result = default
               break

         else:
            break

      return result




   # Provide implementations for some other special functions
   def __str__(self):
      val = self._getValue()
      if val != Parameter.UNSET:
         return "{0} = {1}".format(self._getName(),val)
      else:
         return "{0} = <unset>".format(self._getName())




class Par0S(Parameter):
   '''

   Describes a scalar string-valued parameter. The parameter is initially
   in an unset state.

   Constructor:
      param = Par0S( name, prompt=None, default=Parameter.UNSET,
                     noprompt=False, help=None  )
         name = string
            The parameter name. The supplied string is converted to upper case.
         prompt = string
            The prompt string.
         default = string
            The initial default value.
         noprompt = boolean
            If True, then the user will not be prompted for a parameter value
            if none was supplied on the command line. Instead, the default
            value will be used if set (a NoValueError will be raised otherwise).
         help = string
            The help string

   Properties:
      This class has no extra properties over and above those of
      the Parameter class:

   Methods:
      This class has no extra methods over and above those of
      the Parameter class:

   '''

   def __init__(self, name, prompt=None, default=Parameter.UNSET,
                noprompt=False, help=None ):
      Parameter.__init__(self, name, prompt, default, noprompt, help )

   def _isValid(self):
      value = Parameter._getValue(self)
      Parameter._setValue(self,"{0}".format(value))





class Par0F(Parameter):
   '''

   Describes a scalar floating-point parameter. The parameter is initially
   in an unset state.

   Constructor:
      param = Par0F( name, prompt=None, default=Parameter.UNSET,
                     noprompt=False, help=None, maxval=None, minval=None )
         name = string
            The parameter name. The supplied string is converted to upper case.
         prompt = string
            The prompt string.
         default = float
            The initial default value.
         noprompt = boolean
            If True, then the user will not be prompted for a parameter value
            if none was supplied on the command line. Instead, the default
            value will be used if set (a NoValueError will be raised otherwise).
         help = string
            The help string
         maxval = float
            The maximum acceptable value for the parameter.
         minval = float
            The minimum acceptable value for the parameter.

   Properties:
      This class defines the following properties in addition to those of
      the Parameter class:

      maxval = float
         The maximum acceptable value for the parameter. If "None", no
         upper limit is imposed. If maxval is less than minval, then the
         range between them is a disallowed range, rather than an allowed
         range.
      minval = float
         The minimum acceptable value for the parameter. If "None", no
         lower limit is imposed.

   Methods:
      This class has no extra methods over and above those of
      the Parameter class:

   '''

   def __init__(self, name, prompt=None, default=Parameter.UNSET,
                noprompt=False, help=None, maxval=None, minval=None ):
      Parameter.__init__(self, name, prompt, default, noprompt, help )
      self.__maxval = None
      self.__minval = None
      self._setMaxVal(maxval)
      self._setMinVal(minval)

   def _getMaxVal(self):
      return self.__maxval
   def _setMaxVal(self,val):
      self.__maxval = float(val) if  val != None else None
   def _getMinVal(self):
      return self.__minval
   def _setMinVal(self,val):
      self.__minval = float(val) if  val != None else None

   minval = property(_getMinVal, _setMinVal, None, "The minimum parameter value")
   maxval = property(_getMaxVal, _setMaxVal, None, "The maximum parameter value")

   def _isValid(self):
      val = Parameter._getValue(self)

      try:
         val = float(val)
         minv = self._getMinVal()
         maxv = self._getMaxVal()

         big = (val > maxv) if maxv != None else False
         small = (val < minv) if minv != None else False

         if (maxv != None) and (minv != None):
            if (maxv > minv) and (big or small):
                  raise InvalidParameterError("\n{0}Illegal value ('{1}') obtained for parameter '{2}'.\nIt must be between {3} and {4}.".format(_cmd_token(),val,self._getName(),minv,maxv))
            elif (maxv <= minv) and (big and small):
                  raise InvalidParameterError("\n{0}Illegal value ('{1}') obtained for parameter '{2}'.\nIt must not be between {3} and {4}.".format(_cmd_token(),val,self._getName(),maxv,minv))

         elif big:
            raise InvalidParameterError("\n{0}Illegal value ('{1}') obtained for parameter '{2}'.\nIt must be no more than {3}.".format(_cmd_token(),val,self._getName(),maxv))

         elif small:
            raise InvalidParameterError("\n{0}Illegal value ('{1}') obtained for parameter '{2}'.\nIt must be no less than {3}.".format(_cmd_token(),val,self._getName(),minv))

         Parameter._setValue(self,val)

      except TypeError:
         raise InvalidParameterError("\n{0}Illegal value ('{1}') obtained for parameter '{2}'.\nIt must be numerical value.".format(_cmd_token(),val,self.name))






class Par0I(Parameter):
   '''

   Describes a scalar integer parameter. The parameter is initially
   in an unset state.

   Constructor:
      param = Par0I( name, prompt=None, default=Parameter.UNSET,
                     noprompt=False, help=None, maxval=None, minval=None )
         name = string
            The parameter name. The supplied string is converted to upper case.
         prompt = string
            The prompt string.
         default = float
            The initial default value.
         noprompt = boolean
            If True, then the user will not be prompted for a parameter value
            if none was supplied on the command line. Instead, the default
            value will be used if set (a NoValueError will be raised otherwise).
         help = string
            The help string
         maxval = int
            The maximum acceptable value for the parameter.
         minval = int
            The minimum acceptable value for the parameter.

   Properties:
      This class defines the following properties in addition to those of
      the Parameter class:

      maxval = int
         The maximum acceptable value for the parameter. If "None", no
         upper limit is imposed. If maxval is less than minval, then the
         range between them is a disallowed range, rather than an allowed
         range.
      minval = int
         The minimum acceptable value for the parameter. If "None", no
         lower limit is imposed.

   Methods:
      This class has no extra methods over and above those of
      the Parameter class:

   '''

   def __init__(self, name, prompt=None, default=Parameter.UNSET,
                noprompt=False, help=None, maxval=None, minval=None ):
      Parameter.__init__(self, name, prompt, default, noprompt, help )
      self.__maxval = None
      self.__minval = None
      self._setMaxVal(maxval)
      self._setMinVal(minval)

   def _getMaxVal(self):
      return self.__maxval
   def _setMaxVal(self,val):
      self.__maxval = int(round(float(val))) if  val != None else None
   def _getMinVal(self):
      return self.__minval
   def _setMinVal(self,val):
      self.__minval = int(round(float(val))) if  val != None else None

   minval = property(_getMinVal, _setMinVal, None, "The minimum parameter value")
   maxval = property(_getMaxVal, _setMaxVal, None, "The maximum parameter value")

   def _isValid(self):
      val = Parameter._getValue(self)

      try:
         val = int(val)
         minv = self._getMinVal()
         maxv = self._getMaxVal()

         big = (val > maxv) if maxv != None else False
         small = (val < minv) if minv != None else False

         if (maxv != None) and (minv != None):
            if (maxv > minv) and (big or small):
                  raise InvalidParameterError("\n{0}Illegal value ('{1}') obtained for parameter '{2}'.\nIt must be between {3} and {4}.".format(_cmd_token(),val,self._getName(),minv,maxv))
            elif (maxv <= minv) and (big and small):
                  raise InvalidParameterError("\n{0}Illegal value ('{1}') obtained for parameter '{2}'.\nIt must not be between {3} and {4}.".format(_cmd_token(),val,self._getName(),maxv,minv))

         elif big:
            raise InvalidParameterError("\n{0}Illegal value ('{1}') obtained for parameter '{2}'.\nIt must be no more than {3}.".format(_cmd_token(),val,self._getName(),maxv))

         elif small:
            raise InvalidParameterError("\n{0}Illegal value ('{1}') obtained for parameter '{2}'.\nIt must be no less than {3}.".format(_cmd_token(),val,self._getName(),minv))

         Parameter._setValue(self,val)

      except TypeError:
         raise InvalidParameterError("\n{0}Illegal value ('{1}') obtained for parameter '{2}'.\nIt must be an integer value.".format(_cmd_token(),val,self.name))






class Par0L(Parameter):
   '''

   Describes a scalar logical/boolean parameter. The parameter is initially
   in an unset state. Acceptable values are "0", "1", "Y", "N", "T", "F",
   "YES", "NO", "TRUE", "FALSE" (case insensive).

   Constructor:
      param = Par0L( name, prompt=None, default=Parameter.UNSET,
                     noprompt=False, help=None )
         name = string
            The parameter name. The supplied string is converted to upper case.
         prompt = string
            The prompt string.
         default = float
            The initial default value.
         noprompt = boolean
            If True, then the user will not be prompted for a parameter value
            if none was supplied on the command line. Instead, the default
            value will be used if set (a NoValueError will be raised otherwise).
         help = string
            The help string

   Properties:
      This class has no extra properties over and above those of
      the Parameter class:

   Methods:
      This class has no extra methods over and above those of
      the Parameter class:

   '''

   def __init__(self, name, prompt=None, default=Parameter.UNSET,
                noprompt=False, help=None, maxval=None, minval=None ):
      Parameter.__init__(self, name, prompt, default, noprompt, help )

   def _isValid(self):
      val = "{0}".format(Parameter._getValue(self)).strip().upper()
      if val == "0" or val == "F" or val == "FALSE" or val == "N" or val == "NO":
         val = False
      elif val == "1" or val == "T" or val == "TRUE" or val == "Y" or val == "YES":
         val = True
      else:
         raise InvalidParameterError("\n{0}Illegal value ('{1}') obtained for parameter '{2}'.\nIt must be one of 0,1,n,y,no,yes,t,f,true,false.".format(_cmd_token(),val,self._getName()))
      Parameter._setValue(self,val)





class ParChoice(Parameter):
   """

   Describes a string parameter that is restricted to one of several
   allowed values. The parameter is initially in an unset state.

   Constructor:
      param = ParChoice( name, choices, prompt=None, default=Parameter.UNSET,
                         noprompt=False, help=None )
         name = string
            The parameter name. The supplied string is converted to upper case.
         choices = list<string>
            A list of strings giving the allowed values of the parameter.
            They are converted to upper case, and white space is removed
            from both ends of each option. Matching is case insensitive.
         prompt = string
            The prompt string.
         default = string
            The initial default value.
         noprompt = boolean
            If True, then the user will not be prompted for a parameter value
            if none was supplied on the command line. Instead, the default
            value will be used if set (a NoValueError will be raised otherwise).
         help = string
            The help string

   Properties:
      This class defines the following properties in addition to those of
      the Parameter class:

      choices = list<string>
         A list of strings giving the allowed values of the parameter.
         They are converted to upper case, and white space is removed
         from both ends of each option. Matching is case insensitive.

   Methods:
      This class has no extra methods over and above those of
      the Parameter class:

   """


   def __init__(self, name, choices, prompt=None, default=Parameter.UNSET,
                noprompt=False, help=None ):
      Parameter.__init__(self, name, prompt, default, noprompt, help )
      self.__choices = None
      self._setChoices(choices)

   def _getChoices(self):
      return self.__choices
   def _setChoices(self,val):
      if val == None:
         self.__choices = None
      else:
         self.__choices = []
         for opt in val:
            opt = "{0}".format(opt)
            self.__choices.append(opt.strip().upper())

   choices = property(_getChoices, _setChoices, None, "The allowed parameter values")

   def _isValid(self):
      val = "{0}".format(Parameter._getValue(self)).strip()
      uval = val.upper()

      nmatch = 0
      choices = self._getChoices()

      for opt in choices:
         if opt == uval:
            newval = opt
            nmatch = 1
            break

         if opt.find(uval) == 0:
            newval = opt
            nmatch += 1

      if nmatch == 0 or nmatch > 1:
         text = "\n{0}".format(_cmd_token())
         if nmatch == 0:
            text += "Illegal "
         else:
            text += "Ambiguous "
         text += "value ('{0}') obtained for parameter '{1}'.\nIt must be one of '".format(val,self._getName())
         first = True
         for opt in choices:
            if first:
               text += "{0}".format(opt)
               first = False
            else:
               text += ",{0}".format(opt)
         text += "'"
         raise InvalidParameterError(text)

      Parameter._setValue(self,newval)





class ParNDG(Parameter):
   '''

   Describes a string parameter that specifies one or more existing NDFs
   using the syntax of an NDG group expression. The parameter is initially
   in an unset state. The "value" property of the parameter holds an
   instance of the starutil.NDG class.

   Constructor:
      param = ParNDG( name, prompt=None, default=Parameter.UNSET,
                      noprompt=False, help=None, exists=True, maxsize=None,
                      minsize=1 )
         name = string
            The parameter name. The supplied string is converted to upper case.
         prompt = string
            The prompt string.
         default = string
            The initial default value.
         noprompt = boolean
            If True, then the user will not be prompted for a parameter value
            if none was supplied on the command line. Instead, the default
            value will be used if set (a NoValueError will be raised otherwise).
         help = string
            The help string
         exists = boolean
            If True, the supplied NDFs must all exist. If False, no check is
            made on whether they exist or not.
         maxsize = int
            The largest number of NDFs allowed in the group. Unlimited if
            maxsize=None.
         minsize = int
            The smallest number of NDFs allowed in the group. May be zero.

   Properties:
      This class defines the following properties in addition to those of
      the Parameter class:

      exists = boolean
         If True, the supplied NDFs must all exist. If False, no check is
         made on whether they exist or not.
      maxsize = int
         The largest number of NDFs allowed in the group. Unlimited if
         maxsize=None.
      minsize = int
         The smallest number of NDFs allowed in the group. May be zero.

   Methods:
      This class has no extra methods over and above those of
      the Parameter class:

   '''

   def __init__(self, name, prompt=None, default=Parameter.UNSET,
                noprompt=False, help=None, exists=True, maxsize=None,
                minsize=1 ):
      Parameter.__init__(self, name, prompt, default, noprompt, help )
      self.__exists = None
      self.__maxsize = None
      self.__minsize = None
      self._setExists(exists)
      self._setMaxSize(maxsize)
      self._setMinSize(minsize)

   def _getExists(self):
      return self.__exists
   def _setExists(self,exists):
      if exists:
         self.__exists = True
      else:
         self.__exists = False

   def _getMaxSize(self):
      return self.__maxsize
   def _setMaxSize(self,size):
      if size == None:
         self.__maxsize = None
      else:
         size = int(size)
         if size < 1 :
            size = 1
         self.__maxsize = size

   def _getMinSize(self):
      return self.__minsize
   def _setMinSize(self,size):
      if size == None:
         self.__minsize = 1
      else:
         size = int(size)
         if size < 0 :
            size = 0
         self.__minsize = size

   minsize = property(_getMinSize, _setMinSize, None, "The minimum number of NDFs")
   maxsize = property(_getMaxSize, _setMaxSize, None, "The maximum number of NDFs")
   exists = property(_getExists, _setExists, None, "Must the supplied NDFs exist?")

   def _isValid(self):
      val = Parameter._getValue(self)
      if val == None:
         size = 0
      else:
         try:
            val = NDG("{0}".format(val).strip(),self.exists)
            size = len(val)
         except AtaskError:
            raise InvalidParameterError( "\n{0}Cannot access a group of existing NDFs using parameter '{1}' ('{2}').".format(_cmd_token(),Parameter._getName(self),val) )

      if size < self.minsize:
         raise InvalidParameterError( "\n{0}Too few NDFs ({1}) given for parameter '{2}' - at least {3} must be supplied.".format(_cmd_token(),size,Parameter._getName(self), self._getMinSize()) )
      elif self.maxsize != None and size > self.maxsize:
         raise InvalidParameterError( "\n{0}Too many NDFs ({1}) given for parameter '{2}' - no more than {3} can be supplied.".format(_cmd_token(),size,Parameter._getName(self), self._getMaxSize()) )
      Parameter._setValue(self,val)









#  -------------------  Specifying groups of NDFs ---------------------------

class NDG(object):
   '''

   Encapsulates a immutable group of NDF names.

   Each NDG object that represents more than one NDF has a corresponding
   text file on disk containing the explicit paths to the NDFs in the
   group. These text files are stored in a temporary directory with path
   specified by class variable NDG.tempdir. This directory is created (if
   it does not already exist) when the first NDG object is created, and
   is deleted (together with any files in it) when the class method
   NDG.cleanup() is called. Any temporary NDFs specified by this class
   are also placed in this directory. By default, the temporary directory
   is created within "/tmp" but this can be changed by setting the STAR_TEMP
   environment variable to something other than "/tmp".

   When used as an iterable, the NDG class behaves liks a Python list
   in which each element is a string specifying an individual NDF.

   The string representation of an NDG object is a group expression which
   can be passed to a starlink ATASK.

   Constructors:

      ndg_instance = NDG( gexp, exists=True )
         gexp = string
            An NDG group expression.
         exists = boolean
            If True, the new NDG represents a group of existing NDFs, and
            a NoNdfError will be raised if any of the NDF specified by
            "gexp" do not exist or cannot be accessed. If False, the new
            NDG represents a group of new NDFs - they do not need to exist.

      ndg_instance = NDG( ndfs )
         ndfs = list | tuple
            A list or tuple in which each element is either a string
            holding the name of an NDF, or a reference to an NDG. The
            new NDG contains all the supplied NDFs.

      ndg_instance = NDG( size )
         size = integer
            The new NDG represents a group of new NDFs with the specified
            size. The NDFs are placed in the temporary directory specified
            by the class variable NDG.tempdir, and have autmatically
            generated names.

      ndg_instance = NDG( ndg, mod=None )
         ndg = NDG
            An existing NDG object that determines the size of the new
            NDG object. The new object will contain the same number of
            NDFs as the specified object.
         mod = string
            If supplied, the NDF names for the new group are formed by
	    modifying the names in the supplied group using the "mod"
	    string as a GRP "modification element" (e.g. "*_a" or
	    "*|_ed_|bg|"). In this case, the NDFs in the supplied group
	    must already exist. If "mod" is not supplied, the NDFs in the
	    new group are placed in the temporary directory specified by
	    the class variable NDG.tempdir, and have automatically
	    generated names. In this case the NDFs in the supplied group
	    need not already exist.

   Properties:
      comment = string
         Text that is written to the group's list file, if any, as comments
         at the top of the file.
      file = string (read-only)
         The full path to the temporary text file containing the NDFs in
	 the group.
      tmpdir = string (read-only)
         The full path to the directory that holds the groups temporary
         files. This is either None (if the group has no temporary files),
         or a copy of the value of the class "tempdir" variable at the time
         the NDG object was created.


   Methods:
      result = ndg_instance.empty()
         This deletes the files associated with the NDG, leaving the NDG
         empty. It always returns None.
      new_instance = ndg_instance.filter( pattern=None )
         Creates a new NDG object by filtering the NDF names contained
         in an existing NDG object. The pattern should conform to the
         syntax used for the PATTERN parameter by the KAPPA:NDFECHO
         command. See SUN/95. "None" is returned if no NDFs match the
         supplied pattern. If "pattern" is None, the new NDF is a copy of
         the supplied NDF, but excluding the names of any NDFs that do not
         exist.
      ndg_instance.remove( ndf )
         If "ndf " is a string, this removes the NDF with the specified
         path from the NDG (it does nothing if the NDF is not found in the
         NDG). If "ndf " is an integer, it removes all NDFs with index
         greater than or equal to the supplied integer.
      ndg_instance.list()
         Print the list of NDF paths.
      ndg_instance.save( label ):
         Save the paths to the NDFs that form a specified NDG, within
         a file with the given label, so that the NDG can be identified
         on any subequent restarted runs (see the "load" class method).


   Class Variables:
      NDG.tempdir = string
         The path to the temporary directory in which NDFs and text files
	 are stored. The default value is None, which causes a new
	 directory to be created when the first NDG is created, using the
	 mkdtemp() function in the tempfile module. The used value is
	 then assigned to the NDG.tempdir class variable. The value of
	 this variable may be changed to force subsequent NDG objects to
	 use a different temporary directory.
      NDG.overwrite = boolean
         If False (the default), then the sequence numbers used to create
         temportary files will exclude any files that already exist in the
         NDG temporary directory (i.e. any existing files will not be
         over-written). If True, then the sequence number starts from 1
         regardless of whether wthis causes existing files to be over-written.

   Class Methods:
      NDG.cleanup():
         The directory specified by the current value of the class
	 "tempdir" variable is deleted, together with all files in it.
	 All NDG objects created so far that refer to the deleted
	 temporary directory are then reset so that each one represents
	 an empty group.
      ndg_instance = NDG.load( label ):
         Create a new NDG from the names in a previously saved file with
         the given label (see method "save").
      NDG.tempfile(suffix=".lis"):
         Returns the path to a new file in the temporary directory. It is
         guaranteed that this file does not exist, and so can be created
         safely. The file type (suffix) fo the file may be specified, but
         defaults to ".lis".
      NDG.subdir():
         Creates a new subdirectory within the NDG temporary directory,
         and returns its absolute path. The name of the subdirectory is
         created automatically, beginning with "TMP_".

   To Do:
      - Allow comment property to be supplied as an argument to each
      constructor.

   '''

   # The index value to include in the next list file name.
   __nobj = 0

   #  References for all issued NDG objects
   instances = []

   # The full path to the temporary directory.
   tempdir = None

   # Start the sequence number for new files at 1, even if files already
   # exist with these sequence numbers?
   overwrite = False

   # Return the path to the directory to hold temporary files. If nothing
   # has been set for the NDG.tempdir variable, use the tempfile module to
   # create a tempdir. Otherwise, check the named directory exists. If not,
   # create it.
   @classmethod
   def _gettmpdir(cls):
      if NDG.tempdir == None:
         dir = os.environ["STAR_TEMP"] if "STAR_TEMP" in os.environ else None
         NDG.tempdir = tempfile.mkdtemp( prefix='NDG_', dir=dir )
      elif not os.path.isdir(NDG.tempdir):
         os.mkdir( NDG.tempdir )

   # For convenience of allowing others to see the files, make them
   # world-readable. Also make the directory world-executable.
      st = os.stat( NDG.tempdir )
      os.chmod( NDG.tempdir, st.st_mode | stat.S_IRGRP | stat.S_IROTH | stat.S_IXGRP | stat.S_IXOTH )
      return NDG.tempdir

   # Return the path to the temporary list file to use for a group. It is
   # placed in the specified tempdir.
   @classmethod
   def __getfile(cls,tempdir,suffix=".lis"):
      return "{0}/group{1}_{2}".format(tempdir,NDG.__nobj,suffix)

   # Find and store the next unused "nobj" value, then return the temp
   # directory path.
   @classmethod
   def __setnobj(cls):
      NDG.__nobj += 1
      tmpdir = NDG._gettmpdir()
      pattern = "{0}/group{1}_*".format(tmpdir,NDG.__nobj)
      if not NDG.overwrite:
         while len(glob.glob(pattern)) > 0:
            NDG.__nobj += 1
            pattern = "{0}/group{1}_*".format(tmpdir,NDG.__nobj)
      return tmpdir

   # The instance initialiser.
   def __init__(self, p1, p2=None ):

      # Assume initially that this NDG does not have any files in the
      # temporary directory
      self.__tmpdir = None
      self.__file = None

      # As yet we do not have a list of NDFs to put in the group.
      self.__ndfs = None

      # The integer object count associated with the NDG instance.
      self.__nobj = None

      # As yet we do not have a comment
      self.__comment = None

      # Assume initially that all NDF paths in the new NDG are absolute
      isabs = True

      # Assume initially that we are not creating paths to NDFs in the
      # tempdir.
      intemp = False

      # Find out how many NDFs there will be in the group.
      if isinstance(p1,str):
         gexp = shell_quote(p1)
         if p2 != None:
            if p2:
               exists = True
            else:
               exists = False
         else:
            exists = True

         # Use KAPPA:NDFECHO to list the NDFs in the group expression to
         # the above named text file. Also save the NDF names in a Python
         # list.
         if exists:
            try:
               self.__ndfs = invoke("$KAPPA_DIR/ndfecho ndf=\"{0}\" abspath=yes".format(gexp),True)
            except AtaskError:
               raise NoNdfError("\n\nCannot access one or more of the NDFs specified by '{0}'.".format(p1))
         else:
            self.__ndfs = invoke("$KAPPA_DIR/ndfecho ndf=! mod=\"{0}\" abspath=yes".format(gexp),True)

      # If the first argument is a list or tuple, create a group containing
      # the contents of the list or tuple as NDF names. Flag that we do not
      # know these to be absolute.
      elif ( isinstance(p1,list) or isinstance(p1,tuple) ) and p2 == None:
         isabs = False
         self.__ndfs = []
         for ndf in p1:
            if isinstance(ndf,str):
               self.__ndfs.append( ndf )
            elif isinstance(ndf,NDG):
               for a in ndf.__ndfs:
                  self.__ndfs.append( a )
            else:
               self.__ndfs.append( "{0}".format(ndf) )

      # If the first argument is an integer, the group will contain the
      # specified number of NDFs. We cannot determine their paths as
      # yet since we have not yet decided on a tempdir.
      elif isinstance(p1,int) and p2 == None:
         nfile = int(p1)
         intemp = True

      # If the first argument is an NDG, and the second argument is not
      # supplied, the group size is the same as the number of NDFs in the
      # supplied NDG object. Since the supplied NDG may contain one or more
      # container files, each of which may contain mutliple NDFs, we need to
      # use kappa:ndfecho to determine the size of the supplied group. Do
      # this first assuming the NDFs exist in order to get full expansion
      # of container files, and then try again without this assumption if
      # an error occurs.
      elif isinstance(p1,NDG) and p2 == None:
         try:
            invoke("$KAPPA_DIR/ndfecho ndf=\"{0}\" quiet".format(p1))
            nfile = get_task_par( "size", "ndfecho" )
            intemp = True
         except AtaskError:
            try:
               invoke("$KAPPA_DIR/ndfecho ndf=! mod=\"{0}\" quiet".format(p1))
               nfile = get_task_par( "size", "ndfecho" )
            except AtaskError:
               nfile = len(p1.__ndfs)

      # If the first argument is an NDG, and the second argument is a
      # string, create names by modifying the supplied NDG using the
      # string as a modification element.
      elif isinstance(p1,NDG) and isinstance(p2,str):
         gexp1 = shell_quote("{0}".format(p1))
         gexp2 = shell_quote(p2)

         try:
            self.__ndfs = invoke("$KAPPA_DIR/ndfecho ndf=\"{0}\" mod=\"{1}\" abspath=yes".format(gexp1,gexp2),True)
         except AtaskError:
            raise NoNdfError("\n\nCannot access one or more of the NDFs specified by '{0}'.".format(gexp1))

      else:
         raise UsageError("\n\nArguments for NDG constructor are of inappropriate type '{0}...'.".format(p1.__class__.__name__))

      # Ensure we know the size of the group. Also strip and leading and
      # trailing quotes from the path.
      if self.__ndfs != None:
         ndflist = []
         for ndf in self.__ndfs:
            while ndf.startswith("'") or ndf.startswith('"'):
               ndf = ndf[1:]
            while ndf.endswith("'") or ndf.endswith('"'):
               ndf = ndf[:-1]
            if not ndf.isspace():
               ndflist.append(ndf)
         self.__ndfs = ndflist
         nfile = len( self.__ndfs )

      # If we are going to create a list file (i.e. if there is more than
      # one NDF in the group), or if we are going to place any NDFs in the
      # tempdir, get a reference to the tempdir, and (if we are not
      # over-writing existing files) find a unique identifying integer for
      # files belonging to the group.
      if nfile > 1 or intemp:
         self.__tmpdir = NDG.__setnobj()

      # If the NDG represents more than one NDF, decide on the name of the
      # text file to hold the NDF list.
      if nfile > 1:
         self.__file = NDG.__getfile(self.__tmpdir)

      # Now ensure we have a list of NDFs. Create the specified number
      # of names for new (currently non-existent) NDFs within the temp
      # directory.
      if self.__ndfs == None:
         self.__nobj = NDG.__nobj
         self.__ndfs = []
         for i in range(nfile):
            ndf = "{0}/group{1}_{2}".format(self.__tmpdir,NDG.__nobj,i+1)
            self.__ndfs.append(ndf)


      # If there is more than one NDF in the group, write the names of
      # the NDFs to a text file in the tempdir.
      if nfile > 1:
         self.__writeFile()

      # Ensure the NDF paths are absolute.
      if nfile > 0 and not isabs:
         self.__ndfs = invoke("$KAPPA_DIR/ndfecho ndf=! mod=\"{0}\" abspath=yes".format(self),True)
         if len(self.__ndfs) > 1:
            if not self.__file:
               self.__tmpdir = NDG._gettmpdir()
               self.__file = NDG.__getfile(self.__tmpdir)
            self.__writeFile()

         elif self.__file != None:
            os.remove(self.__file)
            self.__file = None

      #  Record a reference to this instance in the class "instances"
      #  variables so that the NDG.cleanup() method can empty it.
      NDG.instances.append(self)


   # The files associated with the NDG are deleted, and the NDG structure
   # itself is reset to represent an empty group. Use kappa:erase to
   # delete the NDFs since they may be NDFs stored within other NDFs.
   def empty( self ):
      global DEBUG
      msg_out( "Emptying NDG \"group{0}\" in {1}".format(self.__nobj,self.__tmpdir), DEBUG )

      for ndf in self.__ndfs:
         invoke("$KAPPA_DIR/erase object=\"{0}\" ok=yes".format(ndf),annul=True)
      if self.__file != None:
         try:
            os.remove( self.__file )
         except:
            pass

      self.__file = None
      self.__ndfs = []
      self.__tmpdir = None

      return None



   #  Create a new NDG by filtering an existing NDG.
   def filter( self, pattern=None ):
      result = None
      if pattern:
         try:
            ndfs = invoke("$KAPPA_DIR/ndfecho ndf=\"{0}\" abspath=yes pattern={1}".format(self,pattern),True)
         except AtaskError:
            ndfs = invoke("$KAPPA_DIR/ndfecho ndf=! mod=\"{0}\" abspath=yes pattern={1}".format(self,pattern),True)
      else:
         ndfs = invoke("$KAPPA_DIR/ndfecho ndf=! mod=\"{0}\" abspath=yes exists=yes".format(self),True)

      if get_task_par( "nmatch", "ndfecho" ) > 0:
         result = NDG( ndfs )
      return result

   #  Save an NDG to a text file with a given label in the tempdir.
   def save( self, label ):
      fpath = os.path.join(NDG.tempdir,"{0}.grp".format(label))
      if os.path.exists(fpath):
         raise UsageError("\n\nThe directory {0} already has an NDG group "
                          "called {1}".format(NDG.tempdir,label) )
      fd = open( fpath, "w" )
      for ndf in self:
         fd.write("{0}\n".format(ndf))
      fd.close()

   #  Remove a single NDF (specified by path) from an NDG, or remove all
   #  NDFs with index greater than or equal to a given index
   def remove( self, ndf ):
      if isinstance(ndf,str):
         if ndf in self.__ndfs:
            self.__ndfs.remove(ndf)
      elif isinstance(ndf,int):
         if len(self.__ndfs) > ndf:
            del self.__ndfs[ndf:]
      self.__writeFile()

   #  Print the NDFs in a NDG.
   def list( self ):
      for ndf in self.__ndfs:
         print( ndf )

   # Allow the NDG to be indexed like a list of NDF names
   def __len__(self):
      return len(self.__ndfs)
   def __getitem__(self, key):
      return self.__ndfs[key]
   def __setitem__(self, key, value):
      raise UsageError("\n\nAttempt to change the contents of an NDG object (programming error).")
   def __delitem__(self, key):
      raise UsageError("\n\nAttempt to delete contents of an NDG object (programming error).")
   def __iter__(self):
      self.__inext = -1
      return self
   def __next__(self):
      return self.next()
   def next(self):
      self.__inext += 1
      if self.__inext < len( self.__ndfs ):
         return self.__ndfs[ self.__inext ]
      else:
         raise StopIteration


   # Define "protected" accessor methods for all public properties
   def _getFile(self):  # Read-only
      return self.__file
   def _getTmpDir(self):  # Read-only
      return self.__tmpdir
   def _getComment(self):
      return self.__comment
   def _setComment(self,text):
      self.__comment = text
      self.__writeFile()

   # Define public properties for all public fields
   file = property(_getFile, None, None, "The group list file")
   tmpdir = property(_getTmpDir, None, None, "The directory containing the groups temporary files")
   comment = property(_getComment, _setComment, None, "Text describing the group")

   # Write the contents of the group to the list file.
   def __writeFile(self):
      global DEBUG
      if self.__file != None:
         msg_out( "Writing NDG list file: {0}".format(self.__file), DEBUG )
         fd = open(self.__file,"w")
         if self.__comment:
            if isinstance(self.__comment,list) or isinstance(self.__comment,tuple):
               for line in self.__comment:
                  line.strip().replace("\n", "\n# ")
                  fd.write("# {0}\n".format(line))
            else:
               line = "{0}".format(self.__comment).strip().replace("\n", "\n# ")
               fd.write("# {0}\n".format(line))
         for ndf in self.__ndfs:
            fd.write("{0}\n".format(ndf))
         fd.close()


   # Create a new NDG from a previously saved NDG.
   @classmethod
   def load( cls, label, report=False ):
      fpath = os.path.join(NDG.tempdir,"{0}.grp".format(label))
      if os.path.exists(fpath):
         return NDG( "^{0}".format(fpath) )
      elif report:
         raise UsageError("\n\nNo NDG labelled '{0}' can be found in "
                          "directory {1}".format(label,NDG.tempdir) )
      else:
         return None

   # Each active NDG object is reset so that it represents an empty
   # group. The NDG.tempdir directory is then deleted.
   @classmethod
   def cleanup(cls):
      for ndg in NDG.instances:
         if ndg.__tmpdir != None and ndg.__tmpdir == NDG.tempdir:
            ndg.__file = None
            ndg.__ndfs = []
            ndg.__tmpdir = None
      if NDG.tempdir != None:
         shutil.rmtree( NDG.tempdir )

   # Return the path to a new temporary file.
   @classmethod
   def tempfile(cls,suffix=".lis"):
      tmpdir = NDG.__setnobj()
      return NDG.__getfile( tmpdir, suffix )

   # Create a new subdirectory within the temporary directory, and return
   # its absolute path.
   @classmethod
   def subdir(cls):
      dir = NDG._gettmpdir()
      return tempfile.mkdtemp( prefix='TMP_', dir=dir )

   # Format an NDG into a shell quoted group expression
   def __str__(self):
      if self.__file:
         return shell_quote("^{0}".format(_rmcwd(self.__file)))
      elif len(self.__ndfs) > 0:
         return shell_quote("{0}".format(_rmcwd(self.__ndfs[0])))
      else:
         return shell_quote("")

   # Test equivalence of two NDGs
   def __eq__(self, other):
      if isinstance(other, self.__class__):
         if len(self) == len(other):
            for (sndf,ondf) in zip( self.__ndfs, other.__ndfs ):
               if sndf != ondf:
                  return False
            return True
         else:
            return False
      else:
         return False

   def __ne__(self, other):
      return not (self == other)


#  -------------------  Exceptions ---------------------------

class StarUtilError(Exception):
   """

   A base class for all the classes of Exception that can be raised by
   this module.

   """
   pass

class UnknownParameterError(StarUtilError):
   """

   A class of Exception that is raised when an unknown parameter
   value is supplied on the command line.

   """
   pass


class NoValueError(StarUtilError):
   """

   A class of Exception that is raised when a null value is supplied for
   a parameter.

   """
   pass

class AbortError(StarUtilError):
   """

   A class of Exception that is raised when an abort is supplied for
   a parameter.

   """
   pass


class InvalidParameterError(StarUtilError):
   """

   A class of Exception that is raised when an invalid value is supplied
   for a parameter.

   """
   pass


class UsageError(StarUtilError):
   """

   A class of Exception that is raised when a error in the use of this
   module's API is detected.

   """
   pass



class NoNdfError(StarUtilError):
   """

   A class of Exception that is raised when an error occurs accessing a
   group of supposedly existing NDFs.

   """
   pass



class AtaskError(StarUtilError):
   """

   A class of Exception that is raised when a error occurs when running
   an atask.

   """
   pass



