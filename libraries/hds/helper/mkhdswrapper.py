#!python

"""
Given the HDS include file, generate a wrapper that calls
the v4 or v5 APIs depending on the type of the input
locator.

Reads through hds.h and for every API definition, generates
a wrapper routine.

Notes:

- datCcopy is only routine that takes two locators and returns a third.
- Routines taking two input locators:
    datCopy, datMove

All other routines take one locator and may or may not return a
locator. That locator will be of the correct type.

The copy/move routines will need special code to be able to handle
locators from different implementations.

Routines that don't take a locator at all:
   datCctyp - call v5
   datChscn - call v5
   datErmsg - Use wrapper implementation that uses largest error table (default to v5)
   hdsEwild - Wrapper implementation
   hdsFlush - Call both (only error if both fail)
   hdsGtune - call both (only error if both fail or both different)
   hdsShow  - call one or both depending on whether we have used any v5 or v4 locators.
   hdsState - call both (error if different)
   hdsStop - call both
   hdsInfoI - calls both and adds the result
   hdsTune  - call both routines

Routines that open a file:

   hdsOpen - Try v5 and if that fails try v4
   hdsWild - presumably will use hdsOpen internally.
             This routine has to be implemented in the wrapper
             so that the wrapped hdsOpen can be called.

Creating a file:

   hdsNew - always creates v5 unless environment variable
            indicates to only open v4.
   datTemp - uses same logic as for hdsNew

Ideally v5 files would have a different file ending to v4
but that will require lots of code changes in other packages
that are assuming just one file extension.

Will not be run repeatedly as there will eventually be special
code for these notable routines. File is retained for historical
interest.

"""

import re

# Pattern match to find a function
hfunc_re = re.compile(r"^((dat|hds)[A-Z][A-Za-z0-9]+)\(")

def version_names(line):
    v4 = line.replace("(", "_v4(")
    v5 = line.replace("(", "_v5(")
    return (v4,v5)

# Code for the different type of functions
def func_simple(func,line):
    (v4,v5) = version_names(line)
    # are we dealing with locator or locator1?
    locvar = "locator"
    if line.find("locator1") >= 0:
        locvar = "locator1"
    elif line.startswith("datAnnul") or line.startswith("datPrmry") or line.startswith("hdsErase") or line.startswith("hdsClose"):
        locvar = "*locator"
    v4_dims = v4_free = thing = ''
    if 'ndim, dims' in line or 'ndim, pntr, dims' in line:
        thing = 'dims'
    elif 'ndim, subs' in line:
        thing = 'subs'
    if thing:
        v4_dims = 'hdsdim_v4 *{0}_v4 = dat1ExportV4Dims( "{1}", ndim, {0}, status );\n    '.format(thing, func)
        v4_free = '\n    if( {0}_v4 ) starFree( {0}_v4 );'.format(thing)
        v4 = v4.replace(thing, '(hdsdim *) {0}_v4'.format(thing))
    elif 'ndim, lower, upper' in line:
        v4_dims = """hdsdim_v4 *lower_v4 = dat1ExportV4Dims( "{0}", ndim, lower, status );
    hdsdim_v4 *upper_v4 = dat1ExportV4Dims( "{0}", ndim, upper, status );
    """.format(func)
        v4_free = """
    if( lower_v4 ) starFree( lower_v4 );
    if( upper_v4 ) starFree( upper_v4 );"""
        v4 = v4.replace('lower, upper', '(hdsdim *) lower_v4, (hdsdim *) upper_v4')
    print("""
  int retval = 0;
  int instat = *status;
  int isv5 = ISHDSv5({0});
  EnterCheck(\"{3}\",*status);
  if (isv5) {{
    retval = {1}
  }} else {{
    {4}LOCK_MUTEX;
    retval = {2}
    UNLOCK_MUTEX;{5}
  }}
  HDS_CHECK_STATUS(\"{3}\",(isv5 ? "(v5)" : "(v4)"));
  return retval;""".format(locvar, v5, v4, func, v4_dims, v4_free))


def func_special(func,line):
    print("  /* Requires special code */")
    print('  printf("Aborting. Special code required in: %s\\n", "{0}");'.format(line))
    print("  abort();")
    if line.find("status") > -1:
        print("  return *status;")

def func_both(func,line):
    (v4,v5) = version_names(line)
    print("""  int retval = 0;
  int instat = *status;
  EnterCheck(\"{2}\",*status);
  if (*status != SAI__OK) return *status;
  retval = {0}
  LOCK_MUTEX;
  retval = {1}
  UNLOCK_MUTEX;
  HDS_CHECK_STATUS(\"{2}\", "(both)");
  return retval;""".format(v5,v4,func))

def func_versioned(func,line):
    (v4,v5) = version_names(line)
    v4 = v4.replace('dims', '(hdsdim *) dims_v4')
    print("""  int retval = 0;
  int instat = *status;
  const char * used = "(none)";
  EnterCheck(\"{2}\",*status);
  if (*status != SAI__OK) return *status;
  if (hds1UseVersion5()) {{
    retval = {0}
    used = "(v5)";
  }} else {{
    hdsdim_v4 *dims_v4 = dat1ExportV4Dims( "{2}", ndim, dims, status );
    LOCK_MUTEX;
    retval = {1}
    UNLOCK_MUTEX;
    if( dims_v4 ) starFree( dims_v4 );
    used = "(v4)";
  }}
  HDS_CHECK_STATUS(\"{2}\", used);
  return retval;""".format(v5,v4,func))

def func_void(func, line):
    (v4,v5) = version_names(line)
    print("""  EnterCheck(\"{3}\",-1);
  if (ISHDSv5({0})) {{
    {1}
  }} else {{
    LOCK_MUTEX;
    {2}
    UNLOCK_MUTEX;
  }}
  return;""".format("locator", v5, v4,func))

def func_v5void(func,line):
    (v4,v5) = version_names(line)
    print('  EnterCheck("'+func+'",-1);')
    print("  "+v5)
    print("  return;")

def func_v5(func,line):
    (v4,v5) = version_names(line)
    if line.find("status)") > -1:
        print("""  int retval = 0;
  int instat = *status;
  EnterCheck(\"{1}\",*status);
  if (*status != SAI__OK) return *status;
  retval = {0}
  HDS_CHECK_STATUS(\"{1}\","(v5)");
  return retval;""".format(v5,func))
    else:
        print('  EnterCheck("'+func+'",-1);')
        print("  return " +v5)

def func_v5only(func, line):
    (v4,v5) = version_names(line)
    print("""  int retval = 0;
  int instat = *status;
  int isv5 = ISHDSv5(locator);
  EnterCheck("{1}",*status);
  if (isv5) retval = {0}
  HDS_CHECK_STATUS("{1}","(v5)");
  return retval;""".format(v5, func))

def func_copy(func,line):
    (v4,v5) = version_names(line)
    vXtoY = line.replace("(", "XtoY(")
    vXtoY = vXtoY.replace("datC","dat1C")
    loc1 = "locator1"
    if line.startswith("datMove"):
        loc1 = "*locator1"
    print("""  /* Requires special code */
  int instat = *status;
  int isv5 = 0;
  int loc1isv5 = 0;
  int loc2isv5 = 0;
  EnterCheck(\"{3}\",*status);
  if (*status != SAI__OK) return *status;
  loc1isv5 = ISHDSv5({0});
  loc2isv5 = ISHDSv5(locator2);
  if (loc1isv5 && loc2isv5) {{
    /* Just call the v5 code */
    isv5 = 1;
    {1}
  }} else if ( !loc1isv5 && !loc2isv5 ) {{
    isv5 = 0;
    LOCK_MUTEX;
    {2}
    UNLOCK_MUTEX;
  }} else {{
    /* Manual copy of X to Y */
    if (loc1isv5) {{
      isv5 = -1;
    }} else {{
      isv5 = -2;
    }}
    LOCK_MUTEX;
    {4}
    UNLOCK_MUTEX;
  }}
  {{
    const char *helptxt = "(unexpected)";
    if (isv5 == 1) {{
      helptxt = "(v5)";
    }} else if (isv5 == 0) {{
      helptxt = "(v4)";
    }} else if (isv5 == -1) {{
      helptxt = "(v5->v4)";
    }} else if (isv5 == -2) {{
      helptxt = "(v4->v5)";
    }}
    HDS_CHECK_STATUS(\"{3}\",helptxt);
  }}
  return *status;""".format(loc1,v5,v4,func,vXtoY))

def func_datMove(func,line):
    (v4,v5) = version_names(line)
    print("""  /* Requires special code */
  int instat = *status;
  int isv5 = 0;
  int loc1isv5 = 0;
  int loc2isv5 = 0;
  EnterCheck(\"{2}\",*status);
  if (*status != SAI__OK) return *status;
  loc1isv5 = ISHDSv5(*locator1);
  loc2isv5 = ISHDSv5(locator2);
  if (loc1isv5 && loc2isv5) {{
    /* Just call the v5 code */
    isv5 = 1;
    {0}
  }} else if ( !loc1isv5 && !loc2isv5 ) {{
    isv5 = 0;
    LOCK_MUTEX;
    {1}
    UNLOCK_MUTEX;
  }} else {{
    HDSLoc * parenloc = NULL;
    char namestr[DAT__SZNAM+1];
    /* Just do a copy */
    datCopy(*locator1, locator2, name_str, status);
    /* and then erase - HDS API insists that we can not erase
       based on a locator so we need to get the parent and this name. */
    datName(*locator1, namestr, status);
    datParen(*locator1, &parenloc, status);
    datAnnul(locator1, status);
    datErase(parenloc, namestr, status);
    datAnnul(&parenloc, status);
  }}
  HDS_CHECK_STATUS(\"{2}\",(isv5 ? "(v5)" : "(v4)"));
  return *status;""".format(v5,v4,func))

def func_hdsOpen(func,line):
    print("""  int instat = *status;
  EnterCheck(\"hdsOpen\",*status);
  if (*status != SAI__OK) return *status;
  /* HDSv4 can reliably spot when a file is not v4
     format so for now we open in v4 and catch that specific error */
  LOCK_MUTEX;
  hdsOpen_v4(file_str, mode_str, locator, status);
  UNLOCK_MUTEX;
  if (*status == DAT__INCHK || *status == DAT__FILIN) {
    emsAnnul(status);
    hdsOpen_v5(file_str, mode_str, locator, status);
  }
  HDS_CHECK_STATUS( "hdsOpen", file_str);
  return *status;""")

def func_hdsGtune(func,line):
    print("""  int instat = *status;
  const char * used = "(none)";
  EnterCheck(\"hdsGtune\",*status);
  if (*status != SAI__OK) return *status;
  if ( strncasecmp(param_str, "VERSION", 7) == 0 ) {
    hds1GtuneWrapper( param_str, value, status );
    used = "(wrapper)";
  } else {
    hdsGtune_v5(param_str, value, status);
    if (*status == DAT__NOTIM) {
      emsAnnul(status);
      LOCK_MUTEX;
      hdsGtune_v4(param_str, value, status);
      UNLOCK_MUTEX;
      used = "(v4)";
    }
    else {
      used = "(v5)";
    }
  }
  if (*status != SAI__OK) {
    emsRepf("hdsGtune_wrap", "hdsGtune: Error obtaining value of tuning parameter '%s'",
            status, param_str);
  }
  HDS_CHECK_STATUS("hdsGtune", used);
  return *status;""")

def func_hdsTune(func,line):
    print("""  int instat = *status;
  const char * used = "(none)";
  EnterCheck(\"hdsTune\",*status);
  if (*status != SAI__OK) return *status;
  if ( strncasecmp(param_str, "VERSION", 7) == 0 ) {
    hds1TuneWrapper( param_str, value, status );
    used = "(wrapper)";
  } else {
    hdsTune_v5(param_str, value, status);
    LOCK_MUTEX;
    hdsTune_v4(param_str, value, status);
    UNLOCK_MUTEX;
    used = "(both)";
  }
  if (*status != SAI__OK) {
    emsRepf("hdsTune_wrap", "hdsTune: Error setting value of tuning parameter '%s'",
            status, param_str);
  }
  HDS_CHECK_STATUS("hdsTune", used);
  return *status;""")

def func_hdsInfoI(func,line):
    print("""
  int retval = 0;
  int instat = *status;
  *result = 0;
  const char * used = "(both)";
  EnterCheck("hdsInfoI",*status);
  if (*status != SAI__OK) return *status;
  /* Call both versions and sum the result if we have a NULL locator */
  if (!locator) {
    int res_v4 = 0;
    int res_v5 = 0;
    LOCK_MUTEX;
    hdsInfoI_v4(locator, topic_str, extra, &res_v4, status);
    UNLOCK_MUTEX;
    hdsInfoI_v5(locator, topic_str, extra, &res_v5, status);
    retval = *status;
    *result = res_v4 + res_v5;
  } else if (ISHDSv5(locator)) {
    retval = hdsInfoI_v5(locator, topic_str, extra, result, status);
    used = "(v5)";
  } else {
    used = "(v4)";
    LOCK_MUTEX;
    retval = hdsInfoI_v4(locator, topic_str, extra, result, status);
    UNLOCK_MUTEX;
  }
  HDS_CHECK_STATUS("hdsInfoI", used);
  return retval;""")

def func_hdsFlush(func,line):
    print("""  /* We are only allowed to flush a group that actually exists */
  int instat = *status;
  EnterCheck(\"hdsFlush\",*status);
  if (*status != SAI__OK) return *status;

  /* We need a new API that will let us query whether a group
     exists before we try to flush it. _v5 triggers an error
     if the group doesn't exist but v4 does not trigger such an error.
     For now we catch the specific error from v5 and assume that means
     v4 will deal with it. */
  hdsFlush_v5(group_str, status);
  if (*status == DAT__GRPIN) emsAnnul(status);
  LOCK_MUTEX;
  hdsFlush_v4(group_str, status);
  UNLOCK_MUTEX;
  HDS_CHECK_STATUS("hdsFlush", "(both)");
  return *status;""")

def func_hdsCopy(func,line):
    print("""  int instat = *status;
  int ndim = 0;
  hdsdim dims[DAT__MXDIM];
  char type_str[DAT__SZTYP+1];
  HDSLoc * outloc = NULL;
  hdsbool_t struc = 0;
  EnterCheck(\"hdsCopy\",*status);
  if (*status != SAI__OK) return *status;
  /* We always want to end up with output files that match
     the format currently in use for hdsNew (which may depend
     on an environment variable). We can not simply call hdsCopy_v5.
     so we have to do some manual leg work. Would be a bit easier if
     we had a function in this file that returned the default output
     format version so we could call the native version.
   */
  datType( locator, type_str, status );
  datShape( locator, DAT__MXDIM, dims, &ndim, status );
  /* Unfortunately this locator is one level down */
  hdsNew(file_str, name_str, type_str, ndim, dims, &outloc, status );
  /* So we need to walk through and can not simply use datCopy
    - we can use two routines used by dat1CopyXtoY though. */
  datStruc(locator, &struc, status);
  LOCK_MUTEX;
  if (struc) {
    dat1CopyStrucXtoY( locator, outloc, status );
  } else {
    dat1CopyPrimXtoY( locator, outloc, status );
  }
  UNLOCK_MUTEX;
  datAnnul(&outloc, status);
  HDS_CHECK_STATUS("hdsCopy", (ISHDSv5(locator) ? "(v5)" : "(v4)"));
  return *status;""")

def func_datLock(func, line):
    print("""  int retval = 0;
  int instat = *status;
  int isv5 = ISHDSv5(locator);
  EnterCheck("datLock",*status);
  if (isv5) {
    retval = datLock_v5(locator, recurs, readonly, status);
  } else if( *status == SAI__OK && hds1V4LockError() ){
    *status = DAT__VERMM;
    datMsg( "O", locator );
    emsRepf("","datLock: supplied HDS object (^O) uses V4 data format", status );
    emsRepf("","The running application is multi-threaded and so requires V5 data files.",
            status );
  }
  HDS_CHECK_STATUS("datLock","(v5)");
  return retval;""")

def func_datLocked(func, line):
    print("""  int retval = 0;
  int instat = *status;
  int isv5 = ISHDSv5(locator);
  EnterCheck("datLocked",*status);
  if (isv5) {
    retval = datLocked_v5(locator, recurs, status);
  } else {
    retval = -1;  /* Indicates that HDS V4 does not support object locking */
  }
  HDS_CHECK_STATUS("datLocked","(v5)");
  return retval;""")

def func_datUnlock(func, line):
    print("""  int retval = 0;
  int instat = *status;
  int isv5 = ISHDSv5(locator);
  EnterCheck("datUnlock",*status);
  if (isv5) {
    retval = datUnlock_v5(locator, recurs, status);
  } else if( *status == SAI__OK && hds1V4LockError() ){
    *status = DAT__VERMM;
    datMsg( "O", locator );
    emsRepf("","datUnlock: supplied HDS object (^O) uses V4 data format", status );
    emsRepf("","The running application is multi-threaded and so requires V5 data files.",
            status );
  }
  HDS_CHECK_STATUS("datUnlock","(v5)");
  return retval;""")

def func_datShape(func, line):
    print("""
  int retval = 0;
  int instat = *status;
  int isv5 = ISHDSv5(locator);
  EnterCheck("datShape",*status);
  if (isv5) {
    retval = datShape_v5(locator, maxdim, dims, actdim, status);
  } else if( *status == SAI__OK ){
    hdsdim_v4 *dims_v4 = starMalloc( maxdim*sizeof(*dims_v4) );
    if( dims_v4 ) {
       int i;
       LOCK_MUTEX;
       retval = datShape_v4(locator, maxdim, (hdsdim *) dims_v4, actdim, status);
       UNLOCK_MUTEX;
       for( i = 0; i < maxdim; i++ ) dims[ i ] = (hdsdim) dims_v4[ i ];
       starFree( dims_v4 );
    } else {
       *status = DAT__NOMEM;
       emsRep( " ", "datShape wrapper - Error allocating memory", status );
    }
  }
  HDS_CHECK_STATUS("datShape",(isv5 ? "(v5)" : "(v4)"));
  return retval;""")

# Dictionary indicating special cases
special = dict({
    "datCcopy": func_copy,
    "datCctyp": func_v5void,
    "datChscn": func_v5,
    "datCopy": func_copy,
    "datErmsg": func_v5,
    "datLock": func_datLock,
    "datLocked": func_datLocked,
    "datMove": func_datMove,
    "datMsg": func_void,
    "datNolock": func_v5only,
    "datShape": func_datShape,
    "datTemp": func_versioned,
    "datUnlock": func_datUnlock,
    "hdsCopy": func_hdsCopy,
    "hdsEwild": func_special,
    "hdsFlush": func_hdsFlush,
    "hdsGtune": func_hdsGtune,
    "hdsInfoI": func_hdsInfoI,
    "hdsNew":  func_versioned,
    "hdsOpen": func_hdsOpen,
    "hdsShow": func_both,
    "hdsState": func_both,
    "hdsStop": func_both,
    "hdsTune": func_hdsTune,
    "hdsWild": func_special
})

in_prologue = 1
for line in open("hds.h"):
    line = line.strip()
    if line.startswith("int hds"):
        # This is a function that should be ignored
        continue
    func_match = hfunc_re.search(line)
    if func_match:
        hds_function = func_match.group(1)
        print( line[:-1] + " {")  # Without the semi-colon
        # Now we have to convert the prototype to a function call
        # ie datXxx( type1 var1, type2 var2); to datXxx(var1,var2);
        openparen = line.find("(")
        closeparen = line.find(")")
        argsin = line[openparen:closeparen].split(",")
        argsout = []
        for a in argsin:
            # Get rid of array [] specifiers
            arraypos = a.find("[")
            if arraypos > -1:
                a = a[:arraypos]
            parts = a.split()
            varname = parts[-1]
            # Remember to drop pointer derefs
            argsout.append( varname.replace("*", "") )
        # put the line back together
        line = hds_function + "(" + ", ".join(argsout) + ");"
        if hds_function in special:
            special[hds_function](hds_function,line)
        else:
            func_simple(hds_function,line)
        print("}")
    else:
        if in_prologue and line.startswith("/*=="):
            print('/* Code generated by helper/mkhdswrapper.py */')
            print('/* Do not commit changes to this file without also */')
            print('/* adjusting the python code.*/')
            print('')
            print('/* This gives us the PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP static')
            print('   initialiser used below. */')
            print('#define _GNU_SOURCE')
            print('')
            print('#include <stdlib.h>')  # For abort()
            print('#include <stdio.h>')  # For printf()
            print('#include <string.h>')
            print('#include <pthread.h>')
            print('#include "sae_par.h"')
            print('#include "dat_par.h"')
            print('#include "dat1.h"')
            print('#include "hds_types.h"')
            print('#include "ems.h"')
            print('#include "hds.h"')
            print('#include "dat_err.h"')
            print('#include "star/hds_v4.h"')
            print('#include "star/hds_v5.h"')
            print('#include "star/mem.h"')
            print('')
            print('#define ISHDSv5(loc) ((loc) && (loc)->hds_version >= 5)')
            print('')
            print('#if DEBUG_HDS')
            print('#define HDS_CHECK_STATUS(func,txt) if (*status != instat && *status != SAI__OK) { emsRepf("wrap_" func, func ": Error in call to HDS %s", status, txt); printf("Bad status from %s %s: %d\\n", func, txt, *status);}')
            print('static void EnterCheck( const char * func, int status ) { printf("Enter HDS routine: %s [%d]\\n", func,status); }')
            print("#else")
            print('#  define HDS_CHECK_STATUS(func,txt) if (*status != instat && *status != SAI__OK) { emsRepf("wrap_" func, func ": Error in call to HDS %s", status, txt);}')
            print("#  define EnterCheck(A,B) ;")
            print('#endif')
            print("")
            print('/* Define the type used for hdsdim when calling HDS v4 (defined within')
            print('   hds-v4/hds_types.h. It is assumed that HDS v5 and this wrapper package')
            print('   use the same type for hdsdim. */')
            print('typedef int hdsdim_v4;')
            print('')
            print('/* HDS V5 is thread-safe, but V4 is not. So we use a mutex to serialise all')
            print('   calls to V4 functions. Some HDS functions make direct calls to other HDS')
            print('   top level functions. Using a normal mutex to serialise top-level HDS')
            print('   calls would therefore cause deadlock. The right way to fix this would')
            print('   be to re-structure HDS to avoid top-level functions being called from')
            print('   within HDS, but that would be a lot of work. */')
            print('static pthread_mutex_t hdsv4_mutex = PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP;')
            print('#define LOCK_MUTEX pthread_mutex_lock( &hdsv4_mutex );')
            print('#define UNLOCK_MUTEX pthread_mutex_unlock( &hdsv4_mutex );')
            print("")
            print('/* Prototypes for local functions. */')
            print('static hdsdim_v4 *dat1ExportV4Dims( const char *func, int ndim, const hdsdim *dims, int *status );')
            print('')
            print('')
            print('')
            print(line)
            in_prologue = 0
        elif in_prologue:
            # We want to ignore the prologue and write our own
            pass
        elif line.startswith("/* STAR_HDS_H"):
            # this is the end of the include file
            in_prologue = 1
        else:
            print(line)

print('''static hdsdim_v4 *dat1ExportV4Dims( const char *func, int ndim,
                                    const hdsdim *dims, int *status ){
   int i;
   hdsdim_v4 *result = NULL;

   if( *status != SAI__OK ) return NULL;

   result = starMalloc( ndim*sizeof(*result) );
   if( result ) {
      for( i = 0; i < ndim; i++ ) {
         result[ i ] = dims[ i ];
         if( (hdsdim) result[ i ] != dims[ i ] ) {
            *status = DAT__DIMIN;
            emsRepf( "", "%s: Supplied HDS dimension on axis %d (%"
                     HDS_DIM_FORMAT ") is too big to use with an "
                     "HDS V4 data file.", status, func, i + 1, dims[i] );
            starFree( result );
            result = NULL;
            break;
         }
      }

   } else {
      *status = DAT__NOMEM;
      emsRepf( "", "%s: Failed to allocate memory for HDS V4 dimensions.",
               status, func );
   }

   return result;
}


''')
