#ifdef __cplusplus
extern "C" {
#endif
#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"
#ifdef __cplusplus
}
#endif

/* Starlink include files */

#include <nbs_par.h>
#include <nbs_err.h>
#include <sae_par.h>

/* Some memory for strings */
static char str1[512];

/* Array handling code */
#include "arrays/arrays.h"
#include "arrays/arrays.c"

/* Define the packing types */
#define PACKI32 'i'
#define PACKF   'f'
#define PACKD   'd'

static int
not_here(s)
char *s;
{
    croak("%s not implemented on this architecture", s);
    return -1;
}

static double
constant(name)
char *name;
{
    errno = 0;
    switch (*name) {

    case 'N':
     if (strEQ(name,"NBS__SECTIONEXISTED")) return ((double)NBS__SECTIONEXISTED);
     if (strEQ(name,"NBS__TOOMANYDIMS")) return ((double)NBS__TOOMANYDIMS);
     if (strEQ(name,"NBS__TOOMANYBYTES")) return ((double)NBS__TOOMANYBYTES);
     if (strEQ(name,"NBS__BADOFFSET")) return ((double)NBS__BADOFFSET);
     if (strEQ(name,"NBS__BADOPTION")) return ((double)NBS__BADOPTION);
     if (strEQ(name,"NBS__DATANOTSAVED")) return ((double)NBS__DATANOTSAVED);
     if (strEQ(name,"NBS__DEFINING")) return ((double)NBS__DEFINING);
     if (strEQ(name,"NBS__NOTDEFINING")) return ((double)NBS__NOTDEFINING);
     if (strEQ(name,"NBS__NILSID")) return ((double)NBS__NILSID);
     if (strEQ(name,"NBS__NILID")) return ((double)NBS__NILID);
     if (strEQ(name,"NBS__PRIMITIVE")) return ((double)NBS__PRIMITIVE);
     if (strEQ(name,"NBS__NOTPRIMITIVE")) return ((double)NBS__NOTPRIMITIVE);
     if (strEQ(name,"NBS__ITEMNOTFOUND")) return ((double)NBS__ITEMNOTFOUND);
     if (strEQ(name,"NBS__SECTIONNOTFOUND")) return ((double)NBS__SECTIONNOTFOUND);
     if (strEQ(name,"NBS__CANTOPEN")) return ((double)NBS__CANTOPEN);
     if (strEQ(name,"NBS__CANTWRITE")) return ((double)NBS__CANTWRITE);
     if (strEQ(name,"NBS__CANTREAD")) return ((double)NBS__CANTREAD);
     if (strEQ(name,"NBS__NOTOWNER")) return ((double)NBS__NOTOWNER);
     if (strEQ(name,"NBS__TIMEOUT")) return ((double)NBS__TIMEOUT);
     if (strEQ(name,"NBS__DATASAVED")) return ((double)NBS__DATASAVED);
     if (strEQ(name,"NBS__DATANOTRESTORED")) return ((double)NBS__DATANOTRESTORED);
     if (strEQ(name,"NBS__HASIDS")) return ((double)NBS__HASIDS);
     if (strEQ(name,"NBS__NOTTOPLEVEL")) return ((double)NBS__NOTTOPLEVEL);
     if (strEQ(name,"NBS__TOPLEVEL")) return ((double)NBS__TOPLEVEL);
     if (strEQ(name,"NBS__NEVERFOUND")) return ((double)NBS__NEVERFOUND);
     if (strEQ(name,"NBS__INITALLOCFAILED")) return ((double)NBS__INITALLOCFAILED);
     if (strEQ(name,"NBS__NOMOREROOM")) return ((double)NBS__NOMOREROOM);
     if (strEQ(name,"NBS__BADVERSION")) return ((double)NBS__BADVERSION);
     if (strEQ(name,"NBS__IMPOSSIBLE")) return ((double)NBS__IMPOSSIBLE);

    case 'S':
      if (strEQ(name, "SAI__OK"))    return ((double)SAI__OK);
      if (strEQ(name, "SAI__WARN"))  return ((double)SAI__WARN);
      if (strEQ(name, "SAI__ERROR")) return ((double)SAI__ERROR);
      break;

    }
    errno = EINVAL;
    return 0;

not_there:
    errno = ENOENT;
    return 0;
}


MODULE = Starlink::NBS		PACKAGE = Starlink::NBS


double
constant(name)
	char *		name


# Tune all the noticeboard (!)

void
nbs_tune(name, value, oldvalue, status)
  char * name
  int value
  int oldvalue = NO_INIT
  int status
 CODE:
  nbc_tune(name, value, &oldvalue, &status);
 OUTPUT:
  oldvalue
  status

# Tune a specific noticeboard

void
nbs_tune_noticeboard(id, name, value, oldvalue, status)
  int id
  char * name
  int value
  int oldvalue = NO_INIT
  int status
 CODE:
  nbc_tune_noticeboard(id, name, value, &oldvalue, &status);
 OUTPUT:
  oldvalue
  status


# Find a noticeboard

void
nbs_find_noticeboard( noticeboard, topid, status)
  char * noticeboard
  int topid = NO_INIT
  int status
 CODE:
  nbc_find_noticeboard(noticeboard, &topid, &status);
 OUTPUT:
  topid
  status

# Unmap a noticeboard
void
nbs_lose_noticeboard(id, option, status)
  int id
  char * option
  int status
 CODE:
  nbc_lose_noticeboard(id, option, &status);
 OUTPUT:
  status


# Find an item in a noticeboard
void
nbs_find_item(envid, name, id, status)
  int envid
  char * name
  int id = NO_INIT
  int status
 CODE:
  nbc_find_item(envid, name, &id, &status);
 OUTPUT:
  id
  status


# Find the nth item in a noticeboard
void
nbs_find_nth_item(envid, posn, id, status)
  int envid
  int posn
  int id = NO_INIT
  int status
 CODE:
  nbc_find_nth_item(envid, posn, &id, &status);
 OUTPUT:
  id
  status

# Lose the found item
void
nbs_lose_item(id, option, status)
  int id
  char * option
  int status
 CODE:
  nbc_lose_item(id, option, &status);
 OUTPUT:
  status


# Get information from the noticeboard

# Get the type
void
nbs_get_type(id, type, status)
  int id
  char * type = NO_INIT
  int status
 CODE:
  type = str1; /* Get some space */
  nbc_get_type(id, type, &status);
 OUTPUT:
  type
  status


# Get the size
void
nbs_get_size(id, maxbytes, actbytes, status)
  int id
  int maxbytes = NO_INIT
  int actbytes = NO_INIT
  int status
 CODE:
  nbc_get_size(id, &maxbytes, &actbytes, &status);
 OUTPUT:
  maxbytes
  actbytes
  status

# Get the number of children of a structured item
void
nbs_get_children(id, children, status)
  int id
  int children = NO_INIT
  int status
 CODE:
  nbc_get_children(id, &children, &status);
 OUTPUT:
  children
  status

# Get the name of an item associated with an identifier
void
nbs_get_name(id, name, status)
  int id
  char * name = NO_INIT
  int status
 CODE:
  name = str1; /* Get some space */
  nbc_get_name(id, name, &status);
 OUTPUT:
  name
  status

# Determine whether we have a primitive or not
void
nbs_get_primitive(id, primitive, status)
  int id
  int primitive = NO_INIT
  int status
 CODE:
  nbc_get_primitive(id, &primitive, &status);
 OUTPUT:
  primitive
  status

# Get general info on noticeboard
# May not work since type of return value is unknown
void
nbs_get_info(id, name, value, status)
  int id
  char * name
  int value = NO_INIT
  int status
 CODE:
  nbc_get_info(id, name, &value, &status);
 OUTPUT:
  value
  status

# Determine whether noticeboard has been updated 'recently'
void
nbs_get_updated(id, updated, status)
  int id
  int updated = NO_INIT
  int status
 CODE:
  nbc_get_updated(id, &updated, &status);
 OUTPUT:
  updated
  status

# Get the shape of the item
# Might be a problem if actdims is greater than maxdims

void
nbs_get_shape(id, maxdims, dims, actdims, status)
  int id
  int maxdims
  int * dims = NO_INIT
  int actdims = NO_INIT
  int status
 CODE:
  dims = get_mortalspace(maxdims, PACKI32);
  nbc_get_shape(id, &maxdims, dims, &actdims, &status);
  unpack1D( (SV*)ST(2), (void *)dims, PACKI32, actdims);
 OUTPUT:
  maxdims
  dims
  actdims
  status


########################## -- NBS definition -- ###########################

void
nbs_begin_definition(sid, status)
  int sid = NO_INIT
  int status
 CODE:
  nbc_begin_definition(&sid, &status);
 OUTPUT:
  sid
  status

void
nbs_define_structure(envsid, name, type, sid, status)
  int envsid
  char * name
  char * type
  int sid = NO_INIT
  int status
 CODE:
  nbc_define_structure(envsid, name, type, &sid, &status);
 OUTPUT:
  sid
  status

void
nbs_define_primitive(envsid, name, type, maxdims, maxbytes, sid, status)
  int envsid
  char * name
  char * type
  int maxdims
  int maxbytes
  int sid = NO_INIT
  int status
 CODE:
  nbc_define_primitive(envsid, name, type, maxdims, maxbytes, &sid, &status);
 OUTPUT:
  sid
  status

void
nbs_define_shape(sid, ndims, dims, status)
  int sid
  int ndims
  int * dims
  int status
 PROTOTYPE: $$\@$
 CODE:
  nbc_define_shape(sid, ndims, dims, &status);
 OUTPUT:
  status


void
nbs_end_definition(name, option, status)
  char * name
  char * option
  int status
 CODE:
  nbc_end_definition(name, option, &status);
 OUTPUT:
  status

void
nbs_restore_definition(name, save_name, status)
  char * name
  char * save_name
  int status
 CODE:
  nbc_restore_definition(name, save_name, &status);
 OUTPUT:
  status


void
nbs_restore_noticeboard(name, save_name, status)
  char * name
  char * save_name
  int status
 CODE:
  nbc_restore_noticeboard(name, save_name, &status);
 OUTPUT:
  status


void
nbs_save_noticeboard(id, status)
  int id
  int status
 CODE:
  nbc_save_noticeboard(id, &status);
 OUTPUT:
  status



########################### -- P  U  T  S -- #############################

# Put a character variable
# Do not allow character arrays
# Always assume an offset of 0 for now

void
nbs_put_value_c(id, string, status)
  int id
  char * string
  int status
 INIT:
  int offset;
  int i;
  int maxbytes;
  int actbytes;
  char space = '\0';
 CODE:
  offset = 0;
  nbc_get_size(id, &maxbytes, &actbytes, &status); /* Get the size */
  nbc_put_cvalue(id, offset, string, &status);     /* Put in the string */

  /* Fill with spaces - not required now that I have fixed the get*/
  /* for (i=strlen(string)+1; i <= actbytes; i++)
  {
    printf("Looping %d...\n",i);
    nbc_put_cvalue( id, i,&space, &status);
  }
  */

 OUTPUT:
  status


# Put an array
# I know it's a pain but that is just tough :-) '
# Even more work for me if I want to support arrays and scalars

# Integers
void
nbs_put_value_i(id, nvals, value, status)
  int id
  int nvals
  int * value
  int status
 PREINIT:
  int items;
  int outbytes;
  int maxbytes;
  int actbytes;
 CODE:
  /* Find how much space Ive got */
  nbc_get_size(id, &maxbytes, &actbytes, &status);

  if (status == 0) {
    /* Use sizeof */
    items = maxbytes / sizeof(int);

    /* Fix it so that number of items in noticeboard must be
       equal to number of items in incoming array */

    if (items == nvals) {
      outbytes = sizeof(int) * nvals;
      nbc_put_value(id, 0, outbytes, value, &status);
    } else {
      status = NBS__TOOMANYBYTES;
    }
  }

 OUTPUT:
  status


# Reals/Floats
void
nbs_put_value_f(id, nvals, value, status)
  int id
  int nvals
  float * value
  int status
 PREINIT:
  int items;
  int outbytes;
  int maxbytes;
  int actbytes;
 CODE:
  /* Find how much space Ive got */
  nbc_get_size(id, &maxbytes, &actbytes, &status);

  if (status == 0) {
    /* Use sizeof */
    items = maxbytes / sizeof(float);

    /* Fix it so that number of items in noticeboard must be
       equal to number of items in incoming array */

    if (items == nvals) {
      outbytes = sizeof(float) * nvals;
      nbc_put_value(id, 0, outbytes, value, &status);
    } else {
      status = NBS__TOOMANYBYTES;
    }
  }

 OUTPUT:
  status

# Doubles
void
nbs_put_value_d(id, nvals, value, status)
  int id
  int nvals
  double * value
  int status
 PREINIT:
  int items;
  int outbytes;
  int maxbytes;
  int actbytes;
 CODE:
  /* Find how much space Ive got */
  nbc_get_size(id, &maxbytes, &actbytes, &status);

  if (status == 0) {
    /* We still assume 8 bytes for doubles - very lazy */
    items = maxbytes / sizeof(double);

    /* Fix it so that number of items in noticeboard must be
       equal to number of items in incoming array */

    if (items == nvals) {
      outbytes = sizeof(double) * nvals;
      nbc_put_value(id, 0, outbytes, value, &status);
    } else {
      status = NBS__TOOMANYBYTES;
    }
  }

 OUTPUT:
  status

# Logicals
void
nbs_put_value_l(id, nvals, value, status)
  int id
  int nvals
  int * value
  int status
 PREINIT:
  int items;
  int outbytes;
  int maxbytes;
  int actbytes;
 CODE:
  /* Find how much space Ive got */
  nbc_get_size(id, &maxbytes, &actbytes, &status);

  if (status == 0) {
    /* We still assume 4 bytes for ints - very lazy */
    items = maxbytes / sizeof(int);

    /* Fix it so that number of items in noticeboard must be
       equal to number of items in incoming array */

    if (items == nvals) {
      outbytes = sizeof(int) * nvals;
      nbc_put_value(id, 0, outbytes, value, &status);
    } else {
      status = NBS__TOOMANYBYTES;
    }
  }

 OUTPUT:
  status


########################### -- G  E  T  S -- #############################

# Get some values from the noticeboard
# This is not simply a case of reading some values since we need
# To know the type of data stored in the notice board before we
# Can convert it to perl data types
# Therefore this can not simply be a simple mapping of the nbs
# Library call (nbs_get_value). Need to do a bit more work
# Have one XS call for each type and then put the type checking
# into the perl module (could do it with void* but that would involve
# a somehat cleverer type map for arrays.c (I think)
# One thing I can do in C is work out the size of the entry
# Note that this mean that perl always returns NBS entries in
# an array context.

# Get Integers
void
nbs_get_value_i(id, values, status)
  int id
  int * values = NO_INIT
  int status
 INIT:
  int actbytes;
  int maxbytes;
  int items;
 CODE:
  nbc_get_size(id, &maxbytes, &actbytes, &status);

  if (status == 0) {

    items = actbytes / sizeof(int);  /* Should we Assume 4 byte integers? */

    values = get_mortalspace(items, PACKI32); /* Get some memory */

    /* Get the value */
    nbc_get_value(id, 0, actbytes, values, &actbytes, &status);

    if (status == 0) {
      items = actbytes / sizeof(int); /* Still assuming 4 byte ints */

    /* Unpack into a perl array */
      if (items > 0)
	unpack1D( (SV*)ST(1), (void *)values, PACKI32, items);
    }
  }

 OUTPUT:
  values
  status

# Get Reals/floats
void
nbs_get_value_f(id, values, status)
  int id
  float * values = NO_INIT
  int status
 INIT:
  int actbytes;
  int maxbytes;
  int items;
 CODE:
  nbc_get_size(id, &maxbytes, &actbytes, &status);

  if (status == 0) {

    items = actbytes / 4;  /* Assume 4 byte floats */

    values = get_mortalspace(items, PACKF); /* Get some memory */

    /* Get the value */
    nbc_get_value(id, 0, actbytes, values, &actbytes, &status);

    if (status == 0) {
      items = actbytes / sizeof(float); /* Still assuming 4 byte floats */

    /* Unpack into a perl array */
      if (items > 0)
	unpack1D( (SV*)ST(1), (void *)values, PACKF, items);
    }
  }

 OUTPUT:
  values
  status

# Get doubles
void
nbs_get_value_d(id, values, status)
  int id
  double * values = NO_INIT
  int status
 INIT:
  int actbytes;
  int maxbytes;
  int items;
 CODE:
  nbc_get_size(id, &maxbytes, &actbytes, &status);

  if (status == 0) {

    items = actbytes / sizeof(double);  /* Assume 8 byte doubles */

    values = get_mortalspace(items, PACKD); /* Get some memory */

    /* Get the value */
    nbc_get_value(id, 0, actbytes, values, &actbytes, &status);

    if (status == 0) {
      items = actbytes / sizeof(double); /* Still assuming 8 byte doubles */

    /* Unpack into a perl array */
      if (items > 0)
	unpack1D( (SV*)ST(1), (void *)values, PACKD, items);
    }
  }

 OUTPUT:
  values
  status

# Get Logicals (ie ints) A little bit superfluous really
void
nbs_get_value_l(id, values, status)
  int id
  int * values = NO_INIT
  int status
 INIT:
  int actbytes;
  int maxbytes;
  int items;
 CODE:
  nbc_get_size(id, &maxbytes, &actbytes, &status);

  if (status == 0) {

    items = actbytes / sizeof(int);  /* Assume 4 byte integers */

    values = get_mortalspace(items, PACKI32); /* Get some memory */

    /* Get the value */
    nbc_get_value(id, 0, actbytes, values, &actbytes, &status);

    if (status == 0) {
      items = actbytes / sizeof(int); /* Still assuming 4 byte ints */

    /* Unpack into a perl array */
      if (items > 0)
	unpack1D( (SV*)ST(1), (void *)values, PACKI32, items);
    }
  }

 OUTPUT:
  values
  status


# Get character strings (bytes)
# This is no longer an array - just a big string
# Lots of scope for confusion there.

void
nbs_get_value_c(id, cvalue, status)
  int id
  char * cvalue = NO_INIT
  int status
 INIT:
  int actbytes;
  int maxbytes;
  SV* work;
 CODE:
  nbc_get_size(id, &maxbytes, &actbytes, &status);

  if (status == 0) {

    /* Get some workspace the perl way */
    work = sv_2mortal(newSVpv("", 0));
    SvGROW((SV*)work, actbytes+1);
    cvalue = SvPV(work,PL_na);

    /* Get the value */
    nbc_get_value(id, 0, actbytes, cvalue, &actbytes, &status);

    /* Add a null on the end */
    *(cvalue+actbytes) = '\0';

  }  else {
    /* Protect against random stuff ending up in cvalue from an error */
    cvalue = str1;
    strcpy(cvalue,"!!error!!");
  }

 OUTPUT:
  cvalue
  status



# Return size (in bytes) of ints and floats, shorts
# by pack type (see Perl pack command)  [b, r and w are FORTRAN types]

int
nbs_byte_size(packtype)
  char * packtype
 PROTOTYPE: $
 CODE:
  switch (*packtype) {

  case 'a':
  case 'A':
    RETVAL = sizeof(char);
    break;

  case 'b':
  case 'B':
  case 'c':
  case 'C':
    RETVAL = sizeof(char);
    break;

  case 'd':
  case 'D':
    RETVAL = sizeof(double);
    break;

  case 'i':
  case 'I':
    RETVAL = sizeof(int);
    break;

  case 'f':
  case 'r':
  case 'R':
  case 'F':
    RETVAL = sizeof(float);
    break;

  case 'l':
  case 'L':
    RETVAL = sizeof(long);
    break;

  case 's':
  case 'S':
  case 'w':
  case 'W':
    RETVAL = sizeof(short);
    break;

  default:
    RETVAL = 1;
  }
 OUTPUT:
  RETVAL




