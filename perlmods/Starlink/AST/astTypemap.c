/*
*     astTypemap.c
 
*  Purpose:
*     Helper code for AST object typemap processing and object handling
 
*  Description:
*     This file implements functions useful for converting AST C structs
*     to Perl objects and Perl objects back to the corresponding C struct.
*     Mainly used in the typemap file but can be used to simplify processing
*     of PPCODE return arguments.
*
*     In order to use these functions in a typemap file, declare each
*     AST struct as a T_ASTOBJ and define the following INPUT and OUTPUT
*     entries:
*
*     TYPEMAP
*     AstObject *   T_ASTOBJ
*
*     INPUT
*     T_ASTOBJ
*    	if (sv_derived_from($arg, ntypeToClass(\"${ntype}\"))) {
*	    IV tmp = extractAstIntPointer( $arg );
*	    $var = INT2PTR($type,tmp);
*	}
*	else
*	    Perl_croak(aTHX_ \"$var is not of class %s\",ntypeToClass(\"${ntype}\"))
* 
*     OUTPUT
*     T_ASTOBJ
*	$arg = createPerlObject(\"${ntype}\", (void*)$var);

*  Copyright:
*     Copyright (C) 2004 Tim Jenness.
*     All Rights Reserved.
 
*  Authors:
*     TIMJ: Tim Jenness (JAC)
 
*  History:
*     24-FEB-2004 (TIMJ):
*        Original version
*
*/

/* prototypes */
#ifdef __cplusplus
extern "C" {
#endif
#include "EXTERN.h"   /* std perl include */
#include "perl.h"     /* std perl include */
#include "XSUB.h"     /* XSUB include */
#include "ppport.h"
#ifdef __cplusplus
}
#endif

#include "ast.h"
#include "astTypemap.h"

/* The name of the attribute in the perl object that handles the
   IV representation of a pointer. */
static char  pntrAttrib[9] = "_pointer";

/* The root namespace we are dealing with */
static char NAMESPACE[14] = "Starlink::AST";

/* 
   Given the XS version of the class name (which is directly related
   to the struct name - AstObject * maps to AstObjectPtr), and a
   pointer to an ast object, return an a reference to a Perl hash
   blessed into the appropriate namespace. For example AstChannelPtr
   will become Starlink::AST::Channel namespace.

   See function  ntypeToClass function for details of the XS to
   namespace mapping.

*/

SV* createPerlObject( char * xsntype, AstObject * var ) {
  SV** retval;
  HV * hash_object = newHV();
  SV * pval;
  SV * rv;

  /* extract the pointer to an int and store it in an SV */
  pval = newSViv( PTR2IV(var) );

  /* Store that SV into a hash using the appropriate key */
  retval = hv_store( hash_object, pntrAttrib, strlen(pntrAttrib),
		     pval,0);

  /* If the store fails, free up the SV created earlier and croak */  
  if (retval == NULL ) {
    SvREFCNT_dec( pval );
    Perl_croak(aTHX_ "Error storing AstObject pointer into hash\n");
  }

  /* Now create a reference to the hash object
     Do not increment the reference count since at the end of this
     we still only want a single reference to the hash to exist */ 
  rv =  newRV_noinc( (SV*)hash_object );

  /* Bless the reference into a class. We translate the XS ntype
     value into an appropriate Perl namespace */
  return sv_bless(rv, gv_stashpv( ntypeToClass(xsntype), 1));

}


/* Given an AST object, return an IV containing the pointer to the
   corresponding AST struct. Must use INT2PTR to convert this
   value to an actual pointer.

*/

IV extractAstIntPointer( SV * arg ) {
  SV ** elem;
  HV * hash_object;

  /* Make sure we have a ref to a hash and get hold of hash */
  /* Code comes from T_HVREF typemap entry */
  if (SvROK(arg) && SvTYPE(SvRV(arg))==SVt_PVHV)
    hash_object = (HV*)SvRV(arg);
  else
    Perl_croak(aTHX_ "Arg is not a hash reference");

  /* Fetch the integer from the hash */
  elem = hv_fetch( hash_object, pntrAttrib, strlen(pntrAttrib), 0);

  /* make sure we got something */
  if (elem == NULL ) {
    Perl_croak(aTHX_ "Error extracting _pointer attribute from object");
  }

  /* extract the actual IV from the element */  
  return SvIV( *elem );
}


/* Convert an XS ntype value (eg AstFitsChanPtr) to an appropriate user
   friendly perl namespace (eg Starlink::AST::FitsChan) 

   Note that "AstObjectPtr" is special-cased to "Starlink::AST"

*/

char * ntypeToClass ( char * ntype ) {
  SV * buffer;
  int len;
  char * offset;

  /* Easy case - we want the default namespace */
  if ( strcmp(ntype, "AstObjectPtr" ) == 0 ) {
    return NAMESPACE;
  }

  /* Bit harder - convert we need to extract the bit between the
     Ast and the Ptr and append that to NAMESPACE:: */

  /* Get a mortal SV so I do not need to worry about strcpy et al */
  buffer = sv_2mortal(newSVpv("",0));

  /* Copy in all the bits */
  sv_catpvn( buffer, NAMESPACE, strlen(NAMESPACE) );
  sv_catpvn( buffer, "::", 2 );
  len = strlen(ntype) - 6; /* Length without "Ast" and "Ptr" */
  offset = ntype + 3;   /* jump in 3 characters */
  sv_catpvn( buffer, offset, len ); /* append substring */

  /* now return the pointer */
  return SvPVX( buffer );
}

/* An internal hash object attribute accessor return the relevant SV given
   a reference to the object and a attribute name.

   Returns NULL if no value is stored or if the supplied SV is not a reference.

   Does not set astError. Croaks if the SV is defined but is not of the
   correct type.
*/

SV* getPerlObjectAttr ( SV * myobject, char * attr ) {
  SV** elem;
  HV * hash_object;

  if (myobject == NULL || !SvOK(myobject) ) {
    return NULL;
  }
 
  /* Make sure we have a reference to a hash */
  if (SvROK(myobject) && SvTYPE(SvRV(myobject))==SVt_PVHV)
    hash_object = (HV*)SvRV(myobject);
  else
    Perl_croak(aTHX_ "Ast object must be a reference to a hash");

  /* retrieve the element */
  elem = hv_fetch( hash_object, attr, strlen(attr), 0);

  /* trap for undef */
  if (elem == NULL || !SvOK(*elem) ) {
    return NULL;
  } else {
    return *elem;
  }
}
