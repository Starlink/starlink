static char const rcsid[] = "@(#) $Id$";
/*
** This file contains the TestPoint routines used for profiling
** and coverage analysis of the code.
**
** Copyright (C) 1997-2000 D. Richard Hipp
**
** This library is free software; you can redistribute it and/or
** modify it under the terms of the GNU Library General Public
** License as published by the Free Software Foundation; either
** version 2 of the License, or (at your option) any later version.
**
** This library is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
** Library General Public License for more details.
**
** You should have received a copy of the GNU Library General Public
** License along with this library; if not, write to the
** Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
** Boston, MA  02110-1301, USA.
**
** Author contact information:
**   drh@acm.org
**   http://www.hwaci.com/drh/
*/
/*
** A macro named "TestPoint" is defined which increments a counter
** whenever it is encountered.  This is very efficient, and should
** not impact performance of the system.  For delivery, the macro
** can be nulled out by recompiling without the COVERAGE_TEST macro
** defined.
**
** See also the "renumber.c" program which can be used
** to assign unique numbers to all of the TestPoint(0) macros.
*/
#include "tcl.h"
#include "htmltest.h"

#if INTERFACE

#if defined(COVERAGE_TEST)
# define TestPoint(X)      {extern int HtmlTPArray[]; HtmlTPArray[X]++;}
# define UNTESTED          HtmlTPUntested(__FILE__,__LINE__)
# define CANT_HAPPEN       HtmlTPCantHappen(__FILE__,__LINE__)
# define HtmlVerifyLock(H) if((H)->locked==0)HtmlTPCantHappen(__FILE__,__LINE__)
#else
# define TestPoint(X)
# define UNTESTED
# define CANT_HAPPEN
# define HtmlVerifyLock(H)
#endif

#endif /* INTERFACE */

/*
** The following global array keeps track of the number of visits to
** each testpoint.  The size of the array must be set manually to the
** be at least one greater than the largest TestPoint number.
*/
#if defined(COVERAGE_TEST)
int HtmlTPArray[2000];
#endif

/* Needed by the EslTestPointDump routine
*/
#include <stdio.h>

/*
** Recursion depth
*/
#if defined(DEBUG)
int HtmlDepth = 0;
#endif
#if INTERFACE
#if defined(DEBUG)
#define HtmlPush HtmlDepth+=2
#define HtmlPop  HtmlDepth-=2
#else
#define HtmlPush
#define HtmlPop
#endif
#endif

/* This function is called to print the values of all elements of the
** TP_Array to the given file.  Values are printed in decimal, one per line.
*/
void HtmlTestPointDump(char *filename){
#if defined(COVERAGE_TEST)
  FILE *fp;

  fp = fopen(filename,"a");
  if( fp ){
    int i;
    for(i=0; i<sizeof(HtmlTPArray)/sizeof(HtmlTPArray[0]); i++){
      if( HtmlTPArray[i]>0 ){
        fprintf(fp,"%d %d\n",i,HtmlTPArray[i]);
      }
    }
  }
  fclose(fp);
#endif
}

/* This function reports an error to stderr when code that is marked
** UNTESTED gets executed.
*/
void HtmlTPUntested(const char *zFile, int line){
#ifndef USE_TCL_STUBS
  fprintf(stderr,"Untested HTML Widget code executed in file %s line %d\n",
          zFile,line);
#endif
}

/* This function reports an error to stderr when safety code that should
** never execute is called.
*/
void HtmlTPCantHappen(const char *zFile, int line){
#ifndef USE_TCL_STUBS
  fprintf(stderr,"Unplanned behavior in the HTML Widget in file %s line %d\n",
          zFile,line);
#endif
}
