static char const rcsid[] = "@(#) $Id$";
/*
** Routines for processing URLs.
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
#include <tk.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include "htmlurl.h"

#if LOCAL_INTERFACE
/*
** A parsed URI is held in an instance of the following structure.
** Each component is recorded in memory obtained from HtmlAlloc().
**
** The examples are from the URI
**
**    http://192.168.1.1:8080/cgi-bin/printenv?name=xyzzy&addr=none#frag
*/
struct HtmlUri {
  char *zScheme;             /* Ex: "http" */
  char *zAuthority;          /* Ex: "192.168.1.1:8080" */
  char *zPath;               /* Ex: "cgi-bin/printenv" */
  char *zQuery;              /* Ex: "name=xyzzy&addr=none" */
  char *zFragment;           /* Ex: "frag" */
};
#endif

char * StringMove(char *s1, const char *s2) {
   size_t n = strlen(s2);
   memmove(s1, s2, n+1);
   return s1;
}

/*
** Return the length of the next component of the URL in z[] given
** that the component starts at z[0].  The initial sequence of the
** component must be zInit[].  The component is terminated by any
** character in zTerm[].  The length returned is 0 if the component
** doesn't exist.  The length includes the zInit[] string, but not
** the termination character.
**
**        Component        zInit      zTerm
**        ----------       -------    -------
**        scheme           ""         ":/?#"
**        authority        "//"       "/?#"
**        path             "/"        "?#"
**        query            "?"        "#"
**        fragment         "#"        ""
*/
static int ComponentLength(const char *z, const char *zInit, const char *zTerm){
  int i, n;
  for(n=0; zInit[n]; n++){
    if( zInit[n]!=z[n] ) return 0;
  }
  while( z[n] ){
    for(i=0; zTerm[i]; i++){
      if( z[n]==zTerm[i] ) return n;
    }
    n++;
  }
  return n;
}

/*
** Duplicate a string of length n.
*/
static char *StrNDup(const char *z, int n){
  char *zResult;
  if( n<=0 ){
    n = strlen(z);
  }
  zResult = HtmlAlloc( n + 1 );
  if( zResult ){
    memcpy(zResult, z, n);
    zResult[n] = 0;
    TestPoint(0);
  }
  return zResult;
}

/*
** Parse a text URI into an HtmlUri structure.
*/
static HtmlUri *ParseUri(const char *zUri){
  HtmlUri *p;
  int n;

  p = HtmlAlloc( sizeof(*p) );
  if( p==0 ) return 0;
  memset(p, 0, sizeof(*p));
  if( zUri==0 || zUri[0]==0 ) return p;
  while( isspace(zUri[0]) ){ zUri++; }
  n = ComponentLength(zUri, "", ":/?# ");
  if( n>0 && zUri[n]==':' ){
    p->zScheme = StrNDup(zUri, n);
    zUri += n+1;
  }
  n = ComponentLength(zUri, "//", "/?# ");
  if( n>0 ){
    p->zAuthority = StrNDup(&zUri[2], n-2);
    zUri += n;
  }
  n = ComponentLength(zUri, "", "?# ");
  if( n>0 ){
    p->zPath = StrNDup(zUri, n);
    zUri += n;
  }
  n = ComponentLength(zUri, "?", "# ");
  if( n>0 ){
    p->zQuery = StrNDup(&zUri[1], n-1);
    zUri += n;
  }
  n = ComponentLength(zUri, "#", " ");
  if( n>0 ){
    p->zFragment = StrNDup(&zUri[1], n-1);
  }
  return p;
}

/*
** Delete an HtmlUri structure.
*/
static void FreeUri(HtmlUri *p){
  if( p==0 ) return;
  if( p->zScheme )    HtmlFree(p->zScheme);
  if( p->zAuthority ) HtmlFree(p->zAuthority);
  if( p->zPath )      HtmlFree(p->zPath);
  if( p->zQuery )     HtmlFree(p->zQuery);
  if( p->zFragment )  HtmlFree(p->zFragment);
  HtmlFree(p);
}

/*
** Create a string to hold the given URI.  Memory to hold the string
** is obtained from HtmlAlloc() and must be freed by the calling
** function.
*/
static char *BuildUri(HtmlUri *p){
  int n = 1;
  char *z;
  if( p->zScheme )    n += strlen(p->zScheme)+1;
  if( p->zAuthority ) n += strlen(p->zAuthority)+2;
  if( p->zPath )      n += strlen(p->zPath)+1;
  if( p->zQuery )     n += strlen(p->zQuery)+1;
  if( p->zFragment )  n += strlen(p->zFragment)+1;
  z = HtmlAlloc( n );
  if( z==0 ) return 0;
  n = 0;
  if( p->zScheme ){
    sprintf(z, "%s:", p->zScheme);
    n = strlen(z);
  }
  if( p->zAuthority ){
    sprintf(&z[n], "//%s", p->zAuthority);
    n += strlen(&z[n]);
  }
  if( p->zPath ){
    sprintf(&z[n], "%s", p->zPath);
    n += strlen(&z[n]);
  }
  if( p->zQuery ){
    sprintf(&z[n], "?%s", p->zQuery);
    n += strlen(&z[n]);
  }
  if( p->zFragment ){
    sprintf(&z[n], "#%s", p->zFragment);
  }else{
    z[n] = 0;
  }
  return z;
}

/*
** Replace the string in *pzDest with the string in zSrc
*/
static void ReplaceStr(char **pzDest, const char *zSrc){
  if( *pzDest!=0 ) HtmlFree(*pzDest);
  if( zSrc==0 ){
    *pzDest = 0;
  }else{
    *pzDest = StrNDup(zSrc, -1);
  }
}

/*
** Remove leading and trailing spaces from the given string.  Return
** a new string obtained from HtmlAlloc().
*/
static char *Trim(char *z){
  int i;
  char *zNew;
  while( isspace(*z) ) z++;
  i = strlen(z);
  zNew = HtmlAlloc( i+1 );
  if( zNew==0 ) return 0;
  strcpy(zNew, z);
  if( i>0 && isspace(zNew[i-1]) ){
    i--;
    zNew[i] = 0;
  }
  return zNew;
}

/*
** The input azSeries[] is a sequence of URIs.  This command must
** resolve them all and put the result in the interp->result field
** of the interpreter associated with the HTML widget.  Return
** TCL_OK on success and TCL_ERROR if there is a failure.
**
** This function can cause the HTML widget to be deleted or changed
** arbitrarily.
*/
int HtmlCallResolver(
  HtmlWidget *htmlPtr,      /* The widget that is doing the resolving. */
  char **azSeries           /* A list of URIs.  NULL terminated */
){
  int rc = TCL_OK;          /* Return value of this function. */
  char *z;

  HtmlVerifyLock(htmlPtr);
  if( htmlPtr->zResolverCommand && htmlPtr->zResolverCommand[0] ){
    /*
    ** Append the current base URI then the azSeries arguments to the
    ** TCL command specified by the -resolvercommand optoin, then execute
    ** the result.
    **
    ** The -resolvercommand could do nasty things, such as delete
    ** the HTML widget out from under us.  Be prepared for the worst.
    */
    Tcl_DString cmd;
    Tcl_DStringInit(&cmd);
    Tcl_DStringAppend(&cmd, htmlPtr->zResolverCommand, -1);
    if( htmlPtr->zBaseHref && htmlPtr->zBaseHref[0] ){
      z = Trim(htmlPtr->zBaseHref);
    }else if( htmlPtr->zBase && htmlPtr->zBase[0] ){
      z = Trim(htmlPtr->zBase);
    }
    if( z ){
      Tcl_DStringAppendElement(&cmd, z);
      HtmlFree(z);
    }
    while( azSeries[0] ){
      z = Trim(azSeries[0]);
      if( z ){
        Tcl_DStringAppendElement(&cmd, z);
        HtmlFree(z);
      }
      azSeries++;
    }
    HtmlLock(htmlPtr);
    rc = Tcl_GlobalEval(htmlPtr->interp, Tcl_DStringValue(&cmd));
    Tcl_DStringFree(&cmd);
    if( HtmlUnlock(htmlPtr) ) return TCL_ERROR;
    if( rc!=TCL_OK ){
      Tcl_AddErrorInfo(htmlPtr->interp,
         "\n    (-resolvercommand executed by HTML widget)");
    }
  }else{
    /*
    ** No -resolvercommand has been specified.  Do the default
    ** resolver algorithm specified in section 5.2 of RFC 2396.
    */
    HtmlUri *base, *term;
    if( htmlPtr->zBaseHref && htmlPtr->zBaseHref[0] ){
      base = ParseUri(htmlPtr->zBaseHref);
    }else{
      base = ParseUri(htmlPtr->zBase);
    }
    while( azSeries[0] ){
      term = ParseUri(azSeries[0]);
      azSeries++;
      if( term->zScheme==0 && term->zAuthority==0 && term->zPath==0
          && term->zQuery==0 && term->zFragment ){
        ReplaceStr(&base->zFragment, term->zFragment);
      }else if( term->zScheme ){
        HtmlUri temp;
        temp = *term;
        *term = *base;
        *base = temp;
      }else if( term->zAuthority ){
        ReplaceStr(&base->zAuthority, term->zAuthority);
        ReplaceStr(&base->zPath, term->zPath);
        ReplaceStr(&base->zQuery, term->zQuery);
        ReplaceStr(&base->zFragment, term->zFragment);
      }else if( term->zPath && (term->zPath[0]=='/' || base->zPath==0) ){
        ReplaceStr(&base->zPath, term->zPath);
        ReplaceStr(&base->zQuery, term->zQuery);
        ReplaceStr(&base->zFragment, term->zFragment);
      }else if( term->zPath && base->zPath ){
        char *zBuf;
        int i, j;
        zBuf = HtmlAlloc( strlen(base->zPath) + strlen(term->zPath) + 2 );
        if( zBuf ){
          sprintf(zBuf,"%s", base->zPath);
          for(i=strlen(zBuf)-1; i>=0 && zBuf[i]!='/'; i--){ zBuf[i] = 0; }
          strcat(zBuf, term->zPath);
          for(i=0; zBuf[i]; i++){
            if( zBuf[i]=='/' && zBuf[i+1]=='.' && zBuf[i+2]=='/' ){
              StringMove(&zBuf[i+1], &zBuf[i+3]);
              i--;
              continue;
            }
            if( zBuf[i]=='/' && zBuf[i+1]=='.' && zBuf[i+2]==0 ){
              zBuf[i+1] = 0;
              continue;
            }
            if( i>0 && zBuf[i]=='/' && zBuf[i+1]=='.' && zBuf[i+2]=='.'
                   && (zBuf[i+3]=='/' || zBuf[i+3]==0) ){
              for(j=i-1; j>=0 && zBuf[j]!='/'; j--){}
              if( zBuf[i+3] ){
                StringMove(&zBuf[j+1], &zBuf[i+4]);
              }else{
                zBuf[j+1] = 0;
              }
              i = j-1;
              if( i<-1 ) i = -1;
              continue;
            }
          }
          HtmlFree(base->zPath);
          base->zPath = zBuf;
        }
        ReplaceStr(&base->zQuery, term->zQuery);
        ReplaceStr(&base->zFragment, term->zFragment);
     }
      FreeUri(term);
    }
    Tcl_SetResult(htmlPtr->interp, BuildUri(base), TCL_DYNAMIC);
    FreeUri(base);
  }
  return rc;
}

/*
** This is a convenient wrapper routine for HtmlCallResolver.
** It makes a copy of the result into memory obtained from HtmlAlloc()
** and invokes Tcl_ResetResult().
*/
char *HtmlResolveUri(HtmlWidget *htmlPtr, char *zUri){
  char *azSeq[2];
  char *zSrc;
  int result;

  if( zUri==0 || *zUri==0 ) return 0;
  azSeq[0] = zUri;
  azSeq[1] = 0;
  HtmlLock(htmlPtr);
  result = HtmlCallResolver(htmlPtr, azSeq);
  if( HtmlUnlock(htmlPtr) ) return 0;
  if( result==TCL_OK ){
    const char *result = Tcl_GetStringResult(htmlPtr->interp);
    zSrc = HtmlAlloc( strlen(result) + 1 );
    if( zSrc ) strcpy(zSrc, result);
  }else{
    zSrc = 0;
  }
  Tcl_ResetResult(htmlPtr->interp);
  return zSrc;
}
