 /*
 				flag.c

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*
*	Part of:	SExtractor
*
*	Author:		E.BERTIN (IAP, Leiden observatory & ESO)
*
*	Contents:	Function related to external flagging.
*
*	Last modify:	18/11/98
*
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*/

#include	<limits.h>
#include	<stdlib.h>
#include        <string.h>  /* PWD: change here */

#include	"define.h"
#include	"globals.h"
#include	"plist.h"
#include	"flag.h"

/********************************* getflags *********************************
PROTO   void	getflags(objstruct *obj, pliststruct *pixel)
PURPOSE Return the composited flags extracted from the flag-maps.
INPUT   obj structure,
	pixel list.
OUTPUT  -.
NOTES   -.
AUTHOR  E. Bertin (IAP & Leiden & ESO)
VERSION 18/11/98
 ***/
void	getflags(objstruct *obj, pliststruct *pixel)
  {
   pliststruct	*pixt;
   FLAGTYPE	imaflag,cimaflag,
		*flagstack, *fs;
   int		i,n,nmax,nc,nflag,nflag0,
		*nflagstack, *nfs;

  for (i=0; i<prefs.nimaisoflag; i++)
    {
    nmax = 0;
    imaflag = 0;
    switch(prefs.flag_type[i])
      {
      case FLAG_OR:
        for (pixt=pixel+obj->firstpix;pixt>=pixel;
		pixt=pixel+PLIST(pixt,nextpix))
          if (cimaflag = PLISTFLAG(pixt,flag[i]))
            {
            imaflag |= cimaflag;
            nmax++;
            }
        break;
      case FLAG_AND:
        for (pixt=pixel+obj->firstpix;pixt>=pixel;
		pixt=pixel+PLIST(pixt,nextpix))
          if (cimaflag = PLISTFLAG(pixt,flag[i]))
            {
            imaflag &= cimaflag;
            nmax++;
            }
        break;
      case FLAG_MIN:
        imaflag = UINT_MAX;
        for (pixt=pixel+obj->firstpix;pixt>=pixel;
		pixt=pixel+PLIST(pixt,nextpix))
          if (cimaflag = PLISTFLAG(pixt,flag[i]))
            {
            if (cimaflag<imaflag)
              {
              imaflag = cimaflag;
              nmax = 1;
              }
            else if (cimaflag==imaflag)
              nmax++;
            }
        if (!nmax)
          imaflag = 0;
        break;
      case FLAG_MAX:
        imaflag = 0;
        for (pixt=pixel+obj->firstpix;pixt>=pixel;
		pixt=pixel+PLIST(pixt,nextpix))
          if (cimaflag = PLISTFLAG(pixt,flag[i]))
            {
            if (cimaflag>imaflag)
              {
              imaflag = cimaflag;
              nmax = 1;
              }
            else if (cimaflag==imaflag)
              nmax++;
            }
        if (!nmax)
          imaflag = 0;
        break;
      case FLAG_MOST:
/*------ Allocate memory for the buffers */
        nflag = FLAG_BUFSIZE;
        QCALLOC(flagstack, FLAGTYPE, nflag);
        QCALLOC(nflagstack, int, nflag);
/*------ Count flag values */
        for (pixt=pixel+obj->firstpix;pixt>=pixel;
		pixt=pixel+PLIST(pixt,nextpix))
          if (cimaflag = PLISTFLAG(pixt,flag[i]))
            {
            for (n=nflag, fs=flagstack, nfs=nflagstack; n-- && *nfs; nfs++)
              if (*(fs++) == cimaflag)
                {
                (*nfs)++;
                break;
                }
            if (n<0)
              {
              nflag0 = nflag;
              nflag += FLAG_BUFSIZE;
              QREALLOC(flagstack, FLAGTYPE, nflag)
              fs = flagstack + nflag0;
              memset(fs, 0, FLAG_BUFSIZE*sizeof(FLAGTYPE));
              QREALLOC(nflagstack, int, nflag)
              nfs = nflagstack + nflag0;
              memset(nfs, 0, FLAG_BUFSIZE*sizeof(int));
              }
            if (!*nfs)
              {
              *fs = cimaflag;
              *nfs = 1;
              }
            }

/*------ Explore the list we have built and search for most frequent flags */
        for (n=nflag, fs=flagstack, nfs=nflagstack; n-- && *nfs; fs++)
          if ((nc=*(nfs++))>nmax)
            {
            nmax = nc;
            imaflag = *fs;
            }

/*------ Free memory allocated for the buffers */
        free(flagstack);
        free(nflagstack);
        break;
      default:
        error(EXIT_FAILURE, "*Internal Error*: Unknown FLAG_TYPE","");
      }

    if (i<prefs.imaflag_size)
      obj->imaflag[i] = imaflag;
    if (i<prefs.imanflag_size)
      obj->imanflag[i] = nmax;
    }

  return;
  }

/******************************* mergeflags *********************************
PROTO   void	mergeflags(objstruct *objmaster, objstruct *objslave)
PURPOSE Composite flag extracted from the flag-maps.
INPUT   obj structure (,
	pixel list.
OUTPUT  -.
NOTES   -.
AUTHOR  E. Bertin (IAP & Leiden & ESO)
VERSION 29/04/98
 ***/
void	mergeflags(objstruct *objmaster, objstruct *objslave)
  {
   FLAGTYPE	*masterflag,*slaveflag;
   int		i, *masternflag,*slavenflag;

  masterflag = objmaster->imaflag;
  masternflag = objmaster->imanflag;
  slaveflag = objslave->imaflag;
  slavenflag = objslave->imanflag;
  for (i=0; i<prefs.nimaisoflag; i++,
	masterflag++,masternflag++,slaveflag++,slavenflag++)
    switch(prefs.flag_type[i])
      {
      case FLAG_OR:
        *masterflag |= *slaveflag;
        *masternflag += *slavenflag;
        break;
      case FLAG_AND:
        *masterflag &= *slaveflag;
        *masternflag += *slavenflag;
        break;
      case FLAG_MIN:
        if (*slaveflag == *masterflag)
          *masternflag += *slavenflag;
        else if (*slaveflag<*masterflag)
          {
          *masterflag = *slaveflag;
          *masternflag = *slavenflag;
          }
        break;
      case FLAG_MAX:
        if (*slaveflag == *masterflag)
          *masternflag += *slavenflag;
        else if (*slaveflag>*masterflag)
          {
          *masterflag = *slaveflag;
          *masternflag = *slavenflag;
          }
        break;
      case FLAG_MOST:
        if (*slavenflag>*masternflag)
          {
          *masterflag = *slaveflag;
          *masternflag = *slavenflag;
          }
        break;
      default:
        error(EXIT_FAILURE, "*Internal Error*: Unknown FLAG_TYPE","");
      }

  return;
  }

