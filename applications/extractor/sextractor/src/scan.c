   /*
 				scan.c

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*
*	Part of:	SExtractor
*
*	Author:		E.BERTIN (IAP)
*
*	Contents:	functions for extraction of connected pixels from
*			a pixmap.
*
*	Last modify:	02/04/2003
*
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*/

#ifdef HAVE_CONFIG_H
#include        "config.h"
#endif

#include	<math.h>
#include	<stdio.h>
#include	<stdlib.h>
#include        <string.h>    /* PWD: change here */

#include	"define.h"
#include	"globals.h"
#include	"prefs.h"
#include	"back.h"
#include	"check.h"
#include	"clean.h"
#include	"extract.h"
#include	"filter.h"
#include	"image.h"
#include	"plist.h"

/****************************** scanimage ************************************
PROTO   void scanimage(picstruct *field, picstruct *dfield, picstruct *ffield,
        picstruct *wfield, picstruct *dwfield)
PURPOSE Scan of the large pixmap(s). Main loop and heart of the program.
INPUT   Measurement field pointer,
        Detection field pointer,
        Flag field pointer,
        Measurement weight-map field pointer,
        Detection weight-map field pointer,
OUTPUT  -.
NOTES   -.
AUTHOR  E. Bertin (IAP & Leiden & ESO)
VERSION 02/04/2003
 ***/
void	scanimage(picstruct *field, picstruct *dfield, picstruct **pffield,
		int nffield, picstruct *wfield, picstruct *dwfield)

  {
   static infostruct	curpixinfo, *info, *store,
			initinfo, freeinfo, *victim;
   picstruct		*ffield;
   checkstruct		*check;
   objliststruct       	objlist;
   objstruct		*cleanobj;
   pliststruct		*pixel, *pixt;
   picstruct		*cfield, *cdwfield;

   char			*marker, newmarker, *blankpad, *bpt,*bpt0;
   int			co, i,j, flag, luflag,pstop, xl,xl2,yl, cn,
			nposize, stacksize, w, h, blankh, maxpixnb,
			varthreshflag;
   short	       	trunflag;
   PIXTYPE		thresh, relthresh, cdnewsymbol, cdvar,
			*scan,*dscan,*cdscan,*dwscan,*cdwscan,*scant,*wscan;
   FLAGTYPE		*pfscan[MAXFLAG];
   status		cs, ps, *psstack;
   int			*start, *end, ymax;

/*----- Beginning of the main loop: Initialisations  */
  thecat.ntotal = thecat.ndetect = 0;

/* cfield is the detection field in any case */
  cfield = dfield? dfield:field;
  
/* cdwfield is the detection weight-field if available */
  cdwfield = dwfield? dwfield:(prefs.dweight_flag?wfield:NULL);

/* If WEIGHTing and no absolute thresholding, activate threshold scaling */
  varthreshflag = (cdwfield && prefs.thresh_type[0]!=THRESH_ABSOLUTE);

  if (varthreshflag)
    relthresh = prefs.dthresh[0];

  w = cfield->width;
  h = cfield->height;
  objlist.dthresh = cfield->dthresh;
  objlist.thresh = cfield->thresh;
  cfield->yblank = 1;
  field->y = field->stripy = 0;
  field->ymin = field->stripylim = 0;
  field->stripysclim = 0;
  if (dfield)
    {
    dfield->y = dfield->stripy = 0;
    dfield->ymin = dfield->stripylim = 0;
    dfield->stripysclim = 0;
    }
  if (nffield)
    for (i=0; i<nffield; i++)
      {
      ffield = pffield[i];
      ffield->y = ffield->stripy = 0;
      ffield->ymin = ffield->stripylim = 0;
      ffield->stripysclim = 0;
      }
  if (wfield)
    {
    wfield->y = wfield->stripy = 0;
    wfield->ymin = wfield->stripylim = 0;
    wfield->stripysclim = 0;
    }
  if (dwfield)
    {
    dwfield->y = dwfield->stripy = 0;
    dwfield->ymin = dwfield->stripylim = 0;
    dwfield->stripysclim = 0;
    }

/*Allocate memory for buffers */
  stacksize = w+1;
  QMALLOC(info, infostruct, stacksize);
  QMALLOC(store, infostruct, stacksize);
  QMALLOC(marker, char, stacksize);
  QMALLOC(dumscan, PIXTYPE, stacksize);
  QMALLOC(psstack, status, stacksize);
  QMALLOC(start, int, stacksize);
  QMALLOC(end, int, stacksize);
  lutzalloc(w,h);
  allocparcelout();
  if (prefs.filter_flag)
    {
    QMALLOC(cdscan, PIXTYPE, stacksize);
    if (cdwfield)
      QMALLOC(cdwscan, PIXTYPE, stacksize);
/*-- One needs a buffer to protect filtering if source-blanking applies */
    if (prefs.blank_flag)
      {
      blankh = thefilter->convh/2+1;
      QMALLOC(blankpad, char, w*blankh);
      cfield->yblank -= blankh;
      if (dfield)
        field->yblank = cfield->yblank;
      bpt = blankpad;
      }
    }

/* Some initializations */

  thresh = objlist.dthresh;
  initinfo.pixnb = 0;
  initinfo.flag = 0;
  initinfo.firstpix = initinfo.lastpix = -1;

  for (xl=0; xl<stacksize; xl++)
    {
    marker[xl]  = 0 ;
    dumscan[xl] = -BIG ;
    }

  co = pstop = 0;
  objlist.nobj = 1;
  curpixinfo.pixnb = 1;

/* Init cleaning procedure */
  initclean();

/*----- Allocate memory for the pixel list */
  init_plist();

  if (!(pixel = objlist.plist = malloc(nposize=prefs.mem_pixstack*plistsize)))
    error(EXIT_FAILURE, "Not enough memory to store the pixel stack:\n",
        "           Try to decrease MEMORY_PIXSTACK");

/*----- at the beginning, "free" object fills the whole pixel list */
  freeinfo.firstpix = 0;
  freeinfo.lastpix = nposize-plistsize;
  pixt = pixel;
  for (i=plistsize; i<nposize; i += plistsize, pixt += plistsize)
    PLIST(pixt, nextpix) = i;
  PLIST(pixt, nextpix) = -1;

/*----- Here we go */
  for (yl=0; yl<=h;)
    {
    ps = COMPLETE;
    cs = NONOBJECT;
    if (yl==h)
      {
/*---- Need an empty line for Lutz' algorithm to end gracely */
      if (prefs.filter_flag)
        {
        free(cdscan);
        if (cdwfield)
          free(cdwscan);
        }
      cdwscan = cdscan = dumscan;
      }
    else
      {
      if (nffield)
        for (i=0; i<nffield; i++)
          {
          ffield = pffield[i];
          pfscan[i] = (ffield->stripy==ffield->stripysclim)?
		  (FLAGTYPE *)loadstrip(ffield, (picstruct *)NULL)
		: &ffield->fstrip[ffield->stripy*ffield->width];
          }
      if (wfield)
        wscan = (wfield->stripy==wfield->stripysclim)?
		  (PIXTYPE *)loadstrip(wfield, (picstruct *)NULL)
		: &wfield->strip[wfield->stripy*wfield->width];
      else
        wscan = NULL;
      scan = (field->stripy==field->stripysclim)?
		  (PIXTYPE *)loadstrip(field, wfield)
		: &field->strip[field->stripy*field->width];
      if (dwfield)
        dwscan = (dwfield->stripy==dwfield->stripysclim)?
		  (PIXTYPE *)loadstrip(dwfield,
				dfield?(picstruct *)NULL:dwfield)
		: &dwfield->strip[dwfield->stripy*dwfield->width];
      else
        dwscan = wscan;
      if (dfield)
        dscan = (dfield->stripy==dfield->stripysclim)?
		  (PIXTYPE *)loadstrip(dfield, dwfield)
		: &dfield->strip[dfield->stripy*dfield->width];
      else
        dscan = scan;

      if (prefs.filter_flag)
        {
        filter(cfield, cdscan);
        if (cdwfield)
          filter(cdwfield, cdwscan);
        }
      else
        {
        cdscan = dscan;
        cdwscan = dwscan;
        }

      if (check=prefs.check[CHECK_FILTERED])
        writecheck(check, cdscan, w);
      }

    trunflag = (yl==0 || yl==h-1)? OBJ_TRUNC:0;

    for (xl=0; xl<=w; xl++)
      {
      if (xl == w)
        cdnewsymbol = -BIG;
      else
        cdnewsymbol = cdscan[xl];

      newmarker = marker[xl];
      marker[xl] = 0;

      curpixinfo.flag = trunflag;
      if (varthreshflag)
        thresh = relthresh*sqrt(cdvar = ((xl==w || yl==h)? 0.0:cdwscan[xl]));
      luflag = cdnewsymbol > thresh?1:0;

      if (luflag)
        {
        if (xl==0 || xl==w-1)
          curpixinfo.flag |= OBJ_TRUNC;
        pixt = pixel + (cn=freeinfo.firstpix);
        freeinfo.firstpix = PLIST(pixt, nextpix);

/*------- Running out of pixels, the largest object becomes a "victim" ------*/

        if (freeinfo.firstpix==freeinfo.lastpix)
          {
          sprintf(gstr, "%d,%d", xl+1, yl+1);
          warning("Pixel stack overflow at position ", gstr);
          maxpixnb = 0;
          for (i=0; i<=w; i++)
            if (store[i].pixnb>maxpixnb)
              if (marker[i]=='S' || (newmarker=='S' && i==xl))
                {
                flag = 0;
                if (i<xl)
                  for (j=0; j<=co; j++)
                    flag |= (start[j]==i);
                if (!flag)
                  maxpixnb = (victim = &store[i])->pixnb;
                }
          for (j=1; j<=co; j++)
            if (info[j].pixnb>maxpixnb)
              maxpixnb = (victim = &info[j])->pixnb;

          if (!maxpixnb)
            error(EXIT_FAILURE, "*Fatal Error*: something is badly bugged in ",
		"scanimage()!");
          if (maxpixnb <= 1)
            error(EXIT_FAILURE, "Pixel stack overflow in ", "scanimage()");
          freeinfo.firstpix = PLIST(pixel+victim->firstpix, nextpix);
          PLIST(pixel+victim->lastpix, nextpix) = freeinfo.lastpix;
          PLIST(pixel+(victim->lastpix=victim->firstpix), nextpix) = -1;
          victim->pixnb = 1;
          victim->flag |= OBJ_OVERFLOW;
          }

/*---------------------------------------------------------------------------*/
        curpixinfo.lastpix = curpixinfo.firstpix = cn;
        PLIST(pixt, nextpix) = -1;
        PLIST(pixt, x) = xl;
        PLIST(pixt, y) = yl;
        PLIST(pixt, value) = scan[xl];
        if (PLISTEXIST(dvalue))
          PLISTPIX(pixt, dvalue) = dscan[xl];
        if (PLISTEXIST(cdvalue))
          PLISTPIX(pixt, cdvalue) = cdnewsymbol;
        if (PLISTEXIST(flag))
          for (i=0; i<nffield; i++)
            PLISTFLAG(pixt, flag[i]) = pfscan[i][xl];
        if (PLISTEXIST(dthresh))
          PLISTPIX(pixt, dthresh) = thresh;
        if (PLISTEXIST(var))
          PLISTPIX(pixt, var) = wscan[xl];

        if (cs != OBJECT)
/*------------------------------- Start Segment -----------------------------*/

          {
          cs = OBJECT;
          if (ps == OBJECT)
            {
            if (start[co] == UNKNOWN)
              {
              marker[xl] = 'S';
              start[co] = xl;
              }
            else
              marker[xl] = 's';
            }
          else
            {
            psstack[pstop++] = ps;
            marker[xl] = 'S';
            start[++co] = xl;
            ps = COMPLETE;
            info[co] = initinfo;
            }
          }

/*---------------------------------------------------------------------------*/
        }

      if (newmarker)

/*---------------------------- Process New Marker ---------------------------*/

        {
        if (newmarker == 'S')
          {
          psstack[pstop++] = ps;
          if (cs == NONOBJECT)
            {
            psstack[pstop++] = COMPLETE;
            info[++co] = store[xl];
            start[co] = UNKNOWN;
            }
          else
            update (&info[co],&store[xl], pixel);
          ps = OBJECT;
          }
        else if (newmarker == 's')
          {
          if ((cs == OBJECT) && (ps == COMPLETE))
            {
            pstop--;
            xl2 = start[co];
            update (&info[co-1],&info[co], pixel);
            if (start[--co] == UNKNOWN)
              start[co] = xl2;
            else
              marker[xl2] = 's';
            }
          ps = OBJECT;
          }
        else if (newmarker == 'f')
          ps = INCOMPLETE;
        else if (newmarker == 'F')
          {
          ps = psstack[--pstop];
          if ((cs == NONOBJECT) && (ps == COMPLETE))
            {
            if (start[co] == UNKNOWN)
              {
              if ((int)info[co].pixnb >= prefs.ext_minarea)
                sortit(field, dfield, wfield, cdwfield, &info[co], &objlist);
/* ------------------------------------ free the chain-list */

              PLIST(pixel+info[co].lastpix, nextpix) = freeinfo.firstpix;
              freeinfo.firstpix = info[co].firstpix;
              }
            else
              {
              marker[end[co]] = 'F';
              store[start[co]] = info[co];
              }
            co--;
            ps = psstack[--pstop];
            }
          }
        }
/*---------------------------------------------------------------------------*/

      if (luflag)
        update (&info[co],&curpixinfo, pixel);
      else
        {
        if (cs == OBJECT)
/*-------------------------------- End Segment ------------------------------*/
          {
          cs = NONOBJECT;
          if (ps != COMPLETE)
            {
            marker[xl] = 'f';
            end[co] = xl;
            }
          else
            {
            ps = psstack[--pstop];
            marker[xl] = 'F';
            store[start[co]] = info[co];
            co--;
            }
          }
        }

      if (prefs.blank_flag && xl<w)
        {
        if (prefs.filter_flag)
	  *(bpt++) = (luflag)?1:0;
        else if (luflag)
          dscan[xl] = -BIG;
        if (dfield && luflag)
          scan[xl] = -BIG;
        }
/*--------------------- End of the loop over the x's -----------------------*/
      }

/* Detected pixel removal at the end of each line */
    if (prefs.blank_flag && yl<h)
      {
      if (prefs.filter_flag)
        {
        bpt = bpt0 = blankpad + w*((yl+1)%blankh);
        if (cfield->yblank >= 0)
          {
          scant = &PIX(cfield, 0, cfield->yblank);
          for (i=w; i--; scant++)
            if (*(bpt++))
              *scant = -BIG;
          if (dfield)
            {
            bpt = bpt0;
            scant = &PIX(field, 0, cfield->yblank);
            for (i=w; i--; scant++)
              if (*(bpt++))
                *scant = -BIG;
            }
          bpt = bpt0;
          }
        }
      cfield->yblank++;
      if (dfield)
        field->yblank = cfield->yblank;
      }

/*-- Prepare markers for the next line */
    yl++;
    field->stripy = (field->y=yl)%field->stripheight;
    if (dfield)
      dfield->stripy = (dfield->y=yl)%dfield->stripheight;
    if (nffield)
      for (i=0; i<nffield; i++)
        {
        ffield = pffield[i];
        ffield->stripy = (ffield->y=yl)%ffield->stripheight;
        }
    if (wfield)
      wfield->stripy = (wfield->y=yl)%wfield->stripheight;
    if (dwfield)
      dwfield->stripy = (dwfield->y=yl)%dwfield->stripheight;

/*-- Remove objects close to the ymin limit if ymin is ready to increase */
    if (cfield->stripy==cfield->stripysclim)
      {
      cleanobj = cleanobjlist->obj+cleanobjlist->nobj-1;
      for (i=cleanobjlist->nobj; i--; cleanobj--)
        {
        if (cleanobj->ycmin <= cfield->ymin)
          {
/*-------- Warn if there is a possibility for any aperture to be truncated */
          if ((ymax=cleanobj->ycmax) > cfield->ymax)
            {
            sprintf(gstr, "Object at position %.0f,%.0f ",
		cleanobj->mx+1, cleanobj->my+1);
            QWARNING(gstr, "may have some apertures truncated:\n"
		"          You might want to increase MEMORY_BUFSIZE");
            }
          else if (ymax>cfield->yblank && prefs.blank_flag)
            {
            sprintf(gstr, "Object at position %.0f,%.0f ",
		cleanobj->mx+1, cleanobj->my+1);
            QWARNING(gstr, "may have some unBLANKed neighbours:\n"
		"          You might want to increase MEMORY_PIXSTACK");
            }
          endobject(field, dfield, wfield, cdwfield, i, cleanobjlist);
          subcleanobj(i);
          cleanobj = cleanobjlist->obj+i;	/* realloc in subcleanobj() */
          }
        }
      }

    if (!((yl+1)%16))
      NPRINTF(OUTPUT, "\33[1M> Line:%5d  "
		"Objects: %8d detected / %8d sextracted\n\33[1A",
	yl+1, thecat.ndetect, thecat.ntotal);
/*--------------------- End of the loop over the y's -----------------------*/
    }

/* Removal or the remaining pixels */
  if (prefs.blank_flag && prefs.filter_flag && (cfield->yblank >= 0))
    for (j=blankh-1; j--; yl++)
      {
      bpt = bpt0 = blankpad + w*(yl%blankh);
      scant = &PIX(cfield, 0, cfield->yblank);
      for (i=w; i--; scant++)
        if (*(bpt++))
          *scant = -BIG;
      if (dfield)
        {
        bpt = bpt0;
        scant = &PIX(field, 0, cfield->yblank);
        for (i=w; i--; scant++)
          if (*(bpt++))
            *scant = -BIG;
        }
      cfield->yblank++;
      if (dfield)
        field->yblank = cfield->yblank;
      }

/* Now that all "detected" pixels have been removed, analyse detections */
  for (j=cleanobjlist->nobj; j--;)
    {
    endobject(field, dfield, wfield, cdwfield, 0, cleanobjlist);
    subcleanobj(0);
    }

  endclean();

/*Free memory */

  freeparcelout();
  free(pixel);
  lutzfree();
  free(info);
  free(store);
  free(marker);
  free(dumscan);
  free(psstack);
  free(start);
  free(end);
  if (prefs.blank_flag && prefs.filter_flag)
    free(blankpad);

  return;
  }


/********************************* update ************************************/
/*
update object's properties each time one of its pixels is scanned by lutz()
*/
void  update(infostruct *infoptr1, infostruct *infoptr2, pliststruct *pixel)

  {
  infoptr1->pixnb += infoptr2->pixnb;
  infoptr1->flag |= infoptr2->flag;
  if (infoptr1->firstpix == -1)
    {
    infoptr1->firstpix = infoptr2->firstpix;
    infoptr1->lastpix = infoptr2->lastpix;
    }
  else if (infoptr2->lastpix != -1)
    {
    PLIST(pixel+infoptr1->lastpix, nextpix) = infoptr2->firstpix;
    infoptr1->lastpix = infoptr2->lastpix;
    }

  return;
  }

/********************************* sortit ************************************/
/*
build the object structure.
*/
void  sortit(picstruct *field, picstruct *dfield, picstruct *wfield,
	picstruct *dwfield, infostruct *info, objliststruct *objlist)

  {
   picstruct		*cfield;
   objliststruct	objlistout, *objlist2;
   static objstruct	obj;
   objstruct		*cobj;
   pliststruct		*pixel;
   int 			i,j,n;

  cfield = dfield? dfield: field;

  pixel = objlist->plist;
  objlistout.obj = NULL;
  objlistout.plist = NULL;
  objlistout.nobj = objlistout.npix = 0;

/*----- Allocate memory to store object data */

  objlist->obj = &obj;
  objlist->nobj = 1;

  memset(&obj, 0, (size_t)sizeof(objstruct));
  objlist->npix = info->pixnb;
  obj.firstpix = info->firstpix;
  obj.lastpix = info->lastpix;
  obj.flag = info->flag;
  obj.dthresh = objlist->dthresh;
  obj.thresh = objlist->thresh;

  preanalyse(0, objlist, ANALYSE_FAST);

/*----- Check if the current strip contains the lower isophote... */
  if ((int)obj.ymin < cfield->ymin)
    obj.flag |= OBJ_ISO_PB;

  if (!(obj.flag & OBJ_OVERFLOW) && (createsubmap(objlist, 0) == RETURN_OK))
    {
    if (parcelout(objlist, &objlistout) == RETURN_OK)
      objlist2 = &objlistout;
    else
      {
      objlist2 = objlist;
      for (i=0; i<objlist2->nobj; i++)
        objlist2->obj[i].flag |= OBJ_DOVERFLOW;
      sprintf(gstr, "%.0f,%.0f", obj.mx+1, obj.my+1);
      warning("Deblending overflow for detection at ", gstr);
      }
    free(obj.submap);
    }
  else
    objlist2 = objlist;

  for (i=0; i<objlist2->nobj; i++)
    {
    preanalyse(i, objlist2, ANALYSE_FULL|ANALYSE_ROBUST);
    analyse(field, dfield, i, objlist2);
    cobj = objlist2->obj + i;
    if (prefs.blank_flag)
      {
      if (createblank(objlist2,i) != RETURN_OK)
        {
/*------ Not enough mem. for the BLANK vignet: flag the object now */
        cobj->flag |= OBJ_OVERFLOW;
        cobj->blank = cobj->dblank = NULL;
        sprintf(gstr, "%.0f,%.0f", cobj->mx+1, cobj->my+1);
        warning("Memory overflow during masking for detection at ", gstr);
        }
      }

    if ((n=cleanobjlist->nobj) >= prefs.clean_stacksize)
      {
       objstruct	*cleanobj;
       int		ymin, ymax, victim;

      ymin = 2000000000;	/* No image is expected to be that tall ! */
      cleanobj = cleanobjlist->obj;
      for (j=0; j<n; j++, cleanobj++)
        if (cleanobj->ycmax < ymin)
          {
          victim = j;
          ymin = cleanobj->ycmax;
          }

/*---- Warn if there is a possibility for any aperture to be truncated */
      if (field->ymax < field->height)
        {
        cleanobj = &cleanobjlist->obj[victim];
        if ((ymax=cleanobj->ycmax) > field->ymax)
          {
          sprintf(gstr, "Object at position %.0f,%.0f ",
		cleanobj->mx+1, cleanobj->my+1);
          QWARNING(gstr, "may have some apertures truncated:\n"
		"          You might want to increase MEMORY_OBJSTACK");
          }
        else if (ymax>field->yblank && prefs.blank_flag)
          {
          sprintf(gstr, "Object at position %.0f,%.0f ",
		cleanobj->mx+1, cleanobj->my+1);
          QWARNING(gstr, "may have some unBLANKed neighbours\n"
		"          You might want to increase MEMORY_OBJSTACK");
          }
        }

      endobject(field, dfield, wfield, dwfield, victim, cleanobjlist);
      subcleanobj(victim);
      }

/* Only add the object if it is not swallowed by cleaning */
    if (!prefs.clean_flag || clean(field, dfield, i, objlist2))
      addcleanobj(cobj);
    }

  free(objlistout.plist);
  free(objlistout.obj);

  return;
  }


/******************************** preanalyse *********************************
PROTO   void preanalyse(int no, objliststruct *objlist, int analyse_type)
PURPOSE Compute basic image parameters from the pixel-list for each detection.
INPUT   objlist number,
        objlist pointer,
        analysis switch flag.
OUTPUT  -.
NOTES   -.
AUTHOR  E. Bertin (IAP & Leiden & ESO)
VERSION 28/11/98
 ***/
void  preanalyse(int no, objliststruct *objlist, int analyse_type)

  {
   objstruct	*obj = &objlist->obj[no];
   pliststruct	*pixel = objlist->plist, *pixt;
   PIXTYPE	peak, cpeak, val, cval, minthresh, thresht;
   double	thresh,thresh2, t1t2,darea,
		mx,my, mx2,my2,mxy, rv, tv,
		xm,ym, xm2,ym2,xym,
		temp,temp2, theta,pmx2,pmy2;
   int		x, y, xmin,xmax, ymin,ymax,area2, fdnpix, dnpix;
  

/*-----  initialize stacks and bounds */
  thresh = obj->dthresh;
  if (PLISTEXIST(dthresh))
    minthresh = BIG;
  fdnpix = dnpix = 0;
  rv = 0.0;
  peak = cpeak = -BIG;
  ymin = xmin = 2*MAXPICSIZE;    /* to be really sure!! */
  ymax = xmax = 0;

/*-----  integrate results */
  for (pixt=pixel+obj->firstpix; pixt>=pixel; pixt=pixel+PLIST(pixt,nextpix))
    {
    x = PLIST(pixt, x);
    y = PLIST(pixt, y);
    val=PLISTPIX(pixt, dvalue);
    if (cpeak < (cval=PLISTPIX(pixt, cdvalue)))
      cpeak = cval;
    if (PLISTEXIST(dthresh) && (thresht=PLISTPIX(pixt, dthresh))<minthresh)
      minthresh = thresht;
    if (peak < val)
      peak = val;
    rv += cval;
    if (xmin > x)
      xmin = x;
    if (xmax < x)
      xmax = x;
    if (ymin > y)
      ymin = y;
    if (ymax < y)
      ymax = y;
    fdnpix++;
    }    

  if (PLISTEXIST(dthresh))
    obj->dthresh = thresh = minthresh;

/* copy some data to "obj" structure */

  obj->fdnpix = (LONG)fdnpix;
  obj->fdflux = (float)rv;
  obj->fdpeak = cpeak;
  obj->dpeak = peak;
  obj->xmin = xmin;
  obj->xmax = xmax;
  obj->ymin = ymin;
  obj->ymax = ymax;

  if (analyse_type & ANALYSE_FULL)
    {
    mx = my = tv = 0.0;
    mx2 = my2 = mxy = 0.0;
    thresh2 = (thresh + peak)/2.0;
    area2 = 0;
    for (pixt=pixel+obj->firstpix; pixt>=pixel; pixt=pixel+PLIST(pixt,nextpix))
      {
      x = PLIST(pixt,x)-xmin;	/* avoid roundoff errors on big images */
      y = PLIST(pixt,y)-ymin;	/* avoid roundoff errors on big images */
      cval = PLISTPIX(pixt, cdvalue);
      tv += (val = PLISTPIX(pixt, dvalue));
      if (val>thresh)
        dnpix++;
      if (val > thresh2)
        area2++;
      mx += cval * x;
      my += cval * y;
      mx2 += cval * x*x;
      my2 += cval * y*y;
      mxy += cval * x*y;
      }

/*----- compute object's properties */
    xm = mx / rv;			/* mean x */
    ym = my / rv;			/* mean y */

/*-- In case of blending, use previous barycenters */
    if ((analyse_type&ANALYSE_ROBUST) && (obj->flag&OBJ_MERGED))
      {
       double	xn,yn;

      xn = obj->mx-xmin;
      yn = obj->my-ymin;
      xm2 = mx2 / rv + xn*xn - 2*xm*xn;
      ym2 = my2 / rv + yn*yn - 2*ym*yn;
      xym = mxy / rv + xn*yn - xm*yn - xn*ym;
      xm = xn;
      ym = yn;
      }
    else
      {
      xm2 = mx2 / rv - xm * xm;	/* variance of x */
      ym2 = my2 / rv - ym * ym;	/* variance of y */
      xym = mxy / rv - xm * ym;	/* covariance */
      }

/* Handle fully correlated x/y (which cause a singularity...) */
    if ((temp2=xm2*ym2-xym*xym)<0.00694)
      {
      xm2 += 0.0833333;
      ym2 += 0.0833333;
      temp2 = xm2*ym2-xym*xym;
      obj->singuflag = 1;
      }
    else
      obj->singuflag = 0;

    if ((fabs(temp=xm2-ym2)) > 0.0)
      theta = atan2(2.0 * xym,temp) / 2.0;
    else
      theta = PI/4.0;

    temp = sqrt(0.25*temp*temp+xym*xym);
    pmy2 = pmx2 = 0.5*(xm2+ym2);
    pmx2+=temp;
    pmy2-=temp;

    obj->dnpix = (obj->flag & OBJ_OVERFLOW)? obj->fdnpix:(LONG)dnpix;
    obj->dflux = tv;
    obj->mx = xm+xmin;	/* add back xmin */
    obj->my = ym+ymin;	/* add back ymin */
    obj->mx2 = xm2;
    obj->my2 = ym2;
    obj->mxy = xym;
    obj->a = (float)sqrt(pmx2);
    obj->b = (float)sqrt(pmy2);
    obj->theta = theta*180.0/PI;

    obj->cxx = (float)(ym2/temp2);
    obj->cyy = (float)(xm2/temp2);
    obj->cxy = (float)(-2*xym/temp2);

    darea = (double)area2 - dnpix;
    t1t2 = thresh/thresh2;
    if (t1t2>0.0 && !prefs.dweight_flag)
      {
      obj->abcor = (darea<0.0?darea:-1.0)/(2*PI*log(t1t2<1.0?t1t2:0.99)
	*obj->a*obj->b);
      if (obj->abcor>1.0)
        obj->abcor = 1.0;
      }
    else
      obj->abcor = 1.0;
    }

  return;
  }

