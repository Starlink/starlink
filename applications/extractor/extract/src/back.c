 /*
 				back.c

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*
*	Part of:	SExtractor
*
*	Author:		E.BERTIN, IAP & Leiden observatory
*
*	Contents:	functions dealing with background computation.
*
*	Last modify:	09/07/98
*
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*/

#include	<math.h>
#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>

#include	"define.h"
#include	"globals.h"
#include	"back.h"
#include	"field.h"

/******************************** makeback ***********************************/
/*
A background map is established from the frame itself; thus we need to make
at least one first pass through the data.
*/
void	makeback(picstruct *field)

  {
   backstruct	*backmesh, *bm;
   PIXTYPE	*buf, *buft, *bufpos;
   LONG		*histo;
   size_t	fcurpos, fcurpos2, bufsize, bufsize2, bufshift,
		size,meshsize,jumpsize;
   int		i,j,k,m,n, bin, step, nlines,
		lastbite, w,bw,bwx, bh, nx,ny,nb, x,y,h, offset, nlevels,
		lflag;
   float	*sig, qscale, cste;

  w = field->width;
  bw = field->backw;
  bh = field->backh;
  nx = field->nbackx;
  ny = field->nbacky;
  nb = field->nback;

  NFPRINTF(OUTPUT, "Setting up background map");

/* Decide if it is worth displaying progress each 16 lines */

  lflag = (field->width*field->backh >= (size_t)65536);

/* Save current position in file */

#ifdef MEMORY_READ
  prefs.memlink_imaoffset = 0;
#else
  QFTELL(fcurpos, field->file, field->filename);
#endif /* ifdef MEMORY_READ */

/* Allocate a correct amount of memory to store pixels */

  bufsize = (size_t)w*bh;
  meshsize = bufsize;
  nlines = 0;
  if (bufsize > (size_t)BACK_BUFSIZE)
    {
    nlines = BACK_BUFSIZE/w;
    step = (field->backh-1)/nlines+1;
    bufsize = (size_t)(nlines = field->backh/step)*w;
    bufshift = (step/2)*(size_t)w;
    jumpsize = (step-1)*(size_t)w;
    }

/* Allocate some memory */

  QMALLOC(backmesh, backstruct, nx);		/* background information */
  QMALLOC(buf, PIXTYPE, bufsize);		/* pixel buffer */
  QMALLOC(field->back, float, nb);		/* background map */
  QMALLOC(field->backline, PIXTYPE, w);		/* current background line */
  QMALLOC(field->sigma, float, nb);		/* sigma map */
  sig = field->sigma;

/* Loop over the data packets */

  for (j=0; j<ny; j++)
    {
    if (lflag && j)
      NPRINTF(OUTPUT, "\33[1M> Setting up background map at line:%5d\n\33[1A",
	      j*bh);
    if (!nlines)
      {
      if (j == ny-1 && field->npix%bufsize)
        bufsize = field->npix%bufsize;
#    ifdef MEMORY_READ
      memcpy(buf, prefs.memlink_ima+prefs.memlink_imaoffset,
	bufsize*sizeof(PIXTYPE));
      prefs.memlink_imaoffset += bufsize;
#    else
      readdata(field, buf, bufsize);
#    endif /* ifdef MEMORY_READ */
      backstat(backmesh, buf, bufsize,nx, w, bw);
      h = bufsize/w;
      bm = backmesh;
      bufpos = buf;
      bwx = bw;
      offset = w - bwx;
      for (m=0; m++<nx; bm++ , bufpos+=bwx)
        {
        if (m==nx && (lastbite=w%bwx))
          {
          bwx = lastbite;
          offset = w-bwx;
          }
        nlevels = bm->nlevels;
        QCALLOC(bm->histo, LONG, nlevels);
        histo = bm->histo;
        qscale = bm->qscale;
        cste = 0.499999 - bm->qzero/qscale;
        buft = bufpos;
        for (y=h; y--;)
          {
          for (x=bwx; x--;)
            {
            bin = (int)(*(buft++)/qscale + cste);
            if (bin>=0 && bin<nlevels)
              (*(histo+bin))++;
            }
          buft += offset;
          }
        }
      }
    else
      {
#    ifdef MEMORY_READ
      fcurpos2 = prefs.memlink_imaoffset;
#    else
      QFTELL(fcurpos2, field->file, field->filename);
#    endif /* ifdef MEMORY_READ */

      if (j == ny-1 && (n=field->height%field->backh))
        {
        meshsize = n*(size_t)w;
        nlines = BACK_BUFSIZE/w;
        step = (n-1)/nlines+1;
        bufsize = (nlines = n/step)*(size_t)w;
        bufshift = (step/2)*(size_t)w;
        jumpsize = (step-1)*(size_t)w;
        free(buf);
        QMALLOC(buf, PIXTYPE, bufsize);		/* pixel buffer */
        }

#    ifdef MEMORY_READ
      prefs.memlink_imaoffset += bufshift;
#    else
      QFSEEK(field->file, bufshift*field->bytepix, SEEK_CUR, field->filename);
#    endif /* ifdef MEMORY_READ */

      buft = buf;
      for (i=nlines; i--;)
        {
#      ifdef MEMORY_READ
        memcpy(buft, prefs.memlink_ima+prefs.memlink_imaoffset,
		w*sizeof(PIXTYPE));
        prefs.memlink_imaoffset += w;
#      else
        readdata(field, buft, w);
#      endif /* ifdef MEMORY_READ */
        if (i)
#        ifdef MEMORY_READ
          prefs.memlink_imaoffset += jumpsize;
#        else
          QFSEEK(field->file, jumpsize*field->bytepix, SEEK_CUR,
		field->filename);
#        endif /* ifdef MEMORY_READ */
        buft += w;
        }
      backstat(backmesh,buf,bufsize, nx, w, bw);
#    ifdef MEMORY_READ
      prefs.memlink_imaoffset = fcurpos2;
#    else
      QFSEEK(field->file, fcurpos2, SEEK_SET, field->filename);
#    endif /* ifdef MEMORY_READ */

      bm = backmesh;
      for (m=nx; m--; bm++)
        QCALLOC(bm->histo, LONG, bm->nlevels);
      for(size=meshsize, bufsize2=bufsize; size>0; size -= bufsize2)
        {
        if (bufsize2>size)
          bufsize2 = size;
#      ifdef MEMORY_READ
        memcpy(buf, prefs.memlink_ima+prefs.memlink_imaoffset,
		bufsize2*sizeof(PIXTYPE));
        prefs.memlink_imaoffset += bufsize2;
#      else
        readdata(field, buf, bufsize2);
#      endif /* ifdef MEMORY_READ */
        h = bufsize2/w;
        bm = backmesh;
        bufpos = buf;
        bwx = bw;
        offset = w - bwx;
        for (m=0; m++<nx; bm++, bufpos+=bwx)
          {
          if (m==nx && (lastbite=w%bwx))
            {
            bwx = lastbite;
            offset = w-bwx;
            }
          nlevels = bm->nlevels;
          histo = bm->histo;
          qscale = bm->qscale;
          cste = 0.499999 - bm->qzero/qscale;
          buft = bufpos;
          for (y=h; y--;)
            {
            for (x=bwx; x--;)
              {
              bin = (int)(*(buft++)/qscale + cste);
              if (bin>=0 && bin<nlevels)
                (*(histo+bin))++;
              }
            buft += offset;
            }
          }
        }
      }
      
    bm = backmesh;
    for (m=0; m<nx; m++, bm++)
      {
      k = m+nx*j;
      backguess(bm, field->back+k, sig+k);
      free(bm->histo);
      }
    }

/* Free memory */
  free(buf);
  free(backmesh);

/* Go back to the original position */

#ifdef MEMORY_READ
  prefs.memlink_imaoffset = 0;
#else
  QFSEEK(field->file, fcurpos, SEEK_SET, field->filename);
#endif /* ifdef MEMORY_READ */

/* Median-filter and check suitability of the background map */
  NFPRINTF(OUTPUT, "Filtering background map");
  filterback(field);

/* Compute 2nd derivatives along the y-direction */
  if (field->flags^WEIGHT_FIELD)
    {
    NFPRINTF(OUTPUT, "Computing backgound d-map");
    field->dback = makebackspline(field, field->back);
    NFPRINTF(OUTPUT, "Computing backgound-noise d-map");
    field->dsigma = makebackspline(field, field->sigma);
    }

  return;
  }


/******************************** backstat **********************************/
/*
compute robust statistical estimators in multiple background meshes.
*/
void	backstat(backstruct *backmesh, PIXTYPE *buf, size_t bufsize,
			int n, int w, int bw)

  {
   backstruct	*bm;
   int		i, m,h,x,y, npix, offset, lastbite;
   double	pix, sig, mean, sigma, step;
   PIXTYPE	*buft, *bufpos, lcut, hcut;

  h = bufsize/w;
  bm = backmesh;
  offset = w - bw;
  bufpos = buf;
  step = sqrt(2/PI)*QUANTIF_NSIGMA/QUANTIF_AMIN;
  for (m = n; m--; bm++,bufpos+=bw)
    {
    mean = sigma = 0.0;
    if (!m && (lastbite=w%bw))
      {
      bw = lastbite;
      offset = w-bw;
      }
    buft = bufpos;
    for (y=h; y--;)
      {
      for (x=bw; x--;)
        {
        mean += (pix = *(buft++));
        sigma += pix*pix;
        }
      buft += offset;
      }
    npix = bw*h;
    mean /= (double)npix;
    sigma = (sig = sigma/npix - mean*mean)>0.0? sqrt(sig):0.0;
    lcut = bm->lcut = (PIXTYPE)(mean - 2.0*sigma);
    hcut = bm->hcut = (PIXTYPE)(mean + 2.0*sigma);
    mean = sigma = 0.0;
    npix = 0;
    buft = bufpos;
    for (y=h; y--;)
      {
      for (x=bw; x--;)
        {
        pix = *(buft++);
        if (pix>=lcut && pix<=hcut)
          {
          mean += pix;
          sigma += pix*pix;
          npix++;
          }
        }
      buft += offset;
      }
    bm->npix = npix;
    mean /= (double)npix;
    sig = sigma/npix - mean*mean;
    sigma = sig>0.0 ? sqrt(sig):0.0;
    bm->mean = mean;
    bm->sigma = sigma;
    if ((bm->nlevels = (int)(step*npix+1)) > QUANTIF_NMAXLEVELS)
      bm->nlevels = QUANTIF_NMAXLEVELS;
    bm->qscale = sigma>0.0? 2*QUANTIF_NSIGMA*sigma/bm->nlevels : 1.0;
    bm->qzero = mean - QUANTIF_NSIGMA*sigma;
    }

  return;
  }


/******************************* filterback *********************************/
/*
Median filtering of the background map to remove the contribution from bright
sources.
*/
void	filterback(picstruct *field)

  {
   int		i,px,py, np, nx, npxm,npxp, npym,npyp, dpx,dpy, x,y;
   float	*back, *mask, *all, *allt;

  nx = field->nbackx;
  np = field->nback;
  npxm = field->nbackfx/2;
  npxp = field->nbackfx - npxm;
  npym = field->nbackfy/2;
  npyp = field->nbackfy - npym;
  npym *= nx;
  npyp *= nx;

  QMALLOC(mask, float, field->nbackfx*field->nbackfy);
  QMALLOC(all, float, np);
  back = field->back;

  for (py=0; py<np; py+=nx)
    for (px=0; px<nx; px++)
      {
      i=0;
      for (dpy = -npym; dpy< npyp; dpy+=nx)
        for (dpx = -npxm; dpx < npxp; dpx++)
          {
          y = py+dpy;
          x = px+dpx;
          if (y>=0 && y<np && x>=0 && x<nx)
            mask[i++] = back[x+y];
          }
      all[px+py] = hmedian(mask, i);
      }

  memcpy(back, all, np*sizeof(float));
  field->backmean = hmedian(all, np);

  back = field->sigma;
  memcpy(all, back, np*sizeof(float));
  for (py=0; py<np; py+=nx)
    for (px=0; px<nx; px++)
      {
      i=0;
      for (dpy = -npym; dpy< npyp; dpy+=nx)
        for (dpx = -npxm; dpx < npxp; dpx++)
          {
          y = py+dpy;
          x = px+dpx;
          if (y>=0 && y<np && x>=0 && x<nx)
            mask[i++] = back[x+y];
          }
      all[px+py] = hmedian(mask, i);
      }

  memcpy(back, all, np*sizeof(float));
  field->backsig = hmedian(all, np);

  if (field->backsig<=0.0)
    {
    allt = all+np;
    for (i=np; i-- && *(--allt)>0.0;);
    if (i>=0 && i<(np-1))
      field->backsig = hmedian(allt+1, np-1-i);
    else
      {
      if (field->flags&(DETECT_FIELD|MEASURE_FIELD))
        warning("Image contains mainly constant data; ",
		"I'll try to cope with that...");
      field->backsig = 1.0;
      }
    }
  free(mask);
  free(all);

  return;
  }


/******************************* backguess **********************************/
/*
Estimate the background from a histogram;
*/
float	backguess(backstruct *bkg, float *mean, float *sigma)

#define	EPS	(1e-4)	/* a small number */

  {
   LONG		*histo, *hilow, *hihigh, *histot;
   unsigned long lowsum, highsum, sum;
   double	ftemp, mea, sig, sig1, med, dpix;
   int		i, n, lcut,hcut, nlevelsm1, pix;

  histo = bkg->histo;
  hcut = nlevelsm1 = bkg->nlevels-1;
  lcut = 0;

  sig = 10.0*nlevelsm1;
  sig1 = 1.0;
  for (n=100; n-- && (sig>=0.1) && (fabs(sig/sig1-1.0)>EPS);)
    {
    sig1 = sig;
    sum = mea = sig = 0.0;
    lowsum = highsum = 0;
    histot = hilow = histo+lcut;
    hihigh = histo+hcut;
    for (i=lcut; i<=hcut; i++)
      {
      if (lowsum<highsum)
        lowsum += *(hilow++);
      else 
        highsum +=  *(hihigh--);
      sum += (pix = *(histot++));
      mea += (dpix = (double)pix*i);
      sig += dpix*i;
      }

    med = (hihigh-histo)+0.5+((double)highsum-lowsum)/(2.0*(*hilow>*hihigh?
						*hilow:*hihigh));
    if (sum)
      {
      mea /= (double)sum;
      sig = sig/sum - mea*mea;
      }
    sig = sig>0.0?sqrt(sig):0.0;
    lcut = (ftemp=med-3.0*sig)>0.0 ?(int)(ftemp>0.0?ftemp+0.5:ftemp-0.5):0;
    hcut = (ftemp=med+3.0*sig)<nlevelsm1 ?(int)(ftemp>0.0?ftemp+0.5:ftemp-0.5)
								: nlevelsm1;
    }

 *mean = fabs(sig)>0.0? (fabs(bkg->sigma/(sig*bkg->qscale)-1) < 0.0 ?
			    bkg->qzero+mea*bkg->qscale
			    :(fabs((mea-med)/sig)< 0.3 ?
			      bkg->qzero+(2.5*med-1.5*mea)*bkg->qscale
			     :bkg->qzero+med*bkg->qscale))
                       :bkg->qzero+mea*bkg->qscale;

  *sigma = sig*bkg->qscale;


  return *mean;
  }


/******************************** localback *********************************/
/*
Compute Local background if possible.
*/
float	localback(picstruct *field, objstruct *obj)

  {
   static backstruct	backmesh;
   int			bxmin,bxmax, bymin,bymax, ixmin,ixmax, iymin,iymax,
			bxnml,bynml, oxsize,oysize, npix,
			i, x,y, bin, w,sh, bmn, pbs;
   float		bkg, bqs,cste;
   LONG			*bmh;
   PIXTYPE		*backpix, *bp, *strip, *st,
			pix;

  strip = field->strip;
  w = field->width;
  sh = field->stripheight;
  pbs = prefs.pback_size;

/* Estimate background in a 'rectangular annulus' around the object */
  oxsize = obj->xmax - obj->xmin + 1;
  oysize = obj->ymax - obj->ymin + 1;
  bxnml = oxsize<w/2? oxsize/4 : (w-oxsize)/4;
  bynml = oysize<field->height/2? oysize/4 : (field->height-oysize)/4;
  bxmin = (ixmin = obj->xmin - bxnml) - pbs;
  bxmax = (ixmax = obj->xmax+1 + bxnml) + pbs;
  bymin = (iymin = obj->ymin - bynml) - pbs;
  bymax = (iymax = obj->ymax+1 + bynml) + pbs;

  if (bymin>=field->ymin && bymax<field->ymax
	&& bxmin>=0 && bxmax<w)
    {
    npix = (bxmax-bxmin)*(bymax-bymin) - (ixmax-ixmin)*(iymax-iymin);

    QMALLOC(backpix, PIXTYPE, npix);
    bp = backpix;

/*--store all the pixels*/
    npix = 0;
    for (y=bymin; y<bymax; y++)
      {
      st = strip + (y%sh)*w + bxmin;
      for (x=pbs; x--;)
        if ((pix=*(st++))>-BIG)
          {
          *(bp++) = pix;
          npix++;
          }
      st += ixmax-ixmin;
      for (x=pbs; x--;)
        if ((pix=*(st++))>-BIG)
          {
          *(bp++) = pix;
          npix++;
          }
      }

    for (y=bymin; y<iymin; y++)
      {
      st = strip + (y%sh)*w + ixmin;
      for (x=ixmax-ixmin; x--;)
        if ((pix=*(st++))>-BIG)
          {
          *(bp++) = pix;
          npix++;
          }
      }
    for (y=iymax; y<bymax; y++)
      {
      st = strip + (y%sh)*w + ixmin;
      for (x=ixmax-ixmin; x--;)
        if ((pix=*(st++))>-BIG)
          {
          *(bp++) = pix;
          npix++;
          }
      }

    backstat(&backmesh, backpix, npix, 1, 1, 1);
    QCALLOC(backmesh.histo, LONG, backmesh.nlevels);
    bmh = backmesh.histo;
    bmn = backmesh.nlevels;
    cste = 0.499999 - backmesh.qzero/(bqs = backmesh.qscale);
    bp = backpix;
    for (i=npix; i--;)
      {
      bin = (int)(*(bp++)/bqs + cste);
      if (bin>=0 && bin<bmn)
        (*(bmh+bin))++;
      }

    free(backpix);
    backguess(&backmesh, &bkg, &obj->sigbkg);
    obj->bkg += (obj->dbkg = bkg);
    free(backmesh.histo);
    }
  else
    {
    obj->dbkg = bkg = 0.0;
    obj->sigbkg = field->backsig;
    }

  return bkg;
  }


/************************************ back ***********************************/
/*
return background at position x,y (linear interpolation between background
map vertices).
*/
PIXTYPE	back(picstruct *field, int x, int y)

  {
   int		nx,ny, xl,yl, pos;
   double	dx,dy, cdx;
   float	*b, b0,b1,b2,b3;

  b = field->back;
  nx = field->nbackx;
  ny = field->nbacky;

  dx = (double)x/field->backw - 0.5;
  dy = (double)y/field->backh - 0.5;
  dx -= (xl = (int)dx);
  dy -= (yl = (int)dy);

  if (xl<0)
    {
    xl = 0;
    dx -= 1.0;
    }
  else if (xl>=nx-1)
    {
    xl = nx<2 ? 0 : nx-2;
    dx += 1.0;
    }

  if (yl<0)
    {
    yl = 0;
    dy -= 1.0;
    }
  else if (yl>=ny-1)
    {
    yl = ny<2 ? 0 : ny-2;
    dy += 1.0;
    }

  pos = yl*nx + xl;
  cdx = 1 - dx;

  b0 = *(b+=pos);		/* consider when nbackx or nbacky = 1 */
  b1 = nx<2? b0:*(++b);
  b2 = ny<2? *b:*(b+=nx);
  b3 = nx<2? *b:*(--b);

  return (PIXTYPE)((1-dy)*(cdx*b0 + dx*b1) + dy*(dx*b2 + cdx*b3));
  }


/******************************* makebackspline ******************************/
/*
Pre-compute 2nd derivatives along the y direction at background nodes.
*/
float *makebackspline(picstruct *field, float *map)

  {
   int		x,y, nbx,nby,nbym1;
   float	*dmap,*dmapt,*mapt, *u, temp;

  nbx = field->nbackx;
  nby = field->nbacky;
  nbym1 = nby - 1;
  QMALLOC(dmap, float, field->nback);
  for (x=0; x<nbx; x++)
    {
    mapt = map+x;
    dmapt = dmap+x;
    if (nby>1)
      {
      QMALLOC(u, float, nbym1);	/* temporary array */
      *dmapt = *u = 0.0;	/* "natural" lower boundary condition */
      mapt += nbx;
      for (y=1; y<nbym1; y++, mapt+=nbx)
        {
        temp = -1/(*dmapt+4);
        *(dmapt += nbx) = temp;
        temp *= *(u++) - 6*(*(mapt+nbx)+*(mapt-nbx)-2**mapt);
        *u = temp;
        }
      *(dmapt+=nbx) = 0.0;	/* "natural" upper boundary condition */
      for (y=nby-2; y--;)
        {
        temp = *dmapt;
        dmapt -= nbx;
        *dmapt = (*dmapt*temp+*(u--))/6.0;
        }
      free(u);
      }
    else
      *dmapt = 0.0;
    }

  return dmap;
  }


/******************************* subbackline *********************************/
/*
Interpolate background at line y (bicubic spline interpolation between
background map vertices) and subtract it from the current line.
*/
void	subbackline(picstruct *field, int y, PIXTYPE *line)

  {
   int		i,j,x,yl, nbx,nbxm1,nby, nx,width, ystep, changepoint;
   float	dx,dx0,dy,dy3, cdx,cdy,cdy3, temp, xstep,
		*node,*nodep,*dnode, *blo,*bhi,*dblo,*dbhi, *u;
   PIXTYPE	*backline;

  nbx = field->nbackx;
  nbxm1 = nbx - 1;
  nby = field->nbacky;
  if (nby > 1)
    {
    dy = (float)y/field->backh - 0.5;
    dy -= (yl = (int)dy);
    if (yl<0)
      {
      yl = 0;
      dy -= 1.0;
      }
    else if (yl>=nby-1)
      {
      yl = nby<2 ? 0 : nby-2;
      dy += 1.0;
      }
/*-- Interpolation along y for each node */
    cdy = 1 - dy;
    dy3 = (dy*dy*dy-dy);
    cdy3 = (cdy*cdy*cdy-cdy);
    ystep = nbx*yl;
    blo = field->back + ystep;
    bhi = blo + nbx;
    dblo = field->dback + ystep;
    dbhi = dblo + nbx;
    QMALLOC(node, float, nbx);	/* Interpolated background */
    nodep = node;
    for (x=nbx; x--;)
      *(nodep++) = cdy**(blo++) + dy**(bhi++) + cdy3**(dblo++) + dy3**(dbhi++);

/*-- Computation of 2nd derivatives along x */
    QMALLOC(dnode, float, nbx);	/* 2nd derivative along x */
    if (nbx>1)
      {
      QMALLOC(u, float, nbxm1);	/* temporary array */
      *dnode = *u = 0.0;	/* "natural" lower boundary condition */
      nodep = node+1;
      for (x=nbxm1; --x; nodep++)
        {
        temp = -1/(*(dnode++)+4);
        *dnode = temp;
        temp *= *(u++) - 6*(*(nodep+1)+*(nodep-1)-2**nodep);
        *u = temp;
        }
      *(++dnode) = 0.0;	/* "natural" upper boundary condition */
      for (x=nbx-2; x--;)
        {
        temp = *(dnode--);
        *dnode = (*dnode*temp+*(u--))/6.0;
        }
      free(u);
      dnode--;
      }
    }
  else
    {
/*-- No interpolation and no new 2nd derivatives needed along y */
    node = field->back;
    dnode = field->dback;
    }

/*-- Interpolation along x */
  width = field->width;
  backline = field->backline;
  if (nbx>1)
    {
    nx = field->backw;
    xstep = 1.0/nx;
    changepoint = nx/2;
    dx  = (xstep - 1)/2;	/* dx of the first pixel in the row */
    dx0 = ((nx+1)%2)*xstep/2;	/* dx of the 1st pixel right to a bkgnd node */
    blo = node;
    bhi = node + 1;
    dblo = dnode;
    dbhi = dnode + 1;
    for (x=i=0,j=width; j--; i++, dx += xstep)
      {
      if (i==changepoint && x>0 && x<nbxm1)
        {
        blo++;
        bhi++;
        dblo++;
        dbhi++;
        dx = dx0;
        }
      cdx = 1 - dx;
      *(line++) -= (*(backline++) = (PIXTYPE)(cdx*(*blo+(cdx*cdx-1)**dblo)
			+ dx*(*bhi+(dx*dx-1)**dbhi)));
      if (i==nx)
        {
        x++;
        i = 0;
        }
      }
    }
  else
    for (j=width; j--;)
      *(line++) -= (*(backline++) = (PIXTYPE)*node);

  if (nby>1)
    {
    free(node);
    free(dnode);
    }

  return;
  }


/******************************* backrmsline ********************************
PROTO   void backrmsline(picstruct *field, int y, PIXTYPE *line)
PURPOSE Bicubic-spline interpolation of the background noise along the current
        scanline (y).
INPUT   Measurement or detection field pointer,
        Current line position. 
        Where to put the data. 
OUTPUT  -.
NOTES   Most of the code is a copy of subbackline(), for optimization reasons.
AUTHOR  E. Bertin (IAP & Leiden & ESO)
VERSION 02/02/98
 ***/
void	backrmsline(picstruct *field, int y, PIXTYPE *line)

  {
   int		i,j,x,yl, nbx,nbxm1,nby, nx,width, ystep, changepoint;
   float	dx,dx0,dy,dy3, cdx,cdy,cdy3, temp, xstep,
		*node,*nodep,*dnode, *blo,*bhi,*dblo,*dbhi, *u;

  nbx = field->nbackx;
  nbxm1 = nbx - 1;
  nby = field->nbacky;
  if (nby > 1)
    {
    dy = (float)y/field->backh - 0.5;
    dy -= (yl = (int)dy);
    if (yl<0)
      {
      yl = 0;
      dy -= 1.0;
      }
    else if (yl>=nby-1)
      {
      yl = nby<2 ? 0 : nby-2;
      dy += 1.0;
      }
/*-- Interpolation along y for each node */
    cdy = 1 - dy;
    dy3 = (dy*dy*dy-dy);
    cdy3 = (cdy*cdy*cdy-cdy);
    ystep = nbx*yl;
    blo = field->sigma + ystep;
    bhi = blo + nbx;
    dblo = field->dsigma + ystep;
    dbhi = dblo + nbx;
    QMALLOC(node, float, nbx);	/* Interpolated background */
    nodep = node;
    for (x=nbx; x--;)
      *(nodep++) = cdy**(blo++) + dy**(bhi++) + cdy3**(dblo++) + dy3**(dbhi++);

/*-- Computation of 2nd derivatives along x */
    QMALLOC(dnode, float, nbx);	/* 2nd derivative along x */
    if (nbx>1)
      {
      QMALLOC(u, float, nbxm1);	/* temporary array */
      *dnode = *u = 0.0;	/* "natural" lower boundary condition */
      nodep = node+1;
      for (x=nbxm1; --x; nodep++)
        {
        temp = -1/(*(dnode++)+4);
        *dnode = temp;
        temp *= *(u++) - 6*(*(nodep+1)+*(nodep-1)-2**nodep);
        *u = temp;
        }
      *(++dnode) = 0.0;	/* "natural" upper boundary condition */
      for (x=nbx-2; x--;)
        {
        temp = *(dnode--);
        *dnode = (*dnode*temp+*(u--))/6.0;
        }
      free(u);
      dnode--;
      }
    }
  else
    {
/*-- No interpolation and no new 2nd derivatives needed along y */
    node = field->sigma;
    dnode = field->dsigma;
    }

/*-- Interpolation along x */
  width = field->width;
  if (nbx>1)
    {
    nx = field->backw;
    xstep = 1.0/nx;
    changepoint = nx/2;
    dx  = (xstep - 1)/2;	/* dx of the first pixel in the row */
    dx0 = ((nx+1)%2)*xstep/2;	/* dx of the 1st pixel right to a bkgnd node */
    blo = node;
    bhi = node + 1;
    dblo = dnode;
    dbhi = dnode + 1;
    for (x=i=0,j=width; j--; i++, dx += xstep)
      {
      if (i==changepoint && x>0 && x<nbxm1)
        {
        blo++;
        bhi++;
        dblo++;
        dbhi++;
        dx = dx0;
        }
      cdx = 1 - dx;
      *(line++) = (PIXTYPE)(cdx*(*blo+(cdx*cdx-1)**dblo)
			+ dx*(*bhi+(dx*dx-1)**dbhi));
      if (i==nx)
        {
        x++;
        i = 0;
        }
      }
    }
  else
    for (j=width; j--;)
      *(line++) = (PIXTYPE)*node;

  if (nby>1)
    {
    free(node);
    free(dnode);
    }

  return;
  }


/********************************* copyback **********************************/
/*
Copy sub-structures related to background procedures (mainly freeing memory).
*/
void	copyback(picstruct *infield, picstruct *outfield)

  {
  QMEMCPY(infield->back, outfield->back, float, infield->nback);
  QMEMCPY(infield->dback, outfield->dback, float, infield->nback);
  QMEMCPY(infield->sigma, outfield->sigma, float, infield->nback);
  QMEMCPY(infield->dsigma, outfield->dsigma, float, infield->nback);
  QMEMCPY(infield->backline, outfield->backline, PIXTYPE, infield->width);

  return;
  }


/********************************* endback ***********************************/
/*
Terminate background procedures (mainly freeing memory).
*/
void	endback(picstruct *field)

  {
  free(field->back);
  free(field->dback);
  free(field->sigma);
  free(field->dsigma);
  free(field->backline);

  return;
  }
