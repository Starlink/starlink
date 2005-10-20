/*
 				catout.c

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*
*	Part of:	SExtractor
*
*	Author:		E.BERTIN (IAP, Leiden observatory & ESO)
*                       PETER W. DRAPER (STARLINK, University of Durham)
*
*	Contents:	functions for output of catalog data.
*
*	Last modify:	20/07/99: (EB)
*                       24/11/98: (PWD)
*                                 Change ASCII_SKYCAT to output
*                                 real column names. Stop skycattail
*                                 from being written after file is
*                                 closed.
*                       20/03/00: (PWD):
*                                 Added initialisation of obj.rad FLAGs.
*                       08/01/03: Fixed a double free of pointer.
*	Last modify:	16/12/2002
*                                 (EB) for 2.3.
*	Last modify:	26/11/2003
*	Last modify:	25/08/2005
*
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*/
#ifdef HAVE_CONFIG_H
#include        "config.h"
#endif

#include	<math.h>
#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>

#include	"define.h"
#include	"globals.h"
#include	"prefs.h"
#include	"fits/fitscat.h"
#include	"param.h"
#include	"sexhead.h"
#include	"sexhead1.h"
#include	"sexheadsc.h"

catstruct	*fitscat;
tabstruct	*objtab;
FILE		*ascfile;
char		*buf;

/******************************* readcatparams *******************************/
/*
Read the catalog config file
*/
void	readcatparams(char *filename)
  {
   keystruct	*key;
   FILE		*infile;
   char		str[MAXCHAR], *keyword, *sstr;
   int		i, size;

/* Prepare the OBJECTS tables*/
  objtab = new_tab("OBJECTS");

  if ((infile = fopen(filename,"r")) == NULL)
    error(EXIT_FAILURE, "*ERROR*: can't read ", filename);

/* Scan the catalog config file*/
  thecat.nparam = 0;
  while (fgets(str, MAXCHAR, infile))
    {
    sstr = str + strspn(str," \t");
    if (*sstr!=(char)'#' && *sstr!=(char)'\n')
      {
      keyword = strtok(sstr, " \t{[(\n\r");
      if (keyword &&
	(i = findkey(keyword,(char *)objkey,sizeof(keystruct)))!=RETURN_ERROR)
        {
        key = objkey+i;
        add_key(key, objtab, 0);
        *((char *)key->ptr) = (char)'\1';
        thecat.nparam++;
        if (key->naxis)
          {
          for (i=0; i<key->naxis; i++)
            key->naxisn[i] = 1;
          size=t_size[key->ttype];
          for (i=0; (sstr = strtok(NULL, " \t,;.)]}\r")) && *sstr!=(char)'#'
		&& *sstr!=(char)'\n'; i++)
            {
            if (i>=key->naxis)
              error(EXIT_FAILURE, "*Error*: too many dimensions for keyword ",
		keyword);
            if (!(size*=(key->naxisn[i]=atoi(sstr))))
              error(EXIT_FAILURE, "*Error*: wrong array syntax for keyword ",
		keyword);
            }
          key->nbytes = size;
          }
        }
      else
        warning(keyword, " catalog parameter unknown");
      }
    }

  fclose(infile);

/* Now we copy the flags to the proper structures */

  flagobj = outobj;
  flagobj2 = outobj2;
/* Differentiate between outobj and outobj2 vectors */
  memset(&outobj2, 0, sizeof(outobj2));
  updateparamflags();

/* Go back to multi-dimensional arrays for memory allocation */
  if (thecat.nparam)
    for (i=objtab->nkey, key=objtab->key; i--; key = key->nextkey) 
      if (key->naxis)
        {
/*------ Only outobj2 vectors are dynamic */
        if (!*((char **)key->ptr))
          {
          QMALLOC(*((char **)key->ptr), char, key->nbytes);
          key->ptr = *((char **)key->ptr);
          key->allocflag = 1;
          }
        }

  return;
  }


/***************************** updateparamflags ******************************/
/*
Update parameter flags according to their mutual dependencies.
*/
void	updateparamflags()

  {
   int	i;

/*------------------------------ Astrometry ---------------------------------*/
  FLAG(obj2.win_aw) |= FLAG(obj2.win_bw) | FLAG(obj2.win_polarw);
  FLAG(obj2.win_cxxw) |= FLAG(obj2.win_cyyw) | FLAG(obj2.win_cxyw);
  FLAG(obj2.win_thetas) |= FLAG(obj2.win_theta1950)
			| FLAG(obj2.win_theta2000);
  FLAG(obj2.win_thetaw) |= FLAG(obj2.win_thetas);

  FLAG(obj2.win_mx2w) |= FLAG(obj2.win_my2w)
			| FLAG(obj2.win_mxyw)
			| FLAG(obj2.win_thetaw) | FLAG(obj2.win_aw)
			| FLAG(obj2.win_cxxw);

  FLAG(obj2.win_a) |= FLAG(obj2.win_b) | FLAG(obj2.win_theta)
			| FLAG(obj2.win_polar);
  FLAG(obj2.win_cxx) |= FLAG(obj2.win_cyy)
			| FLAG(obj2.win_cxy);
  FLAG(obj2.win_mx2) |= FLAG(obj2.win_my2)
			| FLAG(obj2.win_mxy)
			| FLAG(obj2.win_a) | FLAG(obj2.win_cxx)
			| FLAG(obj2.win_mx2w);

  FLAG(obj2.winposerr_aw) |= FLAG(obj2.winposerr_bw);
  FLAG(obj2.winposerr_cxxw) |= FLAG(obj2.winposerr_cyyw)
			| FLAG(obj2.winposerr_cxyw);
  FLAG(obj2.winposerr_thetas) |= FLAG(obj2.winposerr_theta1950)
			| FLAG(obj2.winposerr_theta2000);
  FLAG(obj2.winposerr_thetaw) |= FLAG(obj2.winposerr_thetas);

  FLAG(obj2.winposerr_mx2w) |= FLAG(obj2.winposerr_my2w)
			| FLAG(obj2.winposerr_mxyw)
			| FLAG(obj2.winposerr_thetaw) | FLAG(obj2.winposerr_aw)
			| FLAG(obj2.winposerr_cxxw);

  FLAG(obj2.winposerr_a) |= FLAG(obj2.winposerr_b) | FLAG(obj2.winposerr_theta);
  FLAG(obj2.winposerr_cxx) |= FLAG(obj2.winposerr_cyy)
			| FLAG(obj2.winposerr_cxy);
  FLAG(obj2.winposerr_mx2) |= FLAG(obj2.winposerr_my2)
			| FLAG(obj2.winposerr_mxy)
			| FLAG(obj2.winposerr_a) | FLAG(obj2.winposerr_cxx)
			| FLAG(obj2.winposerr_mx2w);

  FLAG(obj2.winpos_alpha1950) |= FLAG(obj2.winpos_delta1950)
			| FLAG(obj2.win_theta1950)
			| FLAG(obj2.winposerr_theta1950);
  FLAG(obj2.winpos_alpha2000) |= FLAG(obj2.winpos_delta2000)
			| FLAG(obj2.winpos_alpha1950)
			| FLAG(obj2.win_theta2000)
			| FLAG(obj2.winposerr_theta2000);
  FLAG(obj2.winpos_alphas) |= FLAG(obj2.winpos_deltas)
			| FLAG(obj2.winpos_alpha2000);
  FLAG(obj2.winpos_xw) |= FLAG(obj2.winpos_yw)
			| FLAG(obj2.winpos_alphas);

  FLAG(obj2.poserr_aw) |= FLAG(obj2.poserr_bw);
  FLAG(obj2.poserr_cxxw) |= FLAG(obj2.poserr_cyyw) | FLAG(obj2.poserr_cxyw);
  FLAG(obj2.poserr_thetas) |= FLAG(obj2.poserr_theta1950)
				| FLAG(obj2.poserr_theta2000);
  FLAG(obj2.poserr_thetaw) |= FLAG(obj2.poserr_thetas);

  FLAG(obj2.poserr_mx2w) |= FLAG(obj2.poserr_my2w) | FLAG(obj2.poserr_mxyw)
			| FLAG(obj2.poserr_thetaw) | FLAG(obj2.poserr_aw)
			| FLAG(obj2.poserr_cxxw);

  FLAG(obj2.poserr_a) |= FLAG(obj2.poserr_b) | FLAG(obj2.poserr_theta);
  FLAG(obj2.poserr_cxx) |= FLAG(obj2.poserr_cyy) | FLAG(obj2.poserr_cxy);
  FLAG(obj.poserr_mx2) |= FLAG(obj.poserr_my2) | FLAG(obj.poserr_mxy)
			| FLAG(obj2.poserr_a) | FLAG(obj2.poserr_cxx)
			| FLAG(obj2.poserr_mx2w) | FLAG(obj2.winposerr_mx2w);

  FLAG(obj2.peakalpha1950) |= FLAG(obj2.peakdelta1950);
  FLAG(obj2.alpha1950) |= FLAG(obj2.delta1950) |  FLAG(obj2.theta1950)
			| FLAG(obj2.poserr_theta1950);
  FLAG(obj2.peakalpha2000) |= FLAG(obj2.peakdelta2000)
			| FLAG(obj2.peakalpha1950);
  FLAG(obj2.alpha2000) |= FLAG(obj2.delta2000) | FLAG(obj2.alpha1950)
			| FLAG(obj2.theta2000)
			| FLAG(obj2.poserr_theta2000);
  FLAG(obj2.peakalphas) |= FLAG(obj2.peakdeltas) | FLAG(obj2.peakalpha2000);
  FLAG(obj2.alphas) |= FLAG(obj2.deltas) | FLAG(obj2.alpha2000);
  FLAG(obj2.thetas) |= FLAG(obj2.theta1950) | FLAG(obj2.theta2000);
  FLAG(obj2.thetaw) |= FLAG(obj2.thetas);
  FLAG(obj2.aw) |= FLAG(obj2.bw) | FLAG(obj2.polarw);
  FLAG(obj2.cxxw) |= FLAG(obj2.cyyw) | FLAG(obj2.cxyw);

  FLAG(obj2.mx2w) |= FLAG(obj2.my2w) | FLAG(obj2.mxyw)
			| FLAG(obj2.thetaw) | FLAG(obj2.aw) | FLAG(obj2.cxxw)
			| FLAG(obj2.npixw) | FLAG(obj2.fdnpixw)
			| FLAG(obj2.fwhmw);
  
  FLAG(obj2.peakxw) |= FLAG(obj2.peakyw) | FLAG(obj2.peakalphas);
  FLAG(obj.peakx) |= FLAG(obj.peaky) | FLAG(obj2.peakxw);

  FLAG(obj2.mxw) |= FLAG(obj2.myw) | FLAG(obj2.mx2w) | FLAG(obj2.alphas)
		| FLAG(obj2.poserr_mx2w);
  FLAG(obj2.mamaposx) |= FLAG(obj2.mamaposy);
  FLAG(obj2.flux_win) |= FLAG(obj2.mag_win)|FLAG(obj2.magerr_win)
			    | FLAG(obj2.flux_win) | FLAG(obj2.fluxerr_win);
  FLAG(obj2.winpos_x) |= FLAG(obj2.winpos_y)
			| FLAG(obj2.winposerr_mx2) | FLAG(obj2.win_mx2)
			| FLAG(obj2.winpos_xw) | FLAG(obj2.win_flag)
			| FLAG(obj2.flux_win) |FLAG(obj2.winpos_niter);

/*------------------------------ Photometry ---------------------------------*/

  FLAG(obj2.fluxerr_best) |= FLAG(obj2.magerr_best);

  FLAG(obj2.flux_best) |= FLAG(obj2.mag_best) | FLAG(obj2.fluxerr_best);

  FLAG(obj2.hl_radius) |= FLAG(obj2.winpos_x);

  FLAG(obj2.flux_auto)  |= FLAG(obj2.mag_auto) | FLAG(obj2.magerr_auto)
			| FLAG(obj2.fluxerr_auto)
			| FLAG(obj2.kronfactor)
			| FLAG(obj2.flux_best)
			| FLAG(obj2.flux_radius)
			| FLAG(obj2.hl_radius);
  FLAG(obj2.flux_petro) |= FLAG(obj2.mag_petro) | FLAG(obj2.magerr_petro)
			| FLAG(obj2.fluxerr_petro)
			| FLAG(obj2.petrofactor);

  FLAG(obj2.fluxerr_isocor) |= FLAG(obj2.magerr_isocor)
				| FLAG(obj2.fluxerr_best);

  FLAG(obj2.flux_isocor) |= FLAG(obj2.mag_isocor) | FLAG(obj2.fluxerr_isocor)
			 | FLAG(obj2.flux_best);

  FLAG(obj2.flux_aper) |= FLAG(obj2.mag_aper)|FLAG(obj2.magerr_aper)
			    | FLAG(obj2.fluxerr_aper);

  FLAG(obj.flux_prof) |= FLAG(obj2.mag_prof)|FLAG(obj2.magerr_prof)
			    | FLAG(obj2.flux_prof) | FLAG(obj2.fluxerr_prof);

  FLAG(obj2.flux_galfit) |= FLAG(obj2.mag_galfit) | FLAG(obj2.magerr_galfit)
			    | FLAG(obj2.fluxerr_galfit);

/*---------------------------- External flags -------------------------------*/
  VECFLAG(obj.imaflag) |= VECFLAG(obj.imanflag);

/*------------------------------ PSF-fitting --------------------------------*/
  FLAG(obj2.poserraw_psf) |= FLAG(obj2.poserrbw_psf);
  FLAG(obj2.poserrcxxw_psf) |= FLAG(obj2.poserrcyyw_psf)
			| FLAG(obj2.poserrcxyw_psf);
  FLAG(obj2.poserrthetas_psf) |= FLAG(obj2.poserrtheta1950_psf)
				| FLAG(obj2.poserrtheta2000_psf);
  FLAG(obj2.poserrthetaw_psf) |= FLAG(obj2.poserrthetas_psf);

  FLAG(obj2.poserrmx2w_psf) |= FLAG(obj2.poserrmy2w_psf)
			| FLAG(obj2.poserrmxyw_psf)
			| FLAG(obj2.poserrthetaw_psf) | FLAG(obj2.poserraw_psf)
			| FLAG(obj2.poserrcxxw_psf);

  FLAG(obj2.poserra_psf) |= FLAG(obj2.poserrb_psf)
			| FLAG(obj2.poserrtheta_psf);
  FLAG(obj2.poserrcxx_psf) |= FLAG(obj2.poserrcyy_psf)
			| FLAG(obj2.poserrcxy_psf);
  FLAG(obj2.poserrmx2_psf) |= FLAG(obj2.poserrmy2_psf)
			| FLAG(obj2.poserrmxy_psf)
			| FLAG(obj2.poserra_psf) | FLAG(obj2.poserrcxx_psf)
			| FLAG(obj2.poserrmx2w_psf);

  FLAG(obj2.alpha1950_psf) |= FLAG(obj2.delta1950_psf)
			| FLAG(obj2.poserrtheta1950_psf);
  FLAG(obj2.alpha2000_psf) |= FLAG(obj2.delta2000_psf)
			| FLAG(obj2.alpha1950_psf)
			| FLAG(obj2.poserrtheta2000_psf);
  FLAG(obj2.alphas_psf) |= FLAG(obj2.deltas_psf) | FLAG(obj2.alpha2000_psf);

  FLAG(obj2.xw_psf) |= FLAG(obj2.yw_psf) | FLAG(obj2.poserrmx2w_psf)
			| FLAG(obj2.alphas_psf);

  FLAG(obj2.fluxerr_psf) |= FLAG(obj2.poserrmx2_psf) | FLAG(obj2.magerr_psf);

  FLAG(obj2.mx2_pc) |= FLAG(obj2.my2_pc) | FLAG(obj2.mxy_pc)
			| FLAG(obj2.a_pc) | FLAG(obj2.b_pc)
			| FLAG(obj2.theta_pc) | FLAG(obj2.vector_pc)
			| FLAG(obj2.gdposang) | FLAG(obj2.gdscale)
			| FLAG(obj2.gdaspect) | FLAG(obj2.flux_galfit)
			| FLAG(obj2.gde1) | FLAG(obj2.gde2)
			| FLAG(obj2.gbposang) | FLAG(obj2.gbscale)
			| FLAG(obj2.gbaspect) | FLAG(obj2.gbratio);

  FLAG(obj2.flux_psf) |= FLAG(obj2.mag_psf) | FLAG(obj2.x_psf)
			| FLAG(obj2.y_psf) | FLAG(obj2.xw_psf)
			| FLAG(obj2.fluxerr_psf)
			| FLAG(obj2.niter_psf)
			| FLAG(obj2.chi2_psf)
			| FLAG(obj2.mx2_pc);

/*-------------------------------- Others -----------------------------------*/
  FLAG(obj.fwhm) |= FLAG(obj2.fwhmw);

  FLAG(obj.iso[0]) |= FLAG(obj2.sprob);
  for (i=0; i<NISO; i++)
    FLAG(obj.iso[0]) |= FLAG(obj.iso[i]);

/* PWD: Average radii are all calculated if one is needed */
  for ( i = 0; i < NRAD; i++ ) {
     FLAG(obj.rad[0]) |= FLAG(obj.rad[i]);
  }

  return; 
  }


/********************************** initcat **********************************/
/*
Initialize the catalog header
*/
void	initcat(void)
  {
   keystruct	*key;
   int		i, n;

  if (prefs.cat_type == CAT_NONE)
    return;

  if (prefs.cat_type == ASCII_HEAD || prefs.cat_type == ASCII
      || prefs.cat_type == ASCII_SKYCAT)
    {
    if (prefs.pipe_flag)
      ascfile = stdout;
    else
      if (!(ascfile = fopen(prefs.cat_name, "w+")))
        error(EXIT_FAILURE,"*Error*: cannot open ", prefs.cat_name);
    update_tab(objtab);
    if (prefs.cat_type == ASCII_HEAD && (key = objtab->key))
      for (i=0,n=1; i++<objtab->nkey; key=key->nextkey)
        {
        if (*key->unit)
          fprintf(ascfile, "# %3d %-15.15s %-47.47s [%s]\n",
		n, key->name,key->comment, key->unit);
        else
          fprintf(ascfile, "# %3d %-15.15s %.47s\n",
		n, key->name,key->comment);
        n += key->nbytes/t_size[key->ttype];
        }
    else if (prefs.cat_type == ASCII_SKYCAT && (key = objtab->key))
      {
        /* PWD:  fixed to not use id ra dec mag, but the real names */
/*       if (objtab->nkey<3) */
/*         error(EXIT_FAILURE,"The SkyCat format requires at least 4 parameters:", */
/*               " Id Ra Dec Mag"); */
/*--- We add a tab between rows, as required by Skycat */
      fprintf(ascfile, skycathead, 8.0);
      for (i=0; i++<objtab->nkey; key=key->nextkey)
        {
          /*           if (i>4) { */
          fprintf(ascfile, "%s\t", key->name);
          /*           } */
          sprintf(gstr, "%s\t", key->printf);
          strcpy(key->printf, gstr);
        }
      fprintf(ascfile, "\n------------------\n");
      }
    }
  else
    {
    fitscat = new_cat(1);
    init_cat(fitscat);
    strcpy(fitscat->filename, prefs.cat_name);
    if (open_cat(fitscat, WRITE_ONLY) != RETURN_OK)
      error(EXIT_FAILURE,"*Error*: cannot open for writing ",prefs.cat_name);
    switch(prefs.cat_type)
      {
      case FITS_LDAC:
      case FITS_TPX:
/*------ Save a "pure" primary HDU */
        save_tab(fitscat, fitscat->tab);
        break;

      case FITS_10:
/*------ Add to the primary HDU extraction parameters */
        key = headkey1;
        while (*key->name)
          addkeyto_head(fitscat->tab, key++);
        save_tab(fitscat, fitscat->tab);
        break;
      default:
        error (EXIT_FAILURE, "*Internal Error*: Unknown FITS type in ",
		"initcat()");
      }
    }

  return;
  }


/********************************* reinitcat *********************************/
/*
Initialize the catalog header
*/
void	reinitcat(picstruct *field)
  {
   tabstruct	*tab, *asctab;
   keystruct	*key;

  if (prefs.cat_type == CAT_NONE)
    return;

  if (prefs.cat_type != ASCII_HEAD && prefs.cat_type != ASCII
      && prefs.cat_type != ASCII_SKYCAT)
    {
    update_tab(objtab);
    switch(prefs.cat_type)
      {
      case FITS_LDAC:
/*------ We create a dummy table (only used through its header) */
        QCALLOC(asctab, tabstruct, 1);
        asctab->headnblock = field->fitsheadsize/FBSIZE;
        QMALLOC(asctab->headbuf, char, asctab->headnblock*FBSIZE);
        memcpy(asctab->headbuf, field->fitshead, asctab->headnblock*FBSIZE);
        key = headkey;
        while (*key->name)
          addkeyto_head(asctab, key++);
        tab = new_tab("LDAC_IMHEAD");
        add_tab(tab, fitscat, 0);
        key = new_key("Field Header Card");
        key->ptr = asctab->headbuf;
        asctab->headbuf = NULL;
        free_tab(asctab);
        key->htype = H_STRING;
        key->ttype = T_STRING;
        key->nobj = 1;
        key->nbytes = 80*(fitsfind(key->ptr, "END     ")+1);
        key->naxis = 2;
        QMALLOC(key->naxisn, int, key->naxis);
        key->naxisn[0] = 80;
        key->naxisn[1] = key->nbytes/80;
        add_key(key, tab, 0);
        save_tab(fitscat, tab);
        strcpy(objtab->extname, "LDAC_OBJECTS");
        break;

      case FITS_TPX:
/*------ We create a dummy table (only used through its header) */
        QCALLOC(asctab, tabstruct, 1);
        asctab->headnblock = field->fitsheadsize/FBSIZE;
        QMALLOC(asctab->headbuf, char, asctab->headnblock*FBSIZE);
        memcpy(asctab->headbuf, field->fitshead, asctab->headnblock*FBSIZE);
        key = headkey;
        while (*key->name)
          addkeyto_head(asctab, key++);
        tab = new_tab("TPX_IMHEAD");
        add_tab(tab, fitscat, 0);
        key = new_key("Field Header Card");
        key->ptr = asctab->headbuf;
        asctab->headbuf = NULL;
        free_tab(asctab);
        key->htype = H_STRING;
        key->ttype = T_STRING;
        key->nobj = fitsfind(key->ptr, "END     ")+1;
        key->nbytes = 80;
        key->naxis = 1;
        QMALLOC(key->naxisn, int, key->naxis);
        key->naxisn[0] = 80;
        add_key(key, tab, 0);
        save_tab(fitscat, tab);
        strcpy(objtab->extname, "TPX_OBJECTS");
        break;

      case FITS_10:
/*------ Add to the primary HDU extraction parameters */
/*
        key = headkey1;
        while (*key->name)
          addkeyto_head(fitscat->tab, key++);
        save_tab(fitscat, fitscat->tab);
*/
        break;

      default:
        error (EXIT_FAILURE, "*Internal Error*: Unknown FITS type in ",
		"reinitcat()");
      }

    objtab->cat = fitscat;
    init_writeobj(fitscat, objtab, &buf);
    }

  return;
  }


/********************************* writecat **********************************/
/*
Write out in the catalog each one object.
*/
void	writecat(int n, objliststruct *objlist)
  {
  outobj = objlist->obj[n];

  switch(prefs.cat_type)
    {
    case FITS_10:
    case FITS_LDAC:
    case FITS_TPX:
      write_obj(objtab, buf);
      break;

    case ASCII:
    case ASCII_HEAD:
    case ASCII_SKYCAT:
      print_obj(ascfile, objtab);
      break;

    case CAT_NONE:
      break;

    default:
      error (EXIT_FAILURE, "*Internal Error*: Unknown catalog type", "");
    }

  return;
  }


/********************************** endcat ***********************************/
/*
Terminate the catalog output.
*/
void	endcat()
  {
   keystruct	*key;
   int		i;

  switch(prefs.cat_type)
    {
    case ASCII:
    case ASCII_HEAD:
      if (!prefs.pipe_flag)
        fclose(ascfile);
      break;

    case ASCII_SKYCAT:
      fprintf(ascfile, skycattail); /*PWD: modification here*/
      if (!prefs.pipe_flag)
        fclose(ascfile);
      break;

    case FITS_LDAC:
    case FITS_TPX:
    case FITS_10:
      free_cat(&fitscat,1);
      break;

    case CAT_NONE:
      break;

    default:
      break;
    }

/* Free allocated memory for arrays */
  key = objtab->key;
  for (i=objtab->nkey; i--; key=key->nextkey)
    if (key->naxis && key->allocflag)
      free(key->ptr);

  objtab->key = NULL;
  objtab->nkey = 0;
  free_tab(objtab);

  return;
  }


/******************************** reendcat ***********************************/
/*
Terminate the catalog output.
*/
void	reendcat()
  {
   keystruct	*key;
   tabstruct	*tab;
   OFF_T	pos;
   char		*head;

  switch(prefs.cat_type)
    {
    case ASCII:
    case ASCII_HEAD:
    case ASCII_SKYCAT:
      break;

    case FITS_LDAC:
    case FITS_TPX:
      end_writeobj(fitscat, objtab, buf);
      key = NULL;
      if (!(tab=fitscat->tab->prevtab)
	|| !(key=name_to_key(tab, "Field Header Card")))
        error(EXIT_FAILURE,"*Error*: cannot update table ", "ASCFIELD");
      head = key->ptr;
      fitswrite(head, "SEXNDET ", &thecat.ndetect,H_INT,T_LONG);
      fitswrite(head, "SEXNFIN ", &thecat.ntotal, H_INT,T_LONG);
      fitswrite(head, "SEXDATE ", thecat.ext_date, H_STRING, 0);
      fitswrite(head, "SEXTIME ", thecat.ext_time, H_STRING, 0);
      fitswrite(head, "SEXELAPS", &thecat.ext_elapsed, H_FLOAT, T_DOUBLE);
      QFTELL(fitscat->file, pos, fitscat->filename);
      QFSEEK(fitscat->file, tab->headpos, SEEK_SET, fitscat->filename);
      save_tab(fitscat, tab);
      QFSEEK(fitscat->file, pos, SEEK_SET, fitscat->filename);
      break;

    case FITS_10:
      end_writeobj(fitscat, objtab, buf);
      fitswrite(fitscat->tab->headbuf,"SEXNDET ",&thecat.ndetect,H_INT,T_LONG);
      fitswrite(fitscat->tab->headbuf,"SEXNFIN ",&thecat.ntotal, H_INT,T_LONG);
      QFTELL(fitscat->file, pos, fitscat->filename);
      QFSEEK(fitscat->file, fitscat->tab->headpos, SEEK_SET,fitscat->filename);
      save_tab(fitscat, fitscat->tab);
      QFSEEK(fitscat->file, pos, SEEK_SET, fitscat->filename);
      break;

    case CAT_NONE:
      break;

    default:
      break;
    }

  return;
  }


