/*
 				catout.c

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*
*	Part of:	SExtractor
*
*	Author:		E.BERTIN (IAP, Leiden observatory & ESO)
*                       Peter W. Draper (Starlink, University of Durham)
*
*	Contents:	functions for output of catalog data.
*
*	Last modify:	20/07/99: (EB)
*                       24/11/98: (PWD)
*                                 Change ASCII_SKYCAT to output
*                                 real column names. Stop skycattail
*                                 from being written after file is
*                                 closed.
*
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*/
#include	<math.h>
#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>

#include	"define.h"
#include	"globals.h"
#include	"fitscat.h"
#include	"param.h"
#include	"sexhead.h"
#include	"sexhead1.h"
#include	"sexheadsc.h"

catstruct	*fitscat;
tabstruct	*objtab;
FILE		*ascfile;

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
  cat.nparam = 0;
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
        cat.nparam++;
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
  if (cat.nparam)
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
			| FLAG(obj2.poserr_mx2w);

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
  FLAG(obj2.aw) |= FLAG(obj2.bw);
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

/*------------------------------ Photometry ---------------------------------*/

  FLAG(obj2.fluxerr_best) |= FLAG(obj2.magerr_best);

  FLAG(obj2.flux_best) |= FLAG(obj2.mag_best) | FLAG(obj2.fluxerr_best);

  FLAG(obj2.flux_auto)  |= FLAG(obj2.mag_auto) | FLAG(obj2.magerr_auto)
			| FLAG(obj2.fluxerr_auto)
			| FLAG(obj2.kronfactor)
			| FLAG(obj2.flux_best)
			| FLAG(obj2.flux_radius);

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

  return; 
  }


/********************************** initcat **********************************/
/*
Initialize the catalog header
*/
void	initcat(picstruct *field)
  {
   tabstruct	*tab, *asctab;
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
/*------ Save a "pure" primary HDU */
        save_tab(fitscat, fitscat->tab);
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

      case FITS_BINIMHEAD:
        break;

      case FITS_NOIMHEAD:
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

    objtab->cat = fitscat;
    init_writeobj(fitscat, objtab);
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
      write_obj(objtab);
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
   tabstruct	*tab;
   char		*head;
   int		i;

/* Free allocated memory for arrays */
  key = objtab->key;
  for (i=objtab->nkey; i--; key=key->nextkey)
    if (key->naxis && key->allocflag)
      free(key->ptr);

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
      objtab->key = NULL;
      objtab->nkey = 0;
      free_tab(objtab);
      break;

    case FITS_LDAC:
      end_writeobj(fitscat, objtab);
      if (!(tab=name_to_tab(fitscat, "LDAC_IMHEAD", 0))
	|| !(key=name_to_key(tab, "Field Header Card")))
        error(EXIT_FAILURE,"*Error*: cannot update table ", "ASCFIELD");
      head = key->ptr;
      fitswrite(head, "SEXNDET ", &cat.ndetect,H_INT,T_LONG);
      fitswrite(head, "SEXNFIN ", &cat.ntotal, H_INT,T_LONG);
      fitswrite(head, "SEXDATE ", cat.ext_date, H_STRING, 0);
      fitswrite(head, "SEXTIME ", cat.ext_time, H_STRING, 0);
      fitswrite(head, "SEXELAPS", &cat.ext_elapsed, H_FLOAT, T_DOUBLE);
      QFSEEK(fitscat->file, tab->bodypos-FBSIZE*tab->headnblock, SEEK_SET,
	fitscat->filename);
      save_tab(fitscat, tab);
      free_cat(fitscat,1);
      break;

    case FITS_10:
      end_writeobj(fitscat, objtab);
      fitswrite(fitscat->tab->headbuf, "SEXNDET ", &cat.ndetect,H_INT,T_LONG);
      fitswrite(fitscat->tab->headbuf, "SEXNFIN ", &cat.ntotal, H_INT,T_LONG);
      QFSEEK(fitscat->file, 0, SEEK_SET, fitscat->filename);
      save_tab(fitscat, fitscat->tab);
      free_cat(fitscat,1);
      break;

    case CAT_NONE:
      break;

    default:
      break;
    }

  objtab->key = NULL;
  objtab->nkey = 0;
  free_tab(objtab);

  return;
  }

