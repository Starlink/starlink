 /*
 				assoc.c

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*
*	Part of:	SExtractor
*
*	Author:		E.BERTIN, IAP & Leiden observatory
*
*	Contents:	Routines for catalog-associations.
*
*	Last modify:	29/11/98
*
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*/

#include	<math.h>
#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>

#include	"define.h"
#include	"globals.h"
#include	"assoc.h"

/********************************* comp_assoc ********************************/
/*
Comparison function for sort_assoc().
*/
int	comp_assoc(const void *i1, const void *i2)
  {
   float	*f1,*f2;

  f1 = (float *)i1 + 1;
  f2 = (float *)i2 + 1;
  if (*f1<*f2)
    return -1;
  else return (*f1==*f2)?0:1;
  }


/********************************* sort_assoc ********************************/
/*
Make the presentation histogram, order the assoc-list and build the hash-table.
*/
void  sort_assoc(picstruct *field, assocstruct *assoc)

  {
   int		comp_assoc(const void *i1, const void *i2);
   float	*list, rad;
   int		i,j, step,nobj, *hash;

  step = assoc->ncol;
  nobj = assoc->nobj;
  list = assoc->list;
  qsort(assoc->list, assoc->nobj, step*sizeof(float), comp_assoc);
/* Build the hash table that contains the first object in the sorted list */
/* which may be in reach from the current scanline */
  QMALLOC(assoc->hash, int, field->height);
  list = assoc->list+1;	/* This is where the 1st y coordinate is stored */
  hash = assoc->hash;
  rad = assoc->radius;
  for (i=0, j=0; i<field->height; i++)
    {
/*-- For safety, we keep a 1-pixel margin */
    for (;j<nobj && (int)(*list+rad+1.5)<i; j++, list+=step);
/*-- We use -1 as a code identifying lines with no objects */
    *(hash++) = (j==nobj || (int)(*list-rad-0.5)>i) ? -1 : j;
    }

  return;
  }


/********************************* load_assoc ********************************/
/*
Read an assoc-list, and returns a pointer to the new assoc struct (or NULL if
no list was found).
*/
assocstruct  *load_assoc(char *filename)

  {
   assocstruct	*assoc;
   FILE		*file;
   float	*list, val;
   char		str[MAXCHAR], str2[MAXCHAR], *sstr;
   int		*data,
		i,ispoon,j,k,l, ncol, ndata, nlist, size,spoonsize,
		xindex,yindex,mindex;

  if (!(file = fopen(filename, "r")))
    return NULL;

  NFPRINTF(OUTPUT, "Reading ASSOC input-list...");
  for (i=0; fgets(str, MAXCHAR, file);)
    {
/*-- Examine current input line (discard empty and comment lines) */
    if (!*str || strchr("#\t\n",*str))
      continue;

    if (!i)
      {
      strcpy(str2, str);
/*---- Let's count the number of columns in the first line */
      for (ncol=0; strtok(ncol?NULL:str2, " \t\v\n\r\f"); ncol++);
      if (!ncol)
        error(EXIT_FAILURE, "*Error*: empty line in ", filename);
/*---- Build a look-up table containing the ordering of column data */
      QCALLOC(data, int, ncol);
      k = 1;
      for (j=0; j<prefs.nassoc_data && k<=prefs.assoc_size; j++)
        if ((l=prefs.assoc_data[j]) && --l<ncol)
          data[l] = k++;
      ndata = k-1;
      if (!ndata)
        {
        ndata = ncol;
        if (prefs.assoc_size<ndata)
          ndata = prefs.assoc_size;
        for (j=0; j<ndata; j++)
          data[j] = j+1;
        }
      if (ndata<prefs.assoc_size)
        {
        sprintf(gstr, "no more than %d ASSOC parameters available: ", ncol);
        warning("VECTOR_ASSOC redimensioned: ", gstr);
        prefs.assoc_size = ndata;
        }

      if ((xindex = prefs.assoc_param[0]-1) >= ncol) 
        error(EXIT_FAILURE, "*Error*: ASSOC_PARAMS #1 exceeds the number of ",
		"fields in the ASSOC file");
      if ((yindex = prefs.assoc_param[1]-1) >= ncol) 
        error(EXIT_FAILURE, "*Error*: ASSOC_PARAMS #2 exceeds the number of ",
		"fields in the ASSOC file");
      if (prefs.nassoc_param>2)
        {
        if ((mindex = prefs.assoc_param[2]-1) >= ncol)
          error(EXIT_FAILURE,"*Error*: ASSOC_PARAMS #3 exceeds the number of ",
		"fields in the ASSOC file");
        }
      else
        {
        mindex = -1;
        /* PWD: change here -- prefs.assoc_type == ASSOC_FIRST;*/
        prefs.assoc_type = ASSOC_FIRST;
        }

      nlist = ndata+3;

/*---- Allocate memory for the ASSOC struct and the filtered list */
      QMALLOC(assoc, assocstruct, 1);
      ispoon = ASSOC_BUFINC/(nlist*sizeof(float));
      spoonsize = ispoon*nlist;
      QMALLOC(assoc->list, float, size = spoonsize);
      list = assoc->list;
      }
    else  if (!(i%ispoon))
      {
      QREALLOC(assoc->list, float, size += spoonsize);
      list = assoc->list + i*nlist;
      }

    if (!(++i%1000))
      {
      sprintf(str2, "Reading input list... (%d objects)", i);
      NFPRINTF(OUTPUT, str2);
      }

/*-- Read the data normally */
    *(list+2) = 0.0;
    for (sstr = str, j=0; j<ncol; j++)
      {
      val = (float)strtod(sstr, &sstr);
      if (j==xindex)
        *list = val;
      else if (j==yindex)
        *(list+1) = val;
      else if (j==mindex)
        *(list+2) = val;
      if (k=data[j])
        *(list+2+k) = val;
      }
    list += nlist;
    }

  fclose(file);
  free(data);
  QREALLOC(assoc->list, float, i*nlist);
  assoc->nobj = i;
  assoc->radius = prefs.assoc_radius;
  assoc->ndata = ndata;
  assoc->ncol = nlist;

  return assoc;
  }


/********************************* init_assoc ********************************/
/*
Initialize the association procedure.
*/
void	init_assoc(picstruct *field)

  {
   assocstruct	*assoc;

/* Load the assoc-list */
  if (!(assoc = field->assoc = load_assoc(prefs.assoc_name)))
    error(EXIT_FAILURE, "*Error*: Assoc-list file not found: ",
	prefs.assoc_name);

/* Sort the assoc-list by y coordinates, and build the hash table */
  sort_assoc(field, assoc);

/* Where data go for the current output pattern*/
  assoc->data = outobj2.assoc;

  return;
  }


/********************************** end_assoc ********************************/
/*
Save the ANN weights and free memory related to the assoc operations.
*/
void	end_assoc(picstruct *field)

  {
/* Free the assoc-list */
  if (field->assoc)
    {
    free((field->assoc)->list);
    free((field->assoc)->hash);
    free(field->assoc);
    }

  return;
  }


/********************************** do_assoc *********************************/
/*
Perform the association task for a source and return the number of IDs.
*/
int	do_assoc(picstruct *field, float x, float y)
  {
   assocstruct	*assoc;
   double	aver;
   float	dx,dy, dist, rad, rad2, comp, wparam,
		*list, *input, *data;
   int		h, step, i, flag, iy, nobj;

  assoc = field->assoc;
/* Need to initialize the array */
  memset(assoc->data, 0, prefs.assoc_size*sizeof(float));
  aver = 0.0;

  if (prefs.assoc_type == ASSOC_MIN || prefs.assoc_type == ASSOC_NEAREST)
    comp = BIG;
  else if (prefs.assoc_type == ASSOC_MAX)
    comp = -BIG;

  iy = (int)(y+0.499999);
  if (iy<0 || iy>=field->height)
    return 0;
/* If there is already no candidate in hash table, no need to go further */
  if ((h=assoc->hash[iy])<0)
    return 0;
/* Now loop over possible candidates */
  nobj = assoc->nobj;
  step = assoc->ncol;
  list = assoc->list + step*h;
  rad = assoc->radius;
  rad2 = rad*rad;
  for (flag = 0; (h++<nobj && *(list+1)-rad<y); list+=step)
    {
    dx = *list - x;
    dy = *(list+1) - y;
    if ((dist=dx*dx+dy*dy)<rad2)
      {
      flag++;
      input = list+3;
      if (prefs.assoc_type == ASSOC_FIRST)
        {
        memcpy(assoc->data, input, assoc->ndata*sizeof(float));
        return 1;
        }
      wparam = *(list+2);
      data = assoc->data;
      switch(prefs.assoc_type)
        {
        case ASSOC_NEAREST:
          if (dist<comp)
            {
            memcpy(data, input, assoc->ndata*sizeof(float));
            comp = dist;
            }
          break;
        case ASSOC_MEAN:
          aver += wparam;
          for (i=assoc->ndata; i--;)
            *(data++) += *(input++)*wparam;
          break;
        case ASSOC_MAGMEAN:
          wparam = fabs(wparam)<99.0?DEXP(-0.4*wparam): 0.0;
          aver += wparam;
          for (i=assoc->ndata; i--;)
            *(data++) += *(input++)*wparam;
          break;
        case ASSOC_SUM:
          for (i=assoc->ndata; i--;)
            *(data++) += *(input++);
          break;
        case ASSOC_MAGSUM:
          for (i=assoc->ndata; i--;)
            *(data++) += fabs(wparam=*(input++))<99.0? DEXP(-0.4*wparam):0.0;
          break;
        case ASSOC_MIN:
          if (wparam<comp)
            {
            memcpy(data, input, assoc->ndata*sizeof(float));
            comp = wparam;
            }
          break;
        case ASSOC_MAX:
          if (wparam>comp)
            {
            memcpy(data, input, assoc->ndata*sizeof(float));
            comp = wparam;
            }
          break;
        default:
          error(EXIT_FAILURE, "*Internal Error*: unknown ASSOC type in ",
		"pixlearn()");
        }
      }
    }

/* No candidate found? exit! */
  if (!flag)
    return 0;

/* Terminate the computation of the mean */
  if (prefs.assoc_type == ASSOC_MEAN || prefs.assoc_type == ASSOC_MAGMEAN)
    {
    if (aver<1e-30)
      return 0;
    data = assoc->data;
    for (i=assoc->ndata; i--;)
      *(data++) /= aver;
    }

  if (prefs.assoc_type == ASSOC_MAGSUM)
    {
    data = assoc->data;
    for (i=assoc->ndata; i--;)
      *(data++) = *data>0.0? -2.5*log10(*data):99.0;
    }

  return flag;
  }

