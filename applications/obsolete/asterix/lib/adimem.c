/*  Basic block control
 *
 *  adix_bb_init        - Initialise BB control structure
 *  adix_bb_new         - Allocate and initialise a new BB
 *  adix_bb_alloc       - Allocate a unit from a basic block
 *  adix_bb_nalloc      - Allocate N units from a basic block
 *  adix_bb_free	- Free a unit in a basic block
 */

#include <string.h>
#include <stdio.h>

#include "asterix.h"
#include "aditypes.h"
#include "adimem.h"
#include "adikrnl.h"
#include "adierror.h"
#include "adicface.h"

void strx_exit( ADIstatus status );

#define CHAR_BIT 8
/* #define MEMORY_TRACING  1 */

#ifdef MEMORY_TRACING
#define ADI__MXMTR      1024
static
  char          *allocs[ADI__MXMTR];
static
  short         ainuse[ADI__MXMTR];
static
  int           nafree = ADI__MXMTR;
#endif

static
  int           ADI_G_nblk;             /* Global allocated block count */
ADIblockPtr     ADI_G_blks[ADI__MXBLK]; /* Block address array */
static
  long          ADI_G_nbyte = 0;        /* Global ADI memory count */
static
  long          ADI_G_nalloc = 0;       /* Number of allocations */


void adix_mem_free( void *ptr, size_t nb, ADIstatus status )
  {
  if ( _ok(status) && ptr )
    {
#ifdef MEMORY_TRACING
{
int i,found=0;
for( i=0; i<ADI__MXMTR && ! found; )
  if ( allocs[i] == ptr )
    found = 1;
  else
    i++;
if ( found )
  {
  allocs[i] = NULL;
  nafree++;
  ainuse[nafree-1] = i;
  }
else
{char *cp = (char *)ptr;
  printf( "Illegal memory release - pointer not registered %p\n",cp );}
}
#endif

    free(ptr);
    ADI_G_nbyte -= nb;
    ADI_G_nalloc--;
    }
  }


char *adix_mem_alloc( size_t nb, ADIstatus status )
  {
  char  *newm = NULL;

  if ( _ok(status) )
    {
    if ( nb )
      {
      ADI_G_nbyte += nb;
      ADI_G_nalloc++;
      newm = (char *) malloc( nb );
#ifdef MEMORY_TRACING
if ( newm )
  {
  if ( nafree )
    {
    allocs[ainuse[nafree-1]] = newm; nafree--;
    }
  else
    printf( "Memory tracing ran out of slots - results unreliable\n" );
  }
#endif
      if ( ! newm )
	adic_setec( ADI__OUTMEM, status );
      }
    else
      adic_setecs( ADI__INVARG,
		   "Attempted to allocate zero bytes of memory", status );
    }

  return newm;
  }


/* Set up a basic block control structure */

void adix_bb_init( ADIblockCtrlPtr ctrl, int code, size_t size,
                   int nunit, ADIobj clsid, ADIstatus status )
  {
  if ( !_ok(status) )                   /* Check status on entry */
    return;

  if ( nunit % CHAR_BIT )              /* Check alloc unit multiple of CHAR_BIT */
    {
    adic_seteti( "NBIT", CHAR_BIT );
    adic_setecs( SAI__ERROR, "Allocation cluster should be multiple of ^NBIT", status );
    }
  else
    {
    ctrl->clas = code;
    if ( clsid != ADI__nullid )
      {
      ctrl->cdef = _cdef_data(clsid);
      ctrl->cdef->selfid = clsid;
      }
    else
      {
      ctrl->cdef = NULL;
      }

    ctrl->size = size;
    ctrl->nunit = nunit;
    ctrl->f_block = ADI__nullid;
    }
  }


/* Initialise a new basic block, allowing the caller to override the
 * default cluster size
 */
ADIblockID adix_bb_new( ADIblockCtrl *ctrl, int nunit, ADIstatus status )
  {
  ADIblockID       iblk = ADI__nullid;
  ADIblockPtr	   newb = NULL;
  int              n8bit;

  if ( ADI_G_nblk < ADI__MXBLK )
    {
    newb = (ADIblockPtr) adix_mem_alloc(sizeof(ADIblock),status);

    if ( newb ) {
      newb->ctrl = ctrl;
      newb->nunit = _MAX(nunit,ctrl->nunit);
      newb->nfree = newb->nunit;
      newb->data = adix_mem_alloc(ctrl->size*newb->nunit,status);
      n8bit = (newb->nunit-1)/CHAR_BIT + 1;
      newb->used = (unsigned char *) adix_mem_alloc(n8bit,status);

      if ( newb->used )
	memset( newb->used, 0, n8bit );

      iblk = ADI_G_nblk;                  /* Add block to block list */
      ADI_G_blks[ADI_G_nblk++] = newb;

      newb->next = ctrl->f_block;         /* Insert new block into chain */
      ctrl->f_block = iblk;
      }
    else {
      adic_setetc( "CLS", ctrl->cdef->name, 99 );
      adic_setecs( ADI__OUTMEM, "Unable to allocate new block for class /^CLS/", status );
      }
    }
  else
    adic_setecs( ADI__OUTMEM, "Unable to allocate any more primitive blocks", status );

  return iblk;
  }


ADIobj adix_bb_nalloc( ADIblockCtrlPtr ctrl, ADIboolean scalar,
                       int nval, ADIstatus status )
  {
  ADIblockPtr      	bptr;
  ADIboolean            found = ADI__false;
  int                   i8, j8, ibit;
  ADIobj                newid;          /* New identifier */
  ADIblockID            iblk;           /* Allocation block identifier */
  int                   nneed =         /* Number of used[] slots needed */
                 (nval-1) / CHAR_BIT + 1;
  unsigned char         uchar;
  int                   obno;           /* Object number within block */
  char                  lonib;

  static char nib_fbit[15] =            /* First bit zero in nibble, counting
                                           from the left, LSB is number 0 */
    {0,1,0,2,0,1,0,3,0,1, 0, 2, 0, 1, 0};

  static short bit_mask[] =
    {1,2,4,8,16,32,64,128,256,512,1024};

  iblk = ctrl->f_block;                 /* First allocation block */

  while ( (iblk!=ADI__nullid)           /* Look for block with free slots */
          && ! found )                  /* and sufficient slots */
    {
    if ( (ADI_G_blks[iblk]->nunit>nval)
      && (ADI_G_blks[iblk]->nfree>nval))
      {
      bptr = ADI_G_blks[iblk];		/* Locate the block */

      if ( nval > 1 )			/* Must check contiguous for multiple */
        {				/* allocation */
        int         ncontig;

	for( i8=bptr->nunit/CHAR_BIT; i8; i8-- )
          {
          j8 = i8;
          ncontig = 0;

	  while ( i8 && ! bptr->used[i8] )
	    {
	    ncontig++; i8--;
	    }

	  if ( ncontig >= nneed ) {
	    i8 = j8 - ncontig + 1;
	    found = ADI__true;
	    break;
            }
          }
        }
      else
        {
        for( i8=0;                            /* Scan block to find unused slot */
          (bptr->used[i8] == 0xFF) && i8 < (ctrl->nunit/CHAR_BIT); i8++ );
        found = ADI__true;
        }
      }
    else
      iblk = ADI_G_blks[iblk]->next;
    }

  if ( ! found )                        /* Allocate a new block, using */
    {
    iblk = adix_bb_new( ctrl, nval, 	/* big enough to hold request */
                       status );
    bptr = ADI_G_blks[iblk];
    i8 = 0;
    }

  if ( nval == 1 )                      /* Single element to allocate */
    {
    uchar = bptr->used[i8];
    lonib = uchar & 0xF;                /* Extract low nibble */

    if ( lonib != 0xF )                 /* Any bits free in this nibble */
      ibit = nib_fbit[lonib];
    else
      ibit = 4 + nib_fbit[uchar>>4];

    obno = i8*CHAR_BIT + ibit;          /* Object number in block data */

    bptr->used[i8] |= bit_mask[ibit];   /* Mark object as allocated */

    }
  else                                  /* Multiple allocation */
    {
    obno = i8*CHAR_BIT;

    for( j8=0; j8<(nval/CHAR_BIT);     /* Loop over whole used slots */
         i8++, j8++ )
      bptr->used[j8] = 0xFF;

    if ( nval % CHAR_BIT )             /* Number allocated not divisible */
      {                                 /* by CHAR_BIT */
      uchar = 0;
      for( ibit=0;
           ibit<(nval%CHAR_BIT); ibit++ )
        uchar |= bit_mask[ibit];

      bptr->used[i8] = uchar;
      }
    }

  bptr->nfree -= nval;                  /* Decrease free count */

  _FORM_ID(newid,iblk,obno);            /* Form identifier */

  if ( (ctrl->clas > KT_CODE_han)       /* Non-kernel scalar object? */
                      && scalar )
    newid = adix_newhan( newid,
	  ADI__false, status );

  return newid;
  }


ADIobj adix_bb_alloc( ADIblockCtrl *ctrl, ADIstatus status )
  {
  return adix_bb_nalloc( ctrl, ADI__true, 1, status );
  }


void adix_bb_free( ADIobj *id, int nval, ADIstatus status )
  {
  static short bit_mask[] =
    {1,2,4,8,16,32,64,128,256,512,1024};

  ADIblockPtr 	bptr;
  int		i8,j8,ibit;
  int		iobj;

  _chk_stat;                            /* Check status on entry */

  bptr = _ID_BLOCK(*id);		/* Locate basic block */
  iobj = _ID_SLOT(*id);			/* and slot number */

  i8 = iobj / CHAR_BIT;		        /* Find allocation first cell */

  if ( nval > 1 )                       /* Multiple bit reset */
    {
    for( j8=i8; j8<(nval/CHAR_BIT);     /* Loop over whole used slots */
         i8++, j8++ )
      bptr->used[j8] = 0x00;            /* Reset all bits */

    for( ibit=0; ibit<(nval%CHAR_BIT);  /* Loop over partially used slot */
         ibit++ )
      bptr->used[i8] &=                 /* Reset allocated bit */
          (bit_mask[ibit] ^ 0xFF);
    }
  else                                  /* Single bit to mask */
    {
    ibit = iobj % CHAR_BIT;             /* Identify the bit */

    bptr->used[i8] &=                   /* Reset allocated bit */
          (bit_mask[ibit] ^ 0xFF);

    }
  bptr->nfree += nval;			/* Keep counter up to date */

  *id = ADI__nullid;			/* Reset identifier */
  }


void adix_bb_freeb( ADIblockPtr bptr, ADIstatus status )
  {
  adix_mem_free( bptr->data,
                 bptr->ctrl->size*bptr->nunit,status);

  adix_mem_free( bptr->used,
                 (bptr->nunit-1) / CHAR_BIT + 1,
                 status);
  }


void adix_mem_begin( void )
  {
  int           i;

  for( i=0; i<ADI__MXBLK; i++ )
    ADI_G_blks[i] = NULL;
  ADI_G_nblk = 0;

#ifdef MEMORY_TRACING
  for( i=0; i<ADI__MXMTR; i++ )
    {
    allocs[i] = NULL;
    ainuse[i] = ADI__MXMTR - 1 - i;
    }
#endif
  }

void adix_mem_end( ADIstatus status )
  {
  int           i;

strx_exit( status );
  for( i=0; i<ADI_G_nblk; i++ )
    {
    adix_bb_freeb( ADI_G_blks[i], status );
    adix_mem_free( ADI_G_blks[i], sizeof(ADIblock), status );
    }

#ifdef MEMORY_TRACING
{
int i,j;
for( i=0, j=0; i<ADI__MXMTR; i++ )
  if ( allocs[i] )
    allocs[j++] = allocs[i];
for( ; j<ADI__MXMTR; j++ )
  allocs[j] = NULL;
}
#endif

  if ( ADI_G_nbyte )
    printf( "ADI left %ld bytes in %ld allocations...\n",
            ADI_G_nbyte, ADI_G_nalloc );
  }
