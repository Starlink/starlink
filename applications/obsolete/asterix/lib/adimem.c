/*
*  Name:
*     adimem

*  Purpose:
*     Provides management of dynamic memory

*  Language:
*     ANSI C

*  Description:
*     This module controls the allocation of fixed size dynamic data items
*     within the ADI system.
*
*     ADI allocates dynamic data in blocks of items of the same type.
*     Each ADI data type has an allocation control structure. This
*     contains the information required by adimem to allocate instances
*     of that type, namely the size of the item and the number of items
*     which should be allocated per data block by default. Data objects
*     are referenced using an ADI identifier of type ADIobj. This object
*     contains the block number, and the slot number within that block.
*
*     The main routine used by external software is ADImemAllocObj which
*     allocates a contiguous sequence of objects of a specified type and
*     number. There are 3 possibilities depending on the number of items
*     which are required,
*
*       1) The number is less than or equal to the default allocation size.
*	   ADI looks for a block already dedicated to the specified type
*	   which contains enough vacant slots. If one is not found, a new
*	   block is allocated. In this kind of block allocation is tracked
*	   per object. For objects which are smaller than the object index
*	   type ADIidIndex, a bit array is allocated to store the allocation
*	   status of the objects in the block. Where the object size is
*	   at least as big as ADIidIndex a linked list is maintained.
*
*	2) The number is greater than the default allocation size, but
*	   smaller than the maximum number of slots per block (32768).
*	   A block is allocated with the number of items requested. The
*	   block is dedicated to that request - its items are allocated
*	   and freed from memory in one unit and so object tracking is
*	   no required.
*
*	3) The number is greater than the maximum number of slots per
*	   block. A 'master' block is allocated, which is the root of
* 	   the allocation, and a slave block for each group of 32768
*	   (or part thereof) items. Neither slave nor master blocks
*	   require object tracking. The overhead with this method
*	   is not great - one block structure per 32768 items. For
*	   integers or reals this is one block (22 bytes) per 131072
*	   bytes
*
*     Blocks allocated by these three strategies are distinguished by
*     the value of the 'master' structure member. If the value is -1,
*     the block is of the first type - the different object tracking
*     strategies are distinuished by the value of the allocation bit
*     array address. If the 'master' value points to the block itself
*     then the block is a master block - any other value denotes a
*     slave block.

*  Implementation Deficiencies:
*     If unsigned shorts were use for slot numbers, then the block
*     overhead on large allocations would be halved. If the same thing
*     was done for block numbers, the total memory addressable by ADI
*     would also increase.

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (JET-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     12 Jan 1995 (DJA):
*        Original version.
*     {enter_changes_here}
*/

#include <string.h>
#include <stdio.h>
#include <limits.h>

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

ADIblockPtr     ADI_G_blks[ADI__MXBLK]; /* Block address array */
static
  long          ADI_G_nbyte = 0;        /* Global ADI memory count */
static
  long          ADI_G_nalloc = 0;       /* Number of allocations */


void ADImemFree( void *ptr, size_t nb, ADIstatus status )
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


char *ADImemAlloc( size_t nb, ADIstatus status )
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


/*
 * Finds the offset from id1 to id2 in elements of the blocks they reference
 * If the block id of id2 is different from that of id1, then the former
 * must be a slave block of the latter
 */
ADIinteger ADImemIdOff( ADIobj id1, ADIobj id2, ADIstatus status )
  {
  ADIblock	*b1 = _ID_BLOCK(id1);
  ADIidIndex	s1 = _ID_SLOT(id1);
  ADIblock	*b2 = _ID_BLOCK(id2);
  ADIidIndex	s2 = _ID_SLOT(id2);
  ADIinteger	off = 0;

  if ( b1 != b2 ) {
    if ( b2->master != _ID_IBLK(id1) )
      adic_setecs( ADI__FATAL, "Illegal comparison of memory identifiers", status );
    else
      off = (b2->data - b1->data) / b1->size + s2 - s1;
    }
  else
    off = s2 - s1;

  return off;
  }

/*
 * Add an offset to a block/slot identifier. If the offset takes the
 * routine past the end of the block's allocated data, then the
 * block must be part of a master/slave grouping.
 */
ADIobj ADImemIdAddOff( ADIobj id, ADIinteger off, ADIstatus status )
  {
  ADIidIndex	s = _ID_SLOT(id);
  ADIobj	rval = ADI__nullid;
  ADIinteger	news = s + off;
  ADIidIndex	curb = _ID_IBLK(id);

/* Offset moves data address outside this block? */
  while ( (news > ADI_G_blks[curb]->nunit) && _ok(status) ) {
    ADIblock	*bptr = ADI_G_blks[curb];

/* If this is not a master/slave block then we have an error */
    if ( bptr->master == ADI__nullid )
      adic_setecs( ADI__INVARG, "Illegal operation on memory identifier",
		   status );
    else {
      news -= bptr->nunit;
      curb = bptr->next;
      }
    }

  if ( _ok(status) )
    _FORM_ID(rval,curb,(ADIidIndex) news);

  return rval;
  }


/* Set up a basic block control structure */

void ADImemInitBlock( ADIblockCtrlPtr ctrl, int code, size_t size,
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
      ctrl->cdef = NULL;

    ctrl->size = size;
    ctrl->nunit = nunit;
    ctrl->f_block = ADI__nullid;
    }
  }

void ADImemAllocBlock( ADIclassDef *cdef, ADIblock **newb,
		       ADIidIndex *iblk, ADIstatus status )
  {
  ADIblock	*bptr;
  ADIlogical	found = ADI__false;	/* Found a block slot? */
  ADIidIndex	jblk;

/* Initialise */
  *iblk = ADI__nullid;
  *newb = NULL;

/* Allocate a block slot */
  for( jblk=0; (jblk<ADI__MXBLK) && ! found; ) {
    if ( ADI_G_blks[jblk] )
      jblk++;
    else
      found = ADI__true;
    }

/* Found a block slot? */
  if ( found ) {

/* Allocate the block header */
    bptr = ADI_G_blks[jblk] =
	(ADIblock *) ADImemAlloc(sizeof(ADIblock),status);

/* Allocation went ok? */
    if ( bptr ) {

/* Add to this type's list of blocks */
      bptr->cdef = cdef;
      bptr->size = cdef->alloc.size;
      bptr->next = cdef->alloc.f_block;
      bptr->master = ADI__nullid;
      cdef->alloc.f_block = jblk;

/* Set return values */
      *iblk = jblk;
      *newb = bptr;
      }
    else
      adic_setecs( ADI__OUTMEM,
	"Unable to allocate any more primitive block descriptors", status );
    }
  else
    adic_setecs( ADI__OUTMEM,
	"Maximum number of basic blocks exceeded", status );
  }


/* Initialise a new basic block, allowing the caller to override the
 * default cluster size
 */
ADIidIndex ADImemNewBlock( ADIclassDef *cdef, ADIinteger nunit,
			   ADIstatus status )
  {
  ADIidIndex       iblk;
  ADIblock	   *newb;

/* Allocate a new basic block */
  ADImemAllocBlock( cdef, &newb, &iblk, status );

/* Allocation went ok? */
  if ( _ok(status) ) {

    size_t	bbytes;			/* Number of bytes in block */
    ADIinteger	anval;			/* Number of items in block */

/* Ensure that at least the minimum allocation for this type is used */
    anval = _MAX( nunit, cdef->alloc.nunit );

/* Size of data area */
    bbytes = newb->size*anval;

/* Switch mode of block creation depending on the value of nunit. See */
/* header of this file for details. */

/* Standard block, so we need an allocation mechanism */
    if ( anval <= cdef->alloc.nunit ) {
      ADIidIndex	nsmall = (ADIidIndex) anval;

/* All the objects are available to be allocated */
      newb->nfree = newb->nunit = nsmall;

/* If the object size is smaller than the size of an object index, then */
/* we must use a bit array. Otherwise we can use the more efficient linked */
/* list */
      if ( newb->size < sizeof(ADIidIndex) ) {
	ADIidIndex 	n8bit = (nsmall-1)/CHAR_BIT + 1;

/* Allocate data space including amount for bit allocation array */
	bbytes += n8bit;
	newb->data = ADImemAlloc(bbytes,status);

/* Locate the bit array at the end of the data segment, and zero it */
	newb->vdat.std.used = (unsigned char *) newb->data +
			newb->size*nsmall;
	memset( newb->vdat.std.used, 0, n8bit );
	}
      else {
	char		*cdata;
	int		i;
	ADIidIndex	*ipos = &newb->vdat.std.ffree;

	cdata = newb->data = ADImemAlloc(bbytes,status);

	newb->vdat.std.used = NULL;
	newb->vdat.std.ffree = 0;

/* Construct the linked list */
	for( i=0; i<nsmall; i++ ) {
	  *ipos = i;
	  ipos = (ADIidIndex *) cdata;
	  cdata += newb->size;
	  }
	*ipos = ADI__nullid;
	}
      }

/* Master block? */
    else if ( nunit > ADI_MAX_IDINDEX ) {
      ADIinteger	nleft = anval - ADI_MAX_IDINDEX;
      char		*cdata;
      ADIidIndex	sblk;
      ADIblock		*sbptr;
      ADIidIndex	oldnext = newb->next;
      ADIidIndex	*inblock = &newb->next;

/* Set block up as a master */
      newb->master = iblk;
      newb->vdat.mas.ntotal = anval;
      newb->nunit = ADI_MAX_IDINDEX;
      newb->nfree = 0;

/* Allocate data in one lump */
      cdata = newb->data = ADImemAlloc(bbytes,status);

/* Keep going while more data needing blocks to describe it */
      while ( nleft > 0 ) {

/* Advance the data pointer for the next block */
	cdata += newb->size * ADI_MAX_IDINDEX;

/* Allocate the new block */
	ADImemAllocBlock( cdef, &sbptr, &sblk, status );
	if ( sbptr ) {
          *inblock = sblk;
          inblock = &sbptr->next;
	  sbptr->master = iblk;
          sbptr->data = cdata;
	  sbptr->nunit = (ADIidIndex) _MIN(nleft,ADI_MAX_IDINDEX);
	  }

/* Next batch of data */
	nleft -= ADI_MAX_IDINDEX;
	}

/* Reverse order of slave blocks and master block. This means that the
 * master appears first in the search list, and its slaves after it. The
 * topology at this point looks like,
 *
 *      f_block
 *       |
 *       v
 *      Sn -> ... -> S1 -> M -> P
 *
 * The new topology is,
 *
 *      f_block
 *       |
 *       v
 *       M -> S1 -> ... -> Sn -> P
 */

/* Put the master back at the head of the chain */
      cdef->alloc.f_block = iblk;

/* Make the most recent slave point to 'P' */
      *inblock = oldnext;
      }

/* Not a master block, but still more than default allocation */
    else if ( anval > cdef->alloc.nunit ) {
      newb->nunit = (ADIidIndex) anval;
      newb->nfree = newb->nunit;
      newb->vdat.std.used = NULL;
      newb->data = ADImemAlloc(bbytes,status);
      newb->nfree = 0;
      }
    }

  return iblk;
  }


ADIobj ADImemAllocObj( ADIclassDef *cdef, int nval, ADIstatus status )
  {
  ADIblock      	*bptr;
  ADIlogical            found = ADI__false;
  int                   i8, j8, ibit;
  ADIobj                newid;          /* New identifier */
  ADIidIndex            iblk;           /* Allocation block identifier */
  int                   obno;           /* Object number within block */
  unsigned char		*uptr;

  static char nib_fbit[15] =            /* First bit zero in nibble, counting
					   from the left, LSB is number 0 */
    {0,1,0,2,0,1,0,3,0,1, 0, 2, 0, 1, 0};

  static short bit_mask[] =
    {1,2,4,8,16,32,64,128,256,512,1024};

  iblk = cdef->alloc.f_block;           /* Most recent allocation block */

/* Look for block with free slots */
  while ( (iblk!=ADI__nullid) && ! found ) {
    bptr = ADI_G_blks[iblk];

/* Only blocks with object tracking are of interest. Master and slave */
/* blocks have 'nfree' zero */
    if ( bptr->nfree > nval ) {

      uptr = bptr->vdat.std.used;

/* Easy peasy when linked list tracking is the scheme and only one */
/* required. Where the bit array is used we have no option but to */
/* scan for the first free bit */
      if ( nval == 1 ) {
	if ( uptr ) {
	  for( i8=0; (uptr[i8] == 0xFF) &&
	    i8 < (bptr->nunit/CHAR_BIT); i8++ );
	  }
	else
	  obno = bptr->vdat.std.ffree;

	found = ADI__true;
	}

/* Free slots must be contiguous for multiple allocation. The bit array */
/* case is a bit easier this time... */
      else {
	int         ncontig;

/* Bit allocation? */
	if ( uptr ) {

/* Number of used[] slots needed */
	  int nneed = (nval-1) / CHAR_BIT + 1;

	  for( i8=bptr->nunit/CHAR_BIT; i8; i8-- ) {
	    j8 = i8;
	    ncontig = 0;

	    while ( i8 && ! uptr[i8] ) {
	      ncontig++; i8--;
	      }

	    if ( ncontig >= nneed ) {
	      i8 = j8 - ncontig + 1;
	      found = ADI__true;
	      break;
	      }
	    }
	  }

/* Linked list allocation. Must look for NVAL-1 consective links */
	else {
	  ADIidIndex	next,cur;

	  cur = obno = bptr->vdat.std.ffree;
	  ncontig = 1;
	  while ( obno != ADI__nullid ) {
	    next = *((ADIidIndex *) (bptr->data + cur*bptr->size));
	    if ( next == (cur+1) ) {
	      ncontig++;
	      if ( ncontig == nval ) {
		found = ADI__true;
		break;
		}
	      }
	    else {
	      obno = next; ncontig = 1;
	      }
	    cur = next;
	    }
	  }
	}
      }
    else
      iblk = ADI_G_blks[iblk]->next;
    }

/* If not found allocate a new block for at least nval elements */
  if ( ! found ) {
    iblk = ADImemNewBlock( cdef, nval, status );
    bptr = ADI_G_blks[iblk];
    uptr = bptr->vdat.std.used;
    if ( bptr->nfree && ! uptr )
      obno = bptr->vdat.std.ffree;
    else {
      i8 = 0;
      obno = 0;
      }
    }

/* If the block is not a dedicated or master block */
  if ( bptr->nfree ) {

/* Single element to allocate? */
    if ( nval == 1 ) {

/* Bit allocation? */
      if ( uptr ) {

/* Extract low nibble */
	unsigned char	uchar = uptr[i8];
	char		lonib = uchar & 0xF;

/* Any bits free in this nibble */
	if ( lonib != 0xF )
	  ibit = nib_fbit[lonib];
	else
	  ibit = 4 + nib_fbit[uchar>>4];

/* Object number in block data */
	obno = i8*CHAR_BIT + ibit;

/* Mark object as allocated */
	uptr[i8] |= bit_mask[ibit];
	}

/* Linked list representation */
      else
	bptr->vdat.std.ffree = *((ADIidIndex *) (bptr->data + bptr->size*obno));}

/* Multiple allocation */
    else {

/* Bit allocation? */
      if ( uptr ) {
	obno = i8*CHAR_BIT;

/* Loop over whole used slots, marking them as used */
	for( j8=0; j8<(nval/CHAR_BIT); i8++, j8++ )
	  uptr[j8] = 0xFF;

/* Number allocated not divisible by CHAR_BIT */
	if ( nval % CHAR_BIT ) {
	  unsigned char uchar = 0;
	  for( ibit=0; ibit<(nval%CHAR_BIT); ibit++ )
	    uchar |= bit_mask[ibit];

	  uptr[i8] = uchar;
	  }
	}

/* Linked list method */
      else {
	bptr->vdat.std.ffree = *((ADIidIndex *)
	  (bptr->data + bptr->size*(obno+nval-1)));
	}
      }

/* Decrease free count */
    bptr->nfree -= nval;
    }

/* Form identifier and return it */
  _FORM_ID(newid,iblk,obno);
  return newid;
  }


void ADImemFreeBlock( int iblk, ADIstatus status )
  {
  ADIblock	*bptr = ADI_G_blks[iblk];
  ADIidIndex	*fptr = &(bptr->cdef->alloc.f_block);
  ADIidIndex	onext = bptr->next;
  ADIlogical	slave = ADI__false;

/* Locate the address of the variable pointing to this block in the */
/* type's block chain */
  while ( *fptr != iblk )
    fptr = &ADI_G_blks[*fptr]->next;

/* Standard block? */
  if ( bptr->master == ADI__nullid ) {
    size_t	bbytes = bptr->nunit*bptr->size;

    if ( bptr->vdat.std.used )
      bbytes += (bptr->nunit-1) / CHAR_BIT + 1;

    ADImemFree( bptr->data, bbytes, status );
    }

/* Master block? (ie not slave) */
  else if ( bptr->master == iblk ) {

    ADIidIndex	next = onext;
    int  nslave = (bptr->vdat.mas.ntotal-1) / ADI_MAX_IDINDEX;

/* Loop over slaves, freeing 'em */
    while ( nslave-- ) {
      ADIblock	*sbptr = ADI_G_blks[next];

/* Reset block store */
      ADI_G_blks[next] = NULL;

/* Get the next one in the chain. Update the end-of-chain pointer, onext */
      onext = next = sbptr->next;

/*   Free the slave block data */
      ADImemFree( sbptr, sizeof(ADIblock), status );
      }

/* Free the master's data */
    ADImemFree( bptr->data, bptr->vdat.mas.ntotal * bptr->size, status );
    }

/* Otherwise its a slave */
  else
    slave = ADI__true;

/* Free the block data */
  if ( ! slave ) {
    ADImemFree( bptr, sizeof(ADIblock), status );

/* Makes the slot available for re-use */
    ADI_G_blks[iblk] = NULL;
    }
  }


void ADImemFreeObj( ADIobj *id, int nval, ADIstatus status )
  {
  static short bit_mask[] =
    {1,2,4,8,16,32,64,128,256,512,1024};

  ADIblock 	*bptr = _ID_BLOCK(*id);
  ADIidIndex	iblk = _ID_IBLK(*id);
  ADIidIndex	iobj = _ID_SLOT(*id);

/* Standard block? */
  if ( bptr->master == ADI__nullid ) {

    unsigned char *uptr = bptr->vdat.std.used;

/* Block using bit array? */
    if ( uptr ) {
      int i8 = iobj / CHAR_BIT;		/* Find allocation first cell */
      int j8, ibit;

/* Multiple slots to deallocate? */
      if ( nval > 1 ) {

/* Loop over all the whole 'used' slots, resetting all bits */
	for( j8=i8; j8<(nval/CHAR_BIT); i8++, j8++ )
	  uptr[j8] = 0x00;

/* Loop over bits of partially used slot, resetting bits */
	for( ibit=0; ibit<(nval%CHAR_BIT); ibit++ )
	  uptr[i8] &= (bit_mask[ibit] ^ 0xFF);

/* Adjust free count */
	bptr->nfree -= nval;
	}

/* Single slot to deallocate */
      else {

/* Identify the bit */
	ibit = iobj % CHAR_BIT;

/* Reset allocated bit */
	uptr[i8] &= (bit_mask[ibit] ^ 0xFF);

/* Adjust free count */
	bptr->nfree--;
	}
      }

/* Linked list representation */
    else {

      char		*cdata = bptr->data + iobj*bptr->size;
      int		i;

/* Set the links for the first nval-1 objects to point to the next object */
      for( i=1; i<nval; i++ ) {
	*((ADIidIndex *) cdata) = iobj + i;
	cdata += bptr->size;
	}

/* The last object points to the block's old last object */
      *((ADIidIndex *) cdata) = bptr->vdat.std.ffree;

/* And the block's most recent last object becomes the first object */
      bptr->vdat.std.ffree = iobj;
      }
    }

/* Master or slave block. It is an error to attempt to deallocate a */
/* slot which is not the first slot in a master block */
  else {
    if ( (iobj!=0) || (iblk!=bptr->master) )
      adic_setecs( ADI__PRGERR, "Block deallocation error - entire dedicated basic block must be deallocated at once", status );
    else
      ADImemFreeBlock( iblk, status );
    }

  *id = ADI__nullid;			/* Reset identifier */
  }


void ADImemStart( void )
  {
  int           i;

  for( i=0; i<ADI__MXBLK; i++ )
    ADI_G_blks[i] = NULL;

#ifdef MEMORY_TRACING
  for( i=0; i<ADI__MXMTR; i++ )
    {
    allocs[i] = NULL;
    ainuse[i] = ADI__MXMTR - 1 - i;
    }
#endif
  }

void ADImemStop( ADIstatus status )
  {
  int           i;

strx_exit( status );
  for( i=0; i<ADI__MXBLK; i++ )
    if ( ADI_G_blks[i] )
      ADImemFreeBlock( i, status );

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

