/* Copyright (c) 1993 Association of Universities for Research 
 * in Astronomy. All rights reserved. Produced under National   
 * Aeronautics and Space Administration Contract No. NAS5-26555.
 */
/* hinv.c   Inverse H-transform of NX x NY integer image
 *
 * Programmer: R. White		Date: 23 July 1993
 */
#include <stdio.h>
#include <math.h>
#include <stdlib.h>

static void unshuffle();
extern void hsmooth();

extern void 
hinv(a,nx,ny,smooth,scale)
int a[];
int nx,ny;
int smooth;   /* 0 for no smoothing, else smooth during inversion */
int scale;    /* used if smoothing is specified */
{
int nmax, log2n, i, j, k;
int nxtop,nytop,nxf,nyf,c;
int oddx,oddy;
int shift, bit0, bit1, bit2, mask0, mask1, mask2,
	prnd0, prnd1, prnd2, nrnd0, nrnd1, nrnd2, lowbit0, lowbit1;
int h0, hx, hy, hc;
int s10, s00;
int *tmp;

	/*
	 * log2n is log2 of max(nx,ny) rounded up to next power of 2
	 */
	nmax = (nx>ny) ? nx : ny;
	log2n = log((float) nmax)/log(2.0)+0.5;
	if ( nmax > (1<<log2n) ) {
		log2n += 1;
	}
	/*
	 * get temporary storage for shuffling elements
	 */  
	tmp = (int *) malloc(((nmax+1)/2)*sizeof(int));
	if (tmp == (int *) NULL) {
		fprintf(stderr, "hinv: insufficient memory\n");
		exit(-1);
	}
	/*
	 * set up masks, rounding parameters
	 */
	shift  = 1;
	bit0   = 1 << (log2n - 1);
	bit1   = bit0 << 1;
	bit2   = bit0 << 2;
	mask0  = -bit0;
	mask1  = mask0 << 1;
	mask2  = mask0 << 2;
	prnd0  = bit0 >> 1;
	prnd1  = bit1 >> 1;
	prnd2  = bit2 >> 1;
	nrnd0  = prnd0 - 1;
	nrnd1  = prnd1 - 1;
	nrnd2  = prnd2 - 1;
	/*
	 * round h0 to multiple of bit2
	 */
	a[0] = (a[0] + ((a[0] >= 0) ? prnd2 : nrnd2)) & mask2;
	/*
	 * do log2n expansions
	 *
	 * We're indexing a as a 2-D array with dimensions (nx,ny).
	 */
	nxtop = 1;
	nytop = 1;
	nxf = nx;
	nyf = ny;
	c = 1<<log2n;
	for (k = log2n-1; k>=0; k--) {
		/*
		 * this somewhat cryptic code generates the sequence
		 * ntop[k-1] = (ntop[k]+1)/2, where ntop[log2n] = n
		 */
		c = c>>1;
		nxtop = nxtop<<1;
		nytop = nytop<<1;
		if (nxf <= c) { nxtop -= 1; } else { nxf -= c; }
		if (nyf <= c) { nytop -= 1; } else { nyf -= c; }
		/*
		 * double shift and fix nrnd0 (because prnd0=0) on last pass
		 */
		if (k == 0) {
			nrnd0 = 0;
			shift = 2;
		}
		/*
		 * unshuffle in each dimension to interleave coefficients
		 */
		for (i = 0; i<nxtop; i++) {
			unshuffle(&a[ny*i],nytop,1,tmp);
		}
		for (j = 0; j<nytop; j++) {
			unshuffle(&a[j],nxtop,ny,tmp);
		}
		/*
		 * smooth by interpolating coefficients if SMOOTH != 0
		 */
		if (smooth) hsmooth(a,nxtop,nytop,ny,scale);
		oddx = nxtop % 2;
		oddy = nytop % 2;
		for (i = 0; i<nxtop-oddx; i += 2) {
			s00 = ny*i;				/* s00 is index of a[i,j]	*/
			s10 = s00+ny;			/* s10 is index of a[i+1,j]	*/
			for (j = 0; j<nytop-oddy; j += 2) {
				h0 = a[s00  ];
				hx = a[s10  ];
				hy = a[s00+1];
				hc = a[s10+1];
				/*
				 * round hx and hy to multiple of bit1, hc to multiple of bit0
				 * h0 is already a multiple of bit2
				 */
				hx = (hx + ((hx >= 0) ? prnd1 : nrnd1)) & mask1;
				hy = (hy + ((hy >= 0) ? prnd1 : nrnd1)) & mask1;
				hc = (hc + ((hc >= 0) ? prnd0 : nrnd0)) & mask0;
				/*
				 * propagate bit0 of hc to hx,hy
				 */
				lowbit0 = hc & bit0;
				hx = (hx >= 0) ? (hx - lowbit0) : (hx + lowbit0);
				hy = (hy >= 0) ? (hy - lowbit0) : (hy + lowbit0);
				/*
				 * Propagate bits 0 and 1 of hc,hx,hy to h0.
				 * This could be simplified if we assume h0>0, but then
				 * the inversion would not be lossless for images with
				 * negative pixels.
				 */
				lowbit1 = (hc ^ hx ^ hy) & bit1;
				h0 = (h0 >= 0)
					? (h0 + lowbit0 - lowbit1)
					: (h0 + ((lowbit0 == 0) ? lowbit1 : (lowbit0-lowbit1)));
				/*
				 * Divide sums by 2 (4 last time)
				 */
				a[s10+1] = (h0 + hx + hy + hc) >> shift;
				a[s10  ] = (h0 + hx - hy - hc) >> shift;
				a[s00+1] = (h0 - hx + hy - hc) >> shift;
				a[s00  ] = (h0 - hx - hy + hc) >> shift;
				s00 += 2;
				s10 += 2;
			}
			if (oddy) {
				/*
				 * do last element in row if row length is odd
				 * s00+1, s10+1 are off edge
				 */
				h0 = a[s00  ];
				hx = a[s10  ];
				hx = ((hx >= 0) ? (hx+prnd1) : (hx+nrnd1)) & mask1;
				lowbit1 = hx & bit1;
				h0 = (h0 >= 0) ? (h0 - lowbit1) : (h0 + lowbit1);
				a[s10  ] = (h0 + hx) >> shift;
				a[s00  ] = (h0 - hx) >> shift;
			}
		}
		if (oddx) {
			/*
			 * do last row if column length is odd
			 * s10, s10+1 are off edge
			 */
			s00 = ny*i;
			for (j = 0; j<nytop-oddy; j += 2) {
				h0 = a[s00  ];
				hy = a[s00+1];
				hy = ((hy >= 0) ? (hy+prnd1) : (hy+nrnd1)) & mask1;
				lowbit1 = hy & bit1;
				h0 = (h0 >= 0) ? (h0 - lowbit1) : (h0 + lowbit1);
				a[s00+1] = (h0 + hy) >> shift;
				a[s00  ] = (h0 - hy) >> shift;
				s00 += 2;
			}
			if (oddy) {
				/*
				 * do corner element if both row and column lengths are odd
				 * s00+1, s10, s10+1 are off edge
				 */
				h0 = a[s00  ];
				a[s00  ] = h0 >> shift;
			}
		}
		/*
		 * divide all the masks and rounding values by 2
		 */
		bit2 = bit1;
		bit1 = bit0;
		bit0 = bit0 >> 1;
		mask1 = mask0;
		mask0 = mask0 >> 1;
		prnd1 = prnd0;
		prnd0 = prnd0 >> 1;
		nrnd1 = nrnd0;
		nrnd0 = prnd0 - 1;
	}
	free(tmp);
}

 
static void
unshuffle(a,n,n2,tmp)
int a[];	/* array to shuffle					*/
int n;		/* number of elements to shuffle	*/
int n2;		/* second dimension					*/
int tmp[];	/* scratch storage					*/
{
int i;
int nhalf;
int *p1, *p2, *pt;
 
	/*
	 * copy 2nd half of array to tmp
	 */
	nhalf = (n+1)>>1;
	pt = tmp;
	p1 = &a[n2*nhalf];				/* pointer to a[i]			*/
	for (i=nhalf; i<n; i++) {
		*pt = *p1;
		p1 += n2;
		pt += 1;
	}
	/*
	 * distribute 1st half of array to even elements
	 */
	p2 = &a[ n2*(nhalf-1) ];		/* pointer to a[i]			*/
	p1 = &a[(n2*(nhalf-1))<<1];		/* pointer to a[2*i]		*/
	for (i=nhalf-1; i >= 0; i--) {
		*p1 = *p2;
		p2 -= n2;
		p1 -= (n2+n2);
	}
	/*
	 * now distribute 2nd half of array (in tmp) to odd elements
	 */
	pt = tmp;
	p1 = &a[n2];					/* pointer to a[i]			*/
	for (i=1; i<n; i += 2) {
		*p1 = *pt;
		p1 += (n2+n2);
		pt += 1;
	}
}
