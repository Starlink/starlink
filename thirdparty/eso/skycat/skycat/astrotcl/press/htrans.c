/* htrans.c   H-transform of NX x NY integer image
 *
 * Programmer: R. White		Date: 11 May 1992
 */
#include <stdio.h>
#include <math.h>
#include <stdlib.h>

static void shuffle();

extern void 
htrans(a,nx,ny)
int a[];
int nx, ny;
{
int nmax, log2n, h0, hx, hy, hc, nxtop, nytop, i, j, k;
int oddx, oddy;
int shift, mask, mask2, prnd, prnd2, nrnd2;
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
	if(tmp == (int *) NULL) {
		(void) fprintf(stderr, "htrans: insufficient memory\n");
		exit(-1);
	}
	/*
	 * set up rounding and shifting masks
	 */
	shift = 0;
	mask  = -2;
	mask2 = mask << 1;
	prnd  = 1;
	prnd2 = prnd << 1;
	nrnd2 = prnd2 - 1;
	/*
	 * do log2n reductions
	 *
	 * We're indexing a as a 2-D array with dimensions (nx,ny).
	 */
	nxtop = nx;
	nytop = ny;
	for (k = 0; k<log2n; k++) {
		oddx = nxtop % 2;
		oddy = nytop % 2;
		for (i = 0; i<nxtop-oddx; i += 2) {
			s00 = i*ny;				/* s00 is index of a[i,j]	*/
			s10 = s00+ny;			/* s10 is index of a[i+1,j]	*/
			for (j = 0; j<nytop-oddy; j += 2) {
				/*
				 * Divide h0,hx,hy,hc by 2 (1 the first time through).
				 */
				h0 = (a[s10+1] + a[s10] + a[s00+1] + a[s00]) >> shift;
				hx = (a[s10+1] + a[s10] - a[s00+1] - a[s00]) >> shift;
				hy = (a[s10+1] - a[s10] + a[s00+1] - a[s00]) >> shift;
				hc = (a[s10+1] - a[s10] - a[s00+1] + a[s00]) >> shift;
				/*
				 * Throw away the 2 bottom bits of h0, bottom bit of hx,hy.
				 * To get rounding to be same for positive and negative
				 * numbers, nrnd2 = prnd2 - 1.
				 */
				a[s10+1] = hc;
				a[s10  ] = ( (hx>=0) ? (hx+prnd)  :  hx        ) & mask ;
				a[s00+1] = ( (hy>=0) ? (hy+prnd)  :  hy        ) & mask ;
				a[s00  ] = ( (h0>=0) ? (h0+prnd2) : (h0+nrnd2) ) & mask2;
				s00 += 2;
				s10 += 2;
			}
			if (oddy) {
				/*
				 * do last element in row if row length is odd
				 * s00+1, s10+1 are off edge
				 */
				h0 = (a[s10] + a[s00]) << (1-shift);
				hx = (a[s10] - a[s00]) << (1-shift);
				a[s10  ] = ( (hx>=0) ? (hx+prnd)  :  hx        ) & mask ;
				a[s00  ] = ( (h0>=0) ? (h0+prnd2) : (h0+nrnd2) ) & mask2;
				s00 += 1;
				s10 += 1;
			}
		}
		if (oddx) {
			/*
			 * do last row if column length is odd
			 * s10, s10+1 are off edge
			 */
			s00 = i*ny;
			for (j = 0; j<nytop-oddy; j += 2) {
				h0 = (a[s00+1] + a[s00]) << (1-shift);
				hy = (a[s00+1] - a[s00]) << (1-shift);
				a[s00+1] = ( (hy>=0) ? (hy+prnd)  :  hy        ) & mask ;
				a[s00  ] = ( (h0>=0) ? (h0+prnd2) : (h0+nrnd2) ) & mask2;
				s00 += 2;
			}
			if (oddy) {
				/*
				 * do corner element if both row and column lengths are odd
				 * s00+1, s10, s10+1 are off edge
				 */
				h0 = a[s00] << (2-shift);
				a[s00  ] = ( (h0>=0) ? (h0+prnd2) : (h0+nrnd2) ) & mask2;
			}
		}
		/*
		 * now shuffle in each dimension to group coefficients by order
		 */
		for (i = 0; i<nxtop; i++) {
			shuffle(&a[ny*i],nytop,1,tmp);
		}
		for (j = 0; j<nytop; j++) {
			shuffle(&a[j],nxtop,ny,tmp);
		}
		/*
		 * image size reduced by 2 (round up if odd)
		 */
		nxtop = (nxtop+1)>>1;
		nytop = (nytop+1)>>1;
		/*
		 * divisor doubles after first reduction
		 */
		shift = 1;
		/*
		 * masks, rounding values double after each iteration
		 */
		mask  = mask2;
		prnd  = prnd2;
		mask2 = mask2 << 1;
		prnd2 = prnd2 << 1;
		nrnd2 = prnd2 - 1;
	}
	free(tmp);
}

static void
shuffle(a,n,n2,tmp)
int a[];	/* array to shuffle					*/
int n;		/* number of elements to shuffle	*/
int n2;		/* second dimension					*/
int tmp[];	/* scratch storage					*/
{
int i;
int *p1, *p2, *pt;

	/*
	 * copy odd elements to tmp
	 */
	pt = tmp;
	p1 = &a[n2];
	for (i=1; i < n; i += 2) {
		*pt = *p1;
		pt += 1;
		p1 += (n2+n2);
	}
	/*
	 * compress even elements into first half of A
	 */
	p1 = &a[n2];
	p2 = &a[n2+n2];
	for (i=2; i<n; i += 2) {
		*p1 = *p2;
		p1 += n2;
		p2 += (n2+n2);
	}
	/*
	 * put odd elements into 2nd half
	 */
	pt = tmp;
	for (i = 1; i<n; i += 2) {
		*p1 = *pt;
		p1 += n2;
		pt += 1;
	}
}
