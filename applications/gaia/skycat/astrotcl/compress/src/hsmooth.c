/* hsmooth.c	Smooth H-transform image by adjusting coefficients toward
 *				interpolated values
 *
 * Programmer: R. White		Date: 13 April 1992
 */
#include <stdio.h>
#include <math.h>

#define min(a,b) (((a)<(b)) ? (a) : (b))
#define max(a,b) (((a)>(b)) ? (a) : (b))

extern void 
hsmooth(a,nxtop,nytop,ny,scale)
int a[];			/* array of H-transform coefficients		*/
int nxtop,nytop;	/* size of coefficient block to use			*/
int ny;				/* actual 1st dimension of array			*/
int scale;			/* truncation scale factor that was used	*/
{
int i, j;
int ny2, s10, s00, diff, dmax, dmin, s, smax;
int hm, h0, hp, hmm, hpm, hmp, hpp, hx2, hy2;
int m1,m2;

	/*
	 * Maximum change in coefficients is determined by scale factor.
	 * Since we rounded during division (see digitize.c), the biggest
	 * permitted change is scale/2.
	 */
	smax = (scale >> 1);
	if (smax <= 0) return;
	ny2 = ny << 1;
	/*
	 * We're indexing a as a 2-D array with dimensions (nxtop,ny) of which
	 * only (nxtop,nytop) are used.  The coefficients on the edge of the
	 * array are not adjusted (which is why the loops below start at 2
	 * instead of 0 and end at nxtop-2 instead of nxtop.)
	 */
	/*
	 * Adjust x difference hx
	 */
	for (i = 2; i<nxtop-2; i += 2) {
		s00 = ny*i;				/* s00 is index of a[i,j]	*/
		s10 = s00+ny;			/* s10 is index of a[i+1,j]	*/
		for (j = 0; j<nytop; j += 2) {
			/*
			 * hp is h0 (mean value) in next x zone, hm is h0 in previous x zone
			 */
			hm = a[s00-ny2];
			h0 = a[s00];
			hp = a[s00+ny2];
			/*
			 * diff = 8 * hx slope that would match h0 in neighboring zones
			 */
			diff = hp-hm;
			/*
			 * monotonicity constraints on diff
			 */
			dmax = max( min( (hp-h0), (h0-hm) ), 0 ) << 2;
			dmin = min( max( (hp-h0), (h0-hm) ), 0 ) << 2;
			/*
			 * if monotonicity would set slope = 0 then don't change hx.
			 * note dmax>=0, dmin<=0.
			 */
			if (dmin < dmax) {
				diff = max( min(diff, dmax), dmin);
				/*
				 * Compute change in slope limited to range +/- smax.
				 * Careful with rounding negative numbers when using
				 * shift for divide by 8.
				 */
				s = diff-(a[s10]<<3);
				s = (s>=0) ? (s>>3) : ((s+7)>>3) ;
				s = max( min(s, smax), -smax);
				a[s10] = a[s10]+s;
			}
			s00 += 2;
			s10 += 2;
		}
	}
	/*
	 * Adjust y difference hy
	 */
	for (i = 0; i<nxtop; i += 2) {
		s00 = ny*i+2;
		s10 = s00+ny;
		for (j = 2; j<nytop-2; j += 2) {
			hm = a[s00-2];
			h0 = a[s00];
			hp = a[s00+2];
			diff = hp-hm;
			dmax = max( min( (hp-h0), (h0-hm) ), 0 ) << 2;
			dmin = min( max( (hp-h0), (h0-hm) ), 0 ) << 2;
			if (dmin < dmax) {
				diff = max( min(diff, dmax), dmin);
				s = diff-(a[s00+1]<<3);
				s = (s>=0) ? (s>>3) : ((s+7)>>3) ;
				s = max( min(s, smax), -smax);
				a[s00+1] = a[s00+1]+s;
			}
			s00 += 2;
			s10 += 2;
		}
	}
	/*
	 * Adjust curvature difference hc
	 */
	for (i = 2; i<nxtop-2; i += 2) {
		s00 = ny*i+2;
		s10 = s00+ny;
		for (j = 2; j<nytop-2; j += 2) {
			/*
			 * ------------------    y
			 * | hmp |    | hpp |    |
			 * ------------------    |
			 * |     | h0 |     |    |
			 * ------------------    -------x
			 * | hmm |    | hpm |
			 * ------------------
			 */
			hmm = a[s00-ny2-2];
			hpm = a[s00+ny2-2];
			hmp = a[s00-ny2+2];
			hpp = a[s00+ny2+2];
			h0  = a[s00];
			/*
			 * diff = 64 * hc value that would match h0 in neighboring zones
			 */
			diff = hpp + hmm - hmp - hpm;
			/*
			 * 2 times x,y slopes in this zone
			 */
			hx2 = a[s10  ]<<1;
			hy2 = a[s00+1]<<1;
			/*
			 * monotonicity constraints on diff
			 */
			m1 = min(max(hpp-h0,0)-hx2-hy2, max(h0-hpm,0)+hx2-hy2);
			m2 = min(max(h0-hmp,0)-hx2+hy2, max(hmm-h0,0)+hx2+hy2);
			dmax = min(m1,m2) << 4;
			m1 = max(min(hpp-h0,0)-hx2-hy2, min(h0-hpm,0)+hx2-hy2);
			m2 = max(min(h0-hmp,0)-hx2+hy2, min(hmm-h0,0)+hx2+hy2);
			dmin = max(m1,m2) << 4;
			/*
			 * if monotonicity would set slope = 0 then don't change hc.
			 * note dmax>=0, dmin<=0.
			 */
			if (dmin < dmax) {
				diff = max( min(diff, dmax), dmin);
				/*
				 * Compute change in slope limited to range +/- smax.
				 * Careful with rounding negative numbers when using
				 * shift for divide by 64.
				 */
				s = diff-(a[s10+1]<<6);
				s = (s>=0) ? (s>>6) : ((s+63)>>6) ;
				s = max( min(s, smax), -smax);
				a[s10+1] = a[s10+1]+s;
			}
			s00 += 2;
			s10 += 2;
		}
	}
}
