#ifndef LIN
#define LIN

#ifdef __cplusplus
extern "C" {
#endif

#if !defined(__STDC__) && !defined(__cplusplus)
#ifndef const
#define const
#endif
#endif

struct linprm {
   int flag;
   int naxis;
   double *crpix;
   double *pc;
   double *cdelt;

   /* Intermediates. */
   double *piximg;
   double *imgpix;
};

#if __STDC__  || defined(__cplusplus)
   int linset(struct linprm *);
   int linfwd(const double[], struct linprm *, double[]);
   int linrev(const double[], struct linprm *, double[]);
   int matinv(const int, const double [], double []);
#else
   int linset(), linfwd(), linrev(), matinv();
#endif

extern const char *linset_errmsg[];
extern const char *linfwd_errmsg[];
extern const char *linrev_errmsg[];

#define LINSET 137

#ifdef __cplusplus
};
#endif

#endif /* LIN */
