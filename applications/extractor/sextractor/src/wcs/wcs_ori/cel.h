#ifndef CEL
#define CEL

#include "proj.h"

#ifdef __cplusplus
extern "C" {
#endif

extern int npcode;
extern char pcodes[25][4];

struct celprm {
   int flag;
   double ref[4];
   double euler[5];

#if __STDC__  || defined(__cplusplus)
   int (*prjfwd)(const double, const double,
                 struct prjprm *,
                 double *, double *);
   int (*prjrev)(const double, const double,
                 struct prjprm *,
                 double *, double *);
#else
   int (*prjfwd)();
   int (*prjrev)();
#endif
};

#if __STDC__  || defined(__cplusplus)
   int celset(const char *, struct celprm *, struct prjprm *);
   int celfwd(const char *,
              const double, const double,
              struct celprm *,
              double *, double *,
              struct prjprm *,
              double *, double *);
   int celrev(const char *,
              const double, const double,
              struct prjprm *,
              double *, double *,
              struct celprm *,
              double *, double *);
#else
   int celset(), celfwd(), celrev();
#endif

extern const char *celset_errmsg[];
extern const char *celfwd_errmsg[];
extern const char *celrev_errmsg[];

#define CELSET 137

#ifdef __cplusplus
}
#endif

#endif /* CEL */
