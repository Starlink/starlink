/* This file was automatically generated.  Do not edit! */
void HtmlTestPointDump(char *filename);
#define DEBUG 1
#if defined(DEBUG)
extern int HtmlDepth;
#define HtmlPop  HtmlDepth-=2
#endif
#if !(defined(DEBUG))
#define HtmlPop
#endif
#if defined(DEBUG)
#define HtmlPush HtmlDepth+=2
#endif
#if !(defined(DEBUG))
#define HtmlPush
#endif
void HtmlTPCantHappen(const char *zFile,int line);
#if defined(COVERAGE_TEST)
# define HtmlVerifyLock(H) if((H)->locked==0)HtmlTPCantHappen(__FILE__,__LINE__)
#endif
#if !(defined(COVERAGE_TEST))
# define HtmlVerifyLock(H)
#endif
#if defined(COVERAGE_TEST)
# define CANT_HAPPEN       HtmlTPCantHappen(__FILE__,__LINE__)
#endif
#if !(defined(COVERAGE_TEST))
# define CANT_HAPPEN
#endif
void HtmlTPUntested(const char *zFile,int line);
#if defined(COVERAGE_TEST)
# define UNTESTED          HtmlTPUntested(__FILE__,__LINE__)
#endif
#if !(defined(COVERAGE_TEST))
# define UNTESTED
#endif
#if defined(COVERAGE_TEST)
extern int HtmlTPArray[2000];
# define TestPoint(X)      {extern int HtmlTPArray[]; HtmlTPArray[X]++;}
#endif
#if !(defined(COVERAGE_TEST))
# define TestPoint(X)
#endif
#define INTERFACE 0
