/*
** This file contains C code for a new color browser written using
** Embedded Tk.
*/
#include <stdio.h>

/* Forward reference */
void ReadColorList(void);

/* The main program */
int main(int argc, char **argv){
  ReadColorList();
  Et_Init(&argc,argv);
  ET_INSTALL_COMMANDS;
  ET_INCLUDE( color.tcl );
  ET_INCLUDE( colorhelp.tcl );
  ET_INCLUDE( help.tcl );
  Et_MainLoop();
  return 0;
}

/* The components of the current color are stored in the following
** variables. */
double rRed, rGreen, rBlue, rHue, rSaturation, rIntensity;

/* Each of the named colors is recorded in an instance of the following
** structure */
typedef struct _NamedColor NamedColor;
struct _NamedColor {
  char *zName;         /* Name of this color */
  int r,g,b;           /* Red, green and blue components of this color */
  int dist;            /* Squared distance from this color to current color */
  char *zFg;           /* An appropriate foreground color for this color */
  NamedColor *pNext;   /* Next color in sequence */
};

/* The following variable points to the name of the current color,
** if the current color has a name.
*/
NamedColor *pCurrentColor = 0;

/* The following variable points to a list of all named colors.  The
** named color list is sorted so that the colors closest to the currently
** selected color are near the head of the list.
*/
NamedColor *pColorList = 0;

/* Locate and read the X11 color list file "rgb.txt".
*/
void ReadColorList(void){
  static char *zFilename[] = {
    "/usr/local/lib/X11/rgb.txt",
    "/usr/lib/X11/rgb.txt",
    "/X11/R5/lib/X11/rgb.txt",
    "/X11/R6/lib/X11/rgb.txt",
    "/usr/openwin/lib/X11/rgb.txt",
  };
  FILE *pIn;
  int i;
  char zLine[100];

  for(i=0; i<sizeof(zFilename)/sizeof(zFilename[0]); i++){
    pIn = fopen(zFilename[i],"r");
    if( pIn ) break;
  }
  pColorList = 0;
  if( pIn==0 ){
    /* Unable to open the color database file */
    return;
  }
  while( fgets(zLine,sizeof(zLine),pIn) ){
    int r,g,b;
    NamedColor *pColor;
    char zName[100];
    if( sscanf(zLine,"%d %d %d %[^\n]",&r,&g,&b,zName)!=4 ) continue;
    if( strchr(zName,' ') ) continue;
    if( strncmp(&zName[1],"rey",3)==0 ) continue;
    pColor = (NamedColor*)malloc( sizeof(NamedColor) + strlen(zName) + 1 );
    if( pColor==0 ) break;
    pColor->zName = (char*)&pColor[1];
    strcpy(pColor->zName,zName);
    pColor->r = r;
    pColor->g = g;
    pColor->b = b;
    pColor->zFg = (0.3*r + 0.6*g + 0.1*b>128.0) ? "black" : "white";
    pColor->pNext = pColorList;
    pColorList = pColor;
  }
  fclose(pIn);
}

/* Merge together two sorted lists of NamedColor structures
** return a pointer to the sorted list.  This routine is called in
** support of the SortNamedColors function.
*/
NamedColor *MergeLists(NamedColor *a, NamedColor *b){
  NamedColor *ptr, *head;

  if( a==0 ){
    head = b;
  }else if( b==0 ){
    head = a;
  }else{
    if( a->dist < b->dist ){
      ptr = a;
      a = a->pNext;
    }else{
      ptr = b;
      b = b->pNext;
    }
    head = ptr;
    while( a && b ){
      if( a->dist < b->dist ){
        ptr->pNext = a;
        ptr = a;
        a = a->pNext;
      }else{
        ptr->pNext = b;
        ptr = b;
        b = b->pNext;
      }
    }
    if( a ) ptr->pNext = a;
    else    ptr->pNext = b;
  }
  return head;
}

/* Sort the colors by their distance field.  Smallest first.
*/
#define LISTSIZE 30
void SortNamedColors(void){
  NamedColor *ep, *list;
  NamedColor *set[LISTSIZE];
  int i;

  list = pColorList;
  for(i=0; i<LISTSIZE; i++) set[i] = 0;
  while( list ){
    ep = list;
    list = list->pNext;
    ep->pNext = 0;
    for(i=0; i<LISTSIZE-1 && set[i]!=0; i++){
      ep = MergeLists(ep,set[i]);
      set[i] = 0;
    }
    set[i] = ep;
  }
  ep = 0;
  for(i=0; i<LISTSIZE; i++) if( set[i] ) ep = MergeLists(ep,set[i]);
  pColorList = ep;
}

/* Find the named colors closest to the current color.  Set the 
** "Similar Colors" boxes to the 6 closest colors.  Also set up the
** pCurrentColor pointer, if appropriate.
*/
void FindClosestColors(void){
  NamedColor *pColor;  /* For scanning thru the list of named colors */
  int r,g,b;           /* Integer values (0-255) for Red, Green and Blue */
  int i;               /* Loop counter */

  pCurrentColor = 0;
  if( pColorList==0 ) return;
  r = rRed*256.0;
  g = rGreen*256.0;
  b = rBlue*256.0;
  for(pColor=pColorList; pColor; pColor=pColor->pNext){
    int x, y, z;
    x = 0.30*(r - pColor->r);
    y = 0.61*(g - pColor->g);
    z = 0.11*(b - pColor->b);
    pColor->dist = x*x + y*y + z*z;
  }
  SortNamedColors();
  if( pColorList->dist<3 ){
    pCurrentColor = pColorList;
    pColor = pColorList->pNext;
  }else{
    pColor = pColorList;
  }
  for(i=1; i<=6; i++){
    ET( .near.x%d(i) config -bg %s(pColor->zName) -foreground %s(pColor->zFg) \
           -text %s(pColor->zName) );
    pColor = pColor->pNext;
  }
}

/* Assuming that the global variables rRed, rGreen and rBlue are correct,
** compute new values for the rHue, rSaturation and rIntensity variables,
** and set scales accordingly.
*/
void RgbToHsv(void){
  double min, max;
  int v;

  min = rRed;
  if( min>rGreen ) min = rGreen;
  if( min>rBlue ) min = rBlue;
  max = rRed;
  if( max<rGreen ) max = rGreen;
  if( max<rBlue ) max = rBlue;

  rIntensity = max;
  v = rIntensity*1000.0;
  ET( .scales.intensity.x set %d(v) );

  rSaturation = (max>0.0) ? (max-min)/max : 0.0;
  v = rSaturation*1000.0;
  ET( .scales.saturation.x set %d(v) );

  if( rSaturation==0.0 ){
    /* Leave Hue unchanged */
  }else if( rRed==max ){
    rHue = (rGreen - rBlue)/(6.0*(max - min));
  }else if( rGreen==max ){
    rHue = 2.0/6.0 + (rBlue - rRed)/(6.0*(max - min));
  }else{
    rHue = 4.0/6.0 + (rRed - rGreen)/(6.0*(max - min));
  }
  if( rHue<0 ) rHue += 1.0;
  v = rHue*1000.0;
  ET( .scales.hue.x set %d(v) ); 
}

/* Assuming that the global variables rHue, rSaturation and rIntensity
** are correct, compute new values for the rRed, rGreen and rBlue variables
** and set scales accordingly.
*/
void HsvToRgb(void){
  int r,g,b;
  int i = rHue*6.0;
  double f = rHue*6.0 - i;
  double p = rIntensity*(1 - rSaturation);
  double q = rIntensity*(1 - rSaturation*f);
  double t = rIntensity*(1 - rSaturation*(1-f));
  switch( i ){
    case 0:
      rRed = rIntensity;
      rGreen = t;
      rBlue = p;
      break;

    case 1:
      rRed = q;
      rGreen = rIntensity;
      rBlue = p;
      break;

    case 2:
      rRed = p;
      rGreen = rIntensity;
      rBlue = t;
      break;

    case 3:
      rRed = p;
      rGreen = q;
      rBlue = rIntensity;
      break;

    case 4:
      rRed = t;
      rGreen = p;
      rBlue = rIntensity;
      break;

    case 5:
      rRed = rIntensity;
      rGreen = p;
      rBlue = q;
      break;
  }
  r = rRed*1000.0;
  g = rGreen*1000.0;
  b = rBlue*1000.0;
  ET(
    .scales.red.x set %d(r)
    .scales.green.x set %d(g)
    .scales.blue.x set %d(b)
  );
}

/* Refresh the color information on the screen.
*/
void Refresh(void){
  int r,g,b;
  char *fg;
  char color[30];
  FindClosestColors();
  if( pCurrentColor ){
    strcpy(color,pCurrentColor->zName);
    fg = pCurrentColor->zFg;
  }else{
    r = rRed*65536.0;
    g = rGreen*65536.0;
    b = rBlue*65536.0;
    sprintf(color,"#%04x%04x%04x",r,g,b);
    fg = (0.3*rRed + 0.6*rGreen + 0.1*rBlue > 0.5) ? "black" : "white";
  }
  ET( ChangeSwatch %s(fg) %s(color) %s(color) );
}

/*
** The following routine is called whenever one of the color component
** sliders moves.  The first argument is the name of the slider that
** moved.  The second is its new value between 0 and 999.
*/
ET_PROC( ChangeComponent ){
  double v;

  if( argc!=3 ) return ET_ERROR;
  v = atoi(argv[2])/1000.0;
  switch( argv[1][0] ){
    case 'r':
      rRed = v;
      RgbToHsv();
      break;
    case 'g':
      rGreen = v;
      RgbToHsv();
      break;
    case 'b':
      rBlue = v;
      RgbToHsv();
      break;
    case 'h':
      rHue = v;
      HsvToRgb();
      break;
    case 's':
      rSaturation = v;
      HsvToRgb();
      break;
    case 'i':
      rIntensity = v;
      HsvToRgb();
      break;
  }
  Refresh();
  return ET_OK;
}

/*
** Given a named color, change the current color to that color.
*/
ET_PROC( ChangeColor ){
  NamedColor *pColor;
  int r,g,b;
  if( argc!=2 ) return ET_ERROR;
  for(pColor=pColorList; pColor; pColor=pColor->pNext){
    if( strcmp(pColor->zName,argv[1])==0 ) break;
  }
  if( pColor==0 ) return ET_ERROR;
  rRed = pColor->r/256.0;
  rGreen = pColor->g/256.0;
  rBlue = pColor->b/256.0;
  r = rRed*1000.0;
  g = rGreen*1000.0;
  b = rBlue*1000.0;
  ET(
    .scales.red.x set %d(r)
    .scales.green.x set %d(g)
    .scales.blue.x set %d(b)
  );
  RgbToHsv();
  Refresh();
  return ET_OK;
}
