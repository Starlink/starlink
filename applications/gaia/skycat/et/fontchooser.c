/*
** This program allows the user to view the various fonts
** available on the X server.
**
** Preprocess this file using "et2c" then link with "et.o".
*/
#include "tk.h"     /* This automatically loads Xlib.h */

void main(int argc, char **argv){
  Et_Init(&argc,argv);
		  ET_INSTALL_COMMANDS;
  ET_INCLUDE( fontchooser.tcl );
  Et_MainLoop();
}

/* This function parses up font names as follows:
**
**         Font Family               Font size
**    __________________________  ________________
**   /                          \/                \
**   -misc-fixed-medium-r-normal--10-100-75-75-c-60-iso8859-1
**                                |   |  \___/    | \_______/
**                                |   |    |      |     |
**                                |   |    |      |     `-- Always as shown
**                                |   |    |      |
**             The point size ----'   |    |      `--- 10x average width
**                                    |    |
**              This field ignored----'    `--- Resolution in dots per inch
**
**
** If $name is a font name (the first 6 fields of the X11 font name)
** then this procedure defines the global variable $Font($name), giving
** it as a value a list of available font sizes in accending order.
** Only fonts of a particular resolution are included.  By default, the
** resolution selected is 75dpi, but this can be changed by the
** argument to the command.
**
** This command also creates global variable FontCount which holds the
** number of entries in the Font() array.
*/
ET_PROC( FindFonts ){
  char **fontnames;    /* The names of all fonts in the selected resolution */
  int count;           /* Number of fonts */
  int i;               /* Loop counter */
  char pattern[400];   /* Buffer to hold a pattern used to select fonts. */

  if( argc==1 ){
    strcpy(pattern,"*-75-75-*-*-iso8859-1");
  }else if( argc==2 ){
    extern int atoi();
    int resolution = atoi(argv[1]);
    if( resolution<=0 ){
      sprintf(pattern,"*-*-*-*-*-iso8859-1");
    }else{
      sprintf(pattern,"*-%d-%d-*-*-iso8859-1",resolution,resolution);
    }
  }
  fontnames = XListFonts(Et_Display,pattern,1000,&count);
  ET(
    catch {unset Font}
    set FontCount 0
  );
  for(i=0; i<count; i++){
    int k;               /* Counts the '-' characters in the name */
    char *cp;            /* For scanning thru the name */
    char *nameStart;     /* First non-'-' character in the font name */
    int pointSize;       /* The point size for this font in pixels */

    cp = fontnames[i];
    k = 0;
    while( *cp=='-' ){
      cp++;
      k++;
    }
    nameStart = cp;
    while( *cp && k<7 ){
      if( *cp=='-' ){
        k++;
        if( k==6 ) *cp = 0;
      }
      cp++;
    }
    pointSize = atoi(cp);
    if( pointSize<=0 ) continue;  /* Ignore scalable fonts */
    ET( 
      if {![info exists {Font(%s(nameStart))}]} {
        set {Font(%s(nameStart))} {}
        incr FontCount
      }
      lappend {Font(%s(nameStart))} {%s(cp)}
    );
  }
  XFreeFontNames(fontnames);
  ET(
    foreach i [array names Font] {
      set Font($i) [lsort -command FontSizeCompare $Font($i)]
    }
  );
  return ET_OK;
}

/* This procedure is used to sort font size fields.  By "font size" we
** mean the tail of the font name, as follows:
**
**                      10-100-75-75-c-60-iso8859-1
**                      |   |  \___/    | \_______/
**                      |   |    |      |     `-- Always as shown
**                      |   |    |      |
**   The point size ----'   |    |      `--- 10x average width
**                          |    |
** 10x point size (ignored)-'    `--- Resolution in dots per inch (ignored)
**
** We want to sort the font sizes in accending order using the point
** size as the primary sort key and the average width as the secondary
** key.  This command is called from the "lsort" command with two
** arguments which are to font sizes to be compared.  We must return
** negative, zero, or positive if the first font size is less than,
** equal to, or greater than the second.
*/
ET_PROC( FontSizeCompare ){
  int leftHeight;
  int leftWidth;
  int rightHeight;
  int rightWidth;
  int result;

  if( argc!=3 ){
    interp->result = "Wrong # args";
    return ET_ERROR;
  }
  if( sscanf(argv[1],"%d-%*d-%*d-%*d-%*c-%d",&leftHeight,&leftWidth)!=2 ){
    interp->result = "First argument is not a font size";
    return ET_ERROR;
  }
  if( sscanf(argv[2],"%d-%*d-%*d-%*d-%*c-%d",&rightHeight,&rightWidth)!=2 ){
    interp->result = "Second argument is not a font size";
    return ET_ERROR;
  }
  result = leftHeight - rightHeight;
  if( result==0 ) result = leftWidth - rightWidth;
  sprintf(interp->result,"%d",result);
  return ET_OK;
}
