package Starlink::AST::Tk;

=head1 NAME

Starlink::AST::Tk - AST wrapper to the Tk library

=head1 SYNOPSIS

   use Starlink::AST::Tk

The main methods which need to be registered with the AST package
are shown below,

   $status = _GFlush( $w );
   $status = _GLine( $w, \@x, \@y );
   $status = _GMark( $w, \@x, \@y, $type );
   $status = _GText( $w, $text, $x, $y, $just, $upx, $upy );
   ( $status, $xb, $yb ) = _GTxtExt( $w, $text, $x, $y, $just, $upx, $upy );
   ( $status, $chv, $chh ) = _GQch( $w );
   ( $status, $old_value ) = _GAttr( $w, $attr, $value, $prim );

The following helper methods are also provided,

   my ( $status, $alpha, $beta ) = _GScales( $w )

=head1 DESCRIPTION

This file implements the low level graphics functions required by the rest
of AST, by calling suitable Tk::Canvas functions. In all the routines $w
is a reference to the Tk::Canvas object on which we're plotting.

=head1 NOTES

All the functions in this module are private, and are intended to be called
from the AST module. None of these functions should be considered to be part
of the packages public interface.

=head1 REVISION

$Id$

=cut

use strict;
use vars qw/ $VERSION /;
use constant R2D     => 57.29578;        # Radians to degrees factor
use constant FLT_MAX => 3.40282347e+38;  # Maximum float on ix86 platform

use Tk;
use Tk::Font;
use Starlink::AST;
use Carp;

'$Revision$ ' =~ /.*:\s(.*)\s\$/ && ($VERSION = $1);

# Constants describing locations in the external array
use constant EXT_ATTR => 9;

# reference height for characters as a fraction of the 
# full height/width of the display
use constant DEFAULT_CHAR_HEIGHT => 1 / 40;

# Static look up tables mapping AST index to Tk equivalent
# Note that the "black" and "white" entries are really default
# background + foreground
# They should be obtained from the canvas itself
my @COLOURS = qw( black
		  white
		  red
		  green
		  blue
		  cyan
		  magenta
		  yellow
		  orange
		  chartreuse
		  springgreen
		  skyblue
		  purple
		  pink
		  darkgrey
		  grey
	       );

# Line styles are
#    solid, long dashes, dash-dot-dash-dot, dotted and dash-dot-dot-dot.
my @LINE_STYLES = (undef,
		   undef,  # default is solid
		   '-',
		   '-.',
		   '..',
		   '-...',
		  );

# Look up from AST attribute number to Attribute hash key
my %GRF_PRIM = (
		&Starlink::AST::Grf::GRF__LINE() => 'LINE',
		&Starlink::AST::Grf::GRF__MARK() => 'MARK',
		&Starlink::AST::Grf::GRF__TEXT() => 'TEXT',
	       );

# Similarly for style
my %GRF_STYLE = (
		 &Starlink::AST::Grf::GRF__STYLE() => 'LINE',
		 &Starlink::AST::Grf::GRF__WIDTH() => 'WIDTH',
		 &Starlink::AST::Grf::GRF__SIZE() => 'SIZE',
		 &Starlink::AST::Grf::GRF__FONT() => 'FONT',
		 &Starlink::AST::Grf::GRF__COLOUR() => 'COLOUR',
		);

# Text Anchors
# points on a compass except no rotation is allowed
my @AnchorPoints = qw/ n ne e se s sw w ne /;
my @AnchorAng    = qw/ 0 45 90 135 180 225 270 315 /;
my %Text_Anchors = (
		    'CC' => 'center',
		    'CL' => 'w',
		    'CR' => 'e',

		    'TC' => 'n',
		    'TL' => 'nw',
		    'TR' => 'ne',

		    'BC' => 's',
		    'BL' => 'sw',
		    'BR' => 'se',
		   );


=head1 METHODS

=over 4

=item B<_GFlush>

This function ensures that the display device is up-to-date, by flushing 
any pending graphics to the output device.

   my $status = _GFlush( $w );

=cut

sub _GFlush {
   my $external = shift;
   my $canvas = $$external[0];
   $canvas->update();
   return 1;
}

=item B<_GLine>

This function displays lines joining the given positions.

   my $status = _GLine( $w, \@x, \@y );

=cut

sub _GLine {
   my ( $external, $xf, $yf ) = @_;
   my $canvas = $$external[0];

   #use Data::Dumper;
   #print "\n# _GLine()\n";
   #print Dumper( $xf ) . "\n";
   #print Dumper( $yf ) . "\n";

   if( scalar(@$xf) > 1 && scalar(@$xf) == scalar(@$yf) ) {

     # convert GRAPHICS coordinates to pixel coordinates
     my @points = map {
       _CooTranslate( $external, $xf->[$_], $yf->[$_]);
     } ( 0.. $#$xf );

     # modifiers
     my %opts;

     # Now add additional style information
     %opts = _attr_to_colour( $canvas, $external->[EXT_ATTR], 'LINE' );
     my $t = $canvas->createLine( @points, %opts );
     $canvas->addtag( 'ASTGLine', 'withtag', $t);

   }
   return 1;
}

=item B<_GMark>

This function displays markers at the given positions.

   my $status = _GMark( $w, \@x, \@y, $type );

where $type is an integer used to indicate the type of marker required.

=cut

sub _GMark {
   my ($external, $xf, $yf, $type) = @_;
   my $canvas = $$external[0];
   my $width = $canvas->cget( '-width' );

   #use Data::Dumper;
   #print "\n# _GMark()\n";
   #print Dumper( $xf ) . "\n";
   #print Dumper( $yf ) . "\n";

   if( scalar(@$xf) && scalar(@$xf) == scalar(@$yf) ) {

     my %opts;
     %opts = _attr_to_colour( $canvas, $external->[EXT_ATTR], 'MARK');

     # Get the symbol size in pixels
     my $size = _char_height( $canvas, $external->[EXT_ATTR], 'MARK');

     # and work out how the symbol should be displayed

     # scale factor for some of the types
     my $scale = 1;

     my $prim;
     my $fill = 0;

     # Set parameters for duplicated symbols
     if ($type == 0 || $type == 6 || $type == 19 || $type == 16) {
       # squares of all types
       $prim = 'square';
       if ($type == 19) {
	 $scale = 1.5;
       } elsif ($type == 16) {
	 $scale = 0.5;
	 $fill = 1;
       }
     } elsif ($type == 1 || $type == 4 || $type == 17 ||
	      ( $type >= 20 && $type <= 27)) {
       $prim = 'circle';

       if ( $type == 1 ) {
	 $scale = 0.1;
       } elsif ($type == 4 || $type == 23) {
	 $scale = 1;
       } elsif ($type == 17) {
	 $scale = 0.75;
	 $fill = 1;
       } elsif ($type == 20) {
	 $scale = 0.25;
       } elsif ($type == 21) {
	 $scale = 0.5;
       } elsif ($type == 22) {
	 $scale = 0.75;
       } elsif ($type == 24) {
	 $scale = 1.5;
       } elsif ($type == 25) {
	 $scale = 1.75;
       } elsif ($type == 26) {
	 $scale = 2.0;
       } elsif ($type == 27) {
	 $scale = 3.0;
       }

     } elsif ($type == 2 ) {
       # cross
       $prim = 'cross';
     } else {
       # do not yet know how to render this. Default
       $prim = 'circle';
       $scale = 1;
       $fill = 1;
     }

     # Configure the fill color
     if (!$fill) {
       # by default the color to opt code uses -fill
       # but we need to use -outline if fill is not specified
       if (exists $opts{'-fill'}) {
	 $opts{'-outline'} = $opts{'-fill'};
	 delete $opts{'-fill'};
       }
     }

     foreach my $i ( 0 ... $#$xf ) {

       # convert to canvas coordinates
       my ( $x, $y ) =  _CooTranslate( $external, $xf->[$i], $yf->[$i]);

       # now draw them
       my $item;
       if ($prim eq 'square') {

	 my $dist = $size * $scale / 2;
	 my $x1 = $x - $dist;
	 my $x2 = $x + $dist;
	 my $y1 = $y - $dist;
	 my $y2 = $y + $dist;

	 $item = $canvas->createRectangle( $x1, $y1, $x2, $y2, %opts );

       } elsif ($prim eq 'circle') {

	 my $dist = $size * $scale / 2;
	 my $x1 = $x - $dist;
	 my $x2 = $x + $dist;
	 my $y1 = $y - $dist;
	 my $y2 = $y + $dist;

	 $item = $canvas->createOval( $x1, $y1, $x2, $y2, %opts );


       } elsif ($prim eq 'cross') {

	 # we may have to respect line widths
	 my $dist = $size * $scale / 2;
	 my $x1 = $x - $dist;
	 my $x2 = $x + $dist;
	 my $y1 = $y - $dist;
	 my $y2 = $y + $dist;

	 $item = $canvas->createLine( $x, $y1, $x, $y2, %opts );
	 $canvas->addtag( 'ASTGMark', 'withtag', $item);
	 $item = $canvas->createLine( $x1, $y, $x2, $y, %opts );



       } else {
	 # should not happen
	 ReportGrfError("_GMark: Bizarre inability to determine symbol type");
	 return (0);
       }

       $canvas->addtag( 'ASTGMark', 'withtag', $item);
     }
   }
   return 1;
}

=item B<_GText>

This function displays a character string $text at a given position using 
a specified justification and up-vector.

   my $status = _GText( $text, $x, $y, $just, $upx, $upy );

where $x is the reference x coordinate, $y is the reference y coordinate, 
and where $just is a character string which specifies the location within
the text string which is to be placed at the reference position given by x
and y. The first character may be 'T' for "top", 'C' for "centre", or 'B'
for "bottom", and specifies the vertical location of the reference position.
Note, "bottom" corresponds to the base-line of normal text. Some characters 
(eg "y", "g", "p", etc) descend below the base-line. The second  character
may be 'L' for "left", 'C' for "centre", or 'R'  for "right", and specifies
the horizontal location of the  reference position. If the string has less
than 2 characters then 'C' is used for the missing characters.

And $upx is the x component of the up-vector for the text, in graphics
world coordinates. If necessary the supplied value should be negated to
ensure that positive values always refer to displacements from  left to
right on the screen.

While $upy is the y component of the up-vector for the text, in graphics
world coordinates. If necessary the supplied value should be negated to
ensure that positive values always refer to displacements from  bottom to
top on the screen.

Note that we match the PGPLOT definition of default character height.
ie the default character height one-fortieth of the height or width of
the view surface (whichever is less).

An optional argument can be provided at the end of the argument list
(AST will not do this). This should be a ref to a scalar. On return,
it will contain the object that was just plotted.

=cut

sub _GText {
   my ( $external, $text, $xf, $yf, $just, $upx, $upy, $ref ) = @_;
   my $canvas = $$external[0];
   #print "# _GText: Placeholder routine called\n";

   #use Data::Dumper;
   #print "\n# _GText( $text )\n";
   #print Dumper( $xf ) . "\n";
   #print Dumper( $yf ) . "\n";

   print "$text : $just \n" if $text =~ /decl/i;

   if( defined $text && length($text) != 0 ) {

     # work out the X and Y anchoring
     my $xa = substr($just,1,1) || 'C';
     my $ya = substr($just,0,1) || 'C';

     # and reconstruct it
     $just = $ya. $xa;

     # Try to fudge the most common rotation
     if ($upx == 1 && $upy == 0 ) {
       # massage to attempt vertical text
       $text = join("\n", split(//, $text));
     } elsif ($upx == -1 && $upy == 0 ) {
       # massage to attempt vertical text
       $text = join("\n", reverse split(//, $text));
     }

     # option handling
     my %opts;
     %opts = _attr_to_colour( $canvas, $external->[EXT_ATTR], 'TEXT');

     # translate to the pixel coordinates
     my ( $x, $y ) =  _CooTranslate($external, $xf, $yf);

     # Specify text anchor
     $opts{'-anchor'} = ( exists $Text_Anchors{$just} ?
			  $Text_Anchors{$just} : $Text_Anchors{CC});


     # apply scaling
     my $charh = _char_height( $canvas, $external->[EXT_ATTR], 'TEXT');

     # select the font object
     my $fi = $external->[EXT_ATTR]->{TEXT}->{FONT};
     my $font = $external->[EXT_ATTR]->{FONTS}->[$fi];

     # size in pixels is specified as negative number
     $font->configure( '-size' => (-1 * $charh ) );

     # draw text
     my $item = $canvas->createText( $x, $y, -text => $text, %opts,
				     -font => $font,
				   );
     $canvas->addtag( 'ASTGText', 'withtag', $item);

     # if we have a scalar ref
     $$ref = $item if defined $ref;

   }

   # Return, all is well strangely
   return 1;
}


=item B<_GScales>

This function returns two values (one for each axis) which scale
increments on the corresponding axis into a "normal" coordinate system in
which: The axes have equal scale in terms of (for instance) millimetres
per unit distance, X values increase from left to right and the Y values 
increase from bottom to top.

   my ( $status, $alpha, $beta ) = _GScales( $w )

=cut

sub _GScales {
    my ( $external, $alpha, $beta ) = @_;
    #print "# _GScales: Placeholder routine called\n";
    my $canvas = $$external[0];
    my ($xglo,$xghi,$yglo,$yghi) = @$external[1 .. 4];
    my ($xplo,$xphi,$yplo,$yphi) = @$external[5 .. 8];
    my ($xmin,$xmax,$ymin,$ymax) = _CooBox($external);

    my ($nx1, $nx2, $ny1, $ny2);
    my ($wx1, $wx2, $wy1, $wy2);

    $nx1 = $xmin;
    $nx2 = $xmax;
    $ny1 = $ymax;
    $ny2 = $ymin;

    $wx1 = $xglo;
    $wx2 = $xghi;
    $wy1 = $yghi;
    $wy2 = $yglo;

    if( $wx2 != $wx1 && $wy2 != $wy1 && $nx2 != $nx1 && $ny2 != $ny1 ) {
       $alpha = ( $nx2 - $nx1 ) / ( $wx2 - $wx1 );
       $beta = ( $ny2 - $ny1 ) / ( $wy2 - $wy1 );
    } else {
       ReportGrfError("_GScales: The graphics window has zero size");
       return (0);
    }
    return ( 1, $alpha, $beta );
}


=item B<_GTxExt>

This function returns the corners of a box which would enclose the 
supplied character string if it were displayed using astGText. The 
returned box INCLUDES any leading or trailing spaces.

   my ( $status, $xb, $yb ) = _GTxExt( $w, $text, $x, $y, $just, $upx, $upy);

where $x is the reference x coordinate, $y is the reference y coordinate, 
and where $justification is a character string which specifies the
location within the text string which is to be placed at the reference
position given by x and y. The first character may be 'T' for "top", 'C'
for "centre", or 'B' for "bottom", and specifies the vertical location of
the reference position. Note, "bottom" corresponds to the base-line of
normal text. Some characters  (eg "y", "g", "p", etc) descend below the
base-line. The second  character may be 'L' for "left", 'C' for "centre",
or 'R'  for "right", and specifies the horizontal location of the 
reference position. If the string has less than 2 characters then 'C' is
used for the missing characters. 

And $upx is the x component of the up-vector for the text, in graphics
world coordinates. If necessary the supplied value should be negated to
ensure that positive values always refer to displacements from  left to
right on the screen.

While $upy is the y component of the up-vector for the text, in graphics
world coordinates. If necessary the supplied value should be negated to
ensure that positive values always refer to displacements from  bottom to
top on the screen.

Finally $xb is a refernce to an array of 4 elements in which to return the
x coordinate of each corner of the bounding box, and $yb is a reference to
an array of 4 elements in which to return the y coordinate of each corner
of the bounding box.

Notes:
     -  The order of the corners is anti-clockwise (in world coordinates)
        starting at the bottom left.
     -  A NULL value for "just" causes a value of "CC" to be used.
     -  Both "upx" and "upy" being zero causes an error.
     -  Any unrecognised character in "just" causes an error.
     -  Zero is returned for all bounds of the box if an error occurs.

=cut

sub _GTxExt {
  my $external = shift;
  my $canvas = $external->[0];

  # The easy plan is to:
  #   get GText to plot the text
  #   Attach a new tag to it
  #   read the bounding box
  #   delete the item from the canvas
  my $item;
  _GText( $external, @_, \$item );

  # add a unique tag
  $canvas->addtag('ASTGTxExt_TEMP', 'withtag', $item);

  # read the bounding box
  my @bbox = $canvas->bbox( 'ASTGTxExt_TEMP' );

  # delete the text
  $canvas->delete( 'ASTGTxExt_TEMP' );

  # to convert these coordinates back to the correct units
  # we need the width and height of the canvas
  my $width = $canvas->cget( '-width' );
  my $height = $canvas->cget( '-height' );

  # convert to AST form
  my @xb = @bbox[0,2,2,0];
  my @yb = @bbox[1,1,3,3];

  @xb = map { $_ / $width } @xb;
  @yb = map { 1 - $_ / $height } @yb;

  #use Data::Dumper;
  #print Dumper([$_[0], \@xb, \@yb]);

  # Return
  return (1, \@xb, \@yb);
}

=item B<_GQch>

This function returns the heights of characters drawn vertically and
horizontally in world coordinates.

   my ( $status, $chv, $chh ) = _GQch( $w );

Where $chv is the height of characters drawn with a vertical
baseline. This will be an increment in the X axis.  Where $chh is the
height of characters drawn with a horizontal baseline. This will be an
increment in the Y axis.


=cut

sub _GQch {
   my $external = shift;
   my $canvas = $$external[0];
   #print "# _GQch: Placeholder routine called\n";

   # the Tk definition of world coordinates (from AST viewpoint)
   # is fraction of viewport
   my $charh = _char_height( $canvas, $external->[EXT_ATTR], 'TEXT' );
   my $chv = $charh / $canvas->cget( '-width' );
   my $chh = $charh / $canvas->cget( '-height' );
   return (1, $chv, $chh );
}


=item B<_GAttr>

This function returns the current value of a specified graphics
attribute, and optionally establishes a new value. The supplied
value is converted to an integer value if necessary before use.


   my ( $status, $old_value ) = _GAttr( $w, $attr, $value, $prim );

Where $attr is an integer value identifying the required attribute. 
The following symbolic values are defined in the AST grf.h:

           GRF__STYLE  - Line style.
           GRF__WIDTH  - Line width.
           GRF__SIZE   - Character and marker size scale factor.
           GRF__FONT   - Character font.
           GRF__COLOUR - Colour index.

$value is a new value to store for the attribute. If this is 
AST__BAD no value is stored, and $old_value is a scalar containing
the old attribute value, if this is NULL no value is returned. 

Finally $prim is the sort of graphics primitive to be drawn with 
the new attribute. Identified by the following values defined in 
AST's grf.h:

           GRF__LINE
           GRF__MARK
           GRF__TEXT

=cut

sub _GAttr {
   my ( $external, $att, $val, $prim ) = @_;

   # Get the attribute hash
   my $attr = $external->[EXT_ATTR];

   # possible return value
   my $old_value = undef;

   # determine which primitive we are dealing with
   my $PRIM = $GRF_PRIM{$prim};
   if (!defined $PRIM) {
     ReportGrfError("_GAttr: Unknown primitive: $prim\n");
     return ( 0 );
   }

   # and which style
   my $STYLE = $GRF_STYLE{$att};
   if (!defined $STYLE) {
     ReportGrfError("_GAttr: Unknown style: $att\n");
     return ( 0 );
   }

   # Now process the style request, relying on each plotting system
   # to be able to work out what these numbers mean
   $old_value = $attr->{$PRIM}->{$STYLE};
   $attr->{$PRIM}->{$STYLE} = $val if $val != &Starlink::AST::AST__BAD();

   # map to bad value if appropriate
   $old_value = &Starlink::AST::AST__BAD() if !defined $old_value;

   return (1, $old_value);
}


=item B<_GCap>

This function is called by the AST Plot class to determine if the
grf module has a given capability, as indicated by the "cap"
argument.

  $has_cap = _GCap( $cap, $value );

The capability string should be one of the following constants
provided in the Starlink::AST::Grf namespace:

GRF__SCALES: This function should return a non-zero value if
it implements the astGScales function, and zero otherwise. The
supplied "value" argument should be ignored.

GRF__MJUST: This function should return a non-zero value if
the astGText and astGTxExt functions recognise "M" as a
character in the justification string. If the first character of
a justification string is "M", then the text should be justified
with the given reference point at the bottom of the bounding box.
This is different to "B" justification, which requests that the
reference point be put on the baseline of the text, since some
characters hang down below the baseline. If the astGText or
astGTxExt function cannot differentiate between "M" and "B",
then this function should return zero, in which case "M"
justification will never be requested by Plot. The supplied
"value" argument should be ignored.

GRF__ESC: This function should return a non-zero value if the
astGText and astGTxExt functions can recognise and interpret
graphics escape sequences within the supplied string. These
escape sequences are described below. Zero should be returned
if escape sequences cannot be interpreted (in which case the
Plot class will interpret them itself if needed). The supplied
"value" argument should be ignored only if escape sequences cannot
be interpreted by astGText and astGTxExt. Otherwise, "value"
indicates whether astGText and astGTxExt should interpret escape
sequences in subsequent calls. If "value" is non-zero then
escape sequences should be interpreted by astGText and
astGTxExt. Otherwise, they should be drawn as literal text.

Zero should be returned if the supplied capability is not recognised.

=cut

sub _GCap {
  my $cap = shift;
  my $value = shift;
  #print "# _GCap: Placeholder routine called [assume lack capability]\n";
  return 0;
}

# Internal error setting routine
sub ReportGrfError {
  my $text = shift;
  warn "Generated AST error in perl Tk callback: $text\n";
  Starlink::AST::_Error( &Starlink::AST::Status::AST__GRFER(), $text);
}


sub _CooBox {
   my $external = shift;
   my $canvas = $$external[0];
   my ($xglo,$xghi,$yglo,$yghi) = @$external[1 .. 4];
   my ($xplo,$xphi,$yplo,$yphi) = @$external[5 .. 8];

   my $width = $canvas->cget( '-width' );
   my $height = $canvas->cget( '-height' );
   
   my $xleft  = $xglo*$width;
   my $xright = $xglo*$width + $xphi;
   my $ybottom = $yghi*$height;
   my $ytop    = $yghi*$height - $yphi;
   
   #print "# width = $width, height = $height\n";
   #print "# Gbox $xglo,$xghi,$yglo,$yghi\n";
   #print "# Pbox $xplo,$xphi,$yplo,$yphi\n";
   #print "# xleft = $xleft, xright = $xright\n";
   #print "# ytop = $ytop, ybottom = $ybottom\n";
   
   return ($xleft,$xright,$ybottom,$ytop);
}

# convert GRAPHICS coordinates (0,1)
# to canvas pixel coordinates

sub _CooTranslate {
   my ($external, $xf, $yf) = @_;
   my $canvas = $$external[0];
   my ($xglo,$xghi,$yglo,$yghi) = @$external[1 .. 4];
   my ($xplo,$xphi,$yplo,$yphi) = @$external[5 .. 8];
   my ($xmin,$xmax,$ymin,$ymax) = _CooBox($external);

   my $width = $canvas->cget( '-width' );
   my $height = $canvas->cget( '-height' );
   
   my $x = $xf*$width;
   my $y = (1 - $yf)*$height;

   #print "# _CooTranslate( $xf, $yf )\n";
   #print "# width = $width, height = $height\n";
   #print "# Gbox $xglo,$xghi,$yglo,$yghi\n";
   #print "# Pbox $xplo,$xphi,$yplo,$yphi\n";      
   #print "# X $xf -> $x\n# Y $yf -> $y\n\n";
   return ( $x, $y );
}   


# Given the attributes hash and the primitive type,
# return a hash with the correct arguments for the canvas
# color settings

sub _attr_to_colour {
  my ($canvas, $external, $PRIM) = @_;

  my %opts;
  my $ci = $external->{$PRIM}->{COLOUR};
  if (defined $ci && $ci >= 0 && $ci <= $#COLOURS) {
    # background color
    if ($ci == 0) {
      $opts{'-fill'} = $canvas->cget('-background');
    } else {
      $opts{'-fill'} = $COLOURS[$ci]
    }
  }
  return %opts;
}

# Calculate the default character height

# To match PGPLOT we define the default character height one-fortieth
# of the height or width of the view surface (whichever is less).

# Size specifications are then relative to this unit

# this height is returned in pixels

sub _def_char_height {
  my $canvas = shift;
  my $width = $canvas->cget('-width');
  my $height = $canvas->cget('-height');

  my $wh = $width * DEFAULT_CHAR_HEIGHT;
  my $hh = $height * DEFAULT_CHAR_HEIGHT;

  return ( $wh < $hh ? $wh : $hh );
}

# Return the character height in pixels for this particular primitive
# type.
# Requires the canvas, attr hash and the primitive name (TEXT,MARK,LINE)

sub _char_height {
  my ($canvas, $attr, $prim) = @_;

  my $dch = _def_char_height( $canvas );
  my $scale = $attr->{$prim}->{SIZE};
  $scale = 1 unless defined $scale;
  return ($dch * $scale);
}

# Routine to initialise the attributes. This includes the creation
# of Font objects that match the desired PGPLOT types

sub _init_canvas_attrs {
  my ($canvas, $attr) = @_;

  # prefill size attr to 1 since that is what PGPLOT does
  $attr->{TEXT}->{SIZE} = 1;
  $attr->{TEXT}->{FONT} = 1;
  $attr->{MARK}->{SIZE} = 1;

  # This will be a lookup just like the Colour array except that
  # it has to be per-widget rather than global
  my @fonts;

  # Simple font
  $fonts[1] = $canvas->Font( -family => 'Courier',
			     -slant => 'roman',
			     -size => 12,
			   );

  # roman font
  $fonts[2] = $canvas->Font( -family => 'Times',
			     -slant => 'roman',
			     -size => 12,
			   );

  # italic
  $fonts[3] = $canvas->Font( -family => 'Time',
			     -slant => 'italic',
			     -size => 12,
			   );

  # script
  $fonts[4] = $canvas->Font( -family => 'Helvetica',
			     -slant => 'italic',
			     -size => 12,
			   );

  $attr->{FONTS} = \@fonts;

}

=back

=head1 COPYRIGHT

Copyright (C) 2004 University of Exeter.
Copyright (C) 2005 Particle Physics and Astronomy Research Council.
All Rights Reserved.

This program is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 2 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful,but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
this program; if not, write to the Free Software Foundation, Inc., 59 Temple
Place,Suite 330, Boston, MA  02111-1307, USA

=head1 AUTHORS

Alasdair Allan E<lt>aa@astro.ex.ac.ukE<gt>,
Tim Jenness E<lt>t.jenness@jach.hawaii.eduE<gt>

=cut

package Starlink::AST::Plot;

use strict;
use vars qw/ $VERSION /;

use Starlink::AST::Tk;

sub tk {
  my $self = shift;
  my $canvas = shift;

  # Store the information we need to pass to the plot functions
  my @external;
  push @external, $canvas;
  push @external, $self->GBox();
  push @external, $self->PBox();

  my $attr = {};
  push @external, $attr; # hash for attributes

  # initialise the attribute hash
  Starlink::AST::Tk::_init_canvas_attrs( $canvas, $attr );

  # register callbacks
  $self->GExternal( \@external );
  $self->GFlush(\&Starlink::AST::Tk::_GFlush);  
  $self->GLine(\&Starlink::AST::Tk::_GLine);
  $self->GMark(\&Starlink::AST::Tk::_GMark);
  $self->GText(\&Starlink::AST::Tk::_GText);
  $self->GTxExt(\&Starlink::AST::Tk::_GTxExt);
  $self->GQch(\&Starlink::AST::Tk::_GQch);
  $self->GAttr(\&Starlink::AST::Tk::_GAttr);
  $self->GScales(\&Starlink::AST::Tk::_GScales);
  $self->GCap(\&Starlink::AST::Tk::_GCap);


  return 1; 
}

1;
