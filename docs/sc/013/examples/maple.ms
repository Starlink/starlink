# read into maple with   > read `maple.ms`:

# Define a 2-d gaussian
gi:=amp*exp(-(aparam^2/sa^2+bparam^2/sb^2)/2):
aparam:=cos(theta)*(xc-x0)+sin(theta)*(yc-y0):
bparam:=-sin(theta)*(xc-x0)+cos(theta)*(yc-y0):

# Declare arrays.  We expressed the gaussian in terms of intelligible
# identifiers, but we want these to be output as array references when
# we produce the Fortran at the end 
xa := array(1..5):
ca := array(1..1):

x0 := xa[1]:
y0 := xa[2]:
sa := xa[3]:
sb := xa[4]:
theta := xa[5]:
amp := ca[1]:


# aadf are the variables we'll differentiate with respect to
aadf:=[x0,y0,sa,sb,theta]:
# ...and array of differentials.
dyda:=array(1..6):
for i to 5 do
        dyda[i]:=diff(gi,aadf[i]) 
od:
dyda[6] := gi:

# Output the result as Fortran
fortran (dyda, filename=`gaussianab.f`, optimized);
