# /bin/sh

echo "Running HDSTOOLS test, should say \"HCREATE Version ...\""
./hcreate type=_real inp=./htest 
rm -f htest.sdf
