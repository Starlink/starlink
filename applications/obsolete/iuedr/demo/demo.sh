echo '!  Copying files to working directory.';
cp $IUEDR_DEMO/swp14931.phot .;
cp $IUEDR_DEMO/demo.cmd .;
echo '!  Running IUEDR, please wait...';
iuedr < demo.cmd > demo.lis;
echo '!  Differences with log:';
diff demo.lis $IUEDR_DEMO/demo.ref;
