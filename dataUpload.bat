@echo off
:: Move to the correct directory
cd C:\Buoy\
:: Download the buoy data from WQDataLive
"C:\Program Files (x86)\WinSCP\WinSCP.com" /ini=nul /script=getData.txt
ECHO FTP complete
ECHO START buoy plots
:: Create plots and data files using R script
"C:\Program Files\R\R-3.5.0\bin\x64\Rcmd.exe" BATCH "C:\Buoy\buoyPlots_v3.R"
:: Copy the files to the github directory for pushing 
ECHO COPY files
copy /Y current_conditions.png C:\Users\eleib003\Documents\GitHub\plattsBuoy.github.io\.
copy /Y all_T49m.png C:\Users\eleib003\Documents\GitHub\plattsBuoy.github.io\.
copy /Y all_T25m.png C:\Users\eleib003\Documents\GitHub\plattsBuoy.github.io\.
copy /Y all_T10m.png C:\Users\eleib003\Documents\GitHub\plattsBuoy.github.io\.
copy /Y all_T5m.png C:\Users\eleib003\Documents\GitHub\plattsBuoy.github.io\.
copy /Y all_T1m.png C:\Users\eleib003\Documents\GitHub\plattsBuoy.github.io\.
copy /Y last7_T49m.png C:\Users\eleib003\Documents\GitHub\plattsBuoy.github.io\.
copy /Y last7_T25m.png C:\Users\eleib003\Documents\GitHub\plattsBuoy.github.io\.
copy /Y last7_T10m.png C:\Users\eleib003\Documents\GitHub\plattsBuoy.github.io\.
copy /Y last7_T5m.png C:\Users\eleib003\Documents\GitHub\plattsBuoy.github.io\.
copy /Y last7_T1m.png C:\Users\eleib003\Documents\GitHub\plattsBuoy.github.io\.
copy /Y all_airTemp.png C:\Users\eleib003\Documents\GitHub\plattsBuoy.github.io\.
copy /Y all_pres.png C:\Users\eleib003\Documents\GitHub\plattsBuoy.github.io\.
copy /Y all_wind.png C:\Users\eleib003\Documents\GitHub\plattsBuoy.github.io\.
copy /Y all_Temp_valcour.png C:\Users\eleib003\Documents\GitHub\plattsBuoy.github.io\.
copy /Y all_Temp.png C:\Users\eleib003\Documents\GitHub\plattsBuoy.github.io\.
copy /Y last7_airTemp.png C:\Users\eleib003\Documents\GitHub\plattsBuoy.github.io\.
copy /Y last7_pres.png C:\Users\eleib003\Documents\GitHub\plattsBuoy.github.io\.
copy /Y last7_wind.png C:\Users\eleib003\Documents\GitHub\plattsBuoy.github.io\.
copy /Y last7_Temp_valcour.png C:\Users\eleib003\Documents\GitHub\plattsBuoy.github.io\.
copy /Y temp_profile.png C:\Users\eleib003\Documents\GitHub\plattsBuoy.github.io\.
copy /Y last7.valcour.csv C:\Users\eleib003\Documents\GitHub\plattsBuoy.github.io\.
copy /Y all.valcour.csv C:\Users\eleib003\Documents\GitHub\plattsBuoy.github.io\.

:: Clean up the archive master files (not operational ones)
move master.valcour_* master\.
:: Move to the githubo folder, add files, commit, and push
cd C:\Users\eleib003\Documents\GitHub\lcri99.github.io\
ECHO DO git
git add *.png *.csv
::git add *.png timeSeries.gif
git commit -m "latest pngs and csvs"
git push
:: Go back to Buoy directory and upload NDBC files
:: cd C:\Buoy
:: ECHO DO NDBC TRANSFER BUT WAIT FIRST
:: Wait 120 seconds to avoid peak transfer time, which was a problem
:: timeout 120
:: "C:\Program Files (x86)\WinSCP\WinSCP.com" /ini=nul /script=sendNDBC.txt
:: timeout 15
:: Archive NDBC files
:: move Sta45178* sta\.
