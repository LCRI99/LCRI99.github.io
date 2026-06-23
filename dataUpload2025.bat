@echo off
:: Move to the correct directory
ECHO HELLO NEW R
cd C:\Buoy\
:: Download the buoy data from WQDataLive
ECHO ABOUT TO PULL DATA
"C:\Program Files (x86)\WinSCP\WinSCP.com" /ini=nul /script=getData.txt
ECHO FTP complete
ECHO START buoy plots
:: Create plots and data files using R script
"C:\Users\mihuctb\AppData\Local\Programs\R\R-4.2.1\bin\Rscript.exe" "C:\Buoy\buoyPlots_v5b.R"
:: Copy the files to the github directory for pushing 
ECHO COPY files
copy /Y current_conditions.png LCRI99.github.io\.
copy /Y all_T48m.png LCRI99.github.io\.
copy /Y all_T25m.png LCRI99.github.io\
copy /Y all_T10m.png LCRI99.github.io\
copy /Y all_T5m.png LCRI99.github.io\
copy /Y all_T1m.png LCRI99.github.io\
copy /Y last7_T48m.png LCRI99.github.io\
copy /Y last7_T25m.png LCRI99.github.io\
copy /Y last7_T10m.png LCRI99.github.io\
copy /Y last7_T5m.png LCRI99.github.io\
copy /Y last7_T1m.png LCRI99.github.io\
copy /Y all_airTemp.png LCRI99.github.io\
copy /Y all_pres.png LCRI99.github.io\
copy /Y all_wind.png LCRI99.github.io\
copy /Y all_Temp_valcour.png LCRI99.github.io\
copy /Y last7_airTemp.png LCRI99.github.io\
copy /Y last7_pres.png LCRI99.github.io\
copy /Y last7_wind.png LCRI99.github.io\
copy /Y last7_Temp_valcour.png LCRI99.github.io\
copy /Y temp_profile.png LCRI99.github.io\
copy /Y last7.valcour.csv LCRI99.github.io\
copy /Y all.valcour.csv LCRI99.github.io\
copy /Y all_solar.png LCRI99.github.io\
copy /Y last7_solar.png LCRI99.github.io\
copy /Y all_waveHeight.png LCRI99.github.io\
copy /Y last7_waveHeight.png LCRI99.github.io\
copy /Y all_wavePeriod.png LCRI99.github.io\
copy /Y last7_wavePeriod.png LCRI99.github.io\
copy /Y all_curr2.5m.png LCRI99.github.io\
copy /Y all_curr12.5m.png LCRI99.github.io\
copy /Y all_curr27.5m.png LCRI99.github.io\
copy /Y all_curr47.5m.png LCRI99.github.io\

copy /Y last7_curr2.5m.png LCRI99.github.io\
copy /Y last7_curr12.5m.png LCRI99.github.io\
copy /Y last7_curr27.5m.png LCRI99.github.io\
copy /Y last7_curr47.5m.png LCRI99.github.io\

copy /Y all_T49m_northlake.png LCRI99.github.io\.
copy /Y all_T25m_northlake.png LCRI99.github.io\
copy /Y all_T10m_northlake.png LCRI99.github.io\
copy /Y all_T5m_northlake.png LCRI99.github.io\
copy /Y all_T1m_northlake.png LCRI99.github.io\
copy /Y last7_T49m_northlake.png LCRI99.github.io\
copy /Y last7_T25m_northlake.png LCRI99.github.io\
copy /Y last7_T10m_northlake.png LCRI99.github.io\
copy /Y last7_T5m_northlake.png LCRI99.github.io\
copy /Y last7_T1m_northlake.png LCRI99.github.io\
copy /Y all_airTemp_northlake.png LCRI99.github.io\
copy /Y all_pres_northlake.png LCRI99.github.io\
copy /Y all_wind_northlake.png LCRI99.github.io\
copy /Y all_Temp_northlake.png LCRI99.github.io\
copy /Y last7_airTemp_northlake.png LCRI99.github.io\
copy /Y last7_pres_northlake.png LCRI99.github.io\
copy /Y last7_wind_northlake.png LCRI99.github.io\
copy /Y last7_Temp_northlake.png LCRI99.github.io\


:: Clean up the archive master files (not operational ones)
move master.valcour_* master\.
move master.36_* master\.

:: Move to the githubo folder, add files, commit, and push
cd LCRI99.github.io\
ECHO DO git
git add *.png *.csv index.html
::git add *.png timeSeries.gif
git commit -m "latest pngs and csvs"
git push
:: Go back to Buoy directory and upload NDBC files
cd C:\Buoy
:: ECHO DO NDBC TRANSFER BUT WAIT FIRST
:: Wait 120 seconds to avoid peak transfer time, which was a problem
timeout 120
"C:\Program Files (x86)\WinSCP\WinSCP.com" /ini=nul /script=sendNDBC.txt
timeout 15
:: Archive NDBC files
move Sta45178* sta\.
move Sta45221* sta\.
