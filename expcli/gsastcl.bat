REM a batch file to a DOS command and pause
if "%gsas%" == "" set gsas=c:\gsas
set PGPLOT_FONT=%gsas%\pgl\grfont.dat
set SYMOP=%gsas%\data\symop.dat
%1 %2 %3 %4 %5 %6 %7
pause
del expgui.lck
exit
