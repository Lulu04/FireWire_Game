@echo off

set "APPNAME=FireAndWire"
set "PROJECTFOLDER=C:\Pascal\FireWire_Game\"
set "EXENAME=%APPNAME%.exe"
set "BINARYFOLDER=%PROJECTFOLDER%Binary\"
set "BINARYFILE=%BINARYFOLDER%%EXENAME%"
set "DEBUGFILE=%BINARYFOLDER%%APPNAME%.dbg"
set "LINUXBINARYFILE=%BINARYFOLDER%%APPNAME%"
set "LAZARUS_PROJECT=%PROJECTFOLDER%%APPNAME%.lpi"

rem retrieves the app version
pushd ..\..
set /p VERSION=<version.txt
popd


rem delete binary file
if exist %BINARYFILE% (
  del /q %BINARYFILE%
)

rem delete dbg file
if exist %DEBUGFILE% (
  del /q %DEBUGFILE%
)

rem delete linux binary file
if exist %LINUXBINARYFILE% (
  del /q %LINUXBINARYFILE%
)

rem atlas png file
if exist "%BINARYFOLDER%Atlas.png" (
  del /q "%BINARYFOLDER%Atlas.png"
)

rem compile lazarus project
echo Compiling %EXENAME% version %VERSION% for x86_64
"C:\lazarus\lazbuild.exe" --build-all --quiet --widgetset=win32 --cpu=x86_64 --build-mode=Release --no-write-project %LAZARUS_PROJECT% >NUL 2>NUL

rem check if binary was build
if not exist %BINARYFILE% (
  echo COMPILATION ERROR FOR TARGET x86_64
  pause
  exit /b
)
echo success
echo.

echo constructing 64b zip version
rem copy Binary folder to a temp folder
xcopy %BINARYFOLDER% "%PROJECTFOLDER%release_tools\win\%APPNAME%_%VERSION%" /s /e /i /q
rem delete unecessary folder
rmdir /s /q "%PROJECTFOLDER%release_tools\win\%APPNAME%_%VERSION%\i386-linux"
rmdir /s /q "%PROJECTFOLDER%release_tools\win\%APPNAME%_%VERSION%\i386-win32"
rmdir /s /q "%PROJECTFOLDER%release_tools\win\%APPNAME%_%VERSION%\x86_64-linux"
rmdir /s /q "%PROJECTFOLDER%release_tools\win\%APPNAME%_%VERSION%\x86_64-darwin"

echo compressing
tar.exe -a -c -f "..\%APPNAME%_%VERSION%_Windows64.zip" "%APPNAME%_%VERSION%"

rem delete temp folder
rmdir /s /q "%PROJECTFOLDER%release_tools\win\%APPNAME%_%VERSION%"
echo done
echo.

rem delete binary file
if exist %BINARYFILE% (
  del /q %BINARYFILE%
)
rem delete dbg file
if exist %DEBUGFILE% (
  del /q %DEBUGFILE%
)

rem compile lazarus project
echo Compiling %EXENAME% version %VERSION% for i386
"C:\lazarus\lazbuild.exe" --build-all --quiet --widgetset=win32 --cpu=i386 --build-mode=Release --no-write-project %LAZARUS_PROJECT% >NUL 2>NUL

rem check if binary was build
if not exist %BINARYFILE% (
  echo COMPILATION ERROR FOR TARGET i386
  pause
  exit /b
)
echo success
echo.

echo constructing 32b zip version
rem copy Binary folder to a temp folder
xcopy %BINARYFOLDER% "%PROJECTFOLDER%release_tools\win\%APPNAME%_%VERSION%" /s /e /i /q
rem delete unecessary folder
rmdir /s /q "%PROJECTFOLDER%release_tools\win\%APPNAME%_%VERSION%\i386-linux"
rmdir /s /q "%PROJECTFOLDER%release_tools\win\%APPNAME%_%VERSION%\x86_64-win64"
rmdir /s /q "%PROJECTFOLDER%release_tools\win\%APPNAME%_%VERSION%\x86_64-linux"
rmdir /s /q "%PROJECTFOLDER%release_tools\win\%APPNAME%_%VERSION%\x86_64-darwin"

echo compressing
tar.exe -a -c -f "..\%APPNAME%_%VERSION%_Windows32.zip" "%APPNAME%_%VERSION%"

rem delete temporary folder
rmdir /s /q "%PROJECTFOLDER%release_tools\win\%APPNAME%_%VERSION%"
echo.

rem delete binary file
if exist %BINARYFILE% (
  del /q %BINARYFILE%
)

echo done
