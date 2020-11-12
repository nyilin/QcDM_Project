cd /d %~dp0
SET PATH=%PATH%;%CD%\..\..\PortableEnv\Pandoc;%CD%\..\..\PortableEnv\TinyTeX\bin\win32;%CD%
"..\..\PortableEnv\R-Portable\App\R-Portable\bin\RScript.exe" "Launch QcDMui_browser.R"
pause