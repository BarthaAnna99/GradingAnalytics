@echo off
chcp 65001 > nul
setlocal enabledelayedexpansion

for /d /r %%i in (*) do (
    set "folder=%%~nxi"
    set "newfolder=!folder:ö=o!"
    set "newfolder=!newfolder:ü=u!"
    set "newfolder=!newfolder:ú=u!"
    set "newfolder=!newfolder:ü=u!"
    set "newfolder=!newfolder:ó=o!"
    set "newfolder=!newfolder:ő=o!"
    set "newfolder=!newfolder:ú=u!"
    set "newfolder=!newfolder:é=e!"
    set "newfolder=!newfolder:á=a!"
    set "newfolder=!newfolder:ű=u!"
    set "newfolder=!newfolder:í=i!"
	set "newfolder=!newfolder:Ö=O!"
    set "newfolder=!newfolder:Ü=U!"
    set "newfolder=!newfolder:Ú=U!"
    set "newfolder=!newfolder:Ü=U!"
    set "newfolder=!newfolder:Ó=O!"
    set "newfolder=!newfolder:Ő=O!"
    set "newfolder=!newfolder:Ú=U!"
    set "newfolder=!newfolder:É=E!"
    set "newfolder=!newfolder:Á=A!"
    set "newfolder=!newfolder:Ű=U!"
    set "newfolder=!newfolder:Í=I!"
    set "newfolder=!newfolder: =_!"
    
    if not "!folder!"=="!newfolder!" (
		cd /d ".\GradingAnalytics_SourceData"
        ren "!folder!" "!newfolder!"
        echo Renamed "!folder!" to "!newfolder!"
		cd /d "..\"
    )
)

for /r %%i in (*) do (
    set "oldName=%%~nxi"
    set "newName=!oldName:ö=o!"
    set "newName=!newName:ü=u!"
    set "newName=!newName:ó=o!"
    set "newName=!newName:ő=o!"
    set "newName=!newName:ú=u!"
    set "newName=!newName:é=e!"
    set "newName=!newName:á=a!"
    set "newName=!newName:ű=u!"
    set "newName=!newName:í=i!"
	set "newName=!newName:Ö=O!"
    set "newName=!newName:Ü=U!"
    set "newName=!newName:Ú=U!"
    set "newName=!newName:Ü=U!"
    set "newName=!newName:Ó=O!"
    set "newName=!newName:Ő=O!"
    set "newName=!newName:Ú=U!"
    set "newName=!newName:É=E!"
    set "newName=!newName:Á=A!"
    set "newName=!newName:Ű=U!"
    set "newName=!newName: =_!"
    
    if not "!oldName!"=="!newName!" (
        ren "%%i" "!newName!"
        echo Renamed "%%i" to "!newName!"
    )
)

powershell -noprofile -command "Compress-Archive -Path '.\GradingAnalytics_SourceData' -DestinationPath '.\GradingAnalytics_SourceData.zip'"


endlocal