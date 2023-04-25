@echo OFF

SET CONDAPATH=E:\Anaconda
SET ENVNAME=project_name

if %ENVNAME%==base (set ENVPATH=%CONDAPATH%) else (set ENVPATH=%CONDAPATH%\envs\%ENVNAME%)

call %CONDAPATH%\Scripts\activate.bat %ENVPATH%

python path_name\notify.py 
Rscript.exe path_name\parse_data.R 2> path_name\log.txt

call conda deactivate