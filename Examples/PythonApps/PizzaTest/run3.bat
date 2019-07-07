@set OPS_PATH=%~dp0\..\..\..\Python
@set GEN_PATH=%~dp0\..\..\OPSIdls\PizzaProject\Generated\Python
@set GEN_PATH2=%GEN_PATH%\PizzaProject
@set SAVED_PP=%PYHONPATH%
@set PYTHONPATH=%OPS_PATH%;%GEN_PATH%;%GEN_PATH2%;%PYTHONPATH%
@echo %PYTHONPATH%
@"D:\Program Files (x86)\Microsoft Visual Studio\Shared\Python36_64\python.exe" OPS_Test.py
@set PYTHONPATH=%SAVED_PP%
