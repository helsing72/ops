@set OPS_PATH=%~dp0\..\..\..\Python
@set GEN_PATH=%~dp0\..\..\OPSIdls\TestAll\Generated\Python
@set GEN_PATH2=%GEN_PATH%\TestAll
@set SAVED_PP=%PYHONPATH%
@set PYTHONPATH=%OPS_PATH%;%GEN_PATH%;%GEN_PATH2%;%PYTHONPATH%
@python VerifySerDes.py
@set PYTHONPATH=%SAVED_PP%
