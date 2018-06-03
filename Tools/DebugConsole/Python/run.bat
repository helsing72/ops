@set OPS_PATH=%~dp0\..\..\..\Python
@set GEN_PATH=%~dp0\..\..\..\Common\idl\Generated\Python
@set GEN_PATH2=%GEN_PATH%\opsidls
@set GEN_PATH3=%~dp0\..\..\..\Examples\OPSIdls\PizzaProject\Generated\Python
@set GEN_PATH4=%GEN_PATH3%\PizzaProject
@set SAVED_PP=%PYHONPATH%
@set PYTHONPATH=%OPS_PATH%;%GEN_PATH%;%GEN_PATH2%;%GEN_PATH3%;%GENPATH4%;%PYTHONPATH%
@python ops_debug_console.py %*
@set PYTHONPATH=%SAVED_PP%
