@pushd %~dp0
@echo Building IDL Templates ...
@IF NOT EXIST dist (
	mkdir dist
)

@jar cf "dist/IDLTemplates.jar" -C "src/ops/netbeansmodules/idlsupport" templates
@popd
