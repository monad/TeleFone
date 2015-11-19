dll:
	fsharpc TeleFone.fs --target:library -o lib/TeleFone.dll -r lib/FSharp.Data.dll -r lib/Zlib.Portable.dll -r lib/FSharp.Data.DesignTime.dll 
	@echo "done"

client:
	fsharpc example/Main.fs -o example/main.exe -r lib/TeleFone.dll -I lib
	@echo "done"
