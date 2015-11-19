dll:
	fsharpc TeleFone.fs --target:library -o lib/TeleFone.dll -r lib/FSharp.Data.dll -r lib/Zlib.Portable.dll -r lib/FSharp.Data.DesignTime.dll 

example:
	fsharpc example/Main.fs -o example/main.exe -I lib
