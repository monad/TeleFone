all:
	fsharpc Main.fs -o main.exe -r lib/FSharp.Data.dll

run:
	@fsharpc Main.fs -o main.exe -r lib/FSharp.Data.dll
	mono main.exe
