all: Depth Dive

Depth:
	ghc -o build/DepthCount -odir tmp -hidir tmp src/DepthCount.hs
	rm tmp/*

Dive:
	ghc -o build/Dive -odir tmp -hidir tmp src/Dive.hs
	rm tmp/*

Diagnose:
	ghc -o build/Diagnose -odir tmp -hidir tmp src/Diagnose.hs
	rm tmp/*