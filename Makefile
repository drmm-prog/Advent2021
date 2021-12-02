all: Depth Dive

Depth:
	ghc -o build/DepthCount -odir tmp -hidir tmp src/DepthCount.hs
	rm tmp/*

Dive:
	ghc -o build/Dive -odir tmp -hidir tmp src/Dive.hs
	rm tmp/*