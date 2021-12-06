all: Depth Dive Diagnose Bingo

Depth:
	ghc -o build/DepthCount -odir tmp -hidir tmp src/DepthCount.hs
	rm tmp/*

Dive:
	ghc -o build/Dive -odir tmp -hidir tmp src/Dive.hs
	rm tmp/*

Diagnose:
	ghc -o build/Diagnose -odir tmp -hidir tmp src/Diagnose.hs
	rm tmp/*

Bingo:
	ghc -o build/Bingo -odir tmp -hidir tmp src/Bingo.hs
	rm tmp/*