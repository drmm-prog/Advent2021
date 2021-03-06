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

Venture:
	ghc -o build/Venture -odir tmp -hidir tmp src/Venture.hs
	rm tmp/*

Lantern:
	ghc -o build/Lantern -odir tmp -hidir tmp src/Lantern.hs
	rm tmp/*

Whale:
	ghc -o build/Whale -odir tmp -hidir tmp src/Whale.hs
	rm tmp/*

Display:
	ghc -o build/Display -odir tmp -hidir tmp src/Display.hs
	rm tmp/*