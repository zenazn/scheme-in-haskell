# Our handy-dandy Makefile. Update this if the build process changes
parser : Parser.hs
	ghc -o parser --make Parser.hs