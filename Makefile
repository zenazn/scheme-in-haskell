# Our handy-dandy Makefile. Update this if the build process changes
# It spits out a .o file so it can be ignored in .gitignore
parser : Parser.hs
	ghc -o scheme --make Parser.hs