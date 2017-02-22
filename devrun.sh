EXECNAME="SnakeServer"
killall "$EXECNAME" &> /dev/null
ghc -o "$EXECNAME" Main.hs
./"$EXECNAME"