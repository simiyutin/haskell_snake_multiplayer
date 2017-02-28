EXECNAME="SnakeServer"

##
# USAGE
# ./devrun.sh all -- compile, start server and client
# ./devrun.sh -- compile, start ONLY server
# ./devrun.sh client -- start ONLY client
##

compile()
{
	echo -e "\e[93mCompiling...\e[0m"
	rm -f "$EXECNAME"

	echo -e "\e[44m"
	ghc -o "$EXECNAME" Main.hs
	GHCEXIT_CODE=$?
	echo -e "\e[0m"

	if [[ "$GHCEXIT_CODE" != "0" ]]
	then
	    echo -e "\e[31mCompilation failed\e[0m"
	    exit
	fi
}

kill_server_client()
{
	echo -e "\e[93mKill all servers and telnet clients...\e[0m"
	killall "$EXECNAME" &> /dev/null
	killall telnet &> /dev/null
}

run_server_async()
{
	echo -e "\e[93mRunning server...\e[0m"
	./"$EXECNAME"&
}

run_server_sync()
{
	echo -e "\e[93mRunning server...\e[0m"
	./"$EXECNAME"
}

run_client()
{
	telnet 127.0.0.1 4242
}

case "$1" in
	""|"server")
		compile
                kill_server_client
		run_server_async
		;;
	"client")
		run_client
		;;
        "kill")
		kill_server_client
		;;
        "compile")
		compile
		;;
        "run")
                kill_server_client
		run_server_sync
		;;
	"all")
		compile
                kill_server_client
		run_server_async
		run_client
		;;
	*)
		echo "lolwut?"
		exit
		;;
esac
