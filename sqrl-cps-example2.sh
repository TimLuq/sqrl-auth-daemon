#!/bin/bash
# rename this to ~/.sqrl/bin/sqrl-cps

function startsqrl { # executed when no client is running
  # run sqrl.exe using wine
  nohup wine sqrl.exe /port $1 &> /dev/null &
}
function sqrldone { # when startsqrl was run and everything is done
  message=" There is nothing to do here "
}
  

## parse command line arguments
declare -A args
for i in {1..$#}; do
  case "${$i}" in
  --*)
    tmp="${$i:2}"
    args[${tmp%%=*}]="${tmp#*=}"
    ;;
  esac
done

printf "SQRLCPS\0\x03"

# try to get unique session port
displaysess="${DISPLAY#*:}"
displaysess="${displaysess%.*}"
if [ "$displaysess" == "" ]; then
  # this will most likely never work since there is nowhere
  # to actually display the SQRL client.
  # an alternative browser/CLI client should be called instead
  port="$((25800 + $(id -u)))" 
else
  port="$((23800 + $displaysess))"
fi

## find out if the client is running - if not then try to start it
lsof -ti tcp:$port &> /dev/null
res=$?
sqrlstarted=0
if [ $res == 1 ]; then
  #nothing is listening to the port so start sqrl.exe using wine
  sqrlstarted=1
  startsqrl $port
  sleep "0.3" # wait to give the client time to start
elif [ $res != 0 ]; then
  printf "HTTP/1.1 500 Internal Server Error\r\n"
  printf "Connection: close\r\n"
  printf "Content-Type: text/plain\r\n"
  printf "\r\nUnexpected result while querying port $port"
  exit 1
fi

## relay the CPS request to the users client
curl --include -A "sqrl-cps-bash/0.1" -e "${args[cps-referer]}" \
 "http://127.0.0.1:$port${args[cps-path-info]}${args[cps-query-string]}"
res=$?

if [ $sqrlstarted == 1 ]; then
  sqrldone $port
fi

exit $res
