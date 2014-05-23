#!/bin/sh
exec xterm -geometry 132x30 -sb -title RIETAN -e `dirname "$0"`/rietan_cmd.sh $*