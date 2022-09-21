#!/bin/sh

#whey can work like kbs_main(args..)
swipl -s kbs.pl -g kbs_main -t halt -- $@
