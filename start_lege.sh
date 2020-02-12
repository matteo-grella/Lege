#!/bin/bash

if [ -f lege-img.mem ]; then
	sbcl --dynamic-space-size 5120 --core lege-img.mem
else
	sbcl --dynamic-space-size 5120 --load lege --eval '(start)'

	if [ -f lege-img.mem ]; then
		sbcl --dynamic-space-size 5120 --core lege-img.mem
	fi
fi
