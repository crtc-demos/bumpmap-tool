#!/bin/sh
ocamlc -g -I +camlimages camlimages_core.cma camlimages_all.cma bumpmap.ml -o bumpmap
