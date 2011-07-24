#!/bin/sh
ocamlopt -g -I +camlimages camlimages_core.cmxa camlimages_all.cmxa bumpmap.ml -o bumpmap
