#! /usr/bin/env bash
doas cp **/*.ttf /usr/share/fonts/truetype/

doas fc-cache -f -v

