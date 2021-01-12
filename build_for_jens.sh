#/bin/bash

lazbuild -B epidatamanager.lpi
strip epidatamanager

lazbuild -B --bm=Win64 epidatamanager.lpi
strip epidatamanager.exe

