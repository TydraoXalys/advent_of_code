#!/bin/bash

# Vérification du nombre de paramètres
if [ "$#" -ne 1 ]; then
    echo "Usage: $0 <chiffre>"
    exit 1
fi

# Récupération du chiffre passé en paramètre
day="$1"

# Construction du nom du fichier Python
file="day$day/day$day.py"

# Vérification si le fichier Python existe
if [ ! -f "$file" ]; then
    echo "File \"$file\" not found"
    exit 1
fi

# Exécution du fichier Python
python3 $file