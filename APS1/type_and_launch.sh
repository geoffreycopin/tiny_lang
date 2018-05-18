#! /bin/bash

if [[ $(cat $1 | ./toProlog | swipl --quiet Typage/typechecker.pl)  = "ok" ]]
then
    echo "Le programme est correctement typé !"
    echo "Exécution:"
   cat $1 | ./eval
else
    echo "Le programme n'est pas correctement typé !"
fi
