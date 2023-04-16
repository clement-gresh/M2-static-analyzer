# Projet d'analyseur statique du cours TAS - Rapport de projet

Auteur(s) : 
Nawres ABED
Clément GRESH
M2 STL-INSTA
Sorbonne Université

Partie obligatoire : faîtes entièremement, tous les tests passent avec succès

Extension choisie : complétion disjonctive appliquée au domaine des intervalles
- On définit d'abord le module Disjunctions qui respecte la signature "Domain"
- Disjunctions peut être appliqué à un autre module respectant lui-même la signature "Domain"
- Dans le main, on définit le module DisjunctionIntervalsAnalysis qui est la disjonction appliquée au domaine des intervalles
- L'évaluation de DisjunctionIntervalsAnalysis peut être faite grâce au paramètre "-disjunction-interval"
- le module Disjunctions pourrait être appliqués et tester sur d'autres domaines tels que le domaine des constantes ou des signes

Tests pour les disjonctions sur le domaine des intervalles :
- tests basiques (010x : déclaration, initialisation, rand)
- tests sur les opérateurs arithmétiques (020x : neg, add, sub)
- tests avec des assertions (030x : false, true, include)
- tests sur les boucles (040x : boucles finies et infinies)
