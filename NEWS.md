# stp25aggregare 0.2.7

Neue Test-Funktionen.

## New functions

* `clean_names()`  Sonderzeichen wie z.B. aus dem Import von Excel-Files entfernen.
* `Filter2()`  copie of dplyr::filter(...) 
* `Select_case()` und `subset2()` copie of base::subset(...)
* `Drop_case()`  data[-subset,]
* `Drop_NA()`  copie of dplyr::filter(data, complete.cases(...)) 

 
## Changes to functions

* `Label()` komplet überarbeitet.
* `get_label()` kann optional Units verwenden. `GetLabelOrName()` ist jetzt eine Kopie von `get_label()` und option pattern wurde eliminiert. neue Parameter include.unit und include.names
* `GetData()` Verwendet jetzt readr::read_csv fuer csv
* `Recast2()` ist jetzt eine Variante von `Summarise()`








# stp25aggregare 0.2.1

## New functions
* `Label()` 
* `delet_labels()`  
* `set_label()` 
* `get_label()` ohne Units  `GetLabelOrName()` mit Units
* `as_numeric()` 



# stp25aggregare 0.2.0

## New functions
* `transform2()` Copie of transform()
* `mutate2()`  Copie of mutate()
* `Summarise()` 
* `Wide()`  
* `Long()` 
 
 
## Changes to functions
* `GetData()` speichert jetz automatisch die Rohdaten.
 


# stp25aggregare 0.0.1

## General

* Collection of convenient functions for common statistical computations.

## New functions

* `GetData()` 
* `Melt2()`  
* `Recast()`  
* `XLS()` 
* `Cs2()`  
* `Label()`  
* `upData2()` 
* `CleanUp()`,  `CleanUp_factor`,   `cleanup_names_encoding`
* `dapply2()`  

