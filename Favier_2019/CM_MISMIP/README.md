# Configuration Manager (CM) to launch stand-alone Elmer/Ice idealized simulations.

##  contributors: 
* Lionel Favier (IGE/UGA/CNRS)
* Ignacio Merino (IGE/UGA/CNRS)
* Nicolas Jourdain (IGE/UGA/CNRS)

---------------------------------------

## Creating a new experiment :

First, edit ```createRUN.sh``` and adapt parameters:
```shell
vi createRUN.sh
```

You need 4 arguments to execute ```createRUN.sh```:
1. name of experiment (recommended to contain arg2, arg3, arg4 for clarity)
2. tuning parameter (used to adjust total melt)
3. name of the T,S scenario: 'EXPx' with x in {4,3,10,11,12,13,20,21,22,23}
4. name of the parameterization, in {bg,pdc,pdcstar,lazer,pico}

Example : 
```shell
./createRUN.sh NMP_EXP22LAZER6c0.65 0.65 'EXP22' 'lazer'
```

**NB:** The steps that are described above use the depth-dependant temperature and salinity. If instead you want to use a fixed depth for ambiant temperature and salinity, it is recommended to use the following alternative:

Proceed as previously described but use ```createRUN_Za.sh```instead of ```createRUN.sh```. There are 5 arguments instead of 4:
1. name of experiment (recommended to contain arg2, arg3, arg4 for clarity)
2. tuning parameter (used to adjust total melt)
3. Za (depth for ambiant T,S, in meter)
4. name of the T,S scenario: 'EXPx' with x in {4,3,10,11,12,13,20,21,22,23}
5. name of the parameterization, in {bg,pdc,pdcstar,lazer,pico}

Example:
```shell
./createRUN_Za.sh NMP_EXP10BGZa500c1.06 1.06 500.0 'EXP10' 'bg'
```
